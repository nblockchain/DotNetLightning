namespace DotNetLightning.Channel

open NBitcoin

open DotNetLightning.Utils
open DotNetLightning.Transactions
open DotNetLightning.Crypto
open DotNetLightning.Chain
open DotNetLightning.Serialization.Msgs

open ResultUtils
open ResultUtils.Portability

[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module internal Commitments =
    module private Helpers =
        let isAlreadySent (htlc: UpdateAddHTLCMsg) (proposed: IUpdateMsg list) =
            proposed
            |> List.exists(fun p -> match p with
                                    | :? UpdateFulfillHTLCMsg as u -> u.HTLCId = htlc.HTLCId
                                    | :? UpdateFailHTLCMsg as u -> u.HTLCId = htlc.HTLCId
                                    | :? UpdateFailMalformedHTLCMsg as u -> u.HTLCId = htlc.HTLCId
                                    | _ -> false)

        let makeRemoteTxs
            (staticChannelConfig: StaticChannelConfig)
            (commitTxNumber: CommitmentNumber)
            (localChannelPubKeys: ChannelPubKeys)
            (remotePerCommitmentPoint: PerCommitmentPoint)
            (spec: CommitmentSpec) =
            let localCommitmentPubKeys = remotePerCommitmentPoint.DeriveCommitmentPubKeys localChannelPubKeys
            let remoteCommitmentPubKeys = remotePerCommitmentPoint.DeriveCommitmentPubKeys staticChannelConfig.RemoteChannelPubKeys
            let commitTx =
                Transactions.makeCommitTx staticChannelConfig.FundingScriptCoin
                                          commitTxNumber
                                          staticChannelConfig.RemoteChannelPubKeys.PaymentBasepoint
                                          localChannelPubKeys.PaymentBasepoint
                                          (not staticChannelConfig.IsFunder)
                                          (staticChannelConfig.RemoteParams.DustLimitSatoshis)
                                          localCommitmentPubKeys.RevocationPubKey
                                          staticChannelConfig.LocalParams.ToSelfDelay
                                          remoteCommitmentPubKeys.DelayedPaymentPubKey
                                          localCommitmentPubKeys.PaymentPubKey
                                          remoteCommitmentPubKeys.HtlcPubKey
                                          localCommitmentPubKeys.HtlcPubKey
                                          spec
                                          staticChannelConfig.Network
            result {
                 let! (htlcTimeoutTxs, htlcSuccessTxs) =
                     Transactions.makeHTLCTxs
                         (commitTx.Value.GetGlobalTransaction())
                         (staticChannelConfig.RemoteParams.DustLimitSatoshis)
                         localCommitmentPubKeys.RevocationPubKey
                         (staticChannelConfig.LocalParams.ToSelfDelay)
                         remoteCommitmentPubKeys.DelayedPaymentPubKey
                         remoteCommitmentPubKeys.HtlcPubKey
                         localCommitmentPubKeys.HtlcPubKey
                         spec
                         staticChannelConfig.Network
                return (commitTx, htlcTimeoutTxs, htlcSuccessTxs)
            }

        let makeLocalTXs
            (staticChannelConfig: StaticChannelConfig)
            (commitTxNumber: CommitmentNumber)
            (localChannelPubKeys: ChannelPubKeys)
            (localPerCommitmentPoint: PerCommitmentPoint)
            (spec: CommitmentSpec)
                : Result<(CommitTx * HTLCTimeoutTx list * HTLCSuccessTx list), _> =
            let localCommitmentPubKeys = localPerCommitmentPoint.DeriveCommitmentPubKeys localChannelPubKeys
            let remoteCommitmentPubKeys = localPerCommitmentPoint.DeriveCommitmentPubKeys staticChannelConfig.RemoteChannelPubKeys

            let commitTx =
                Transactions.makeCommitTx staticChannelConfig.FundingScriptCoin
                                          commitTxNumber
                                          localChannelPubKeys.PaymentBasepoint
                                          staticChannelConfig.RemoteChannelPubKeys.PaymentBasepoint
                                          staticChannelConfig.IsFunder
                                          staticChannelConfig.LocalParams.DustLimitSatoshis
                                          remoteCommitmentPubKeys.RevocationPubKey
                                          staticChannelConfig.RemoteParams.ToSelfDelay
                                          localCommitmentPubKeys.DelayedPaymentPubKey
                                          remoteCommitmentPubKeys.PaymentPubKey
                                          localCommitmentPubKeys.HtlcPubKey
                                          remoteCommitmentPubKeys.HtlcPubKey
                                          spec
                                          staticChannelConfig.Network
            result {
                let! (htlcTimeoutTxs, htlcSuccessTxs) =
                    Transactions.makeHTLCTxs (commitTx.Value.GetGlobalTransaction())
                                             (staticChannelConfig.LocalParams.DustLimitSatoshis)
                                             remoteCommitmentPubKeys.RevocationPubKey
                                             staticChannelConfig.RemoteParams.ToSelfDelay
                                             localCommitmentPubKeys.DelayedPaymentPubKey
                                             localCommitmentPubKeys.HtlcPubKey
                                             remoteCommitmentPubKeys.HtlcPubKey
                                             (spec)
                                             staticChannelConfig.Network
                return (commitTx, htlcTimeoutTxs, htlcSuccessTxs)
            }


        let sortBothHTLCs (htlcTimeoutTxs: HTLCTimeoutTx list) (htlcSuccessTxs: HTLCSuccessTx list) =
            let timeoutTXsV = (htlcTimeoutTxs |> Seq.cast<IHTLCTx>)
            let successTXsV = (htlcSuccessTxs |> Seq.cast<IHTLCTx>)
            Seq.append timeoutTXsV successTXsV
            |> List.ofSeq
            |> List.sortBy(fun htlc -> htlc.Value.GetGlobalTransaction().Inputs.[htlc.WhichInput].PrevOut.N)

        let checkUpdateFee (channelOptions: ChannelOptions)
                           (msg: UpdateFeeMsg)
                           (localFeeRate: FeeRatePerKw) =
            let maxMismatch = channelOptions.MaxFeeRateMismatchRatio
            UpdateFeeValidation.checkFeeDiffTooHigh (msg) (localFeeRate) (maxMismatch)

    let sendFulfill (op: OperationFulfillHTLC)
                    (cm: Commitments)
                    (staticChannelConfig: StaticChannelConfig)
                    (remoteNextCommitInfo: RemoteNextCommitInfo) =
        match cm.GetHTLCCrossSigned remoteNextCommitInfo Direction.In op.Id with
        | Some htlc when (cm.LocalChanges.Proposed |> Helpers.isAlreadySent htlc) ->
            htlc.HTLCId |> htlcAlreadySent
        | Some htlc when (htlc.PaymentHash = op.PaymentPreimage.Hash) ->
            let msgToSend: UpdateFulfillHTLCMsg = {
                ChannelId = staticChannelConfig.ChannelId()
                HTLCId = op.Id
                PaymentPreimage = op.PaymentPreimage
            }
            let newCommitments = cm.AddLocalProposal(msgToSend)
            (msgToSend, newCommitments) |> Ok
        | Some htlc ->
            (htlc.PaymentHash, op.PaymentPreimage)
            |> invalidPaymentPreimage
        | None ->
            op.Id
            |> unknownHTLCId

    let receiveFulfill (msg: UpdateFulfillHTLCMsg)
                       (cm: Commitments)
                       (remoteNextCommitInfo: RemoteNextCommitInfo) =
        match cm.GetHTLCCrossSigned remoteNextCommitInfo Direction.Out msg.HTLCId with
        | Some htlc when htlc.PaymentHash = msg.PaymentPreimage.Hash ->
            let commitments = cm.AddRemoteProposal(msg)
            commitments |> Ok
        | Some htlc ->
            (htlc.PaymentHash, msg.PaymentPreimage)
            |> invalidPaymentPreimage
        | None ->
            msg.HTLCId
            |> unknownHTLCId

    let sendFail (nodeSecret: NodeSecret)
                 (op: OperationFailHTLC)
                 (cm: Commitments)
                 (staticChannelConfig: StaticChannelConfig)
                 (remoteNextCommitInfo: RemoteNextCommitInfo) =
        match cm.GetHTLCCrossSigned remoteNextCommitInfo Direction.In op.Id with
        | Some htlc when  (cm.LocalChanges.Proposed |> Helpers.isAlreadySent htlc) ->
            htlc.HTLCId |> htlcAlreadySent
        | Some htlc ->
            let ad = htlc.PaymentHash.ToBytes()
            let rawPacket = htlc.OnionRoutingPacket.ToBytes()
            Sphinx.parsePacket (nodeSecret.RawKey()) ad rawPacket
            |> Result.mapError(ChannelError.CryptoError)
            >>= fun ({ SharedSecret = ss}) ->
                let reason =
                    op.Reason
                    |> function Choice1Of2 b -> Sphinx.forwardErrorPacket(b, ss) | Choice2Of2 f -> Sphinx.ErrorPacket.Create(ss, f)
                let f = {
                    UpdateFailHTLCMsg.ChannelId = staticChannelConfig.ChannelId()
                    HTLCId = op.Id
                    Reason = { Data = reason }
                }
                let nextCommitments = cm.AddLocalProposal(f)
                Ok (f, nextCommitments)
        | None ->
            op.Id |> unknownHTLCId

    let receiveFail (msg: UpdateFailHTLCMsg)
                    (cm: Commitments)
                    (remoteNextCommitInfo: RemoteNextCommitInfo) =
        match cm.GetHTLCCrossSigned remoteNextCommitInfo Direction.Out msg.HTLCId with
        | Some _htlc ->
            result {
                let! _origin =
                    match cm.OriginChannels.TryGetValue(msg.HTLCId) with
                    | true, origin -> Ok origin
                    | false, _ ->
                        msg.HTLCId |> htlcOriginNowKnown
                let nextC = cm.AddRemoteProposal(msg)
                return nextC
            }
        | None ->
            msg.HTLCId |> unknownHTLCId


    let sendFailMalformed (op: OperationFailMalformedHTLC)
                          (cm: Commitments)
                          (staticChannelConfig: StaticChannelConfig)
                          (remoteNextCommitInfo: RemoteNextCommitInfo) =
        // BADONION bit must be set in failure code
        if (op.FailureCode.Value &&& OnionError.BADONION) = 0us then
            op.FailureCode |> invalidFailureCode
        else
            match cm.GetHTLCCrossSigned remoteNextCommitInfo Direction.In op.Id with
            | Some htlc when (cm.LocalChanges.Proposed |> Helpers.isAlreadySent htlc) ->
                htlc.HTLCId |> htlcAlreadySent
            | Some _htlc ->
                let msg = {
                    UpdateFailMalformedHTLCMsg.ChannelId = staticChannelConfig.ChannelId()
                    HTLCId = op.Id
                    Sha256OfOnion = op.Sha256OfOnion
                    FailureCode = op.FailureCode
                }
                let nextCommitments = cm.AddLocalProposal(msg)
                Ok (msg, nextCommitments)
            | None ->
                op.Id |> unknownHTLCId

    let receiveFailMalformed (msg: UpdateFailMalformedHTLCMsg)
                             (cm: Commitments)
                             (remoteNextCommitInfo: RemoteNextCommitInfo) =
        if msg.FailureCode.Value &&& OnionError.BADONION = 0us then
            msg.FailureCode |> invalidFailureCode
        else
            match cm.GetHTLCCrossSigned remoteNextCommitInfo Direction.Out msg.HTLCId with
            | Some _htlc ->
                result {
                    let! _origin =
                        match cm.OriginChannels.TryGetValue(msg.HTLCId) with
                        | true, o -> Ok o
                        | false, _ ->
                            msg.HTLCId |> htlcOriginNowKnown
                    let nextC = cm.AddRemoteProposal(msg)
                    return nextC
                }
            | None ->
                msg.HTLCId |> unknownHTLCId

    let sendFee (op: OperationUpdateFee)
                (staticChannelConfig: StaticChannelConfig)
                (cm: Commitments) =
            if (not staticChannelConfig.IsFunder) then
                "Local is Fundee so it cannot send update fee" |> apiMisuse
            else
                let fee = {
                    UpdateFeeMsg.ChannelId = staticChannelConfig.ChannelId()
                    FeeRatePerKw = op.FeeRatePerKw
                }
                let c1 = cm.AddLocalProposal(fee)
                result {
                    let! reduced =
                        c1.RemoteCommit.Spec.Reduce(c1.RemoteChanges.ACKed, c1.LocalChanges.Proposed) |> expectTransactionError
                    // A node cannot spend pending incoming htlcs, and need to keep funds above the reserve required by
                    // the counter party, after paying the fee, we look from remote's point of view, so if local is funder
                    // remote doesn't pay the fees.
                    let fees = Transactions.commitTxFee(staticChannelConfig.RemoteParams.DustLimitSatoshis) reduced
                    let missing = reduced.ToRemote.ToMoney() - staticChannelConfig.RemoteParams.ChannelReserveSatoshis - fees
                    if (missing < Money.Zero) then
                        return!
                            (staticChannelConfig.LocalParams.ChannelReserveSatoshis, fees,  (-1 * missing))
                            |> cannotAffordFee
                    else
                        return fee, c1
                }

    let receiveFee (channelOptions: ChannelOptions)
                   (localFeerate)
                   (msg: UpdateFeeMsg)
                   (staticChannelConfig: StaticChannelConfig)
                   (cm: Commitments) =
        if staticChannelConfig.IsFunder then
            "Remote is Fundee so it cannot send update fee" |> apiMisuse
        else
            result {
                do! Helpers.checkUpdateFee channelOptions msg localFeerate
                let nextCommitments = cm.AddRemoteProposal(msg)
                let! reduced =
                    nextCommitments.LocalCommit.Spec.Reduce(
                        nextCommitments.LocalChanges.ACKed,
                        nextCommitments.RemoteChanges.Proposed
                    ) |> expectTransactionError
                
                let fees = Transactions.commitTxFee(staticChannelConfig.RemoteParams.DustLimitSatoshis) reduced
                let missing = reduced.ToRemote.ToMoney() - staticChannelConfig.RemoteParams.ChannelReserveSatoshis - fees
                if (missing < Money.Zero) then
                    return!
                        (staticChannelConfig.LocalParams.ChannelReserveSatoshis, fees,  (-1 * missing))
                        |> cannotAffordFee
                else
                    return nextCommitments
            }

    let sendCommit (channelPrivKeys: ChannelPrivKeys)
                   (cm: Commitments)
                   (staticChannelConfig: StaticChannelConfig)
                   (remoteNextCommitInfo: RemoteNextCommitInfo) =
        match remoteNextCommitInfo with
        | RemoteNextCommitInfo.Revoked remoteNextPerCommitmentPoint ->
            result {
                // remote commitment will include all local changes + remote acked changes
                let! spec = cm.RemoteCommit.Spec.Reduce(cm.RemoteChanges.ACKed, cm.LocalChanges.Proposed) |> expectTransactionError
                let! (remoteCommitTx, htlcTimeoutTxs, htlcSuccessTxs) =
                    Helpers.makeRemoteTxs staticChannelConfig
                                          (cm.RemoteCommit.Index.NextCommitment())
                                          (channelPrivKeys.ToChannelPubKeys())
                                          (remoteNextPerCommitmentPoint)
                                          (spec)
                    |> expectTransactionErrors
                let signature,_ = channelPrivKeys.SignWithFundingPrivKey remoteCommitTx.Value
                let sortedHTLCTXs = Helpers.sortBothHTLCs htlcTimeoutTxs htlcSuccessTxs
                let htlcSigs =
                    sortedHTLCTXs
                    |> List.map(
                            (fun htlc -> channelPrivKeys.SignHtlcTx htlc.Value remoteNextPerCommitmentPoint)
                            >> fst
                            >> (fun txSig -> txSig.Signature)
                            )
                let msg = {
                    CommitmentSignedMsg.ChannelId = staticChannelConfig.ChannelId()
                    Signature = !> signature.Signature
                    HTLCSignatures = htlcSigs |> List.map (!>)
                }
                let nextRemoteCommitInfo = {
                    cm.RemoteCommit
                    with
                        Index = cm.RemoteCommit.Index.NextCommitment()
                        Spec = spec
                        RemotePerCommitmentPoint = remoteNextPerCommitmentPoint
                }
                let nextCommitments = {
                    cm with
                        LocalChanges = {
                            cm.LocalChanges with
                                Proposed = []
                                Signed = cm.LocalChanges.Proposed
                        }
                        RemoteChanges = {
                            cm.RemoteChanges with
                                ACKed = []
                                Signed = cm.RemoteChanges.ACKed
                        }
                }
                return msg, nextCommitments, nextRemoteCommitInfo
            }
        | RemoteNextCommitInfo.Waiting _ ->
            CanNotSignBeforeRevocation |> Error

    let private checkSignatureCountMismatch(sortedHTLCTXs: IHTLCTx list) (msg) =
        if (sortedHTLCTXs.Length <> msg.HTLCSignatures.Length) then
            signatureCountMismatch (sortedHTLCTXs.Length, msg.HTLCSignatures.Length)
        else
            Ok()
    let receiveCommit (channelPrivKeys: ChannelPrivKeys)
                      (msg: CommitmentSignedMsg)
                      (staticChannelConfig: StaticChannelConfig)
                      (cm: Commitments)
                          : Result<RevokeAndACKMsg * Commitments, ChannelError> =
        if cm.RemoteHasChanges() |> not then
            ReceivedCommitmentSignedWhenWeHaveNoPendingChanges |> Error
        else
            let commitmentSeed = channelPrivKeys.CommitmentSeed
            let localChannelKeys = channelPrivKeys.ToChannelPubKeys()
            let remoteChannelKeys = staticChannelConfig.RemoteChannelPubKeys
            let nextI = cm.LocalCommit.Index.NextCommitment()
            result {
                let! spec = cm.LocalCommit.Spec.Reduce(cm.LocalChanges.ACKed, cm.RemoteChanges.Proposed) |> expectTransactionError
                let localPerCommitmentPoint = commitmentSeed.DerivePerCommitmentPoint nextI
                let! (localCommitTx, htlcTimeoutTxs, htlcSuccessTxs) =
                    Helpers.makeLocalTXs
                        staticChannelConfig
                        nextI
                        (channelPrivKeys.ToChannelPubKeys())
                        localPerCommitmentPoint
                        spec
                    |> expectTransactionErrors
                let signature, signedCommitTx = channelPrivKeys.SignWithFundingPrivKey localCommitTx.Value

                let sigPair =
                    let localSigPair = seq [(localChannelKeys.FundingPubKey.RawPubKey(), signature)]
                    let remoteSigPair = seq[ (remoteChannelKeys.FundingPubKey.RawPubKey(), TransactionSignature(msg.Signature.Value, SigHash.All)) ]
                    Seq.append localSigPair remoteSigPair
                let tmp =
                    Transactions.checkTxFinalized signedCommitTx localCommitTx.WhichInput sigPair
                    |> expectTransactionError
                let! finalizedCommitTx = tmp
                let sortedHTLCTXs = Helpers.sortBothHTLCs htlcTimeoutTxs htlcSuccessTxs
                do! checkSignatureCountMismatch sortedHTLCTXs msg
                
                let _localHTLCSigs, sortedHTLCTXs =
                    let localHtlcSigsAndHTLCTxs =
                        sortedHTLCTXs |> List.map(fun htlc ->
                            channelPrivKeys.SignHtlcTx htlc.Value localPerCommitmentPoint
                        )
                    localHtlcSigsAndHTLCTxs |> List.map(fst), localHtlcSigsAndHTLCTxs |> List.map(snd) |> Seq.cast<IHTLCTx> |> List.ofSeq

                let remoteHTLCPubKey = localPerCommitmentPoint.DeriveHtlcPubKey remoteChannelKeys.HtlcBasepoint

                let checkHTLCSig (htlc: IHTLCTx, remoteECDSASig: LNECDSASignature): Result<_, _> =
                    let remoteS = TransactionSignature(remoteECDSASig.Value, SigHash.All)
                    match htlc with
                    | :? HTLCTimeoutTx ->
                        (Transactions.checkTxFinalized (htlc.Value) (0) (seq [(remoteHTLCPubKey.RawPubKey(), remoteS)]))
                        |> Result.map(box)
                    // we cannot check that htlc-success tx are spendable because we need the payment preimage; thus we only check the remote sig
                    | :? HTLCSuccessTx ->
                        (Transactions.checkSigAndAdd (htlc) (remoteS) (remoteHTLCPubKey.RawPubKey()))
                        |> Result.map(box)
                    | _ -> failwith "Unreachable!"

                let! txList =
                    List.zip sortedHTLCTXs msg.HTLCSignatures
                    |> List.map(checkHTLCSig)
                    |> List.sequenceResultA
                    |> expectTransactionErrors
                let successTxs =
                    txList |> List.choose(fun o ->
                        match o with
                        | :? HTLCSuccessTx as tx -> Some tx
                        | _ -> None
                    )
                let finalizedTxs =
                    txList |> List.choose(fun o ->
                        match o with
                        | :? FinalizedTx as tx -> Some tx
                        | _ -> None
                    )
                let localPerCommitmentSecret =
                    channelPrivKeys.CommitmentSeed.DerivePerCommitmentSecret cm.LocalCommit.Index
                let localNextPerCommitmentPoint =
                    let perCommitmentSecret =
                        channelPrivKeys.CommitmentSeed.DerivePerCommitmentSecret
                            (cm.LocalCommit.Index.NextCommitment().NextCommitment())
                    perCommitmentSecret.PerCommitmentPoint()

                let nextMsg = {
                    RevokeAndACKMsg.ChannelId = staticChannelConfig.ChannelId()
                    PerCommitmentSecret = localPerCommitmentSecret
                    NextPerCommitmentPoint = localNextPerCommitmentPoint
                }
                
                let nextCommitments =
                    let localCommit1 = { LocalCommit.Index = cm.LocalCommit.Index.NextCommitment()
                                         Spec = spec
                                         PublishableTxs = { PublishableTxs.CommitTx = finalizedCommitTx
                                                            HTLCTxs = finalizedTxs }
                                         PendingHTLCSuccessTxs = successTxs }
                    let ourChanges1 = { cm.LocalChanges with ACKed = []}
                    let theirChanges1 = { cm.RemoteChanges with Proposed = []; ACKed = (cm.RemoteChanges.ACKed @ cm.RemoteChanges.Proposed) }
                    let completedOutgoingHTLCs =
                        let t1 = cm.LocalCommit.Spec.HTLCs
                                 |> Map.filter(fun _ v -> v.Direction = Out)
                                 |> Map.toSeq |> Seq.map (fun (k, _) -> k) |> Set.ofSeq
                        let t2 = localCommit1.Spec.HTLCs |> Map.filter(fun _ v -> v.Direction = Out)
                                 |> Map.toSeq |> Seq.map (fun (k, _) -> k) |> Set.ofSeq
                        Set.difference t1 t2
                    let originChannels1 = cm.OriginChannels |> Map.filter(fun k _ -> Set.contains k completedOutgoingHTLCs)
                    { cm with LocalCommit = localCommit1
                              LocalChanges = ourChanges1
                              RemoteChanges = theirChanges1
                              OriginChannels = originChannels1 }
                return nextMsg, nextCommitments
            }

module RemoteForceClose =
    // The lightning spec specifies that commitment txs use version 2 bitcoin transactions.
    let TxVersionNumberOfCommitmentTxs = 2u

    let check(thing: bool): Option<unit> =
        if thing then
            Some ()
        else
            None

    let tryGetObscuredCommitmentNumber (fundingOutPoint: OutPoint)
                                       (transaction: Transaction)
                                           : Option<ObscuredCommitmentNumber> = option {
        do! check (transaction.Version = TxVersionNumberOfCommitmentTxs)
        let! txIn = Seq.tryExactlyOne transaction.Inputs
        do! check (fundingOutPoint = txIn.PrevOut)
        let! obscuredCommitmentNumber =
            ObscuredCommitmentNumber.TryFromLockTimeAndSequence transaction.LockTime txIn.Sequence
        return obscuredCommitmentNumber
    }

    let tryGetFundsFromRemoteCommitmentTx (commitments: Commitments)
                                          (localChannelPrivKeys: ChannelPrivKeys)
                                          (staticChannelConfig: StaticChannelConfig)
                                          (transaction: Transaction)
                                              : Option<TransactionBuilder> = option {
        let! obscuredCommitmentNumber =
            tryGetObscuredCommitmentNumber
                staticChannelConfig.FundingScriptCoin.Outpoint
                transaction
        let localChannelPubKeys = localChannelPrivKeys.ToChannelPubKeys()
        let remoteChannelPubKeys = staticChannelConfig.RemoteChannelPubKeys
        let commitmentNumber =
            obscuredCommitmentNumber.Unobscure
                false
                localChannelPubKeys.PaymentBasepoint
                remoteChannelPubKeys.PaymentBasepoint
        let perCommitmentSecretOpt =
            commitments.RemotePerCommitmentSecrets.GetPerCommitmentSecret commitmentNumber
        let! perCommitmentPoint =
            match perCommitmentSecretOpt with
            | Some perCommitmentSecret -> Some <| perCommitmentSecret.PerCommitmentPoint()
            | None ->
                if commitments.RemoteCommit.Index = commitmentNumber then
                    Some commitments.RemoteCommit.RemotePerCommitmentPoint
                else
                    None

        let localCommitmentPubKeys =
            perCommitmentPoint.DeriveCommitmentPubKeys localChannelPubKeys
        let remoteCommitmentPubKeys =
            perCommitmentPoint.DeriveCommitmentPubKeys remoteChannelPubKeys

        let transactionBuilder = staticChannelConfig.Network.CreateTransactionBuilder()

        let toRemoteScriptPubKey =
            localCommitmentPubKeys.PaymentPubKey.RawPubKey().WitHash.ScriptPubKey
        let toRemoteIndexOpt =
            Seq.tryFindIndex
                (fun (txOut: TxOut) -> txOut.ScriptPubKey = toRemoteScriptPubKey)
                transaction.Outputs
        let spentToRemoteOutput =
            match toRemoteIndexOpt with
            | None -> false
            | Some toRemoteIndex ->
                let localPaymentPrivKey =
                    perCommitmentPoint.DerivePaymentPrivKey
                        localChannelPrivKeys.PaymentBasepointSecret
                (transactionBuilder.AddKeys (localPaymentPrivKey.RawKey()))
                                   .AddCoins (Coin(transaction, uint32 toRemoteIndex))
                |> ignore
                true

        let spentToLocalOutput =
            match perCommitmentSecretOpt with
            | None -> false
            | Some perCommitmentSecret ->
                let toLocalScriptPubKey =
                    Scripts.toLocalDelayed
                        localCommitmentPubKeys.RevocationPubKey
                        staticChannelConfig.RemoteParams.ToSelfDelay
                        remoteCommitmentPubKeys.DelayedPaymentPubKey
                let toLocalIndexOpt =
                    let toLocalWitScriptPubKey = toLocalScriptPubKey.WitHash.ScriptPubKey
                    Seq.tryFindIndex
                        (fun (txOut: TxOut) -> txOut.ScriptPubKey = toLocalWitScriptPubKey)
                        transaction.Outputs
                match toLocalIndexOpt with
                | None -> false
                | Some toLocalIndex ->
                    let revocationPrivKey =
                        perCommitmentSecret.DeriveRevocationPrivKey
                            localChannelPrivKeys.RevocationBasepointSecret
                    transactionBuilder.Extensions.Add (CommitmentToLocalExtension())
                    (transactionBuilder.AddKeys (revocationPrivKey.RawKey()))
                                       .AddCoins
                        (ScriptCoin(transaction, uint32 toLocalIndex, toLocalScriptPubKey))
                    |> ignore
                    true

        do! check (spentToRemoteOutput || spentToLocalOutput)

        return transactionBuilder
    }

    let tryGetFundsFromLocalCommitmentTx (localChannelPrivKeys: ChannelPrivKeys)
                                         (staticChannelConfig: StaticChannelConfig)
                                         (transaction: Transaction)
                                             : Option<TransactionBuilder> = option {
        let! obscuredCommitmentNumber =
            tryGetObscuredCommitmentNumber
                staticChannelConfig.FundingScriptCoin.Outpoint
                transaction
        let localChannelPubKeys = localChannelPrivKeys.ToChannelPubKeys()
        let remoteChannelPubKeys = staticChannelConfig.RemoteChannelPubKeys
        let commitmentNumber =
            obscuredCommitmentNumber.Unobscure
                true
                localChannelPubKeys.PaymentBasepoint
                remoteChannelPubKeys.PaymentBasepoint

        let perCommitmentPoint =
            localChannelPrivKeys.CommitmentSeed.DerivePerCommitmentPoint commitmentNumber
        let localCommitmentPubKeys =
            perCommitmentPoint.DeriveCommitmentPubKeys localChannelPubKeys
        let remoteCommitmentPubKeys =
            perCommitmentPoint.DeriveCommitmentPubKeys remoteChannelPubKeys

        let transactionBuilder = staticChannelConfig.Network.CreateTransactionBuilder()

        let toLocalScriptPubKey =
            Scripts.toLocalDelayed
                remoteCommitmentPubKeys.RevocationPubKey
                staticChannelConfig.LocalParams.ToSelfDelay
                localCommitmentPubKeys.DelayedPaymentPubKey
        let! toLocalIndex =
            let toLocalWitScriptPubKey = toLocalScriptPubKey.WitHash.ScriptPubKey
            Seq.tryFindIndex
                (fun (txOut: TxOut) -> txOut.ScriptPubKey = toLocalWitScriptPubKey)
                transaction.Outputs

        let delayedPaymentPrivKey =
            perCommitmentPoint.DeriveDelayedPaymentPrivKey
                localChannelPrivKeys.DelayedPaymentBasepointSecret
        transactionBuilder.Extensions.Add (CommitmentToLocalExtension())
        (transactionBuilder.AddKeys (delayedPaymentPrivKey.RawKey()))
                           .AddCoins
            (ScriptCoin(transaction, uint32 toLocalIndex, toLocalScriptPubKey))
        |> ignore

        return transactionBuilder
    }

    let getFundsFromForceClosingTransaction (commitments: Commitments)
                                            (localChannelPrivKeys: ChannelPrivKeys)
                                            (staticChannelConfig: StaticChannelConfig)
                                            (transaction: Transaction)
                                                : Option<TransactionBuilder> =
        let attemptsSeq = seq {
            yield
                tryGetFundsFromRemoteCommitmentTx
                    commitments
                    localChannelPrivKeys
                    staticChannelConfig
                    transaction
            yield
                tryGetFundsFromLocalCommitmentTx
                    localChannelPrivKeys
                    staticChannelConfig
                    transaction
        }
        Seq.tryPick (fun opt -> opt) attemptsSeq

