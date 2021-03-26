namespace DotNetLightning.Channel

open NBitcoin
open DotNetLightning.Utils
open DotNetLightning.Utils.Aether
open DotNetLightning.Utils.Aether.Operators
open DotNetLightning.Crypto
open DotNetLightning.Transactions
open DotNetLightning.Serialization.Msgs

open ResultUtils
open ResultUtils.Portability

type LocalChanges = {
    Signed: IUpdateMsg list
    ACKed: IUpdateMsg list
}
    with
        static member Zero = { Signed = []; ACKed = [] }

        // -- lenses
        static member Signed_: Lens<_, _> =
            (fun lc -> lc.Signed),
            (fun v lc -> { lc with Signed = v })

        static member ACKed_: Lens<_, _> =
            (fun lc -> lc.ACKed),
            (fun v lc -> { lc with ACKed = v })

type RemoteChanges = { 
    Signed: IUpdateMsg list
    ACKed: IUpdateMsg list
}
    with
        static member Zero = { Signed = []; ACKed = [] }

        static member Signed_: Lens<_, _> =
            (fun lc -> lc.Signed),
            (fun v lc -> { lc with Signed = v })

        static member ACKed_: Lens<_, _> =
            (fun lc -> lc.ACKed),
            (fun v lc -> { lc with ACKed = v })


type PublishableTxs = {
    CommitTx: FinalizedTx
    HTLCTxs: FinalizedTx list
}

type LocalCommit = {
    Index: CommitmentNumber
    Spec: CommitmentSpec
    PublishableTxs: PublishableTxs
    /// These are not redeemable on-chain until we get a corresponding preimage.
    PendingHTLCSuccessTxs: HTLCSuccessTx list
}
type RemoteCommit = {
    Index: CommitmentNumber
    Spec: CommitmentSpec
    RemotePerCommitmentPoint: PerCommitmentPoint
}

type RemoteNextCommitInfo =
    | Waiting of RemoteCommit
    | Revoked of PerCommitmentPoint
    with
        static member Waiting_: Prism<RemoteNextCommitInfo, RemoteCommit> =
            (fun remoteNextCommitInfo ->
                match remoteNextCommitInfo with
                | Waiting remoteCommit -> Some remoteCommit
                | Revoked _ -> None),
            (fun waitingForRevocation remoteNextCommitInfo ->
                match remoteNextCommitInfo with
                | Waiting _ -> Waiting waitingForRevocation
                | Revoked _ -> remoteNextCommitInfo)

        static member Revoked_: Prism<RemoteNextCommitInfo, PerCommitmentPoint> =
            (fun remoteNextCommitInfo ->
                match remoteNextCommitInfo with
                | Waiting _ -> None
                | Revoked commitmentPubKey -> Some commitmentPubKey),
            (fun commitmentPubKey remoteNextCommitInfo ->
                match remoteNextCommitInfo with
                | Waiting _ -> remoteNextCommitInfo
                | Revoked _ -> Revoked commitmentPubKey)

        member self.PerCommitmentPoint(): PerCommitmentPoint =
            match self with
            | Waiting remoteCommit -> remoteCommit.RemotePerCommitmentPoint
            | Revoked perCommitmentPoint -> perCommitmentPoint

type Commitments = {
    ProposedLocalChanges: list<IUpdateMsg>
    ProposedRemoteChanges: list<IUpdateMsg>
    LocalNextHTLCId: HTLCId
    RemoteNextHTLCId: HTLCId
    OriginChannels: Map<HTLCId, HTLCSource>
}
    with
        member this.AddLocalProposal(proposal: IUpdateMsg) =
            {
                this with
                    ProposedLocalChanges = proposal :: this.ProposedLocalChanges
            }

        member this.AddRemoteProposal(proposal: IUpdateMsg) =
            {
                this with
                    ProposedRemoteChanges = proposal :: this.ProposedRemoteChanges
            }

        member this.IncrLocalHTLCId() = { this with LocalNextHTLCId = this.LocalNextHTLCId + 1UL }
        member this.IncrRemoteHTLCId() = { this with RemoteNextHTLCId = this.RemoteNextHTLCId + 1UL }

        member internal this.LocalHasUnsignedOutgoingHTLCs() =
            this.ProposedLocalChanges |> List.exists(fun p -> match p with | :? UpdateAddHTLCMsg -> true | _ -> false)

        member internal this.RemoteHasUnsignedOutgoingHTLCs() =
            this.ProposedRemoteChanges |> List.exists(fun p -> match p with | :? UpdateAddHTLCMsg -> true | _ -> false)

