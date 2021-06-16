namespace DotNetLightning.Utils

open System
open NBitcoin
open NBitcoin.BuilderExtensions
open DotNetLightning.Utils

open ResultUtils
open ResultUtils.Portability
open Newtonsoft.Json

type HTLCOfferedParameters = {
    LocalHtlcPubKey: HtlcPubKey
    RemoteHtlcPubKey: HtlcPubKey
}
    with
    static member TryExtractParameters (scriptPubKey: Script): Option<HTLCOfferedParameters> =
        let ops =
            scriptPubKey.ToOps()
            // we have to collect it into a list and convert back to a seq
            // because the IEnumerable that NBitcoin gives us is internally
            // mutable.
            |> List.ofSeq
            |> Seq.ofList

        Console.WriteLine(JsonConvert.SerializeObject(ops))
        let checkOpCode(opcodeType: OpcodeType) = seqParser<Op> {
            let! op = SeqParser.next()
            if op.Code = opcodeType then
                return ()
            else
                return! SeqParser.abort()
        }
    
        let parseToCompletionResult =
            SeqParser.parseToCompletion ops <| seqParser {
                do! checkOpCode OpcodeType.OP_DUP
                do! checkOpCode OpcodeType.OP_HASH160
                let! _opRevocationPubKey = SeqParser.next()
                do! checkOpCode OpcodeType.OP_EQUAL
                do! checkOpCode OpcodeType.OP_IF
                do! checkOpCode OpcodeType.OP_CHECKSIG
                do! checkOpCode OpcodeType.OP_ELSE
                let! opRemoteHtlcPubKey = SeqParser.next()
                let! remoteHtlcPubKey = seqParser {
                    match opRemoteHtlcPubKey.PushData with
                    | null -> return! SeqParser.abort()
                    | bytes ->
                        try
                            return bytes |> PubKey |> HtlcPubKey.HtlcPubKey 
                        with
                        | :? FormatException -> return! SeqParser.abort()
                }
                do! checkOpCode OpcodeType.OP_SWAP
                do! checkOpCode OpcodeType.OP_SIZE
                let! _blabla = SeqParser.next()
                do! checkOpCode OpcodeType.OP_EQUAL
                do! checkOpCode OpcodeType.OP_NOTIF
                do! checkOpCode OpcodeType.OP_DROP
                do! checkOpCode OpcodeType.OP_2
                do! checkOpCode OpcodeType.OP_SWAP
                let! opLocalHtlcPubKey = SeqParser.next()
                let! localHtlcPubKey = seqParser {
                    match opLocalHtlcPubKey.PushData with
                    | null -> return! SeqParser.abort()
                    | bytes ->
                        try
                            return bytes |> PubKey |> HtlcPubKey.HtlcPubKey 
                        with
                        | :? FormatException -> return! SeqParser.abort()
                }
                do! checkOpCode OpcodeType.OP_2
                do! checkOpCode OpcodeType.OP_CHECKMULTISIG
                do! checkOpCode OpcodeType.OP_ELSE
                do! checkOpCode OpcodeType.OP_HASH160
                let! _opPaymentHash = SeqParser.next()
                do! checkOpCode OpcodeType.OP_EQUALVERIFY
                do! checkOpCode OpcodeType.OP_CHECKSIG
                do! checkOpCode OpcodeType.OP_ENDIF
                do! checkOpCode OpcodeType.OP_ENDIF
                
                return {
                    LocalHtlcPubKey = localHtlcPubKey
                    RemoteHtlcPubKey = remoteHtlcPubKey
                }
            }
        match parseToCompletionResult with
        | Ok data -> Some data
        | Error _consumeAllError -> None

type internal HTLCOfferedExtensions() =
    inherit BuilderExtension()
        override self.CanGenerateScriptSig (scriptPubKey: Script): bool =
            (HTLCOfferedParameters.TryExtractParameters scriptPubKey).IsSome

        override self.GenerateScriptSig(scriptPubKey: Script, keyRepo: IKeyRepository, signer: ISigner): Script =
            let parameters =
                match (HTLCOfferedParameters.TryExtractParameters scriptPubKey) with
                | Some parameters -> parameters
                | None ->
                    failwith
                        "NBitcoin should not call this unless CanGenerateScriptSig returns true"
            let pubKey = keyRepo.FindKey scriptPubKey
            // FindKey will return null if it can't find a key for
            // scriptPubKey. If we can't find a valid key then this method
            // should return null, indicating to NBitcoin that the sigScript
            // could not be generated.
            match pubKey with
            | null -> null
            | _ when pubKey = parameters.LocalHtlcPubKey.RawPubKey() ->
                let localHtlcSig = signer.Sign (parameters.LocalHtlcPubKey.RawPubKey())
                Script [
                    Op.GetPushOp (localHtlcSig.ToBytes())
                ]
            | _ when pubKey = parameters.RemoteHtlcPubKey.RawPubKey() ->
                let remoteHtlcSig = signer.Sign (parameters.RemoteHtlcPubKey.RawPubKey())
                Script [
                    Op.GetPushOp (remoteHtlcSig.ToBytes())
                ]
            | _ -> null

        override self.CanDeduceScriptPubKey(_scriptSig: Script): bool =
            false

        override self.DeduceScriptPubKey(_scriptSig: Script): Script =
            raise <| NotSupportedException()

        override self.CanEstimateScriptSigSize(_scriptPubKey: Script): bool =
            false

        override self.EstimateScriptSigSize(_scriptPubKey: Script): int =
            raise <| NotSupportedException()

        override self.CanCombineScriptSig(_scriptPubKey: Script, _a: Script, _b: Script): bool = 
            false

        override self.CombineScriptSig(_scriptPubKey: Script, _a: Script, _b: Script): Script =
            raise <| NotSupportedException()

        override self.IsCompatibleKey(pubKey: PubKey, scriptPubKey: Script): bool =
            match HTLCOfferedParameters.TryExtractParameters scriptPubKey with
            | None -> false
            | Some parameters ->
                parameters.LocalHtlcPubKey.RawPubKey() = pubKey
                || parameters.RemoteHtlcPubKey.RawPubKey() = pubKey


