namespace DotNetLightning.Utils

open NBitcoin
open NBitcoin.Crypto

open System
open System.IO
open System.Net
open System.Linq

open DotNetLightning.Core.Utils.Extensions
open ResultUtils

[<AutoOpen>]
module Primitives =

    type NBitcoin.Utils with
        static member ToUInt16(b: byte [], lendian: bool): uint16 =
            if lendian then
                uint16 (b.[0]) + (uint16 (b.[1]) <<< 8)
            else
                uint16 (b.[1]) + (uint16 (b.[0]) <<< 8)

        static member ToBytes(d: uint16, lendian: bool): byte [] =
            let mutable output = Array.zeroCreate 2
            if lendian then
                output.[0] <- byte d
                output.[1] <- byte (d >>> 8)
            else
                output.[0] <- byte (d >>> 8)
                output.[1] <- byte d
            output

    /// Absolute block height
    [<Struct>]
    type BlockHeight = | BlockHeight of uint32 with
        static member Zero = 0u |> BlockHeight
        static member One = 1u |> BlockHeight
        member x.Value = let (BlockHeight v) = x in v
        member x.AsOffset() =
            x.Value |> Checked.uint16 |> BlockHeightOffset16

        static member (+) (a: BlockHeight, b: BlockHeightOffset16) =
                a.Value + (uint32 b.Value ) |> BlockHeight
        static member (+) (a: BlockHeight, b: BlockHeightOffset32) =
                a.Value + b.Value |> BlockHeight

        static member (-) (a: BlockHeight, b: BlockHeightOffset16) =
            a.Value - (uint32 b.Value) |> BlockHeight
        static member (-) (a: BlockHeight, b: BlockHeightOffset32) =
            a.Value - b.Value |> BlockHeight
            
        static member (-) (a: BlockHeight, b: BlockHeight) =
            a.Value - (b.Value) |> BlockHeightOffset32

    /// **Description**
    ///
    /// 16bit relative block height used for `OP_CSV` locks,
    /// Since OP_CSV allow only block number of 0 ~ 65535, it is safe
    /// to restrict into the range smaller than BlockHeight
    and  [<Struct>] BlockHeightOffset16 = | BlockHeightOffset16 of uint16 with
        member x.Value = let (BlockHeightOffset16 v) = x in v

        static member ofBlockHeightOffset32(bho32: BlockHeightOffset32) =
            BlockHeightOffset16 (uint16 bho32.Value)
        static member op_Implicit (v: uint16) =
            BlockHeightOffset16 v
        static member One = BlockHeightOffset16(1us)
        static member Zero = BlockHeightOffset16(0us)
        static member MaxValue = UInt16.MaxValue |> BlockHeightOffset16
        static member (+) (a: BlockHeightOffset16, b: BlockHeightOffset16) =
            a.Value + b.Value |> BlockHeightOffset16
        static member (-) (a: BlockHeightOffset16, b: BlockHeightOffset16) =
            a.Value - b.Value |> BlockHeightOffset16

    /// **Description**
    ///
    /// 32bit relative block height. For `OP_CSV` locks, BlockHeightOffset16
    /// should be used instead.
    and  [<Struct>] BlockHeightOffset32 = | BlockHeightOffset32 of uint32 with
        member x.Value = let (BlockHeightOffset32 v) = x in v

        static member ofBlockHeightOffset16(bho16: BlockHeightOffset16) =
            BlockHeightOffset32 (uint32 bho16.Value)
        static member op_Implicit (v: uint32) =
            BlockHeightOffset32 v
        static member One = BlockHeightOffset32(1u)
        static member Zero = BlockHeightOffset32(0u)
        static member MaxValue = UInt32.MaxValue |> BlockHeightOffset32
        static member (+) (a: BlockHeightOffset32, b: BlockHeightOffset32) =
            a.Value + b.Value |> BlockHeightOffset32
        static member (-) (a: BlockHeightOffset32, b: BlockHeightOffset32) =
            a.Value - b.Value |> BlockHeightOffset32

    /// Wrapper around NBitcoin's ECDSASignature type for convenience. It has following difference
    /// 1. It is equatable
    /// 2. Some Convenience methods for serialization
    /// 3. Custom `ToString`
    [<CustomEquality;CustomComparison;StructuredFormatDisplay("{AsString}")>]
    type LNECDSASignature = LNECDSASignature of ECDSASignature | Empty with
        member x.Value = match x with LNECDSASignature s -> s | Empty -> failwith "Unreachable!"
        override this.GetHashCode() = hash this.Value
        override this.Equals(obj: obj) =
            match obj with
            | :? LNECDSASignature as o -> (this :> IEquatable<LNECDSASignature>).Equals(o)
            | _ -> false
        interface IEquatable<LNECDSASignature> with
            member this.Equals(o: LNECDSASignature) =
                Utils.ArrayEqual(o.ToBytesCompact(), this.ToBytesCompact())
                
        /// Originally this method is in NBitcoin.Utils.
        /// But we ported here since it was internal method.
        member private this.BigIntegerToBytes(b: BouncyCastle.Math.BigInteger, numBytes: int) =
            if isNull b then null else
            let a = Array.zeroCreate numBytes
            let a2 = b.ToByteArray()
            let sourceIndex = if (a2.Length = numBytes + 1) then 1 else 0;
            let num = System.Math.Min(a2.Length, numBytes)
            array.Copy(a2, sourceIndex, a, numBytes - num, num);
            a

        override this.ToString() =
            sprintf "LNECDSASignature (%A)" (this.ToBytesCompact())
        member this.AsString = this.ToString()

        /// ** Description **
        ///
        /// Bitcoin Layer 1 forces (by consensus) DER encoding for the signatures.
        /// This is not optimal, but remaining as a rule since changing consensus is not easy.
        /// However in layer2, there are no such rules. So we use more optimal serialization by
        /// This function.
        /// Note it does not include the recovery id. so its always 64 bytes
        ///
        /// **Output**
        ///
        /// (serialized R value + S value) in byte array.
        member this.ToBytesCompact() =
            let r = Array.append (this.BigIntegerToBytes(b = this.Value.R, numBytes = 32)) (this.BigIntegerToBytes(this.Value.S, 32))
            if (isNull <| box r.[0]) then r.[0] <- 255uy
            r
            
        /// Logic does not really matter here. This is just for making life easier by enabling automatic implementation
        /// of `StructuralComparison` for wrapper types.
        member this.CompareTo(e: LNECDSASignature) =
            let a = this.ToBytesCompact() |> fun x -> Utils.ToUInt64(x, true)
            let b = e.ToBytesCompact() |>  fun x -> Utils.ToUInt64(x, true)
            a.CompareTo(b)
        interface IComparable with
            member this.CompareTo(o: obj) =
                match o with
                | :? LNECDSASignature as e -> this.CompareTo(e)
                | _ -> -1
                
        member this.ToDER() =
            this.Value.ToDER()
            
        /// Read 64 bytes as r(32 bytes) and s(32 bytes) value
        /// If `withRecId` is `true`, skip first 1 byte
        static member FromBytesCompact(bytes: byte [], ?withRecId: bool) =
            let withRecId = defaultArg withRecId false
            if withRecId && bytes.Length <> 65 then
                invalidArg "bytes" "ECDSASignature specified to have recovery id, but it was not 65 bytes length"
            else if (not withRecId) && bytes.Length <> 64 then
                invalidArg "bytes" "ECDSASignature was not specified to have recovery id, but it was not 64 bytes length."
            else
                let data = if withRecId then bytes.[1..] else bytes
                let r = NBitcoin.BouncyCastle.Math.BigInteger(1, data.[0..31])
                let s = NBitcoin.BouncyCastle.Math.BigInteger(1, data.[32..63])
                ECDSASignature(r, s) |> LNECDSASignature

        static member op_Implicit (ec: ECDSASignature) =
            ec |> LNECDSASignature

    type PaymentHash = | PaymentHash of uint256 with
        member x.Value = let (PaymentHash v) = x in v
        member x.ToBytes(?lEndian) =
            let e = defaultArg lEndian true
            x.Value.ToBytes(e)

        member x.GetRIPEMD160() =
            let b = x.Value.ToBytes()
            Crypto.Hashes.RIPEMD160(b, b.Length)

    type PaymentPreimage =
        private PaymentPreimage of seq<byte>
            with
                // as per BOLT-2:
                static member LENGTH = 32

                static member Create(data: seq<byte>) =
                    if data.Count() <> PaymentPreimage.LENGTH then
                        raise <| ArgumentException(sprintf "Payment preimage length should be %i" PaymentPreimage.LENGTH)
                    PaymentPreimage data

                member this.Value =
                    let (PaymentPreimage v) = this in v

                member this.ToHex() =
                    let h = NBitcoin.DataEncoders.HexEncoder()
                    this.ToByteArray() |> h.EncodeData
                    
                member this.ToBytes() =
                    this.Value

                member this.ToByteArray() =
                    this.Value |> Array.ofSeq

                member this.Hash =
                    this.ToByteArray() |> Crypto.Hashes.SHA256 |> uint256 |> PaymentHash

                member this.ToPrivKey() =
                    this.ToByteArray() |> Key

                member this.ToPubKey() =
                    this.ToPrivKey().PubKey

    let (|PaymentPreimage|) x =
        match x with
        | PaymentPreimage x -> x
        
    type ConnectionId = ConnectionId of Guid
    [<CustomEquality;CustomComparison>]
    type PeerId = PeerId of EndPoint
        with
        member this.Value = let (PeerId ep) = this in ep
        
        override this.GetHashCode() = this.Value.GetHashCode()
        member this.Equals(o: PeerId) =
            this.Value.ToEndpointString().Equals(o.Value.ToEndpointString())
        override this.Equals(o: obj) =
            match o with
            | :? PeerId as p -> this.Equals(p)
            | _ -> false
        interface IEquatable<PeerId> with
            member this.Equals o = this.Equals(o)
        member this.CompareTo(o: PeerId) =
            this.Value.ToEndpointString().CompareTo(o.Value.ToEndpointString())
            
        interface IComparable with
            member this.CompareTo(o: obj) =
                match o with
                | :? PeerId as p -> this.CompareTo(p)
                | _ -> -1

    [<CustomEquality;CustomComparison>]
    type ComparablePubKey = ComparablePubKey of PubKey with
        member x.Value = let (ComparablePubKey v) = x in v
        interface IComparable with
            override this.CompareTo(other) =
                match other with
                | :? ComparablePubKey as n -> this.Value.CompareTo(n.Value)
                | _ -> -1
        override this.GetHashCode() = this.Value.GetHashCode()
        override this.Equals(other) =
            match other with
            | :? ComparablePubKey as n -> this.Value.Equals(n.Value)
            | _              -> false
        static member op_Implicit (pk: PubKey) =
            pk |> ComparablePubKey
            
    [<CustomEquality;CustomComparison>]
    type NodeId = | NodeId of PubKey with
        member x.Value = let (NodeId v) = x in v
        interface IComparable with
            override this.CompareTo(other) =
                match other with
                | :? NodeId as n -> this.Value.CompareTo(n.Value)
                | _ -> -1
        override this.Equals(other) =
            match other with
            | :? NodeId as n -> this.Value.Equals(n.Value)
            | _              -> false
        override this.GetHashCode() =
            this.Value.GetHashCode()

    /// Small wrapper for NBitcoin's OutPoint type
    /// So that it supports comparison and equality constraints
    [<CustomComparison;CustomEquality>]
    type LNOutPoint = LNOutPoint of OutPoint with
        member x.Value = let (LNOutPoint v) = x in v
        
        member this.CompareTo(other: LNOutPoint) =
            if this.Value.Hash > other.Value.Hash then
                1
            else if this.Value.Hash < other.Value.Hash then
                -1
            else
                if this.Value.N > other.Value.N then
                    1
                else if this.Value.N < other.Value.N then
                    -1
                else
                    0
            
        member this.Equals(other: LNOutPoint) =
            (this.Value.Hash = other.Value.Hash) &&
            (this.Value.N = other.Value.N)
            
        override this.Equals(other: obj) =
            if isNull other then false else
            if not <| other :? LNOutPoint then false else
            this.Equals((other :?> LNOutPoint))
            
        override this.GetHashCode() = hash (this.Value.ToBytes())
            
        interface IComparable with
            member this.CompareTo(other) =
                if isNull other then 1 else
                if not <| other :? LNOutPoint then 1 else
                this.CompareTo(other :?> LNOutPoint)
            
        interface IEquatable<LNOutPoint> with
            member this.Equals(other) = this.Equals(other)

    type FeeRatePerKw = | FeeRatePerKw of uint32 with
        member x.Value = let (FeeRatePerKw v) = x in v
        static member FromFee(fee: Money, weight: uint64) =
            (((uint64 fee.Satoshi) * weight) / 1000UL)
            |> uint32
            |> FeeRatePerKw

        member this.ToFee(weight) =
            Money.Satoshis((uint64 this.Value) * weight / 1000UL)
            
        member this.AsNBitcoinFeeRate() =
            this.Value |> uint64 |> Money.Satoshis |> FeeRate

        static member Max(a: FeeRatePerKw, b: FeeRatePerKw) =
            if (a.Value >= b.Value) then a else b
        static member (+) (a: FeeRatePerKw, b: uint32) =
            (a.Value + b) |> FeeRatePerKw
        static member (*) (a: FeeRatePerKw, b: uint32) =
            (a.Value * b) |> FeeRatePerKw
    /// Block Hash
    type BlockId = | BlockId of uint256 with
        member x.Value = let (BlockId v) = x in v

    [<Struct>]
    type HTLCId = | HTLCId of uint64 with
        static member Zero = HTLCId(0UL)
        member x.Value = let (HTLCId v) = x in v

        static member (+) (a: HTLCId, b: uint64) = (a.Value + b) |> HTLCId

    [<Struct>]
    type TxOutIndex = | TxOutIndex of uint16 with
        member x.Value = let (TxOutIndex v) = x in v

    [<Struct>]
    type TxIndexInBlock = | TxIndexInBlock of uint32 with
        member x.Value = let (TxIndexInBlock v) = x in v


    [<Struct;StructuredFormatDisplay("{AsString}")>]
    type ShortChannelId = {
        BlockHeight: BlockHeight
        BlockIndex: TxIndexInBlock
        TxOutIndex: TxOutIndex
    }
        with

        static member From8Bytes(b: byte[]): ShortChannelId =
            let bh = NBitcoin.Utils.ToUInt32 (Array.concat [| b.[0..2]; [| 0uy |] |], false)
            let bi = NBitcoin.Utils.ToUInt32 (Array.concat [| b.[3..5]; [| 0uy |] |], false)
            let txOutIndex = NBitcoin.Utils.ToUInt16(b.[6..7], false)

            {
                BlockHeight = bh |> BlockHeight
                BlockIndex = bi |> TxIndexInBlock
                TxOutIndex = txOutIndex |> TxOutIndex
            }
            
        static member FromUInt64(rawData: uint64) =
            NBitcoin.Utils.ToBytes(rawData, false) |> ShortChannelId.From8Bytes
            
        member this.ToBytes(): byte [] =
            Array.concat [|
                        NBitcoin.Utils.ToBytes(this.BlockHeight.Value, false).[0..2]
                        NBitcoin.Utils.ToBytes(this.BlockIndex.Value, false).[0..2]
                        NBitcoin.Utils.ToBytes(this.TxOutIndex.Value, false)
                    |]
        override this.ToString() =
            sprintf "%dx%dx%d" this.BlockHeight.Value this.BlockIndex.Value this.TxOutIndex.Value
            
        member this.AsString = this.ToString()
            
        static member TryParse(s: string) =
            let items = s.Split('x')
            let err = Error (sprintf "Failed to parse %s" s)
            if (items.Length <> 3)  then err else
            match (items.[0] |> UInt32.TryParse), (items.[1] |> UInt32.TryParse), (items.[2] |> UInt16.TryParse) with
            | (true, h), (true, blockI), (true, outputI) ->
                {
                    BlockHeight = h |> BlockHeight
                    BlockIndex = blockI |> TxIndexInBlock
                    TxOutIndex = outputI |> TxOutIndex
                } |> Ok
            | _ -> err
        static member ParseUnsafe(s: string) =
            ShortChannelId.TryParse s
            |> Result.defaultWith (fun _ -> raise <| FormatException(sprintf "Failed to parse %s" s))

    type UserId = UserId of uint64
    type Delimiter =
        Delimiter of byte []

    type RGB = {
        Red: uint8
        Green: uint8
        Blue: uint8
    }
    type EncodingType =
        | SortedPlain = 0uy
        | ZLib = 1uy
    
    [<StructAttribute>]
    type CommitmentNumber(index: UInt48) =
        member this.Index = index

        override this.ToString() =
            sprintf "%012x (#%i)" this.Index.UInt64 (UInt48.MaxValue - this.Index).UInt64

        static member LastCommitment: CommitmentNumber =
            CommitmentNumber UInt48.Zero

        static member FirstCommitment: CommitmentNumber =
            CommitmentNumber UInt48.MaxValue

        static member ObscureFactor (isFunder: bool)
                                    (localPaymentBasePoint: PubKey)
                                    (remotePaymentBasePoint: PubKey)
                                        : UInt48 =
            let pubKeysHash =
                if isFunder then
                    Hashes.SHA256 <| Array.concat
                        [| localPaymentBasePoint.ToBytes(); remotePaymentBasePoint.ToBytes() |]
                else
                    Hashes.SHA256 <| Array.concat
                        [| remotePaymentBasePoint.ToBytes(); localPaymentBasePoint.ToBytes() |]
            UInt48.FromBytesBigEndian pubKeysHash.[26..]

        member this.PreviousCommitment: CommitmentNumber =
            CommitmentNumber(this.Index + UInt48.One)

        member this.NextCommitment: CommitmentNumber =
            CommitmentNumber(this.Index - UInt48.One)

        member this.Subsumes(other: CommitmentNumber): bool =
            let trailingZeros = this.Index.TrailingZeros
            (this.Index >>> trailingZeros) = (other.Index >>> trailingZeros)

        member this.PreviousUnsubsumed: Option<CommitmentNumber> =
            let trailingZeros = this.Index.TrailingZeros
            let prev = this.Index.UInt64 + (1UL <<< trailingZeros)
            if prev > UInt48.MaxValue.UInt64 then
                None
            else
                Some <| CommitmentNumber(UInt48.FromUInt64 prev)

        member this.Obscure (isFunder: bool)
                            (localPaymentBasePoint: PubKey)
                            (remotePaymentBasePoint: PubKey)
                                : ObscuredCommitmentNumber =
            let obscureFactor =
                CommitmentNumber.ObscureFactor
                    isFunder
                    localPaymentBasePoint
                    remotePaymentBasePoint
            ObscuredCommitmentNumber(this.Index ^^^ obscureFactor)

    and [<StructAttribute>] ObscuredCommitmentNumber(obscuredIndex: UInt48) =
        member this.ObscuredIndex: UInt48 = obscuredIndex

        override this.ToString() =
            sprintf "%012x" this.ObscuredIndex.UInt64

        member this.LockTime: LockTime =
            Array.concat [| [| 0x20uy |]; this.ObscuredIndex.GetBytesBigEndian().[3..] |]
            |> System.UInt32.FromBytesBigEndian
            |> LockTime

        member this.Sequence: Sequence =
            Array.concat [| [| 0x80uy |]; this.ObscuredIndex.GetBytesBigEndian().[..2] |]
            |> System.UInt32.FromBytesBigEndian
            |> Sequence

        static member TryFromLockTimeAndSequence (lockTime: LockTime)
                                                 (sequence: Sequence)
                                                     : Option<ObscuredCommitmentNumber> =
            let lockTimeBytes = lockTime.Value.GetBytesBigEndian()
            let sequenceBytes = sequence.Value.GetBytesBigEndian()
            if lockTimeBytes.[0] <> 0x20uy || sequenceBytes.[0] <> 0x80uy then
                None
            else
                Array.concat [| sequenceBytes.[1..]; lockTimeBytes.[1..] |]
                |> UInt48.FromBytesBigEndian
                |> ObscuredCommitmentNumber
                |> Some

        member this.Unobscure (isFunder: bool)
                              (localPaymentBasePoint: PubKey)
                              (remotePaymentBasePoint: PubKey)
                                  : CommitmentNumber =
            let obscureFactor =
                CommitmentNumber.ObscureFactor
                    isFunder
                    localPaymentBasePoint
                    remotePaymentBasePoint
            CommitmentNumber(this.ObscuredIndex ^^^ obscureFactor)

    [<StructAttribute>]
    type RevocationKey(key: Key) =
        member this.Key = key

        static member BytesLength: int = Key.BytesLength

        static member FromBytes(bytes: array<byte>): RevocationKey =
            RevocationKey <| Key bytes

        member this.ToByteArray(): array<byte> =
            this.Key.ToBytes()

        member this.DeriveChild (thisCommitmentNumber: CommitmentNumber)
                                (childCommitmentNumber: CommitmentNumber)
                                    : Option<RevocationKey> =
            if thisCommitmentNumber.Subsumes childCommitmentNumber then
                let commonBits = thisCommitmentNumber.Index.TrailingZeros
                let index = childCommitmentNumber.Index
                let mutable secret = this.ToByteArray()
                for bit in (commonBits - 1) .. -1 .. 0 do
                    if (index >>> bit) &&& UInt48.One = UInt48.One then
                        let byteIndex = bit / 8
                        let bitIndex = bit % 8
                        secret.[byteIndex] <- secret.[byteIndex] ^^^ (1uy <<< bitIndex)
                        secret <- Hashes.SHA256 secret
                Some <| RevocationKey(Key secret)
            else
                None

        member this.CommitmentPubKey: CommitmentPubKey =
            CommitmentPubKey this.Key.PubKey

    and [<StructAttribute>] CommitmentPubKey(pubKey: PubKey) =
        member this.PubKey = pubKey

        static member BytesLength: int = PubKey.BytesLength

        static member FromBytes(bytes: array<byte>): CommitmentPubKey =
            CommitmentPubKey <| PubKey bytes

        member this.ToByteArray(): array<byte> =
            this.PubKey.ToBytes()


    [<StructAttribute>]
    type CommitmentSeed(masterRevocationKey: RevocationKey) =
        new(key: Key) =
            CommitmentSeed(RevocationKey key)

        member this.MasterRevocationKey = masterRevocationKey

        member this.DeriveRevocationKey (commitmentNumber: CommitmentNumber): RevocationKey =
            let res =
                this.MasterRevocationKey.DeriveChild
                    CommitmentNumber.LastCommitment
                    commitmentNumber
            match res with
            | Some revocationKey -> revocationKey
            | None ->
                failwith
                    "The master revocation key should be able to derive the revocation key for \
                    all commitments. This is a bug."

        member this.DeriveCommitmentPubKey (commitmentNumber: CommitmentNumber): CommitmentPubKey =
            let revocationKey = this.DeriveRevocationKey commitmentNumber
            revocationKey.CommitmentPubKey

