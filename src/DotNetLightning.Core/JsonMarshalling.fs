namespace DotNetLightning

open System
open System.Net

open DotNetLightning.Serialize
open DotNetLightning.Utils
open DotNetLightning.Crypto
open DotNetLightning.Transactions

open NBitcoin
open NBitcoin.Crypto
open Newtonsoft.Json

module JsonMarshalling =
    type internal FinalizedTxConverter() =
        inherit JsonConverter<FinalizedTx>()

        override this.ReadJson(reader: JsonReader, _: Type, _: FinalizedTx, _: bool, serializer: JsonSerializer) =
            let tx = serializer.Deserialize<Transaction> reader
            FinalizedTx tx

        override this.WriteJson(writer: JsonWriter, state: FinalizedTx, serializer: JsonSerializer) =
            serializer.Serialize(writer, state.Value)

    type internal LNECDSASignatureConverter() =
        inherit JsonConverter<LNECDSASignature>()

        override this.ReadJson(reader: JsonReader, _: Type, _: LNECDSASignature, _: bool, serializer: JsonSerializer) =
            let serializedLNECDSASignature = serializer.Deserialize<string> reader
            let hex = NBitcoin.DataEncoders.HexEncoder()
            serializedLNECDSASignature |> hex.DecodeData |> ECDSASignature |> LNECDSASignature

        override this.WriteJson(writer: JsonWriter, state: LNECDSASignature, serializer: JsonSerializer) =
            let hex = NBitcoin.DataEncoders.HexEncoder()
            let serializedLNECDSASignature: string = state.ToDER() |> hex.EncodeData
            serializer.Serialize(writer, serializedLNECDSASignature)

    type internal TxIdConverter() =
        inherit JsonConverter<TxId>()

        override this.ReadJson(reader: JsonReader, _: Type, _: TxId, _: bool, serializer: JsonSerializer) =
            let serializedTxId = serializer.Deserialize<string> reader
            let hex = NBitcoin.DataEncoders.HexEncoder()
            serializedTxId |> hex.DecodeData |> uint256 |> TxId

        override this.WriteJson(writer: JsonWriter, state: TxId, serializer: JsonSerializer) =
            let hex = NBitcoin.DataEncoders.HexEncoder()
            let serializedTxId: string = state.Value.ToBytes() |> hex.EncodeData
            serializer.Serialize(writer, serializedTxId)

    type internal ChannelIdConverter() =
        inherit JsonConverter<ChannelId>()

        override this.ReadJson(reader: JsonReader, _: Type, _: ChannelId, _: bool, serializer: JsonSerializer) =
            let serializedChannelId = serializer.Deserialize<string> reader
            let hex = NBitcoin.DataEncoders.HexEncoder()
            serializedChannelId |> hex.DecodeData |> uint256 |> ChannelId

        override this.WriteJson(writer: JsonWriter, state: ChannelId, serializer: JsonSerializer) =
            let hex = NBitcoin.DataEncoders.HexEncoder()
            let serializedChannelId: string = state.Value.ToBytes() |> hex.EncodeData
            serializer.Serialize(writer, serializedChannelId)

    type internal NodeIdConverter() =
        inherit JsonConverter<NodeId>()

        override this.ReadJson(reader: JsonReader, _: Type, _: NodeId, _: bool, serializer: JsonSerializer) =
            let serializedNodeId = serializer.Deserialize<string> reader
            let hex = NBitcoin.DataEncoders.HexEncoder()
            serializedNodeId |> hex.DecodeData |> PubKey |> NodeId

        override this.WriteJson(writer: JsonWriter, state: NodeId, serializer: JsonSerializer) =
            let serializedNodeId: string = state.Value.ToHex()
            serializer.Serialize(writer, serializedNodeId)

    type internal CommitmentPubKeyConverter() =
        inherit JsonConverter<CommitmentPubKey>()

        override this.ReadJson(reader: JsonReader, _: Type, _: CommitmentPubKey, _: bool, serializer: JsonSerializer) =
            let serializedCommitmentPubKey = serializer.Deserialize<string> reader
            let hex = NBitcoin.DataEncoders.HexEncoder()
            serializedCommitmentPubKey |> hex.DecodeData |> PubKey |> CommitmentPubKey

        override this.WriteJson(writer: JsonWriter, state: CommitmentPubKey, serializer: JsonSerializer) =
            let serializedCommitmentPubKey: string = state.PubKey.ToHex()
            serializer.Serialize(writer, serializedCommitmentPubKey)

    type internal CommitmentNumberConverter() =
        inherit JsonConverter<CommitmentNumber>()

        override this.ReadJson(reader: JsonReader, _: Type, _: CommitmentNumber, _: bool, serializer: JsonSerializer) =
            let serializedCommitmentNumber = serializer.Deserialize<uint64> reader
            CommitmentNumber <| (UInt48.MaxValue - UInt48.FromUInt64 serializedCommitmentNumber)

        override this.WriteJson(writer: JsonWriter, state: CommitmentNumber, serializer: JsonSerializer) =
            let serializedCommitmentNumber: uint64 = (UInt48.MaxValue - state.Index).UInt64
            serializer.Serialize(writer, serializedCommitmentNumber)

    type internal RevocationSetConverter() =
        inherit JsonConverter<RevocationSet>()

        override this.ReadJson(reader: JsonReader, _: Type, _: RevocationSet, _: bool, serializer: JsonSerializer) =
            let keys = serializer.Deserialize<list<CommitmentNumber * RevocationKey>> reader
            RevocationSet.FromKeys keys

        override this.WriteJson(writer: JsonWriter, state: RevocationSet, serializer: JsonSerializer) =
            let keys: list<CommitmentNumber * RevocationKey> = state.Keys
            serializer.Serialize(writer, keys)

    type internal FeatureBitJsonConverter() =
        inherit JsonConverter<FeatureBit>()

        override self.ReadJson(reader: JsonReader, _: Type, _: FeatureBit, _: bool, serializer: JsonSerializer) =
            let serializedFeatureBit = serializer.Deserialize<string> reader
            match (FeatureBit.TryParse serializedFeatureBit) with
            | Ok featureBit -> featureBit
            | Error err -> failwith ("error decoding feature bit: " + err)

        override self.WriteJson(writer: JsonWriter, state: FeatureBit, serializer: JsonSerializer) =
            serializer.Serialize(writer, state.ToString())

    type internal IPAddressJsonConverter() =
        inherit JsonConverter<IPAddress>()

        override self.ReadJson(reader: JsonReader, _: Type, _: IPAddress, _: bool, serializer: JsonSerializer) =
            let serializedIPAddress = serializer.Deserialize<string> reader
            IPAddress.Parse serializedIPAddress

        override self.WriteJson(writer: JsonWriter, state: IPAddress, serializer: JsonSerializer) =
            serializer.Serialize(writer, state.ToString())

    type internal IPEndPointJsonConverter() =
        inherit JsonConverter<IPEndPoint>()

        override __.ReadJson(reader: JsonReader, _: Type, _: IPEndPoint, _: bool, serializer: JsonSerializer) =
            assert (reader.TokenType = JsonToken.StartArray)
            reader.Read() |> ignore
            let ip = serializer.Deserialize<IPAddress> reader
            reader.Read() |> ignore
            let port = serializer.Deserialize<int32> reader
            assert (reader.TokenType = JsonToken.EndArray)
            reader.Read() |> ignore
            IPEndPoint (ip, port)

        override __.WriteJson(writer: JsonWriter, state: IPEndPoint, serializer: JsonSerializer) =
            writer.WriteStartArray()
            serializer.Serialize(writer, state.Address)
            serializer.Serialize(writer, state.Port)
            writer.WriteEndArray()

    type internal BlockHeightConverter() =
        inherit JsonConverter<BlockHeight>()

        override this.ReadJson(reader: JsonReader, _: Type, _: BlockHeight, _: bool, serializer: JsonSerializer) =
            let serializedBlockHeight = serializer.Deserialize<uint32> reader
            BlockHeight serializedBlockHeight

        override this.WriteJson(writer: JsonWriter, state: BlockHeight, serializer: JsonSerializer) =
            serializer.Serialize(writer, state.Value)

    type internal BlockHeightOffset16Converter() =
        inherit JsonConverter<BlockHeightOffset16>()

        override this.ReadJson(reader: JsonReader, _: Type, _: BlockHeightOffset16, _: bool, serializer: JsonSerializer) =
            let serializedBlockHeightOffset16 = serializer.Deserialize<uint16> reader
            BlockHeightOffset16 serializedBlockHeightOffset16

        override this.WriteJson(writer: JsonWriter, state: BlockHeightOffset16, serializer: JsonSerializer) =
            serializer.Serialize(writer, state.Value)

    type internal BlockHeightOffset32Converter() =
        inherit JsonConverter<BlockHeightOffset32>()

        override this.ReadJson(reader: JsonReader, _: Type, _: BlockHeightOffset32, _: bool, serializer: JsonSerializer) =
            let serializedBlockHeightOffset32 = serializer.Deserialize<uint32> reader
            BlockHeightOffset32 serializedBlockHeightOffset32

        override this.WriteJson(writer: JsonWriter, state: BlockHeightOffset32, serializer: JsonSerializer) =
            serializer.Serialize(writer, state.Value)

    type internal FeeRatePerKwConverter() =
        inherit JsonConverter<FeeRatePerKw>()

        override this.ReadJson(reader: JsonReader, _: Type, _: FeeRatePerKw, _: bool, serializer: JsonSerializer) =
            let serializedFeeRatePerKw = serializer.Deserialize<uint32> reader
            FeeRatePerKw serializedFeeRatePerKw

        override this.WriteJson(writer: JsonWriter, state: FeeRatePerKw, serializer: JsonSerializer) =
            serializer.Serialize(writer, state.Value)

    type internal LNMoneyConverter() =
        inherit JsonConverter<LNMoney>()

        override this.ReadJson(reader: JsonReader, _: Type, _: LNMoney, _: bool, serializer: JsonSerializer) =
            let serializedLNMoney = serializer.Deserialize<decimal> reader
            LNMoney.Coins serializedLNMoney

        override this.WriteJson(writer: JsonWriter, state: LNMoney, serializer: JsonSerializer) =
            serializer.Serialize(writer, (decimal state.MilliSatoshi) / (decimal LNMoneyUnit.BTC))

    type internal HTLCIdConverter() =
        inherit JsonConverter<HTLCId>()

        override this.ReadJson(reader: JsonReader, _: Type, _: HTLCId, _: bool, serializer: JsonSerializer) =
            let serializedHTLCId = serializer.Deserialize<uint64> reader
            HTLCId serializedHTLCId

        override this.WriteJson(writer: JsonWriter, state: HTLCId, serializer: JsonSerializer) =
            serializer.Serialize(writer, state.Value)

    type internal TxIndexInBlockConverter() =
        inherit JsonConverter<TxIndexInBlock>()

        override this.ReadJson(reader: JsonReader, _: Type, _: TxIndexInBlock, _: bool, serializer: JsonSerializer) =
            let serializedTxIndexInBlock = serializer.Deserialize<uint32> reader
            TxIndexInBlock serializedTxIndexInBlock

        override this.WriteJson(writer: JsonWriter, state: TxIndexInBlock, serializer: JsonSerializer) =
            serializer.Serialize(writer, state.Value)

    type internal TxOutIndexConverter() =
        inherit JsonConverter<TxOutIndex>()

        override this.ReadJson(reader: JsonReader, _: Type, _: TxOutIndex, _: bool, serializer: JsonSerializer) =
            let serializedTxOutIndex = serializer.Deserialize<uint16> reader
            TxOutIndex serializedTxOutIndex

        override this.WriteJson(writer: JsonWriter, state: TxOutIndex, serializer: JsonSerializer) =
            serializer.Serialize(writer, state.Value)

    let RegisterConverters (settings: JsonSerializerSettings) =
        settings.Converters.Add <| IPAddressJsonConverter()
        settings.Converters.Add <| IPEndPointJsonConverter()
        settings.Converters.Add <| FeatureBitJsonConverter()
        settings.Converters.Add <| CommitmentNumberConverter()
        settings.Converters.Add <| CommitmentPubKeyConverter()
        settings.Converters.Add <| RevocationSetConverter()
        settings.Converters.Add <| BlockHeightConverter()
        settings.Converters.Add <| BlockHeightOffset16Converter()
        settings.Converters.Add <| BlockHeightOffset32Converter()
        settings.Converters.Add <| FeeRatePerKwConverter()
        settings.Converters.Add <| LNMoneyConverter()
        settings.Converters.Add <| HTLCIdConverter()
        settings.Converters.Add <| NodeIdConverter()
        settings.Converters.Add <| TxIdConverter()
        settings.Converters.Add <| ChannelIdConverter()
        settings.Converters.Add <| TxIndexInBlockConverter()
        settings.Converters.Add <| TxOutIndexConverter()
        settings.Converters.Add <| LNECDSASignatureConverter()
        settings.Converters.Add <| FinalizedTxConverter()
        NBitcoin.JsonConverters.Serializer.RegisterFrontConverters settings

