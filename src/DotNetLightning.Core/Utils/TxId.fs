namespace DotNetLightning.Utils

open NBitcoin

[<Struct;StructuralComparison;StructuralEquality>]
type TxId(id: uint256) =
    member x.Value = id
    static member Zero = uint256.Zero |> TxId

