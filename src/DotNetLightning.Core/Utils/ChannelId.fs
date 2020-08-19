namespace DotNetLightning.Utils

open NBitcoin

[<Struct>]
type ChannelId(id: uint256) =
    member x.Value = id
    static member Zero = uint256.Zero |> ChannelId

