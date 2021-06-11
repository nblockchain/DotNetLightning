module SphinxTests

open ResultUtils
open Expecto
open Expecto.Logging
open Expecto.Logging.Message
open NBitcoin
open DotNetLightning.Utils
open DotNetLightning.Utils.OnionError
open DotNetLightning.Serialization.Msgs
open DotNetLightning.Crypto
open DotNetLightning.Crypto.Sphinx

let hex = NBitcoin.DataEncoders.HexEncoder()

let sessionKey = "4141414141414141414141414141414141414141414141414141414141414141" |> hex.DecodeData |> fun h -> new Key(h)
let privKeys = [ "4141414141414141414141414141414141414141414141414141414141414141"
                 "4242424242424242424242424242424242424242424242424242424242424242"
                 "4343434343434343434343434343434343434343434343434343434343434343"
                 "4444444444444444444444444444444444444444444444444444444444444444"
                 "4545454545454545454545454545454545454545454545454545454545454545" ]
                |> List.map(hex.DecodeData >> fun h -> new Key(h))
let pubKeys = privKeys |> List.map(fun k -> k.PubKey)

let expectedPubKeys = [ "02eec7245d6b7d2ccb30380bfbe2a3648cd7a942653f5aa340edcea1f283686619"
                        "0324653eac434488002cc06bbfb7f10fe18991e35f9fe4302dbea6d2353dc0ab1c"
                        "027f31ebc5462c1fdce1b737ecff52d37d75dea43ce11c74d25aa297165faa2007"
                        "032c0b7cf95324a07d05398b240174dc0c2be444d96b159aa6c7f7b1e668680991"
                        "02edabbd16b41c8371b92ef2f04c1185b4f03b6dcd52ba9b78d9d7c89c8f221145" ]
                      |> List.map(hex.DecodeData >> PubKey)

let payloads = [ "000000000000000000000000000000000000000000000000000000000000000000"
                 "000101010101010101000000000000000100000001000000000000000000000000"
                 "000202020202020202000000000000000200000002000000000000000000000000"
                 "000303030303030303000000000000000300000003000000000000000000000000"
                 "000404040404040404000000000000000400000004000000000000000000000000" ]
               |> List.map(hex.DecodeData)

let associatedData = "4242424242424242424242424242424242424242424242424242424242424242" |> hex.DecodeData

let logger = Log.create "Sphinx tests"
let logCore = eventX >> logger.info 
let log _level = logCore

[<Tests>]
let bolt4Tests1 =
    testList "bolt4 tests (legacy hop_data)" [
        testCase "pubkey is as expected" <| fun _ ->
            Expect.equal (pubKeys) (expectedPubKeys) ""

        testCase "generate ephemeral keys and secrets" <| fun _ ->
            let (ephKeys, sharedSecrets) = computeEphemeralPublicKeysAndSharedSecrets sessionKey pubKeys
            let expectedEphKeys = [ "02eec7245d6b7d2ccb30380bfbe2a3648cd7a942653f5aa340edcea1f283686619"
                                    "028f9438bfbf7feac2e108d677e3a82da596be706cc1cf342b75c7b7e22bf4e6e2"
                                    "03bfd8225241ea71cd0843db7709f4c222f62ff2d4516fd38b39914ab6b83e0da0"
                                    "031dde6926381289671300239ea8e57ffaf9bebd05b9a5b95beaf07af05cd43595"
                                    "03a214ebd875aab6ddfd77f22c5e7311d7f77f17a169e599f157bbcdae8bf071f4" ]
                                   |> List.map(hex.DecodeData >> PubKey)
            let expectedSharedSecrets = [ "53eb63ea8a3fec3b3cd433b85cd62a4b145e1dda09391b348c4e1cd36a03ea66"
                                          "a6519e98832a0b179f62123b3567c106db99ee37bef036e783263602f3488fae"
                                          "3a6b412548762f0dbccce5c7ae7bb8147d1caf9b5471c34120b30bc9c04891cc"
                                          "21e13c2d7cfe7e18836df50872466117a295783ab8aab0e7ecc8c725503ad02d"
                                          "b5756b9b542727dbafc6765a49488b023a725d631af688fc031217e90770c328" ]
                                        |>  List.map(hex.DecodeData >> fun h -> new Key(h))
            Expect.equal ephKeys expectedEphKeys ""
            Expect.equal sharedSecrets expectedSharedSecrets ""

        testCase "generate filler" <| fun _ ->
            let (_, sharedSecrets) = computeEphemeralPublicKeysAndSharedSecrets sessionKey pubKeys
            let filler = generateFiller "rho" payloads sharedSecrets
            let expectedFiller = "c6b008cf6414ed6e4c42c291eb505e9f22f5fe7d0ecdd15a833f4d016ac974d33adc6ea3293e20859e87ebfb937ba406abd025d14af692b12e9c9c2adbe307a679779259676211c071e614fdb386d1ff02db223a5b2fae03df68d321c7b29f7c7240edd3fa1b7cb6903f89dc01abf41b2eb0b49b6b8d73bb0774b58204c0d0e96d3cce45ad75406be0bc009e327b3e712a4bd178609c00b41da2daf8a4b0e1319f07a492ab4efb056f0f599f75e6dc7e0d10ce1cf59088ab6e873de377343880f7a24f0e36731a0b72092f8d5bc8cd346762e93b2bf203d00264e4bc136fc142de8f7b69154deb05854ea88e2d7506222c95ba1aab065c8a851391377d3406a35a9af3ac" |> hex.DecodeData
            Expect.equal filler expectedFiller ""

        testCase "Create packet (reference test vector)" <| fun _ ->
            let (onion, _ss) =
                let p = PacketAndSecrets.Create (sessionKey, pubKeys, payloads, associatedData, PacketFiller.DeterministicPacketFiller)
                (p.Packet, p.SharedSecrets)
            let expectedPacket =
                "0002eec7245d6b7d2ccb30380bfbe2a3648cd7a942653f5aa340edcea1f283686619e5f14350c2a76fc232b5e46d421e9615471ab9e0bc887beff8c95fdb878f7b3a71e87f9aab8f6378c6ff744c1f34b393ad28d065b535c1a8668d85d3b34a1b3befd10f7d61ab590531cf08000178a333a347f8b4072e216400406bdf3bf038659793a1f9e7abc789266cc861cabd95818c0fc8efbdfdc14e3f7c2bc7eb8d6a79ef75ce721caad69320c3a469a202f3e468c67eaf7a7cda226d0fd32f7b48084dca885d014698cf05d742557763d9cb743faeae65dcc79dddaecf27fe5942be5380d15e9a1ec866abe044a9ad635778ba61fc0776dc832b39451bd5d35072d2269cf9b040a2a2fba158a0d8085926dc2e44f0c88bf487da56e13ef2d5e676a8589881b4869ed4c7f0218ff8c6c7dd7221d189c65b3b9aaa71a01484b122846c7c7b57e02e679ea8469b70e14fe4f70fee4d87b910cf144be6fe48eef24da475c0b0bcc6565a9f99728426ce2380a9580e2a9442481ceae7679906c30b1a0e21a10f26150e0645ab6edfdab1ce8f8bea7b1dee511c5fd38ac0e702c1c15bb86b52bca1b71e15b96982d262a442024c33ceb7dd8f949063c2e5e613e873250e2f8708bd4e1924abd45f65c2fa5617bfb10ee9e4a42d6b5811acc8029c16274f937dac9e8817c7e579fdb767ffe277f26d413ced06b620ede8362081da21cf67c2ca9d6f15fe5bc05f82f5bb93f8916bad3d63338ca824f3bbc11b57ce94a5fa1bc239533679903d6fec92a8c792fd86e2960188c14f21e399cfd72a50c620e10aefc6249360b463df9a89bf6836f4f26359207b765578e5ed76ae9f31b1cc48324be576e3d8e44d217445dba466f9b6293fdf05448584eb64f61e02903f834518622b7d4732471c6e0e22e22d1f45e31f0509eab39cdea5980a492a1da2aaac55a98a01216cd4bfe7abaa682af0fbff2dfed030ba28f1285df750e4d3477190dd193f8643b61d8ac1c427d590badb1f61a05d480908fbdc7c6f0502dd0c4abb51d725e92f95da2a8facb79881a844e2026911adcc659d1fb20a2fce63787c8bb0d9f6789c4b231c76da81c3f0718eb7156565a081d2be6b4170c0e0bcebddd459f53db2590c974bca0d705c055dee8c629bf854a5d58edc85228499ec6dde80cce4c8910b81b1e9e8b0f43bd39c8d69c3a80672729b7dc952dd9448688b6bd06afc2d2819cda80b66c57b52ccf7ac1a86601410d18d0c732f69de792e0894a9541684ef174de766fd4ce55efea8f53812867be6a391ac865802dbc26d93959df327ec2667c7256aa5a1d3c45a69a6158f285d6c97c3b8eedb09527848500517995a9eae4cd911df531544c77f5a9a2f22313e3eb72ca7a07dba243476bc926992e0d1e58b4a2fc8c7b01e0cad726237933ea319bad7537d39f3ed635d1e6c1d29e97b3d2160a09e30ee2b65ac5bce00996a73c008bcf351cecb97b6833b6d121dcf4644260b2946ea204732ac9954b228f0beaa15071930fd9583dfc466d12b5f0eeeba6dcf23d5ce8ae62ee5796359d97a4a15955c778d868d0ef9991d9f2833b5bb66119c5f8b396fd108baed7906cbb3cc376d13551caed97fece6f42a4c908ee279f1127fda1dd3ee77d8de0a6f3c135fa3f1cffe38591b6738dc97b55f0acc52be9753ce53e64d7e497bb00ca6123758df3b68fad99e35c04389f7514a8e36039f541598a417275e77869989782325a15b5342ac5011ff07af698584b476b35d941a4981eac590a07a092bb50342da5d3341f901aa07964a8d02b623c7b106dd0ae50bfa007a22d46c8772fa55558176602946cb1d11ea5460db7586fb89c6d3bcd3ab6dd20df4a4db63d2e7d52380800ad812b8640887e027e946df96488b47fbc4a4fadaa8beda4abe446fafea5403fae2ef"
                |> hex.DecodeData
            CheckArrayEqual (onion.ToBytes()) (expectedPacket)
            let { Payload = payload0; NextPacket = nextPacket0; SharedSecret = _ss0 }: ParsedPacket =
                Sphinx.parsePacket (privKeys.[0]) (associatedData) (onion.ToBytes())
                |> fun rr -> 
                    Expect.isOk (Result.ToFSharpCoreResult rr) ""
                    Result.defaultWith (fun _ -> failwith "Unreachable") rr
            let { Payload = payload1; NextPacket = nextPacket1; }: ParsedPacket =
                Sphinx.parsePacket (privKeys.[1]) (associatedData) (nextPacket0.ToBytes())
                |> Result.defaultWith(fun _ -> failwith "Fail: bolt4 create packet ref test vector defaultClosure1")
            let { Payload = payload2; NextPacket = nextPacket2; }: ParsedPacket =
                Sphinx.parsePacket (privKeys.[2]) (associatedData) (nextPacket1.ToBytes())
                |> Result.defaultWith(fun _ -> failwith "Fail: bolt4 create packet ref test vector defaultClosure2")
            let { Payload = payload3; NextPacket = nextPacket3; }: ParsedPacket =
                Sphinx.parsePacket (privKeys.[3]) (associatedData) (nextPacket2.ToBytes())
                |> Result.defaultWith(fun _ -> failwith "Fail: bolt4 create packet ref test vector defaultClosure3")
            let { Payload = payload4; NextPacket = nextPacket4; }: ParsedPacket =
                Sphinx.parsePacket (privKeys.[4]) (associatedData) (nextPacket3.ToBytes())
                |> Result.defaultWith(fun _ -> failwith "Fail: bolt4 create packet ref test vector defaultClosure1")

            Expect.equal [payload0; payload1; payload2; payload3; payload4] payloads ""

            Expect.equal nextPacket0.HMAC ("a93aa4f40241cef3e764e24b28570a0db39af82ab5102c3a04e51bec8cca9394" |> hex.DecodeData |> uint256) ""
            Expect.equal nextPacket1.HMAC ("5d1b11f1efeaa9be32eb1c74b113c0b46f056bb49e2a35a51ceaece6bd31332c" |> hex.DecodeData |> uint256) ""
            Expect.equal nextPacket2.HMAC ("19ca6357b5552b28e50ae226854eec874bbbf7025cf290a34c06b4eff5d2bac0" |> hex.DecodeData |> uint256) ""
            Expect.equal nextPacket3.HMAC ("16d4553c6084b369073d259381bb5b02c16bb2c590bbd9e69346cf7ebd563229" |> hex.DecodeData |> uint256) ""
            Expect.equal nextPacket4.HMAC ("0000000000000000000000000000000000000000000000000000000000000000" |> hex.DecodeData |> uint256) ""
            ()

        testCase "last node replies with an error message" <| fun _ ->
            let (onion, ss) =
                let p = PacketAndSecrets.Create (sessionKey, pubKeys, payloads, associatedData, PacketFiller.BlankPacketFiller)
                (p.Packet, p.SharedSecrets)
            let { NextPacket = packet1; SharedSecret = ss0 }: ParsedPacket =
                Sphinx.parsePacket (privKeys.[0]) (associatedData) (onion.ToBytes())
                |> fun r -> 
                    Expect.isOk (Result.ToFSharpCoreResult r) ""
                    Result.defaultWith(fun _ -> failwith "Fail: bolt4 last node replies with err msg defaultClosure0") r
            let { NextPacket = packet2; SharedSecret = ss1 }: ParsedPacket =
                Sphinx.parsePacket (privKeys.[1]) (associatedData) (packet1.ToBytes())
                |> Result.defaultWith(fun _ -> failwith "Fail: bolt4 last node replies with err msg defaultClosure1")
            let { NextPacket = packet3; SharedSecret = ss2 }: ParsedPacket =
                Sphinx.parsePacket (privKeys.[2]) (associatedData) (packet2.ToBytes())
                |> Result.defaultWith(fun _ -> failwith "Fail: bolt4 last node replies with err msg defaultClosure2")
            let { NextPacket = packet4; SharedSecret = ss3 }: ParsedPacket =
                Sphinx.parsePacket (privKeys.[3]) (associatedData) (packet3.ToBytes())
                |> Result.defaultWith(fun _ -> failwith "Fail: bolt4 last node replies with err msg defaultClosure3")
            let { NextPacket = packet5; SharedSecret = ss4 }: ParsedPacket =
                Sphinx.parsePacket (privKeys.[4]) (associatedData) (packet4.ToBytes())
                |> Result.defaultWith(fun _ -> failwith "Fail: bolt4 last node replies with err msg defaultClosure4")

            Expect.isTrue (packet5.IsLastPacket) ""
            let error = ErrorPacket.Create(ss4, { FailureMsg.Code = FailureCode (OnionError.TEMPORARY_NODE_FAILURE); Data = TemporaryNodeFailure })
            let _ =
                let expected = "a5e6bd0c74cb347f10cce367f949098f2457d14c046fd8a22cb96efb30b0fdcda8cb9168b50f2fd45edd73c1b0c8b33002df376801ff58aaa94000bf8a86f92620f343baef38a580102395ae3abf9128d1047a0736ff9b83d456740ebbb4aeb3aa9737f18fb4afb4aa074fb26c4d702f42968888550a3bded8c05247e045b866baef0499f079fdaeef6538f31d44deafffdfd3afa2fb4ca9082b8f1c465371a9894dd8c243fb4847e004f5256b3e90e2edde4c9fb3082ddfe4d1e734cacd96ef0706bf63c9984e22dc98851bcccd1c3494351feb458c9c6af41c0044bea3c47552b1d992ae542b17a2d0bba1a096c78d169034ecb55b6e3a7263c26017f033031228833c1daefc0dedb8cf7c3e37c9c37ebfe42f3225c326e8bcfd338804c145b16e34e4" |> hex.DecodeData
                Expect.equal(expected) error ""

            let error1 = Sphinx.forwardErrorPacket(error, ss3)
            let _ =
                let expected = "c49a1ce81680f78f5f2000cda36268de34a3f0a0662f55b4e837c83a8773c22aa081bab1616a0011585323930fa5b9fae0c85770a2279ff59ec427ad1bbff9001c0cd1497004bd2a0f68b50704cf6d6a4bf3c8b6a0833399a24b3456961ba00736785112594f65b6b2d44d9f5ea4e49b5e1ec2af978cbe31c67114440ac51a62081df0ed46d4a3df295da0b0fe25c0115019f03f15ec86fabb4c852f83449e812f141a9395b3f70b766ebbd4ec2fae2b6955bd8f32684c15abfe8fd3a6261e52650e8807a92158d9f1463261a925e4bfba44bd20b166d532f0017185c3a6ac7957adefe45559e3072c8dc35abeba835a8cb01a71a15c736911126f27d46a36168ca5ef7dccd4e2886212602b181463e0dd30185c96348f9743a02aca8ec27c0b90dca270"
                               |> hex.DecodeData
                Expect.equal expected error1 ""

            let error2 = Sphinx.forwardErrorPacket(error1, ss2)
            let _ =
                let expected = "a5d3e8634cfe78b2307d87c6d90be6fe7855b4f2cc9b1dfb19e92e4b79103f61ff9ac25f412ddfb7466e74f81b3e545563cdd8f5524dae873de61d7bdfccd496af2584930d2b566b4f8d3881f8c043df92224f38cf094cfc09d92655989531524593ec6d6caec1863bdfaa79229b5020acc034cd6deeea1021c50586947b9b8e6faa83b81fbfa6133c0af5d6b07c017f7158fa94f0d206baf12dda6b68f785b773b360fd0497e16cc402d779c8d48d0fa6315536ef0660f3f4e1865f5b38ea49c7da4fd959de4e83ff3ab686f059a45c65ba2af4a6a79166aa0f496bf04d06987b6d2ea205bdb0d347718b9aeff5b61dfff344993a275b79717cd815b6ad4c0beb568c4ac9c36ff1c315ec1119a1993c4b61e6eaa0375e0aaf738ac691abd3263bf937e3"
                               |> hex.DecodeData
                Expect.equal expected error2 ""

            let error3 = Sphinx.forwardErrorPacket (error2, ss1)
            let _ =
                let expected = "aac3200c4968f56b21f53e5e374e3a2383ad2b1b6501bbcc45abc31e59b26881b7dfadbb56ec8dae8857add94e6702fb4c3a4de22e2e669e1ed926b04447fc73034bb730f4932acd62727b75348a648a1128744657ca6a4e713b9b646c3ca66cac02cdab44dd3439890ef3aaf61708714f7375349b8da541b2548d452d84de7084bb95b3ac2345201d624d31f4d52078aa0fa05a88b4e20202bd2b86ac5b52919ea305a8949de95e935eed0319cf3cf19ebea61d76ba92532497fcdc9411d06bcd4275094d0a4a3c5d3a945e43305a5a9256e333e1f64dbca5fcd4e03a39b9012d197506e06f29339dfee3331995b21615337ae060233d39befea925cc262873e0530408e6990f1cbd233a150ef7b004ff6166c70c68d9f8c853c1abca640b8660db2921"
                               |> hex.DecodeData
                Expect.equal expected error3 ""
            
            let error4 = Sphinx.forwardErrorPacket(error3, ss0)
            let _ =
                let expected = "9c5add3963fc7f6ed7f148623c84134b5647e1306419dbe2174e523fa9e2fbed3a06a19f899145610741c83ad40b7712aefaddec8c6baf7325d92ea4ca4d1df8bce517f7e54554608bf2bd8071a4f52a7a2f7ffbb1413edad81eeea5785aa9d990f2865dc23b4bc3c301a94eec4eabebca66be5cf638f693ec256aec514620cc28ee4a94bd9565bc4d4962b9d3641d4278fb319ed2b84de5b665f307a2db0f7fbb757366067d88c50f7e829138fde4f78d39b5b5802f1b92a8a820865af5cc79f9f30bc3f461c66af95d13e5e1f0381c184572a91dee1c849048a647a1158cf884064deddbf1b0b88dfe2f791428d0ba0f6fb2f04e14081f69165ae66d9297c118f0907705c9c4954a199bae0bb96fad763d690e7daa6cfda59ba7f2c8d11448b604d12d"
                               |> hex.DecodeData
                Expect.equal expected error4 ""
            let { OriginNode = pubkey; FailureMsg = failure } =
                ErrorPacket.TryParse(error4, ss)
                |> Result.defaultWith(fun _ -> failwith "Fail: bolt4 last node replies with err msg defaultClosure end")
            Expect.equal (pubKeys.[4]) (pubkey.Value) ""
            Expect.equal (TemporaryNodeFailure) (failure.Data) ""
            ()

        testCase "Intermediate node replies with an error message" <| fun _ ->
            let { Packet = packet; SharedSecrets = ss } =
                Sphinx.PacketAndSecrets.Create(sessionKey, pubKeys, payloads, associatedData, PacketFiller.BlankPacketFiller)
            let { NextPacket = packet1; SharedSecret = ss0 } =
                Sphinx.parsePacket(privKeys.[0]) (associatedData) (packet.ToBytes())
                |> Result.defaultWith(fun _ -> failwith "Fail: bolt4 intrm node replies with err msg defaultClosure0")
            let { NextPacket = packet2; SharedSecret = ss1 } =
                Sphinx.parsePacket(privKeys.[1]) (associatedData) (packet1.ToBytes())
                |> Result.defaultWith(fun _ -> failwith "Fail: bolt4 intrm node replies with err msg defaultClosure1")
            let { SharedSecret = ss2 } =
                Sphinx.parsePacket(privKeys.[2]) (associatedData) (packet2.ToBytes())
                |> Result.defaultWith(fun _ -> failwith "Fail: bolt4 intrm node replies with err msg defaultClosure2")
            
            let error = ErrorPacket.Create(ss2, { Code = OnionError.FailureCode INVALID_REALM; Data = InvalidRealm })
            let error1 = forwardErrorPacket(error, ss1)
            let error2 = forwardErrorPacket(error1, ss0)
            let { OriginNode = pubkey; FailureMsg = failure } =
                ErrorPacket.TryParse(error2, ss)
                |> Result.defaultWith(fun _ -> failwith "Fail: bolt4 intrm node replies with err msg defaultClosure end")
            Expect.equal (pubkey.Value) (pubKeys.[2]) ""
            Expect.equal (InvalidRealm) (failure.Data) ""
            ()
    ]
    
