module ChannelInitializationTests

open FSharp.Control.Reactive
open DotNetLightning.Utils.Primitives
open NBitcoin
open CustomEventAggregator
open DotNetLightning.Peer
open DotNetLightning.Channel
open DotNetLightning.Infrastructure

open System
open System.Net
open System.Threading.Tasks
open DotNetLightning.Chain
open DotNetLightning.Infrastructure.ActorManagers
open DotNetLightning.Infrastructure.Actors
open DotNetLightning.Infrastructure.Interfaces
open DotNetLightning.Serialize.Msgs
open DotNetLightning.Utils
open Expecto
open Foq
open Microsoft.Extensions.Options
open TestConstants

let dummyBlockChainInstanceId = 
    "8888888888888888888888888888888888888888888888888888888888888888"
    |> BlockChainInstanceId
let n = Network.RegTest

let temporaryChannelId =
    "5555555555555555555555555555555555555555555555555555555555555555"
    |> hex.DecodeData
    |> uint256
    |> ChannelId
    
            
let defaultFinalScriptPubKey =
    "5555555555555555555555555555555555555555555555555555555555555555"
    |> hex.DecodeData
    |> Key
    |> fun k -> k.PubKey.WitHash.ScriptPubKey
type internal ActorCreator =
    static member hex = NBitcoin.DataEncoders.HexEncoder()
    
    
    static member getAlice(?keyRepo: IKeysRepository, ?nodeParams) =
        let aliceParam = TestConstants.getAliceParam()
        let keyRepo = defaultArg keyRepo (aliceParam.KeyRepo)
        let peerLogger = TestLogger.create<PeerManager>(ConsoleColor.Red)
        let channelLogger = TestLogger.create<ChannelManager>(ConsoleColor.Magenta)
        let nodeParams = defaultArg nodeParams (Options.Create<ChainConfig>(aliceParam.NodeParams))
        let chainWatcher =
            Mock<IChainWatcher>()
                .Setup(fun x -> <@ x.InstallWatchTx(any(), any()) @>).Returns(true)
                .Setup(fun x -> <@ x.InstallWatchOutPoint(any(), any()) @>).Returns(true)
                .Setup(fun x -> <@ x.WatchAllTxn() @>).Returns(true)
                .Create()
        let broadCaster = DummyBroadCaster()
        let eventAggregator = ReactiveEventAggregator() :> IEventAggregator
        let fundingTxProvider = DummyFundingTxProvider(aliceParam.NodeParams.Network.NBitcoinNetwork)
        {
            NodeParams = aliceParam.NodeParams
            PeerManagerEntity.Id = IPEndPoint.Parse("127.1.1.1") :> EndPoint |> PeerId
            PM = PeerManager(eventAggregator,
                             peerLogger,
                             getTestLoggerFactory(),
                             keyRepo,
                             nodeParams.Value,
                             chainWatcher,
                             broadCaster,
                             DotNetLightningNetworkProvider.getNetwork (NetworkType.Mainnet) ("BTC")
                             )
            CM =
                let channelEventRepo =
                    Mock<IChannelEventStream>
                        .Method(fun x -> <@ x.SetEventsAsync @>).Returns(Task.CompletedTask)
                let chainListener = Mock<IChainListener>().Create()
                let feeEstimator =
                    Mock<IFeeEstimator>.Method(fun x -> <@ x.GetEstSatPer1000Weight @>).Returns(5000u |> FeeRatePerKw)
                    
                ChannelManager(channelLogger,
                               getTestLoggerFactory(),
                               eventAggregator,
                               keyRepo,
                               channelEventRepo,
                               feeEstimator,
                               fundingTxProvider,
                               nodeParams.Value
                               )
            EventAggregator = eventAggregator
            CurrentHeight = 100
            FundingTxProvider = fundingTxProvider
        }
    
    static member getBob(?keyRepo: IKeysRepository, ?nodeParams) =
        let bobParam = TestConstants.getBobParam()
        let keyRepo = defaultArg keyRepo (bobParam.KeyRepo)
        let peerLogger =
            // Mock<ILogger<PeerManager>>().Create()
            TestLogger.create<PeerManager>(ConsoleColor.Blue)
        let channelLogger =
            // Mock<ILogger<ChannelManager>>().Create()
            TestLogger.create<ChannelManager>(ConsoleColor.Green)
        let nodeParams = defaultArg nodeParams (Options.Create<ChainConfig>(bobParam.NodeParams))
        let chainWatcher =
            Mock<IChainWatcher>()
                .Setup(fun x -> <@ x.InstallWatchTx(any(), any()) @>).Returns(true)
                .Setup(fun x -> <@ x.InstallWatchOutPoint(any(), any()) @>).Returns(true)
                .Setup(fun x -> <@ x.WatchAllTxn() @>).Returns(true)
                .Create()
        let broadCaster = DummyBroadCaster()
        let eventAggregator = ReactiveEventAggregator() :> IEventAggregator
        let fundingTxProvider = DummyFundingTxProvider(bobParam.NodeParams.Network.NBitcoinNetwork)
        {
            NodeParams = bobParam.NodeParams
            PeerManagerEntity.Id = IPEndPoint.Parse("127.1.1.2") :> EndPoint |> PeerId
            PM = PeerManager(eventAggregator,
                             peerLogger,
                             getTestLoggerFactory(),
                             keyRepo,
                             nodeParams.Value,
                             chainWatcher,
                             broadCaster,
                             DotNetLightningNetworkProvider.getNetwork (NetworkType.Mainnet) ("BTC")
                             )
            CM =
                let channelEventRepo =
                    Mock<IChannelEventStream>
                        .Method(fun x -> <@ x.SetEventsAsync @>).Returns(Task.CompletedTask)
                let chainListener = Mock<IChainListener>().Create()
                let feeEstimator =
                    Mock<IFeeEstimator>.Method(fun x -> <@ x.GetEstSatPer1000Weight @>).Returns(5000u |> FeeRatePerKw)
                ChannelManager(channelLogger,
                               getTestLoggerFactory(),
                               eventAggregator,
                               keyRepo,
                               channelEventRepo,
                               feeEstimator,
                               fundingTxProvider,
                               nodeParams.Value
                               )
            EventAggregator = eventAggregator
            CurrentHeight = 100
            FundingTxProvider = fundingTxProvider
        }
    
    static member initiateActor(alice, bob) = async {
        let actors = new PeerActors(alice, bob)
        let t1 = actors.Initiator.EventAggregator.AwaitPeerEvent(function | { PeerEvent = PeerEvent.ActTwoProcessed _ } -> Some () | _ -> None)
        let t2 = actors.Responder.EventAggregator.AwaitPeerEvent(function | { PeerEvent = PeerEvent.ActThreeProcessed _ } -> Some () | _ -> None)
        let bobNodeId = bob.CM.KeysRepository.GetNodeSecret().PubKey |> NodeId
        do! actors.Launch(bobNodeId) |> Async.AwaitTask
        let! _ = t1
        let! _ = t2
        return actors
    }
    
    static member initiateOpenedChannel(alice, bob) = async {
        let bobInitTask = alice.EventAggregator.GetObservable<PeerEventWithContext>()
                          |> Observable.awaitFirst(function | { PeerEvent = ReceivedInit(initMsg, _) } -> Some initMsg | _ -> None)
        let! actors = ActorCreator.initiateActor(alice, bob)
        let! bobInit = bobInitTask
        let channelKeys = actors.Initiator.CM.KeysRepository.GetChannelKeys(false)
        let initFunder = { InputInitFunder.PushMSat = TestConstants.pushMsat
                           TemporaryChannelId = temporaryChannelId
                           FundingSatoshis = TestConstants.fundingSatoshis
                           InitFeeRatePerKw = TestConstants.feeratePerKw
                           FundingTxFeeRatePerKw = TestConstants.feeratePerKw
                           LocalParams =
                               alice.PM.MakeLocalParams(channelKeys.ToChannelPubKeys() ,defaultFinalScriptPubKey, true, TestConstants.fundingSatoshis)
                           RemoteInit = if (bobInit.IsSome) then bobInit.Value else failwith "alice did not receive init from bob"
                           ChannelFlags = 0x00uy
                           ChannelKeys = channelKeys }
        
        /// prepare tasks
        let bobAcceptedFundingCreatedTask =
            bob.EventAggregator.AwaitChannelEvent(function WeAcceptedFundingCreated(i, _) -> Some i | _ -> None)
        let aliceAcceptedFundingSignedTask =
            alice.EventAggregator.AwaitChannelEvent(function WeAcceptedFundingSigned(i, _) -> Some i | _ -> None)
        let aliceBothFundingLockedTask =
            alice.EventAggregator.AwaitChannelEvent(function BothFundingLocked _ -> Some () | _ -> None)
        let bobBothFundingLockedTask =
            bob.EventAggregator.AwaitChannelEvent(function BothFundingLocked _ -> Some () | _ -> None)
            
        /// give create outbound command
        let bobNodeId = bob.CM.KeysRepository.GetNodeSecret().PubKey |> NodeId
        do! alice.CM.AcceptCommandAsync({ NodeId = bobNodeId; ChannelCommand = ChannelCommand.CreateOutbound(initFunder) }).AsTask() |> Async.AwaitTask
        
        /// wait for both peer exchange initiation messages
        let! r = bobAcceptedFundingCreatedTask
        Expect.isSome r "timeout"
        let! r = aliceAcceptedFundingSignedTask
        Expect.isSome r "timeout"
            
        
        /// Confirm funding tx
        let fundingTx = (alice.FundingTxProvider :?> DummyFundingTxProvider).DummyTx
        alice.PublishDummyBlockWith([fundingTx])
        alice.PublishDummyBlockWith([])
        alice.PublishDummyBlockWith([])
        bob.PublishDummyBlockWith([fundingTx])
        bob.PublishDummyBlockWith([])
        bob.PublishDummyBlockWith([])
        
        let! r = aliceBothFundingLockedTask
        Expect.isSome r "timeout"
        
        let! r = bobBothFundingLockedTask
        Expect.isSome r "timeout"
            
        
        return actors
    }
    
    static member sendMonoHopUnidirectionalPayment alice bob = async {
        let bobNodeId = bob.CM.KeysRepository.GetNodeSecret().PubKey |> NodeId
        let aliceNodeId = alice.CM.KeysRepository.GetNodeSecret().PubKey |> NodeId

        do! Async.Sleep 1000
        Console.WriteLine("testing mono-hop unidirectional payment")


        let aliceToBobAmount = TestConstants.monoHopPaymentAmount
        let monoHopUnidirectionalPaymentCmd = {
            Amount = aliceToBobAmount
        }

        // Send mono-hop unidirectional payment from alice to bob
        let bobAcceptedMonoHopUnidirectionalPaymentTask =
            bob.EventAggregator.AwaitChannelEvent(function WeAcceptedMonoHopUnidirectionalPayment _ -> Some () | _ -> None)
        do!
            alice.CM.AcceptCommandAsync({
                NodeId = bobNodeId
                ChannelCommand = ChannelCommand.MonoHopUnidirectionalPayment monoHopUnidirectionalPaymentCmd
            }).AsTask() |> Async.AwaitTask

        let! r = bobAcceptedMonoHopUnidirectionalPaymentTask
        Expect.isSome r "timeout waiting for bob to accept mono-hop unidirectional payment"
    }
    
[<Tests>]
let tests =
    testList "Basic Channel handling between 2 peers" [
        testAsync "Channel Initialization" {

            let alice = ActorCreator.getAlice()
            let bob = ActorCreator.getBob()
            let bobInitTask = alice.EventAggregator.AwaitPeerEvent((function | { PeerEvent = ReceivedInit(initMsg, _) } -> Some initMsg | _ -> None))
            let! _actors = ActorCreator.initiateActor(alice, bob)
            
            let! bobInit = bobInitTask
            
            let channelKeys = alice.CM.KeysRepository.GetChannelKeys(false)
            let initFunder = { InputInitFunder.PushMSat = TestConstants.pushMsat
                               TemporaryChannelId = temporaryChannelId
                               FundingSatoshis = TestConstants.fundingSatoshis
                               InitFeeRatePerKw = TestConstants.feeratePerKw
                               FundingTxFeeRatePerKw = TestConstants.feeratePerKw
                               LocalParams =
                                   alice.PM.MakeLocalParams(channelKeys.ToChannelPubKeys() ,defaultFinalScriptPubKey, true, TestConstants.fundingSatoshis)
                               RemoteInit = if (bobInit.IsSome) then bobInit.Value else failwith "alice did not receive init from bob"
                               ChannelFlags = 0x00uy
                               ChannelKeys = channelKeys }
            let aliceChannelEventFuture =
                alice.EventAggregator.AwaitChannelEvent()
            let bobStartedNewInboundChannelTask =
                bob.EventAggregator.AwaitChannelEvent(function NewInboundChannelStarted msg -> Some msg | _ -> None)
            let bobAcceptedOpenChannelTask =
                bob.EventAggregator.AwaitChannelEvent(function WeAcceptedOpenChannel(acceptChannel, _state) -> Some acceptChannel | _ -> None)
                
            let aliceReceivedAcceptChannelTask =
                alice.EventAggregator.AwaitChannelEvent(function WeAcceptedAcceptChannel(i, _) -> Some i | _ -> None)
            
            let bobAcceptedFundingCreatedTask =
                bob.EventAggregator.AwaitChannelEvent(function WeAcceptedFundingCreated(i, _) -> Some i | _ -> None)
                
            let aliceAcceptedFundingSignedTask =
                alice.EventAggregator.AwaitChannelEvent(function WeAcceptedFundingSigned(i, _) -> Some i | _ -> None)
               
            let aliceFundingConfirmedTask =
                alice.EventAggregator.AwaitChannelEvent(function FundingConfirmed _ -> Some () | _ -> None)
                
            let aliceSentFundingLockedTask =
                alice.EventAggregator.AwaitChannelEvent(function WeSentFundingLocked _ -> Some () | _ -> None)
                
            let bobReceivedFundingLockedTask =
                bob.EventAggregator.AwaitChannelEvent(function TheySentFundingLocked _ -> Some () | _ -> None)
                
            let aliceBothFundingLockedTask =
                alice.EventAggregator.AwaitChannelEvent(function BothFundingLocked _ -> Some () | _ -> None)
            let bobBothFundingLockedTask =
                bob.EventAggregator.AwaitChannelEvent(function BothFundingLocked _ -> Some () | _ -> None)
            
            let bobNodeId = bob.CM.KeysRepository.GetNodeSecret().PubKey |> NodeId
            do! alice.CM.AcceptCommandAsync({ NodeId = bobNodeId; ChannelCommand = ChannelCommand.CreateOutbound(initFunder) }).AsTask() |> Async.AwaitTask
            
            let! r = aliceChannelEventFuture
            Expect.isSome r "timeout"
            let! r = bobStartedNewInboundChannelTask
            Expect.isSome (r) "timeout"
            let! r = bobAcceptedOpenChannelTask
            Expect.isSome (r) "timeout"
            
            let! r = aliceReceivedAcceptChannelTask
            Expect.isSome r "timeout"
            
            let! r = bobAcceptedFundingCreatedTask
            Expect.isSome r "timeout"
            
            let! r = aliceAcceptedFundingSignedTask
            Expect.isSome r "timeout"
            
            let fundingTx = (alice.FundingTxProvider :?> DummyFundingTxProvider).DummyTx
            
            // We need three confirmation before we consider funding is locked
            // (3 is default for NodeParams)
            alice.PublishDummyBlockWith([fundingTx])
            alice.PublishDummyBlockWith([])
            alice.PublishDummyBlockWith([])
            let! r = aliceFundingConfirmedTask
            Expect.isSome r "timeout"
            let! r = aliceSentFundingLockedTask
            Expect.isSome r "timeout"
            
            let! r = bobReceivedFundingLockedTask
            Expect.isSome r "timeout"
            
            bob.PublishDummyBlockWith([fundingTx])
            bob.PublishDummyBlockWith([])
            bob.PublishDummyBlockWith([])
            
            let! r = aliceBothFundingLockedTask
            Expect.isSome r "timeout"
            
            let! r = bobBothFundingLockedTask
            Expect.isSome r "timeout"
            
            return ()
        }

        testAsync "Send mono-hop unidirectional payment" {
            do! Async.Sleep 1000

            Console.WriteLine("starting mono-hop unidirectional payment test")
            let alice = ActorCreator.getAlice()
            let bob = ActorCreator.getBob()
            let! actors = ActorCreator.initiateOpenedChannel(alice, bob)
            let bobNodeId = bob.CM.KeysRepository.GetNodeSecret().PubKey |> NodeId
            let aliceNodeId = alice.CM.KeysRepository.GetNodeSecret().PubKey |> NodeId

            do! ActorCreator.sendMonoHopUnidirectionalPayment alice bob

            do! Async.Sleep 1000
            Console.WriteLine("alice signs her commit")

            let aliceAcceptedSign =
                alice.EventAggregator.AwaitChannelEvent(function WeAcceptedOperationSign (_, commitments) -> Some commitments | _ -> None)
            let aliceAcceptedRevokeAndAck =
                alice.EventAggregator.AwaitChannelEvent(function WeAcceptedRevokeAndACK commitments -> Some commitments | _ -> None)
            let bobAcceptedSign =
                bob.EventAggregator.AwaitChannelEvent(function WeAcceptedCommitmentSigned (revokeAndAck, commitments) -> Some (revokeAndAck, commitments) | _ -> None)
            do! alice.CM.AcceptCommandAsync({ NodeId = bobNodeId; ChannelCommand = SignCommitment }).AsTask() |> Async.AwaitTask
            let! r = aliceAcceptedSign
            Expect.isSome r "timeout waiting for alice to accept sign command"
            let! r = bobAcceptedSign
            Expect.isSome r "timeout waiting for bob to accept commitment signature"
            let revokeAndAck, bobCommitments = r.Value
            let! r = aliceAcceptedRevokeAndAck
            Expect.isSome r "timeout waiting for alice to accept revoke_and_ack"
            let aliceCommitments = r.Value


            do! Async.Sleep 1000
            Console.WriteLine("bob signs his commit")

            let bobAcceptedSign =
                bob.EventAggregator.AwaitChannelEvent(function WeAcceptedOperationSign (_, commitments) -> Some commitments | _ -> None)
            let bobAcceptedRevokeAndAck =
                bob.EventAggregator.AwaitChannelEvent(function WeAcceptedRevokeAndACK commitments -> Some commitments | _ -> None)
            let aliceAcceptedSign =
                alice.EventAggregator.AwaitChannelEvent(function WeAcceptedCommitmentSigned (revokeAndAck, commitments) -> Some (revokeAndAck, commitments) | _ -> None)
            do! bob.CM.AcceptCommandAsync({ NodeId = aliceNodeId; ChannelCommand = SignCommitment }).AsTask() |> Async.AwaitTask
            let! r = bobAcceptedSign
            Expect.isSome r "timeout waiting for bob to accept sign command"
            let! r = aliceAcceptedSign
            Expect.isSome r "timeout waiting for alice to accept commitment signature"
            let revokeAndAck, aliceCommitments = r.Value
            let! r = bobAcceptedRevokeAndAck
            Expect.isSome r "timeout waiting for bob to accept revoke_and_ack"
            let bobCommitments = r.Value

            let aliceInitialBalance = LNMoney.Satoshis(TestConstants.fundingSatoshis.Satoshi) - TestConstants.pushMsat
            let aliceFinalBalance = aliceCommitments.LocalCommit.Spec.ToLocal
            let aliceExpectedFinalBalance = aliceInitialBalance - TestConstants.monoHopPaymentAmount

            let bobInitialBalance = TestConstants.pushMsat
            let bobFinalBalance = bobCommitments.LocalCommit.Spec.ToLocal
            let bobExpectedFinalBalance = bobInitialBalance + TestConstants.monoHopPaymentAmount

            do! Async.Sleep 1000

            Expect.equal
                aliceFinalBalance
                aliceExpectedFinalBalance
                "alice's balance was not updated correctly"

            Expect.equal
                bobFinalBalance
                bobExpectedFinalBalance
                "bob's balance was not updated correctly"

            Console.WriteLine("finishing mono-hop unidirectional payment with revoke_and_ack test")
        }

        ptestAsync "Normal channel operation" {
            let alice = ActorCreator.getAlice()
            let bob = ActorCreator.getBob()
            let! actors = ActorCreator.initiateOpenedChannel(alice, bob)
            let bobNodeId = bob.CM.KeysRepository.GetNodeSecret().PubKey |> NodeId
            let aliceNodeId = alice.CM.KeysRepository.GetNodeSecret().PubKey |> NodeId
            let paymentPreImages =
                List.init 9 (fun i ->
                    PaymentPreimage.Create([|for _ in 0..31 -> (uint8 i)|])
                )
                
            let baseAddHTLCOperation = { OperationAddHTLC.Expiry = BlockHeight 130u
                                         Amount = LNMoney.Zero
                                         PaymentHash = paymentPreImages.[0].Hash
                                         Onion = OnionPacket.LastPacket
                                         Upstream = None
                                         Origin = None
                                         CurrentHeight = BlockHeight 101u }
                
            // send update_add_htlc from alice to bob
            let addHtlcOperation = { baseAddHTLCOperation with Amount = LNMoney.MilliSatoshis 1000L }
            do! alice.CM.AcceptCommandAsync({ NodeId = bobNodeId; ChannelCommand = ChannelCommand.AddHTLC addHtlcOperation }).AsTask() |> Async.AwaitTask
            let bobAcceptedAddHTLCTask =
                bob.EventAggregator.AwaitChannelEvent(function WeAcceptedUpdateAddHTLC _ -> Some () | _ -> None)
                
            let! r = bobAcceptedAddHTLCTask
            Expect.isSome r "timeout"
            
            // 1. send update_add_htlc from bob to alice.
            // 2. send from alice to bob again.
            let aliceAcceptedAddHTLCTask =
                alice.EventAggregator.AwaitChannelEvent(function WeAcceptedUpdateAddHTLC _ -> Some () | _ -> None)
            let bobAcceptedAddHTLCTask =
                bob.EventAggregator.AwaitChannelEvent(function WeAcceptedUpdateAddHTLC _ -> Some () | _ -> None)
            let addHtlcOperation = { baseAddHTLCOperation with
                                         Amount = LNMoney.MilliSatoshis 100000L
                                         PaymentHash = paymentPreImages.[1].Hash }
            do! bob.CM.AcceptCommandAsync({ NodeId = aliceNodeId; ChannelCommand = AddHTLC addHtlcOperation }).AsTask() |> Async.AwaitTask
            
            let addHtlcOperation = { baseAddHTLCOperation with
                                         Amount = LNMoney.MilliSatoshis 40000L
                                         PaymentHash = paymentPreImages.[2].Hash }
            do! alice.CM.AcceptCommandAsync({ NodeId = bobNodeId; ChannelCommand = AddHTLC addHtlcOperation }).AsTask() |> Async.AwaitTask
            
            let! r = aliceAcceptedAddHTLCTask
            Expect.isSome r "timeout"
            let! r = bobAcceptedAddHTLCTask
            Expect.isSome r "timeout"
            
            // sign commitment
            do! alice.CM.AcceptCommandAsync({ NodeId = bobNodeId; ChannelCommand = SignCommitment }).AsTask() |> Async.AwaitTask
            return ()
        }
        
    ]

