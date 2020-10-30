namespace DotNetLightning.Utils

type SeqConsumer<'S, 'T> = {
    Consume: seq<'S> -> Option<seq<'S> * 'T>
}

type SeqConsumerBuilder<'S>() =
    member __.Bind<'T0, 'T1>(seqConsumer0: SeqConsumer<'S, 'T0>, func: 'T0 -> SeqConsumer<'S, 'T1>)
                                : SeqConsumer<'S, 'T1> = {
        Consume = fun (sequence0: seq<'S>) ->
            match seqConsumer0.Consume sequence0 with
            | None -> None
            | Some (sequence1, value0) ->
                let seqConsumer1 = func value0
                seqConsumer1.Consume sequence1
    }

    member __.Return<'T>(value: 'T)
                            : SeqConsumer<'S, 'T> = {
        Consume = fun (sequence: seq<'S>) -> Some (sequence, value)
    }

    member __.ReturnFrom<'T>(seqConsumer: SeqConsumer<'S, 'T>): SeqConsumer<'S, 'T> =
        seqConsumer

    member __.Zero(): SeqConsumer<'S, unit> = {
        Consume = fun (sequence: seq<'S>) -> Some (sequence, ())
    }

module SeqConsumer =
    let seqConsumer<'S> = SeqConsumerBuilder<'S>()

    let NextInSeq<'S>(): SeqConsumer<'S, 'S> = {
        Consume = fun (sequence: seq<'S>) ->
            match Seq.tryHead sequence with
            | None -> None
            | Some value -> Some (Seq.tail sequence, value)
    }

    let AbortSeqConsumer<'S, 'T>(): SeqConsumer<'S, 'T> = {
        Consume = fun (_sequence: seq<'S>) -> None
    }

    type ConsumeAllError =
        | SequenceEndedTooEarly
        | SequenceNotReadToEnd

    let ConsumeAll<'S, 'T>(sequence: seq<'S>) (seqConsumer: SeqConsumer<'S, 'T>)
                              : Result<'T, ConsumeAllError> =
        match seqConsumer.Consume sequence with
        | None -> Error SequenceEndedTooEarly
        | Some (consumedSequence, value) ->
            if Seq.isEmpty consumedSequence then
                Ok value
            else
                Error SequenceNotReadToEnd


