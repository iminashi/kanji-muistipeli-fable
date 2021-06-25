module Utils

open Thoth.Json
open Types

/// Formats a time in milliseconds into minutes, seconds and hundreths of a second.
let formatTime time =
    let hundreths = (time / 100) % 10
    let seconds = time / 1000
    let minutes = seconds / 60

    sprintf "%02i:%02i.%i" minutes (seconds % 60) hundreths

let kanjiDecoder : Decoder<KanjiDefinition> =
    Decode.object (fun get -> {
        Meaning = get.Required.At [ "meaning" ] Decode.string
        Kun = get.Required.At [ "kun" ] Decode.string
        On = get.Required.At [ "on" ] Decode.string
    })
