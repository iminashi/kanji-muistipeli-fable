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

let gameTypeDecoder : Decoder<GameType> =
    Decode.string
    |> Decode.andThen (function
        | "Emoji" ->
            Decode.succeed EmojiGame
        | kanji when kanji.StartsWith "Kanji" ->
            match kanji.Split " " |> Array.tryItem 1 with
            | Some "all" ->
                AllLevels
            | Some level ->
                Level (int level)
            | None ->
                Level 1
            |> KanjiGame
            |> Decode.succeed
        | other ->
            Decode.fail $"Failed to decode game type: {other}")

let gameTypeEncoder (gameType: GameType) =
    match gameType with
    | EmojiGame ->
        Encode.string "Emoji"
    | KanjiGame kanjiLevel ->
        let level =
            match kanjiLevel with
            | AllLevels -> "all"
            | Level lvl -> string lvl
        Encode.string $"Kanji {level}"

let rubyRevealDecoder : Decoder<RubyRevealType> =
    Decode.string
    |> Decode.andThen (function
        | "Meaning" ->
            Decode.succeed Meaning
        | "Kun" ->
            Decode.succeed Kun
        | "On" ->
            Decode.succeed On
        | "Random" ->
            Decode.succeed Random
        | other ->
            Decode.fail $"Failed to decode ruby reveal type: {other}")

let rubyRevealEncoder (ruby: RubyRevealType) =
    Encode.string (string ruby)

let difficultyDecoder : Decoder<Difficulty> =
    Decode.string
    |> Decode.andThen (function
        | "Easy" ->
            Decode.succeed Easy
        | "Normal" ->
            Decode.succeed Normal
        | "Hard" ->
            Decode.succeed Hard
        | "Hardest" ->
            Decode.succeed Hardest
        | other ->
            Decode.fail $"Failed to decode difficulty: {other}")

let difficultyEncoder (difficulty: Difficulty) =
    Encode.string (string difficulty)

let settingsDecoder : Decoder<Settings> =
    Decode.object (fun get -> {
        Game = get.Required.At [ "Game" ] gameTypeDecoder
        RubyReveal = get.Required.At [ "RubyReveal" ] rubyRevealDecoder
        Difficulty = get.Required.At [ "Difficulty" ] difficultyDecoder
    })

let settingsEncoder (settings: Settings) =
    [ "Game", gameTypeEncoder settings.Game
      "RubyReveal", rubyRevealEncoder settings.RubyReveal
      "Difficulty", difficultyEncoder settings.Difficulty ]
    |> Encode.object
