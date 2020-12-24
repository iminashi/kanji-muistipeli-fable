module Muistipeli

open Feliz
open Elmish

type KanjiLevel =
    | Level1
    | Level2
    | Level3
    | Level4
    | Level5
    | Level6
    | AllLevels

let getKanjiArray = function
    | Level1 -> Symbols.kanjiLevels.[0]
    | Level2 -> Symbols.kanjiLevels.[1]
    | Level3 -> Symbols.kanjiLevels.[2]
    | Level4 -> Symbols.kanjiLevels.[3]
    | Level5 -> Symbols.kanjiLevels.[4]
    | Level6 -> Symbols.kanjiLevels.[5]
    | AllLevels -> Array.concat Symbols.kanjiLevels

type GameType =
    | EmojiGame
    | KanjiGame of level : KanjiLevel

type RubyRevealType =
    | Meaning
    | Kun
    | On
    | Random

type Difficulty =
    | Easy
    | Normal
    | Hard
    | Hardest

let cardsForDifficulty = function
    | Easy -> 12
    | Normal -> 20
    | Hard -> 30
    | Hardest -> 42

let cardsPerRowForDifficulty = function
    | Easy -> 3
    | Normal -> 5
    | Hard -> 6
    | Hardest -> 7

type Settings =
    { Game : GameType
      RubyReveal : RubyRevealType
      Difficulty : Difficulty }

type Kanji =
    { Character : string
      Kun : string option
      On : string option
      Meaning : string }

type Symbol =
    | Emoji of string
    | Kanji of Kanji
    
type Card =
    { Symbol : Symbol
      RubyText : string option }

type Model =
    { FirstClicked : int option
      SecondClicked : int option
      PairsFound : int
      Cards : Card list
      RevealedCards : Set<int>
      GameWon : bool
      ShowSettings: bool
      NextCardTimeout : int option
      HideCardsTimeout : int option
      Settings : Settings }

type Msg =
    | CardClicked of index : int
    | NewGame
    | CreateCards
    | CreateCard of num : int * deck : Card array
    | HideCards
    | UpdateSettings of Settings
    | ToggleSettings
    | SetGameType of GameType
    | SetRevealType of RubyRevealType
    | SetDifficulty of Difficulty
    | SetNextCardTimeout of int
    | SetHideCardsTimeout of int

let swap i1 i2 (arr : 'a array)  =
    let temp = arr.[i1]
    arr.[i1] <- arr.[i2]
    arr.[i2] <- temp

let getRandomCard index (deck : Card array) =
    let rand = System.Random()
    let randIndex = rand.Next(index, deck.Length)
    swap index randIndex deck
    deck.[index]

let generateSymbols settings =
    let symbols =
        match settings.Game with
        | EmojiGame -> Symbols.emoji
        | KanjiGame lvl -> getKanjiArray lvl

    let rand = System.Random()
    let max = (cardsForDifficulty settings.Difficulty) / 2

    seq { for i in 0..(max - 1) do
            let randIndex = rand.Next(i, symbols.Length)
            swap i randIndex symbols
            yield symbols.[i]
            yield symbols.[i] }

let getKanjiDefinition kanji =
    // TODO
    { Character = kanji
      Kun = Some "KUN"
      On = Some "ON"
      Meaning = "MEANING"}

let createKanji character =
    let def = getKanjiDefinition character
    Kanji { Character = character; Kun = def.Kun; On = def.On; Meaning = def.Meaning}

let createCard gameType character =
    let symbol =
        match gameType with
        | EmojiGame -> Emoji character
        | KanjiGame _ -> createKanji character
    { Symbol = symbol; RubyText = None }

let rec getRubyText reveal symbol =
    match symbol with
    | Emoji _ -> None
    | Kanji kanji ->
        let orElse alternative preferred =
            preferred 
            |> Option.orElse alternative
            |> Option.defaultValue "MISSING READING"
            |> Some

        match reveal with
        | Kun -> kanji.Kun |> orElse kanji.On
        | On -> kanji.On |> orElse kanji.Kun
        | Meaning -> Some kanji.Meaning
        | Random ->
            let rand = System.Random()
            let rb =
                match rand.Next(0, 3) with
                | 0 -> Kun
                | 1 -> On
                | _ -> Meaning
            getRubyText rb symbol

let queueNextCard index deck dispatch =
    let id = Fable.Core.JS.setTimeout (fun _ -> dispatch (CreateCard(index, deck))) 100
    dispatch (SetNextCardTimeout id)

let queueHideCards dispatch =
    let id = Fable.Core.JS.setTimeout (fun _ -> dispatch HideCards) 1000
    dispatch (SetHideCardsTimeout id)

let update (msg: Msg) (state: Model) =
    match msg with
    | SetNextCardTimeout id ->
        { state with NextCardTimeout = Some id }, Cmd.none

    | SetHideCardsTimeout id ->
        { state with HideCardsTimeout = Some id }, Cmd.none

    | SetGameType game ->
        { state with Settings = { state.Settings with Game = game } }, Cmd.none

    | SetRevealType reveal ->
        { state with Settings = { state.Settings with RubyReveal = reveal } }, Cmd.none

    | SetDifficulty difficulty ->
        { state with Settings = { state.Settings with Difficulty = difficulty } }, Cmd.ofMsg NewGame

    | CardClicked index when state.RevealedCards.Contains index ->
        state, Cmd.none

    | CardClicked index ->
        let revealed = state.RevealedCards.Add index

        match state.FirstClicked, state.SecondClicked with
        | Some firstIndex, Some secondIndex ->
            state.HideCardsTimeout
            |> Option.iter Fable.Core.JS.clearTimeout

            // Hide both revealed cards immediately
            let revealed =
                state.RevealedCards
                |> Set.remove secondIndex
                |> Set.remove firstIndex
                |> Set.add index

            { state with FirstClicked = Some index
                         SecondClicked = None
                         RevealedCards = revealed
                         HideCardsTimeout = None }, Cmd.none
        | None, None ->
            { state with FirstClicked = Some index
                         RevealedCards = revealed
                         HideCardsTimeout = None }, Cmd.none

        | Some firstIndex, None ->
            let firstCard = state.Cards.[firstIndex]
            let secondCard = state.Cards.[index]
            if firstCard.Symbol = secondCard.Symbol then
                // Pair found
                let rubyText = getRubyText state.Settings.RubyReveal secondCard.Symbol
                let pairsFound = state.PairsFound + 1
                let cards =
                    state.Cards
                    |> List.map (fun c ->
                        if c.Symbol = firstCard.Symbol then
                            { c with RubyText = rubyText }
                        else
                            c)
                { state with FirstClicked = None
                             SecondClicked = None
                             PairsFound = pairsFound
                             Cards = cards
                             GameWon = pairsFound = cardsForDifficulty state.Settings.Difficulty / 2
                             RevealedCards = revealed }, Cmd.none
            else
                { state with RevealedCards = revealed
                             SecondClicked = Some index }, Cmd.ofSub queueHideCards

        | None, Some _ ->
            failwith "Invalid state."

    | HideCards ->
        let revealed =
            match state.FirstClicked, state.SecondClicked with
            | Some first, Some second ->
                state.RevealedCards
                |> Set.remove first
                |> Set.remove second
            | _ ->
                state.RevealedCards
        
        { state with FirstClicked = None
                     SecondClicked = None
                     RevealedCards = revealed }, Cmd.none
        
    | CreateCards ->
        let deck =
            state.Settings
            |> generateSymbols
            |> Seq.map (createCard state.Settings.Game)
            |> Seq.toArray

        state, Cmd.ofSub (queueNextCard 1 deck)

    | CreateCard (num, deck) ->
        let randomCard = deck |> getRandomCard (num - 1)
        let newModel = { state with Cards = state.Cards @ [ randomCard ] }
        if num < cardsForDifficulty state.Settings.Difficulty then
            newModel, Cmd.ofSub (queueNextCard (num + 1) deck)
        else
            newModel, Cmd.none

    | NewGame ->
        state.NextCardTimeout
        |> Option.iter Fable.Core.JS.clearTimeout
        state.HideCardsTimeout
        |> Option.iter Fable.Core.JS.clearTimeout

        { FirstClicked = None
          SecondClicked = None
          PairsFound = 0
          Cards = []
          RevealedCards = Set.empty
          GameWon = false
          NextCardTimeout = None
          HideCardsTimeout = None
          ShowSettings = state.ShowSettings
          Settings = state.Settings },
        Cmd.ofMsg CreateCards

    | UpdateSettings newSettings ->
        { state with Settings = newSettings }, Cmd.none

    | ToggleSettings ->
        { state with ShowSettings = not state.ShowSettings }, Cmd.none
        
let init () =
    let settings =
        { Game = KanjiGame Level6
          RubyReveal = Meaning
          Difficulty = Easy }

    { FirstClicked = None
      SecondClicked = None
      PairsFound = 0
      Cards = []
      RevealedCards = Set.empty
      GameWon = false
      NextCardTimeout = None
      HideCardsTimeout = None
      ShowSettings = false
      Settings = settings },
    Cmd.ofMsg CreateCards

let view (state: Model) dispatch =
    React.fragment [
        Html.div [
            prop.className "mp-gamecontrols"
            prop.children [
                Html.div [
                    prop.className "mp-button"
                    prop.text "Uusi peli"
                    prop.onClick (fun _ -> dispatch NewGame)
                ]

                Html.div [
                    prop.className "mp-button"
                    prop.text "Asetukset"
                    prop.onClick (fun _ -> dispatch ToggleSettings)
                ]

                Html.div [
                    prop.classes [ "mp-slidedown"
                                   "mp-settings"
                                   "mp-shadow"
                                   if state.ShowSettings then "mp-displayed" ]
                    prop.children [
                        Html.div [
                            prop.className "mp-opt-cont"
                            prop.children [
                                Html.h3 [
                                    prop.className "mp-opt-title"
                                    prop.text "Symbolit"
                                ]
                                Html.div [
                                    prop.classes [ "mp-button"; "mp-option"; if state.Settings.Game = EmojiGame then "mp-selected" ]
                                    prop.text "Emoji"
                                    prop.onClick (fun _ -> dispatch (SetGameType EmojiGame))
                                ]

                                Html.h3 [
                                    prop.className "mp-opt-title"
                                    prop.text "Kanjit"
                                ]
                                yield! [ Level1; Level2; Level3; Level4; Level5; Level6; AllLevels ]
                                |> List.map (fun level ->
                                    Html.div [
                                        prop.classes [ "mp-button"; "mp-option"; if state.Settings.Game = KanjiGame level then "mp-selected" ]
                                        prop.text (
                                            match level with
                                            | AllLevels -> "Kaikki"
                                            | lvl -> sprintf "%s. Luokka" ((string lvl).Substring(5))
                                        )
                                        prop.onClick (fun _ -> dispatch (SetGameType (KanjiGame level)))
                                    ]
                                )

                                Html.h3 [
                                    prop.className "mp-opt-title"
                                    prop.text "Näytä"
                                ]
                                yield! [ Meaning; Kun; On; Random ]
                                |> List.map (fun ruby ->
                                    Html.div [
                                        prop.classes [ "mp-button"; "mp-option"; if state.Settings.RubyReveal = ruby then "mp-selected" ]
                                        prop.text (
                                            match ruby with
                                            | Meaning -> "Merkitys"
                                            | Random -> "Satunnainen"
                                            | r -> string r
                                        )
                                        prop.onClick (fun _ -> dispatch (SetRevealType ruby))
                                    ]
                                )

                                Html.h3 [
                                    prop.className "mp-opt-title"
                                    prop.text "Vaikeus"
                                ]
                                yield! [ Easy; Normal; Hard; Hardest ]
                                |> List.map (fun diff ->
                                    Html.div [
                                        prop.classes [ "mp-button"; "mp-option"; if state.Settings.Difficulty = diff then "mp-selected" ]
                                        prop.text (
                                            match diff with
                                            | Easy -> "Helppo"
                                            | Normal -> "Normaali"
                                            | Hard -> "Vaikea"
                                            | Hardest -> "Vaikein"
                                        )
                                        prop.onClick (fun _ -> dispatch (SetDifficulty diff))
                                    ]
                                )
                            ]
                        ]
                    ]
                ]

                Html.div [
                    prop.className "mp-timer"
                    prop.text "00:00.0"
                ]
            ]
        ]

        Html.div [
            prop.classes [ "mp-gameboard"; if state.GameWon then "mp-blur" ]
            prop.style [
                let cpr = cardsPerRowForDifficulty state.Settings.Difficulty
                let totalCards = cardsForDifficulty state.Settings.Difficulty
                style.custom
                    ("gridTemplateColumns",
                    (sprintf "repeat(%i, auto)" cpr))
                style.custom
                    ("gridTemplateRows",
                    (sprintf "repeat(%i, auto)" (int <| ceil (float totalCards / float cpr))))
            ]
            prop.children (
                state.Cards
                |> List.mapi (fun i card ->
                    Html.div [
                        prop.classes [ "mp-card"; if state.RevealedCards.Contains i then "flipped" ]
                        prop.style [
                            if state.RevealedCards.Contains i then
                                style.cursor.defaultCursor
                        ]
                        prop.key i
                        prop.onClick (fun _ -> dispatch (CardClicked i))
                        prop.children [
                            Html.div [
                                prop.classes [ "mp-side"; "mp-card-front" ]
                                prop.children [
                                    Html.div [
                                        prop.className "mp-front-symbol"
                                        prop.text (
                                            match card.Symbol with
                                            | Kanji k -> k.Character
                                            | Emoji e -> e
                                        )
                                    ]
                                    Html.div [
                                        prop.classes [ "mp-ruby"
                                                       sprintf "mp-ruby-%s" (state.Settings.Difficulty.ToString().ToLower())
                                                       if card.RubyText.IsSome then "mp-ruby-fadein" ]
                                        prop.text (Option.toObj card.RubyText)
                                    ]
                                ]
                            ]
                            Html.div [
                                prop.classes [ "mp-side"; "mp-card-back" ]
                                prop.text (if i % 2 = 0 then Symbols.backIcon1 else Symbols.backIcon2)
                            ]
                        ]
                    ]
                ))
        ]

        Html.div [
            prop.className "mp-gameclear-cont"
            prop.style [
                if state.GameWon then
                    style.display.flex
                else
                    style.display.none
            ]
            prop.children [
                Html.div [
                    prop.classes [ "mp-clearmessage"; "mp-shadow" ]
                    prop.children [
                        Html.p [
                            prop.text "Kaikki parit löydetty!"
                        ]
                        Html.div [
                            prop.className "mp-button"
                            prop.text "OK"
                            prop.onClick (fun _ -> dispatch NewGame)
                        ]
                    ]
                ]
            ]
        ]
    ]
