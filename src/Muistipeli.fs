module Muistipeli

open Feliz
open Elmish
open Types
open Utils
open Fable.SimpleHttp
open Thoth.Json

let private rand = System.Random()

let private getKanjiArray = function
    | Level lvl -> Symbols.kanjiLevels.[lvl - 1]
    | AllLevels -> Array.concat Symbols.kanjiLevels

let private cardsForDifficulty = function
    | Easy    -> 12
    | Normal  -> 20
    | Hard    -> 30
    | Hardest -> 42

let private cardsPerRowForDifficulty = function
    | Easy    -> 3
    | Normal  -> 5
    | Hard    -> 6
    | Hardest -> 7

let private getRandomCard index (deck: Card array) =
    let randIndex = rand.Next(index, deck.Length)
    Array.swapIndexes index randIndex deck
    deck.[index]

let private generateSymbols settings =
    let symbols =
        match settings.Game with
        | EmojiGame -> Symbols.emoji
        | KanjiGame lvl -> getKanjiArray lvl

    let max = (cardsForDifficulty settings.Difficulty) / 2

    seq { for i in 0..(max - 1) do
            let randIndex = rand.Next(i, symbols.Length)
            Array.swapIndexes i randIndex symbols
            yield symbols.[i]
            yield symbols.[i] }

let private createKanji kanjiDefs character =
    let def = kanjiDefs |> Map.find character
    Kanji { Character = character
            Kun = Option.ofString def.Kun
            On = Option.ofString def.On
            Meaning = def.Meaning
            RubyText = None }

let private createCard gameType kanjiDefs character =
    match gameType with
    | EmojiGame -> Emoji character
    | KanjiGame _ -> createKanji kanjiDefs character

let rec private getRubyText reveal (kanji: Kanji) =
    let orElse alternative preferred =
        preferred 
        |> Option.orElse alternative
        |> Option.defaultValue "MISSING READING"

    match reveal with
    | Kun ->
        kanji.Kun |> orElse kanji.On
    | On ->
        kanji.On |> orElse kanji.Kun
    | Meaning ->
        kanji.Meaning
    | Random ->
        let rb =
            match rand.Next(0, 3) with
            | 0 -> Kun
            | 1 -> On
            | _ -> Meaning
        getRubyText rb kanji

let private queueNextCard index deck dispatch =
    let id = Fable.Core.JS.setTimeout (fun _ -> dispatch (CreateCard(index, deck))) 100
    dispatch (SetNextCardTimeout id)

let private queueHideCards dispatch =
    let id = Fable.Core.JS.setTimeout (fun _ -> dispatch HideCards) 1000
    dispatch (SetHideCardsTimeout id)

let init () =
    let loadDefinitions() = async {
        let! statusCode, responseText = Http.get "kanji.json"
        return
            if statusCode = 200 then
                Ok responseText
            else
                Error(statusCode, responseText) }

    let settings =
        { Game = KanjiGame (Level 1)
          RubyReveal = Meaning
          Difficulty = Normal }

    { FirstClicked = None
      SecondClicked = None
      PairsFound = 0
      Cards = Array.empty
      KanjiDefinitions = Map.empty
      RevealedCards = Set.empty
      GameWon = false
      NextCardTimeout = None
      HideCardsTimeout = None
      TimerOn = false
      TimeElapsed = 0
      ShowSettings = false
      Settings = settings
      ErrorMessage = None },
    Cmd.OfAsync.perform loadDefinitions () KanjiDefinitionsLoaded

let update (msg: Msg) (state: Model) =
    match msg with
    | KanjiDefinitionsLoaded kanji ->
        match kanji with
        | Error (code, message) ->
            { state with ErrorMessage = Some $"Kanjimerkkien lataaminen epäonnistui virhekoodilla {code}:\n{message}"}, Cmd.none
        | Ok kanjiDefinitions ->
            match Decode.fromString (Decode.dict kanjiDecoder) kanjiDefinitions with
            | Ok defs ->
                { state with KanjiDefinitions = defs }, Cmd.ofMsg CreateCards
            | Error error ->
                { state with ErrorMessage = Some $"Kanjimerkkien lataaminen epäonnistui. Virhe:\n{error}"}, Cmd.none

    | SetNextCardTimeout id ->
        { state with NextCardTimeout = Some id }, Cmd.none

    | SetHideCardsTimeout id ->
        { state with HideCardsTimeout = Some id }, Cmd.none

    | SetGameType game ->
        { state with Settings = { state.Settings with Game = game } }, Cmd.ofMsg NewGame

    | SetRevealType reveal ->
        { state with Settings = { state.Settings with RubyReveal = reveal } }, Cmd.none

    | SetDifficulty difficulty ->
        { state with Settings = { state.Settings with Difficulty = difficulty } }, Cmd.ofMsg NewGame

    | CardClicked index when state.RevealedCards.Contains index ->
        state, Cmd.none

    | CardClicked index ->
        let revealed = state.RevealedCards.Add index

        match state.FirstClicked, state.SecondClicked with
        | None, None ->
            { state with FirstClicked = Some index
                         RevealedCards = revealed
                         HideCardsTimeout = None }, Cmd.none

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

        | Some firstIndex, None ->
            let firstCard = state.Cards.[firstIndex]
            let secondCard = state.Cards.[index]

            // Pair found
            if firstCard = secondCard then
                let cards =
                    match firstCard with
                    | Emoji _ ->
                        state.Cards
                    | Kanji kanji ->
                        let rubyText = getRubyText state.Settings.RubyReveal kanji
                        state.Cards
                        |> Array.map (fun card ->
                            match card with
                            | Kanji k when k.Character = kanji.Character ->
                                Kanji { k with RubyText = Some rubyText }
                            | other ->
                                other)
                let pairsFound = state.PairsFound + 1
                let gameWon = pairsFound = cardsForDifficulty state.Settings.Difficulty / 2

                { state with FirstClicked = None
                             SecondClicked = None
                             PairsFound = pairsFound
                             Cards = cards
                             GameWon = gameWon
                             TimerOn = not gameWon
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
            |> Seq.map (createCard state.Settings.Game state.KanjiDefinitions)
            |> Seq.toArray

        state, Cmd.ofSub (queueNextCard 1 deck)

    | TimerTick ->
        if state.TimerOn then
            let task = async {
                do! Async.Sleep 100
                return TimerTick }
            { state with TimeElapsed = state.TimeElapsed + 100 }, Cmd.OfAsync.result task
        else
            state, Cmd.none

    | CreateCard (num, deck) ->
        let randomCard = deck |> getRandomCard (num - 1)
        let newModel = { state with Cards = Array.append state.Cards [| randomCard |] }
        if num < cardsForDifficulty state.Settings.Difficulty then
            newModel, Cmd.ofSub (queueNextCard (num + 1) deck)
        else
            { newModel with TimerOn = true }, Cmd.ofMsg TimerTick

    | NewGame ->
        state.NextCardTimeout
        |> Option.iter Fable.Core.JS.clearTimeout
        state.HideCardsTimeout
        |> Option.iter Fable.Core.JS.clearTimeout

        { FirstClicked = None
          SecondClicked = None
          PairsFound = 0
          Cards = Array.empty
          KanjiDefinitions = state.KanjiDefinitions
          RevealedCards = Set.empty
          GameWon = false
          NextCardTimeout = None
          HideCardsTimeout = None
          TimerOn = false
          TimeElapsed = 0
          ShowSettings = state.ShowSettings
          Settings = state.Settings
          ErrorMessage = None },
        Cmd.ofMsg CreateCards

    | UpdateSettings newSettings ->
        { state with Settings = newSettings }, Cmd.none

    | ToggleSettings ->
        { state with ShowSettings = not state.ShowSettings }, Cmd.none

    | HideSettings ->
        { state with ShowSettings = false }, Cmd.none

let renderSettings state dispatch =
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
            yield! KanjiLevel.All
            |> Seq.map (fun level ->
                Html.div [
                    prop.classes [ "mp-button"; "mp-option"; if state.Settings.Game = KanjiGame level then "mp-selected" ]
                    prop.text (
                        match level with
                        | AllLevels -> "Kaikki"
                        | Level level -> $"{level}. Luokka"
                    )
                    prop.onClick (fun _ -> dispatch (SetGameType (KanjiGame level)))
                ]
            )

            Html.h3 [
                prop.className "mp-opt-title"
                prop.text "Näytä"
            ]
            yield! RubyRevealType.All
            |> List.map (fun ruby ->
                Html.div [
                    prop.classes [ "mp-button"; "mp-option"; if state.Settings.RubyReveal = ruby then "mp-selected" ]
                    prop.text (
                        match ruby with
                        | Meaning -> "Merkitys"
                        | Random -> "Satunnainen"
                        | other -> string other
                    )
                    prop.onClick (fun _ -> dispatch (SetRevealType ruby))
                ]
            )

            Html.h3 [
                prop.className "mp-opt-title"
                prop.text "Vaikeus"
            ]
            yield! Difficulty.All
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

let renderControls state dispatch =
    Html.div [
        prop.className "mp-gamecontrols"
        prop.children [
            Html.div [
                prop.className "mp-button"
                prop.text "Uusi peli"
                prop.onClick (fun _ ->
                    dispatch HideSettings
                    dispatch NewGame)
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
                    renderSettings state dispatch
                ]
            ]

            Html.div [
                prop.className "mp-timer"
                prop.text (formatTime state.TimeElapsed)
            ]
        ]
    ]

let renderCard state dispatch index (card: Card) =
    let isRevealed = state.RevealedCards.Contains index
    Html.div [
        prop.classes [ "mp-card"; if isRevealed then "flipped" ]
        prop.style [ if isRevealed then style.cursor.defaultCursor ]
        prop.key index
        prop.onClick (fun _ -> if not isRevealed then dispatch (CardClicked index))
        prop.children [
            Html.div [
                prop.classes [ "mp-side"; "mp-card-front" ]
                prop.children [
                    Html.div [
                        prop.className "mp-front-symbol"
                        prop.text card.Symbol
                    ]
                    match card with
                    | Kanji { RubyText = Some ruby } ->
                        Html.div [
                            prop.classes [ "mp-ruby"
                                           sprintf "mp-ruby-%s" (state.Settings.Difficulty.ToString().ToLower())
                                           "mp-ruby-fadein" ]
                            prop.text ruby
                        ]
                    | _ ->
                        ()
                ]
            ]
            Html.div [
                prop.classes [ "mp-side"; "mp-card-back" ]
                prop.text (Symbols.backIcons.[index % 2])
            ]
        ]
    ]

let renderGameBoard state dispatch =
    Html.div [
        prop.classes [ "mp-gameboard"; if state.GameWon then "mp-blur" ]
        prop.onClick (fun _ -> if state.ShowSettings then dispatch HideSettings)
        prop.style [
            let perRow = cardsPerRowForDifficulty state.Settings.Difficulty
            let totalCards = cardsForDifficulty state.Settings.Difficulty

            style.custom
                ("gridTemplateColumns",
                (sprintf "repeat(%i, auto)" perRow))
            style.custom
                ("gridTemplateRows",
                (sprintf "repeat(%i, auto)" (int <| ceil (float totalCards / float perRow))))
        ]
        prop.children (
            state.Cards
            |> Array.mapi (renderCard state dispatch)
        )
    ]

let renderGameClearMessage dispatch =
    Html.div [
        prop.className "mp-gameclear-cont"
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

let view (state: Model) dispatch =
    React.fragment [
        match state.ErrorMessage with
        | Some errorMessage ->
            Html.div [
                prop.className "error"
                prop.children [
                    Html.strong [
                        prop.text errorMessage
                    ]
                ]
            ]
        | None ->
            ()
        renderControls state dispatch
        renderGameBoard state dispatch
        if state.GameWon then renderGameClearMessage dispatch
    ]
