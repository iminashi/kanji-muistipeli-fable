module Types

type KanjiLevel =
    | Level of int
    | AllLevels
    static member All =
        seq { for i in 1..6 -> Level i
              yield AllLevels }

type GameType =
    | EmojiGame
    | KanjiGame of level : KanjiLevel

type RubyRevealType =
    | Meaning
    | Kun
    | On
    | Random
    static member All = [ Meaning; Kun; On; Random ]

type Difficulty =
    | Easy
    | Normal
    | Hard
    | Hardest
    static member All = [ Easy; Normal; Hard; Hardest ]

type Settings =
    { Game : GameType
      RubyReveal : RubyRevealType
      Difficulty : Difficulty }

type KanjiCharacter = string

type Kanji =
    { Character : KanjiCharacter
      Kun : string option
      On : string option
      Meaning : string
      RubyText : string option }

type KanjiDefinition =
    { Kun : string
      On : string
      Meaning : string }

type Card =
    | Emoji of string
    | Kanji of Kanji
    member this.Symbol =
        match this with
        | Emoji e -> e
        | Kanji k -> k.Character

type Model =
    { FirstClicked : int option
      SecondClicked : int option
      PairsFound : int
      Cards : Card array
      KanjiDefinitions : Map<KanjiCharacter, KanjiDefinition>
      RevealedCards : Set<int>
      GameWon : bool
      ShowSettings : bool
      TimerOn : bool
      NextCardTimeout : int option
      HideCardsTimeout : int option
      TimeElapsed : int
      Settings : Settings
      BackFaceColor : string
      ErrorMessage : string option }

type Msg =
    | CardClicked of index : int
    | NewGame
    | CreateCards
    | TimerTick
    | CreateCard of num : int * deck : Card array
    | HideCards
    | UpdateSettings of Settings
    | ToggleSettings
    | HideSettings
    | SetGameType of GameType
    | SetRevealType of RubyRevealType
    | SetDifficulty of Difficulty
    | SetNextCardTimeout of int
    | SetHideCardsTimeout of int
    | KanjiDefinitionsLoaded of Result<string, int * string>
