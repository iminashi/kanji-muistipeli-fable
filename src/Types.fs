module Types

type KanjiLevel =
    | Level1
    | Level2
    | Level3
    | Level4
    | Level5
    | Level6
    | AllLevels

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

type Settings =
    { Game : GameType
      RubyReveal : RubyRevealType
      Difficulty : Difficulty }

type Kanji =
    { Character : string
      Kun : string option
      On : string option
      Meaning : string }

type KanjiDefinition =
    { Kun : string
      On : string
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
      Cards : Card array
      KanjiDefinitions : Map<string, KanjiDefinition>
      RevealedCards : Set<int>
      GameWon : bool
      ShowSettings : bool
      TimerOn : bool
      NextCardTimeout : int option
      HideCardsTimeout : int option
      TimeElapsed : int
      Settings : Settings
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
