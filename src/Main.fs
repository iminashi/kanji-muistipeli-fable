module Main

open Elmish
open Elmish.React

Program.mkProgram Muistipeli.init Muistipeli.update Muistipeli.view
|> Program.withReactSynchronous "elmish-app"
|> Program.withConsoleTrace
|> Program.run
