[<AutoOpen>]
module Extensions

[<RequireQualifiedAccess>]
module Option =
    let ofString str =
        if System.String.IsNullOrEmpty str then
            None
        else
            Some str
