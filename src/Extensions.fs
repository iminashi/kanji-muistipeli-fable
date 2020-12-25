[<AutoOpen>]
module Extensions

module Option =
    let ofString str =
        if System.String.IsNullOrEmpty str then
            None
        else
            Some str
