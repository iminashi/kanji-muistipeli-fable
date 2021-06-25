[<AutoOpen>]
module Extensions

[<RequireQualifiedAccess>]
module Option =
    /// Creates an option of a string where null or empty string equals None.
    let ofString str =
        match System.String.IsNullOrEmpty str with
        | true  -> None
        | false -> Some str

[<RequireQualifiedAccess>]
module Array =
    /// Swaps two indexes in the given array.
    let swapIndexes i1 i2 (arr : 'a array) =
        let temp = arr.[i1]
        arr.[i1] <- arr.[i2]
        arr.[i2] <- temp