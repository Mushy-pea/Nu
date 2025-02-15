﻿// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2017.

namespace Prime
open System
open Prime

[<RequireQualifiedAccess>]
module Array =

    /// Add (or 'cons') a value to the front of an array.
    let cons elem arr =
        let tailLength = Array.length arr
        let arr2 = Array.zeroCreate (inc tailLength)
        arr2.[0] <- elem
        Array.Copy (arr, 0, arr2, 1, tailLength)
        arr2

    /// Add (or 'snoc) a value to the end of an array.
    let add elem arr =
        let tailLength = Array.length arr
        let arr2 = Array.zeroCreate (inc tailLength)
        arr2.[tailLength] <- elem
        Array.Copy (arr, 0, arr2, 0, tailLength)
        arr2

    /// Remove the first matching element of the array.
    let remove pred (arr : 'a array) =
        match Array.tryFindIndex pred arr with
        | Some index ->
            let arr2 = Array.zeroCreate (dec arr.Length) : 'a array
            Array.Copy (arr, 0, arr2, 0, index)
            Array.Copy (arr, inc index, arr2, index, arr2.Length - index)
            arr2
        | None -> arr

    /// Implement a fold while folder results in Some.
    let foldWhile folder (state : 's) (arr : 't array) =
        Seq.foldWhile folder state arr

    /// Implement a fold while folder results in Right.
    let foldWhileRight folder (state : Either<_, _>) (arr : 't array) =
        Seq.foldWhileRight folder state arr

    /// Implement a fold until folder results in Nome.
    let foldUntil folder (state : 's) (arr : 't array) =
        Seq.foldUntil folder state arr

    /// Implement a fold until folder results in Right.
    let foldUntilRight folder (state : Either<_, _>) (arr : 't array) =
        Seq.foldUntilRight folder state arr

    /// Check that an array is not empty.
    let rec notEmpty arr =
        not (Array.isEmpty arr)

    /// Check that a predicate passes for NO items in an array.
    let rec notExists pred arr =
        not (Array.exists pred arr)

    /// Convert option values to definite values.
    let definitize opts =
        Array.choose id opts

    /// Convert option values to definite values, returning an additional flag to indicate that all values were some.
    let definitizePlus opts =
        let (flag, seq) = Seq.definitizePlus opts
        (flag, Array.ofSeq seq)

    /// Get all but the last item from a list.
    let allButLast arr =
        Array.take (Array.length arr - 1) arr

    /// A more tolerant and open-minded take.
    let tryTake (count : int) (arr : _ array) =
        Seq.tryTake count arr |> Array.ofSeq

    /// A more tolerant and open-minded skip.
    let trySkip (count : int) (arr : _ array) =
        Seq.trySkip count arr |> Array.ofSeq

    /// Perform a sort on elements, preserving order of equal elements.
    let sortStableWith sorter (arr : _ array) =
        arr |> Seq.sortWith sorter |> Array.ofSeq

    /// Perform a sort on elements, preserving order of equal elements.
    let sortStableBy by (arr : _ array) =
        arr |> Seq.sortBy by |> Array.ofSeq

    /// Perform a sort on elements, preserving order of equal elements.
    let sortStable (arr : _ array) =
        arr |> Seq.sort |> Array.ofSeq