module ChessGame.Helper.Adapters

// / Just a wrapper to ensure non-empty list
// module NonEmptyList =
//     type T<'a> = private { first: 'a; rest: 'a list }

//     let create (list: 'a list) =
//         match list with
//         | [] -> Error "Empty list"
//         | first :: rest -> Ok({ first = first; rest = rest })

//     let value ({ first = first; rest = rest }: T<'a>) : 'a list = first :: rest

let combineResults resultA resultB =
    match (resultA, resultB) with
    | Ok a, Ok b -> Ok(a, b)
    | Ok _, Error err -> Error err
    | Error err, Ok _ -> Error err
    | Error err, Error _ -> Error err

let optionToResult (err: 'e) (option: Option<'a>) : Result<'a, 'e> =
    match option with
    | Some x -> Ok x
    | None -> Error err

let predicateToPassThrough err f x =
    match f x with
    | true -> Ok x
    | false -> Error err

/// Transform List<Result> to a Result<List>
let rec transformResultsToListResult resultList =
    match resultList with
    | [] -> Ok []
    | result :: rest ->
        match transformResultsToListResult rest with
        | Ok restResults ->
            match result with
            | Ok value -> Ok(value :: restResults)
            | Error err -> Error err
        | Error err -> Error err
