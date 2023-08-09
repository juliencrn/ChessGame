module ChessGame.Helper.LegalMoves

open ChessGame.Common

// --------------------
// Helpers
// --------------------

type GetLegalMoves = Position -> Position list

let unwrapAlwaysOkResult result =
    match result with
    | Ok value -> value
    | _ -> failwith "This should not happen!"

let rec getMovesRecursively (initialPosition: Position) getPosition : Position list =
    let rec loop acc currentPosition =
        match getPosition currentPosition with
        | Ok newPosition -> loop (newPosition :: acc) newPosition
        | Error _ -> List.rev acc // Stop when the result is None and return the reversed list

    loop [] initialPosition

// --------------------
// Legal moves
// --------------------

let getRookLegalMoves: GetLegalMoves =
    fun position ->
        [ Position.getTopPosition
          Position.getDownPosition
          Position.getLeftPosition
          Position.getRightPosition ]
        |> List.map (fun move -> getMovesRecursively position move)
        |> List.concat

let getBishopLegalMoves: GetLegalMoves =
    fun position ->
        [ Position.getTopLeftPosition
          Position.getTopRightPosition
          Position.getDownLeftPosition
          Position.getDownRightPosition ]
        |> List.map (fun move -> getMovesRecursively position move)
        |> List.concat

let getKingLegalMoves: GetLegalMoves =
    fun position ->
        [ Position.getTopPosition
          Position.getTopRightPosition
          Position.getRightPosition
          Position.getDownRightPosition
          Position.getDownPosition
          Position.getDownLeftPosition
          Position.getLeftPosition
          Position.getTopLeftPosition ]
        |> List.map (fun move -> move position)
        |> List.filter Result.isOk
        |> List.map unwrapAlwaysOkResult

let getQueenLegalMoves: GetLegalMoves =
    fun position ->
        [ getBishopLegalMoves; getRookLegalMoves ]
        |> List.map (fun getMoves -> getMoves position)
        |> List.concat

let getKnightLegalMoves: GetLegalMoves =
    let bind = Result.bind

    fun position ->
        [ position
          |> Position.getTopPosition
          |> bind Position.getTopPosition
          |> bind Position.getLeftPosition
          position
          |> Position.getTopPosition
          |> bind Position.getTopPosition
          |> bind Position.getRightPosition
          position
          |> Position.getRightPosition
          |> bind Position.getRightPosition
          |> bind Position.getTopPosition
          position
          |> Position.getRightPosition
          |> bind Position.getRightPosition
          |> bind Position.getDownPosition
          position
          |> Position.getDownPosition
          |> bind Position.getDownPosition
          |> bind Position.getLeftPosition
          position
          |> Position.getDownPosition
          |> bind Position.getDownPosition
          |> bind Position.getRightPosition
          position
          |> Position.getLeftPosition
          |> bind Position.getLeftPosition
          |> bind Position.getTopPosition
          position
          |> Position.getLeftPosition
          |> bind Position.getLeftPosition
          |> bind Position.getDownPosition ]
        |> List.filter Result.isOk
        |> List.map unwrapAlwaysOkResult

let getPawnLegalMoves: GetLegalMoves =
    fun position ->
        [ Position.getTopPosition ]
        |> List.map (fun move -> move position)
        |> List.filter Result.isOk
        |> List.map unwrapAlwaysOkResult

let getLegalMoveFn (piece: Piece) : GetLegalMoves =
    match piece.kind with
    | King -> getKingLegalMoves
    | Queen -> getQueenLegalMoves
    | Rook -> getRookLegalMoves
    | Bishop -> getBishopLegalMoves
    | Knight -> getKnightLegalMoves
    | Pawn -> getPawnLegalMoves
