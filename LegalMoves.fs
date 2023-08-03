module LegalMoves

open Domain.Types
open Move.Functions

// --------------------
// Helpers
// --------------------

type GetLegalMoves = Position -> Position list

let unwrapAlwaysOkResult result =
    match result with
    | Ok value -> value
    | _ -> failwith "This should not happen!"

let rec getMovesRecursively (initialPosition: Position) (move: Move) : Position list =
    let rec loop acc currentPosition =
        match move currentPosition with
        | Ok newPosition -> loop (newPosition :: acc) newPosition
        | Error _ -> List.rev acc // Stop when the result is None and return the reversed list

    loop [] initialPosition

// --------------------
// Legal moves
// --------------------

let getRookLegalMoves: GetLegalMoves =
    fun position ->
        [ moveTop; moveDown; moveLeft; moveRight ]
        |> List.map (fun move -> getMovesRecursively position move)
        |> List.concat

let getBishopLegalMoves: GetLegalMoves =
    fun position ->
        [ moveTopLeft; moveTopRight; moveDownLeft; moveDownRight ]
        |> List.map (fun move -> getMovesRecursively position move)
        |> List.concat

let getKingLegalMoves: GetLegalMoves =
    fun position ->
        [ moveTop
          moveTopRight
          moveRight
          moveDownRight
          moveDown
          moveDownLeft
          moveLeft
          moveTopLeft ]
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
        [ position |> moveTop |> bind moveTop |> bind moveLeft
          position |> moveTop |> bind moveTop |> bind moveRight
          position |> moveRight |> bind moveRight |> bind moveTop
          position |> moveRight |> bind moveRight |> bind moveDown
          position |> moveDown |> bind moveDown |> bind moveLeft
          position |> moveDown |> bind moveDown |> bind moveRight
          position |> moveLeft |> bind moveLeft |> bind moveTop
          position |> moveLeft |> bind moveLeft |> bind moveDown ]
        |> List.filter Result.isOk
        |> List.map unwrapAlwaysOkResult

let getPawnLegalMoves: GetLegalMoves =
    fun position ->
        [ moveTop ]
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
