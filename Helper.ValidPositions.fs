module ChessGame.Helper.ValidPositions

open ChessGame.Common
open ChessGame.Helper.Adapters


// --------------------
// Helpers
// --------------------

type GetValidPositions = Position -> Position list

let findCell (board: Board) (position: Position) =
    board |> List.tryFind (fun cell -> cell.position = position)

let filterNonePositions (items: List<Option<'a>>) : List<'a> =
    items
    |> List.map (fun option ->
        match option with
        | Some item -> [ item ]
        | None -> [])
    |> List.concat

let getPossibleNextCells board position (fns: List<Position -> Result<Position, string>>) =
    fns
    |> List.map (fun f -> f position |> resultToOption)
    |> List.map (fun o -> o |> Option.bind (findCell board))
    |> filterNonePositions

let getValidPositionsFor
    (board: Board)
    (camp: Camp)
    (getPositionFn: Position -> Result<Position, string>)
    (initialPosition: Position)
    : List<Position> =
    let rec iterate acc currentPosition : Position list =
        getPositionFn currentPosition
        |> resultToOption
        |> Option.bind (findCell board)
        |> Option.bind (fun cell ->
            match cell.state with
            | Empty -> Some(iterate (cell.position :: acc) cell.position)
            | Occupied piece ->
                if piece.camp = camp then
                    Some acc
                else
                    Some(cell.position :: acc))
        |> Option.defaultWith (fun () -> acc)

    iterate [] initialPosition

// --------------------
// Find valid positions
// --------------------

let getRookValidPositions board camp : GetValidPositions =
    fun position ->
        [ Position.getTopPosition
          Position.getDownPosition
          Position.getLeftPosition
          Position.getRightPosition ]
        |> List.map (fun f -> getValidPositionsFor board camp f position)
        |> List.concat

let getBishopValidPositions board camp : GetValidPositions =
    fun position ->
        [ Position.getTopLeftPosition
          Position.getTopRightPosition
          Position.getDownLeftPosition
          Position.getDownRightPosition ]
        |> List.map (fun f -> getValidPositionsFor board camp f position)
        |> List.concat

let getKingValidPositions board camp : GetValidPositions =
    fun position ->
        [ Position.getTopPosition
          Position.getTopRightPosition
          Position.getRightPosition
          Position.getDownRightPosition
          Position.getDownPosition
          Position.getDownLeftPosition
          Position.getLeftPosition
          Position.getTopLeftPosition ]
        |> getPossibleNextCells board position
        |> List.map (fun cell ->
            match cell.state with
            | Empty -> Some cell.position
            | Occupied piece -> if piece.camp = camp then None else Some cell.position)
        |> filterNonePositions

let getQueenValidPositions board camp : GetValidPositions =
    fun position ->
        [ getBishopValidPositions; getRookValidPositions ]
        |> List.map (fun f -> f board camp position)
        |> List.concat

let getKnightValidPositions board camp : GetValidPositions =
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
        |> List.map (fun r -> resultToOption r)
        |> List.map (fun o -> o |> Option.bind (findCell board))
        |> filterNonePositions
        |> List.map (fun cell ->
            match cell.state with
            | Empty -> Some cell.position
            | Occupied piece -> if piece.camp = camp then None else Some cell.position)
        |> filterNonePositions

let isInitialPawnPlace (camp: Camp) (position: Position) : bool =
    let initialPawnPositions camp =
        let f rank =
            File.getAll () |> List.map (fun file -> Position.create file rank)

        match camp with
        | White -> f Rank2
        | Black -> f Rank7

    List.contains position (initialPawnPositions camp)

let getPawnValidPositions board camp : GetValidPositions =
    // TODO: When Pawn reach the adverse last line, it can be changed to any lost piece
    // TODO: allow 2 move on first move only
    // TODO: "En passant"
    fun position ->
        // Pawn can move forward, one cell at the time
        let capturingMoves =
            // white move top, black move bottom
            if camp = White then
                [ Position.getTopLeftPosition; Position.getTopRightPosition ]
            else
                [ Position.getDownLeftPosition; Position.getDownRightPosition ]

        let positionsWhenCapturing: Position list =
            capturingMoves
            |> getPossibleNextCells board position
            |> List.map (fun cell ->
                match cell.state with
                | Empty -> None
                | Occupied piece -> if piece.camp = camp then None else Some cell.position)
            |> filterNonePositions


        // Pawn can eat adverse one cell forward at left or right (diagonal)
        let classicMoves =
            // white move top, black move bottom
            if camp = White then
                [ Position.getTopPosition ]
            else
                [ Position.getDownPosition ]

        let positionsWhenMovingForward =
            classicMoves
            |> getPossibleNextCells board position
            |> List.map (fun cell ->
                // Pawn cannot move forward if the next position is Occupied
                match cell.state with
                | Empty -> Some cell.position
                | Occupied _ -> None)
            |> filterNonePositions

        if List.length positionsWhenMovingForward > 0 && isInitialPawnPlace camp position then
            // new move forward
            let positionsWhenMovingForwardTwice =
                classicMoves
                |> getPossibleNextCells board positionsWhenMovingForward[0]
                |> List.map (fun cell ->
                    // Pawn cannot move forward if the next position is Occupied
                    match cell.state with
                    | Empty -> Some cell.position
                    | Occupied _ -> None)
                |> filterNonePositions

            List.concat
                [ positionsWhenCapturing
                  positionsWhenMovingForward
                  positionsWhenMovingForwardTwice ]
        else
            List.concat [ positionsWhenCapturing; positionsWhenMovingForward ]

let getValidPositionsFn (board: Board) (camp: Camp) (piece: Piece) : GetValidPositions =
    let baseFn =
        match piece.kind with
        | King -> getKingValidPositions
        | Queen -> getQueenValidPositions
        | Rook -> getRookValidPositions
        | Bishop -> getBishopValidPositions
        | Knight -> getKnightValidPositions
        | Pawn -> getPawnValidPositions

    baseFn board camp
