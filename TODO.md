# ADT

- pieces (type, color)
- board (cells, rows/cols)

# functions

- move x y
- calcLegalMoves x y piece
- fix out-of-bounds error
- fix inputs errors [0-7]
-

## GUI functions

- draw board

# Scenarios

- Initialize game
- Piece moved by Player 1
- Piece moved by Player 2

## Workflows

nothing -> Start game -> Start Chess Board

type StartGame =
unit -> ChessBoard

type Player1Move piece -> ChessBoard -> ChessBoard
type Player2Move piece -> ChessBoard -> ChessBoard

## Grid

type RowPosition =
| 1
| 2
| 3
| 4
| 5
| 6
| 7
| 8

type ColPosition =
| 1
| 2
| 3
| 4
| 5
| 6
| 7
| 8

type CellPosition = {
row: RowPosition;
col: ColPosition;
}

type PlayerXMove =
GameState \* CellPosition -> GameState

type PlayerYMove =
GameState \* CellPosition -> GameState

# Board

type Player = White | Black

type Piece = {
player: Player
kind: Rock | Queen | ...
}

type Cell = {
position: CellPosition;
state: CellState
}

type CellState =
| Piece
| Empty

type GameState = { cells: Cell list }

## What is the output ? What the UI needs?

type GetCell GameState -> Cell list

## When the gamer stops?

type GameStatus = InProcess | Win of Player | IDLE

type type PlayerXMove =
GameState _ PlayerXPosition ->
GameState _ GameStatus

## What kind of errors can happen?

- UI creates invalid gameState? no it's in core domain
- UI passes in an invalid Cell Position? no it's in core domain restricted
- UI passes in a valid position but at the wrong time? Yes
- Could X player plays twice?
- What about when the game has ended? - game should needs to not accept moves after the end

## Returning the available moves (for a given Player, for a given Piece, aka LegalMoves)

type LegalMovesForQueen = CellPosition list

1. List available piece to move for Player X
2. When Selected, list all LegalMove for that piece
3. Move it then return (back to 1) the list of available pieces for Player Y

This way, Player X cannot play twice and at the end of the game, the list of available pieces/moves will be empty

type MoveResult =
| PlayerXMoved as GameState _ ValidPiecesToMoveForPlayerY
| PlayerYMoved as GameState _ ValidPiecesToMoveForPlayerX
| GameWon aof GameState \* Player

# Architecture

Domain
Types
Functions

Interface

GUI

main() =
Impl domain via interface
Draw GUI from domain Data
Game Loop
