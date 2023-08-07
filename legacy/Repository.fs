module Repository

open System.IO
open Newtonsoft.Json
open Domain.Types

let filePath = "gameState.json"

/// Function to save game state to JSON file
let saveGameStateToFile (state: GameState) =
    let json = JsonConvert.SerializeObject(state, Formatting.Indented)
    File.WriteAllText(filePath, json)

/// Function to load game state from JSON file
let loadGameStateFromFile () : GameState option =
    if File.Exists(filePath) then
        let json = File.ReadAllText(filePath)
        JsonConvert.DeserializeObject<GameState>(json) |> Some
    else
        None
