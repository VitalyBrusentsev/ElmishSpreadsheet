module Spreadsheet

open Elmish
open Elmish.React

open Cell
open Models

// ----------------------------------------------------------------------------
// EVENT HANDLING
// ----------------------------------------------------------------------------

let update msg state =
  match msg with
  | StartEdit(pos) ->
      { state with Active = Some pos; Range = None }, Cmd.none

  | CancelEdit ->
      { state with Active = None }, Cmd.none

  | UpdateValue(pos, value) ->
      let newCells =
          if value = ""
              then Map.remove pos state.Cells
              else Map.add pos value state.Cells
      { state with Cells = newCells }, Cmd.none
  | Select range -> 
      { state with Range = Some range }, Cmd.ofMsg CancelEdit

open Views

// ----------------------------------------------------------------------------
// ENTRY POINT
// ----------------------------------------------------------------------------

let initial () =
  { Cols = [|'A' .. 'K'|] |> Array.map Column.ofChar
    Rows = [|1 .. 15|]
    Active = None
    Range = Some { TopLeft = (Column.ofChar 'B', 2); BottomRight = (Column.ofChar 'D', 5) }
#if DEBUG
    DebugMessage = ""
#endif    
    Cells = Map.empty }, Cmd.none

open Fable.Elmish.ElmishToReact

let private program = Program.mkProgram initial update view
let externalisedProgram =
  Externalised.externalise program

let private spreadSheetReactComponent = elmishToReact externalisedProgram

ReactToWebComponents.register "spreadsheet-component" spreadSheetReactComponent