module Spreadsheet

open Elmish
open Elmish.React

open Cell
open Models

// ----------------------------------------------------------------------------
// EVENT HANDLING
// ----------------------------------------------------------------------------

let update msg state =
  match state.Editor, msg with
  | _, StartEdit(pos) ->
    { state with Editor = Active pos }, Cmd.none

  | Active, CancelEdit ->
    { state with Editor = Nothing }, Cmd.none

  | Active, UpdateValue(pos, value) ->
  let newCells =
      if value = ""
          then Map.remove pos state.Cells
          else Map.add pos value state.Cells
  { state with Cells = newCells }, Cmd.none
  | _, Select range -> 
    { state with Editor = Selection range }, Cmd.ofMsg CancelEdit
  | _ ->
    state, Cmd.none

open Views

// ----------------------------------------------------------------------------
// ENTRY POINT
// ----------------------------------------------------------------------------

let initial () =
  { Cols = [|'A' .. 'K'|] |> Array.map Column.ofChar
    Rows = [|1 .. 15|]
    Editor = Selection { TopLeft = (Column.ofChar 'B', 2); BottomRight = (Column.ofChar 'D', 5) }
    Cells = Map.empty }, Cmd.none

open Fable.Elmish.ElmishToReact

let private program = Program.mkProgram initial update view
let externalisedProgram =
  Externalised.externalise program

let private spreadSheetReactComponent = elmishToReact externalisedProgram

ReactToWebComponents.register "spreadsheet-component" spreadSheetReactComponent