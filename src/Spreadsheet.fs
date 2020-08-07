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
    { state with Editor = Active pos }, Cmd.none

  | CancelEdit ->
    { state with Editor = Nothing }, Cmd.none

  | UpdateValue(pos, value) ->
    let newCells =
      if value = ""
          then Map.remove pos state.Cells
          else Map.add pos value state.Cells
    { state with Cells = newCells }, Cmd.none
  | Select range -> 
    { state with Editor = Selection range }, Cmd.none
  | SelectColumn column ->
    let range = { TopLeft = { Column = column; Row = 1 }
                  BottomRight = { Column = column; Row = state.Rows |> Array.length } }
    { state with Editor = Selection range}, Cmd.none
  | SelectRow row ->
    let range = { TopLeft = { Column = 1 |> Column.ofIndex; Row = row }
                  BottomRight = { Column = state.Cols |> Array.length |> Column.ofIndex; Row =row } }
    { state with Editor = Selection range}, Cmd.none
  | ChangeSelection range ->
    { state with Editor = ChangingSelection range }, Cmd.none

open Views

// ----------------------------------------------------------------------------
// ENTRY POINT
// ----------------------------------------------------------------------------

let initial () =
  { Cols = [|'A' .. 'K'|] |> Array.map Column.ofChar
    Rows = [|1 .. 15|]
    Editor = Selection { 
                         TopLeft = { Column = Column.ofChar 'B'; Row = 2 }
                         BottomRight = { Column = Column.ofChar 'D' ; Row = 5 } }
    Cells = Map.empty }, Cmd.none

open Fable.Elmish.ElmishToReact

let private program = Program.mkProgram initial update view
let externalisedProgram =
  Externalised.externalise program

let private spreadSheetReactComponent = elmishToReact externalisedProgram

ReactToWebComponents.register "spreadsheet-component" spreadSheetReactComponent