module Views

open Fable.React
open Fable.React.Props
open Fable.Core.JsInterop

open Cell
open Evaluator
open Models

let keyMatch expected result (ke:Browser.Types.KeyboardEvent) = 
  if int ke.keyCode = expected then 
    Some result 
  else 
    None

let (|Escape|_|) = keyMatch 27 Escape 
let (|Tab|_|) =  keyMatch 9 Tab 
let (|Enter|_|) = keyMatch 13 Enter
let (|Arrow|_|) (ke: Browser.Types.KeyboardEvent) =
  if ke.ctrlKey || ke.shiftKey then 
    None
  else 
    match int ke.keyCode with
    | 37 -> Arrow Left |> Some
    | 38 -> Arrow Up |> Some
    | 39 -> Arrow Right |> Some
    | 40 -> Arrow Down |> Some
    | _ -> None


let getPosition ((col, row): Position) (direction: Direction) cols rows =
    match direction with
    | Up -> if row = 1 then None else Some (col, row - 1)
    | Down -> if row = rows then None else Some (col, row + 1)
    | Left -> Column.tryPrev col |> Option.map (fun col1 -> (col1, row))
    | Right -> Column.tryNext col |> Option.map (fun col1 -> col1, row)

let getMovement (state: SpreadsheetState) (direction: Direction) : Movement =
    match state.Active with
    | None -> Invalid
    | Some position ->
        match (getPosition position direction (state.Cols |> Array.length) (state.Rows |> Array.length)) with
        | Some (col, row) when Array.contains col state.Cols && Array.contains row state.Rows ->
            MoveTo (col, row)
        | _ -> Invalid

let getKeyPressEvent (state: SpreadsheetState) trigger = fun ke ->
    match ke with
    | Enter -> 
        match getMovement state Direction.Down with
        | Invalid -> trigger <| CancelEdit
        | MoveTo pos -> trigger <| StartEdit pos

    | Escape -> trigger <| CancelEdit

    | Tab -> 
        match getMovement state Direction.Right with
        | Invalid -> trigger <| CancelEdit
        | MoveTo pos -> trigger <| StartEdit pos

    | Arrow direction ->
        match getMovement state direction with
            | Invalid -> ()
            | MoveTo position -> trigger(StartEdit(position))
    | _ -> ()

let renderEditor (trigger:Event -> unit) pos state value =
  td [ Class "selected"] [
    input [
      AutoFocus true
      OnKeyDown (getKeyPressEvent state trigger)
      OnInput (fun e -> trigger (UpdateValue (pos, e.target ? value )))
      Value value ]
  ]

let prettyPos pos = sprintf "%s%d" (pos |> fst |> Column.pretty) (pos |> snd)

let pretty range = 
  sprintf "%s:%s" (range.TopLeft |> prettyPos) (range.BottomRight |> prettyPos)

let inRange range (x,y) = 
  let x1, y1 = range.TopLeft
  let x2, y2 = range.BottomRight
  x <=x2 && x >= x1 && y <= y2 && y >= y1

let mapPosToStyles rangeOpt pos (value: string option) = 
  let activeBorderColor = "#1a73e8"
  let activeBackgroundColor = "rgba(23, 102, 202, 0.2)"
  match rangeOpt with
  | Some range when inRange range pos -> [
    if value.IsSome then yield BackgroundColor activeBackgroundColor
    if range.TopLeft |> fst = fst pos then yield BorderLeftColor activeBorderColor
    if range.TopLeft |> snd = snd pos then yield BorderTopColor activeBorderColor
    if range.BottomRight |> fst = fst pos then yield BorderRightColor activeBorderColor
    if range.BottomRight |> snd = snd pos then yield BorderBottomColor activeBorderColor ]
  | _ -> []

let onMouseMove trigger pos state (e: Browser.Types.MouseEvent) = 
  if e.buttons = 1.0 then 
    let start = match state.Range with None -> pos | Some range -> range.TopLeft
    e.preventDefault()
    trigger (Select { TopLeft = start; BottomRight = pos })

let renderView trigger pos state (value:option<_>) =
  td
    [ Style [
        if value.IsNone then yield Background "#ffb0b0"
        yield! mapPosToStyles state.Range pos value ]
      OnClick (fun _ -> trigger (Select {TopLeft = pos; BottomRight = pos}))
      OnDoubleClick (fun _ -> trigger(StartEdit(pos)) ) 
      OnMouseMove <| onMouseMove trigger pos state  ]
    [ str (Option.defaultValue "#ERR" value) ]

let renderCell trigger pos state =
  let value = Map.tryFind pos state.Cells
  if state.Active = Some pos then
    renderEditor trigger pos state (Option.defaultValue "" value)
  else
    let value =
      match value with
      | Some value ->
          parse value |> Option.bind (evaluate Set.empty state.Cells) |> Option.map string
      | _ -> Some ""
    renderView trigger pos state value

let view state trigger =
  let empty = td [] []
  let header h = th [] [str h]
  let headers = state.Cols |> Array.map (Column.pretty >> header)
  let headers = [| yield empty; yield! headers |]

  let row cells = tr [] cells
  let cells n =
    let cells = state.Cols |> Array.map (fun h -> renderCell trigger (h, n) state)
    [| yield header (string n)
       yield! cells |]
  let rows = state.Rows |> Array.map (fun r -> tr [] (cells r))

  table [] [
    tr [] headers
    tbody [] rows
  ]