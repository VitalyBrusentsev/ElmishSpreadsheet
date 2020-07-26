module SpreadSheet

// Build your own Excel 365 in an hour with F# by Tomas Petricek!
// Watch the video of the talk here: https://www.youtube.com/watch?v=Bnm71YEt_lI

module Parsec =
    type ParseStream<'T> = int * list<'T>
    type Parser<'T, 'R> = Parser of (ParseStream<'T> -> option<ParseStream<'T> * 'R>)

    /// Returned by the `slot` function to create a parser slot that is filled later
    type ParserSetter<'T, 'R> =
      { Set : Parser<'T, 'R> -> unit }

    /// Ignore the result of the parser
    let ignore (Parser p) = Parser(fun input ->
      p input |> Option.map (fun (i, r) -> i, ()))

    /// Creates a delayed parser whose actual parser is set later
    let slot () =
      let mutable slot = None
      { Set = fun (Parser p) -> slot <- Some p },
      Parser(fun input -> slot.Value input)

    /// If the input matches the specified prefix, produce the specified result
    let prefix (prefix:list<'C>) result = Parser(fun (offset, input) ->
      let rec loop (word:list<'C>) input =
        match word, input with
        | c::word, i::input when c = i -> loop word input
        | [], input -> Some(input)
        | _ -> None

      match loop prefix input with
      | Some(input) -> Some((offset+List.length prefix, input), result)
      | _ -> None)

    /// Parser that succeeds when either of the two arguments succeed
    let (<|>) (Parser p1) (Parser p2) = Parser(fun input ->
      match p1 input with
      | Some(input, res) -> Some(input, res)
      | _ -> p2 input)

    /// Run two parsers in sequence and return the result as a tuple
    let (<*>) (Parser p1) (Parser p2) = Parser(fun input ->
      match p1 input with
      | Some(input, res1) ->
          match p2 input with
          | Some(input, res2) -> Some(input, (res1, res2))
          | _ -> None
      | _ -> None)

    /// Transforms the result of the parser using the specified function
    let map f (Parser p) = Parser(fun input ->
      p input |> Option.map (fun (input, res) -> input, f res))

    /// Run two parsers in sequence and return the result of the second one
    let (<*>>) p1 p2 = p1 <*> p2 |> map snd

    /// Run two parsers in sequence and return the result of the first one
    let (<<*>) p1 p2 = p1 <*> p2 |> map fst

    /// Succeed without consuming input
    let unit res = Parser(fun input -> Some(input, res))

    /// Parse using the first parser and then call a function to produce
    /// next parser and parse the rest of the input with the next parser
    let bind f (Parser p) = Parser(fun input ->
      match p input with
      | Some(input, res) ->
          let (Parser g) = f res
          match g input with
          | Some(input, res) -> Some(input, res)
          | _ -> None
      | _ -> None)

    /// Parser that tries to use a specified parser, but returns None if it fails
    let optional (Parser p) = Parser(fun input ->
      match p input with
      | None -> Some(input, None)
      | Some(input, res) -> Some(input, Some res) )

    /// Parser that succeeds if the input matches a predicate
    let pred p = Parser(function
      | offs, c::input when p c -> Some((offs+1, input), c)
      | _ -> None)

    /// Parser that succeeds if the predicate returns Some value
    let choose p = Parser(function
      | offs, c::input -> p c |> Option.map (fun c -> (offs + 1, input), c)
      | _ -> None)

    /// Parse zero or more repetitions using the specified parser
    let zeroOrMore (Parser p) =
      let rec loop acc input =
        match p input with
        | Some(input, res) -> loop (res::acc) input
        | _ -> Some(input, List.rev acc)
      Parser(loop [])

    /// Parse one or more repetitions using the specified parser
    let oneOrMore p =
      (p <*> (zeroOrMore p))
      |> map (fun (c, cs) -> c::cs)


    let anySpace = zeroOrMore (pred (fun t -> t = ' '))

    let char tok = pred (fun t -> t = tok)

    let separated sep p =
      p <*> zeroOrMore (sep <*> p)
      |> map (fun (a1, args) -> a1::(List.map snd args))

    let separatedThen sep p1 p2 =
      p1 <*> zeroOrMore (sep <*> p2)
      |> map (fun (a1, args) -> a1::(List.map snd args))

    let separatedOrEmpty sep p =
      optional (separated sep p)
      |> map (fun l -> defaultArg l [])

    let number = pred (fun t -> t <= '9' && t >= '0')

    let integer = oneOrMore number |> map (fun nums ->
      nums |> List.fold (fun res n -> res * 10 + (int n - int '0')) 0)

    let letter = pred (fun t ->
      (t <= 'Z' && t >= 'A') || (t <= 'z' && t >= 'a'))

    let run (Parser(f)) input =
      match f (0, List.ofSeq input) with
      | Some((i, _), res) when i = Seq.length input -> Some res
      | _ -> None

module Evaluator =

    // ----------------------------------------------------------------------------
    // DOMAIN MODEL
    // ----------------------------------------------------------------------------

    type ColumnName = private { Col: char }

    module ColumnName =
      let ofChar c = { Col = System.Char.ToUpper c } 
      let pretty c = c.Col |> string
      let tryPrev c = 
        if c.Col = 'A' then 
          None 
        else 
          (int c.Col) - 1 |> char |> ofChar |> Some
      let tryNext c = 
        if c.Col = 'K' then 
          None 
        else 
          (int c.Col) + 1 |> char |> ofChar |> Some

    open Parsec

    type Position = ColumnName * int

    type Expr =
      | Reference of Position
      | Number of int
      | Binary of Expr * char * Expr

    // ----------------------------------------------------------------------------
    // PARSER
    // ----------------------------------------------------------------------------

    // Basics: operators (+, -, *, /), cell reference (e.g. A10), number (e.g. 123)
    let operator = char '+' <|> char '-' <|> char '*' <|> char '/'
    let reference = letter <*> integer |> map (fun (c,d) -> ((ColumnName.ofChar c), d) |> Reference )
    let number = integer |> map Number

    // Nested operator uses need to be parethesized, for example (1 + (3 * 4)).
    // <expr> is a binary operator without parentheses, number, reference or
    // nested brackets, while <term> is always bracketed or primitive. We need
    // to use `expr` recursively, which is handled via mutable slots.
    let exprSetter, expr = slot ()
    let brack = char '(' <*>> anySpace <*>> expr <<*> anySpace <<*> char ')'
    let term = number <|> reference <|> brack
    let binary = term <<*> anySpace <*> operator <<*> anySpace <*> term |> map (fun ((l,op), r) -> Binary(l, op, r))
    let exprAux = binary <|> term
    exprSetter.Set exprAux

    // Formula starts with `=` followed by expression
    // Equation you can write in a cell is either number or a formula
    let formula = char '=' <*>> anySpace <*>> expr
    let equation = anySpace <*>> (formula <|> number) <<*> anySpace

    // Run the parser on a given input
    let parse input = run equation input

    // ----------------------------------------------------------------------------
    // EVALUATOR
    // ----------------------------------------------------------------------------

    let rec evaluate visited (cells:Map<Position, string>) expr =
      match expr with
      | Number num ->
          Some num

      | Binary(l, op, r) ->
          let ops = dict [ '+', (+); '-', (-); '*', (*); '/', (/) ]
          evaluate visited cells l |> Option.bind (fun l ->
            evaluate visited cells r |> Option.map (fun r ->
              ops.[op] l r ))

      | Reference pos when Set.contains pos visited ->
          None

      | Reference pos ->
          cells.TryFind pos |> Option.bind (fun value ->
            parse value |> Option.bind (fun parsed ->
              evaluate (Set.add pos visited) cells parsed))

open Elmish
open Elmish.React
open Fable.React
open Fable.React.Props
open Fable.Core.JsInterop

open Evaluator

// ----------------------------------------------------------------------------
// DOMAIN MODEL
// ----------------------------------------------------------------------------

type Range = { TopLeft: Position; BottomRight: Position }

type Event =
  | UpdateValue of Position * string
  | StartEdit of Position
  | CancelEdit
  | Select of Range

type EditorState = 
  | Nothing
  | Editor of Position
  | Selection of Range

type SpreadsheetState =
  { Rows : int list
    Active : Position option
    Range: Range option
    Cols : ColumnName list
    Cells : Map<Position, string> }

type Movement =
    | MoveTo of Position
    | Invalid

type Direction = Up | Down | Left | Right

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

// ----------------------------------------------------------------------------
// RENDERING
// ----------------------------------------------------------------------------

let getPosition ((col, row): Position) (direction: Direction) =
    match direction with
    | Up -> Some (col, row - 1)
    | Down -> Some (col, row + 1)
    | Left -> 
        match ColumnName.tryPrev col with
        | Some col1 -> Some (col1, row)
        | None -> None
      
    | Right -> 
        match ColumnName.tryNext col with
        | Some col1 -> Some (col1, row)
        | None -> None

let getMovement (state: SpreadsheetState) (direction: Direction) : Movement =
    match state.Active with
    | None -> Invalid
    | Some position ->
        match (getPosition position direction) with
        | Some (col, row) when List.contains col state.Cols && List.contains row state.Rows ->
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

let prettyPos pos = sprintf "%s%d" (pos |> fst |> ColumnName.pretty) (pos |> snd)

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
  let headers = state.Cols |> List.map (ColumnName.pretty >> header)
  let headers = empty::headers

  let row cells = tr [] cells
  let cells n =
    let cells = state.Cols |> List.map (fun h -> renderCell trigger (h, n) state)
    header (string n) :: cells
  let rows = state.Rows |> List.map (fun r -> tr [] (cells r))

  table [] [
    tr [] headers
    tbody [] rows
  ]

// ----------------------------------------------------------------------------
// ENTRY POINT
// ----------------------------------------------------------------------------

let initial () =
  { Cols = ['A' .. 'K'] |> List.map ColumnName.ofChar
    Rows = [1 .. 15]
    Active = None
    Range = Some { TopLeft = (ColumnName.ofChar 'B', 2); BottomRight = (ColumnName.ofChar 'D', 5) }
    Cells = Map.empty }, Cmd.none

Program.mkProgram initial update view
|> Program.withReactBatched "main"
|> Program.run