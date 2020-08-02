module Evaluator

open FsToolkit.ErrorHandling

type ColumnName = private { Col: char }

module ColumnName =
  let ofChar c = { Col = System.Char.ToUpper c } 
  let pretty c = c.Col |> string
  let tryPrev c = 
    if c.Col = 'A' then 
      None 
    else 
      (int c.Col) - 1 |> char |> ofChar |> Some
  let tryNext c (maxColumns: int) = 
    if (int c.Col) - (int 'A') + 1 = maxColumns then
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
      option {
        let! left = evaluate visited cells l
        let! right = evaluate visited cells r
        return ops.[op] left right
      }

  | Reference pos when Set.contains pos visited ->
      None

  | Reference pos -> option {
      let! cell = cells.TryFind pos
      let! value = parse cell
      let newVisited = visited |> Set.add pos
      return! evaluate newVisited cells value
  }
