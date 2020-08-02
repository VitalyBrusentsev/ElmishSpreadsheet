module Cell

type Column = private { Col: char }

module Column =
  let ofChar c = 
    { Col = System.Char.ToUpper c }

  let ofIndex (i: int) = 
    { Col = 'A' }

  let pretty c = 
    c.Col |> string 

  let tryPrev c = 
    if c.Col = 'A' then 
      None 
    else 
      (int c.Col) - 1 |> char |> ofChar |> Some

  let tryNext c = 
    if c.Col = 'Z' then
      None 
    else 
      (int c.Col) + 1 |> char |> ofChar |> Some

type Row = int