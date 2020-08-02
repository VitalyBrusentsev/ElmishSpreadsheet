module Cell

type Column = private | Col of char

module Column =
  let ofChar c = 
    System.Char.ToUpper c |> Col

  let ofIndex (i: int) = 
    (int 'A' + i - 1) |> char |> Col

  let pretty (Col c) = 
    c |> string 

  let tryPrev (Col c) = 
    if c = 'A' then 
      None 
    else 
      (int c) - 1 |> char |> ofChar |> Some

  let tryNext (Col c) = 
    if c = 'Z' then
      None 
    else 
      (int c) + 1 |> char |> ofChar |> Some

type Position = Column * int
