include Apron.Environment

let empty = make [||] [||]

let add_int i e = add e [|i|] [||]

let add_real r e = add e [||] [|r|]
