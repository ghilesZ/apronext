module Generatorext = Generatorext
module Tconsext = Tconsext
module Texprext = Texprext
module Linconsext = Linconsext
module Abstractext = Abstractext
module Intervalext = Intervalext
module Scalarext = Scalarext
module Coeffext = Coeffext
module Environmentext = Environmentext
module Apol = Apol
module Abox = Abox
module Aoct = Aoct

let () =
  let open Apron.Manager in
  Printexc.register_printer (function
    | Error e -> Some (Format.asprintf "%a" print_exclog e)
    | _ -> None )
