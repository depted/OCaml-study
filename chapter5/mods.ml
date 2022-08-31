let b = "bigred"
let inc x = x + 1 
module M = struct 
  let y = 42 
end

(*
  mods.cmo: this is a ompiled module object file, aka bytecode
  #load "mods.cmo";;   

  if you use the command, then you would use bytecode written like

  module Mods = struct
    let b = "bigred"
    let inc x = x + 1
    module M = struct
      let y = 42
    end
  end
*)

(* need to know how to use dune and utop in 
   https://cs3110.github.io/textbook/chapters/modules/toplevel.html
*)