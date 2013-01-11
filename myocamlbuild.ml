open Ocamlbuild_plugin;;

dispatch begin function
| After_rules ->
      Pathname.define_context "src" ["lib"]
| _ -> ()
end
