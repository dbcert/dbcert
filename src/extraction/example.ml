(************************************************************************************)
(**                                                                                 *)
(**                             The DBCert Library                                  *)
(**                                                                                 *)
(**            LRI, CNRS & Université Paris-Sud, Université Paris-Saclay            *)
(**                                                                                 *)
(**                        Copyright 2016-2019 : FormalData                         *)
(**                                                                                 *)
(**         Authors: Véronique Benzaken                                             *)
(**                  Évelyne Contejean                                              *)
(**                  Mohammed Hachmaoui                                             *)
(**                  Chantal Keller                                                 *)
(**                                                                                 *)
(************************************************************************************)


(*** Example of use of the extracted compiler ***)

let t1 : Sql_query_to_js.relname = Util.char_list_of_string "t1"

let schema : Sql_query_to_js.relname -> Sql_query_to_js.attribute0 Sql_query_to_js.Fset.set = fun r ->
  if r = t1 then
    Sql_query_to_js.Fset.mk_set Sql_query_to_js.oAN Sql_query_to_js.fAN [Sql_query_to_js.Attr_Z ['a']]
  else
    Sql_query_to_js.Fset.empty Sql_query_to_js.oAN Sql_query_to_js.fAN

let qSQL : Sql_query_to_js.relname Sql_query_to_js.sql_query =
  Sql_query_to_js.Sql_Table t1

let qJS = Sql_query_to_js.sql_query_to_js schema qSQL

let _ =
  match qJS with
    | Some q -> Printf.printf "%s\n" (Util.string q)
    | None -> Printf.printf "Compilation failed"
