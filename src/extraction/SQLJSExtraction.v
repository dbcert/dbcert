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


(*** Extraction (inspired by Q*Cert QcertExtraction module) ***)

Require Extraction.
Extraction Language OCaml.

Require Import ExtrOcamlBasic ExtrOcamlString ExtrOcamlNatInt ExtrOcamlZInt.
From Qcert Require Import Float ExtrOcamlFloatNatIntZInt.
Extraction Blacklist String List.

Require Import Qcert.Utils.Digits.
From Qcert Require Import TechRule DesignerRule.

Extract Inlined Constant PrimFloat.neg_infinity => "Float.neg_infinity".

Extract Constant Digits.nat_to_string10 => "(fun x -> Qcert_lib.Util.char_list_of_string (string_of_int x))".

Extract Constant TechRule.tech_rule => "camp_rule".
Extract Constant DesignerRule.designer_rule => "camp_rule".
Extract Constant TechRule.tech_rule_to_camp_rule => "(fun fruntime x -> x)".
Extract Constant DesignerRule.designer_rule_to_camp_rule => "(fun fruntime x -> x)".

(* Our modules *)

From SQLFS Require Values.
From SQLToNRACert Require ToEJson.

(* To bypass a bug in the extraction *)
Extract Constant Values.NullValues.FVal => "(Fset.build coq_OVal : value Fset.coq_Rcd)".

(* To remove typing information in attribute names *)
Extract Constant TnullTD.attribute_to_string => "
fun a ->
  match Obj.magic a with
  | Attr_string s -> s
  | Attr_Z s -> s
  | Attr_bool s -> s
".

Extraction "extraction/sql_query_to_js.ml" ToEJson.sql_query_to_js ToEJson.sql_query_to_nra.
