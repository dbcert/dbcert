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


(*** Glue around the extracted compiler ***)

(** Glue for SELECT queries **)

let relname_to_extracted : Basics.relname -> Sql_query_to_js.relname
  = Util.char_list_of_string

let set_op_to_extracted : Basics.set_op -> Sql_query_to_js.set_op = function
  | Basics.Union -> Sql_query_to_js.Union
  | Basics.Intersect -> Sql_query_to_js.Inter
  | Basics.Except -> Sql_query_to_js.Diff

let and_or_to_extracted : Basics.and_or -> Sql_query_to_js.and_or = function
  | Basics.And_F -> Sql_query_to_js.And_F
  | Basics.Or_F -> Sql_query_to_js.Or_F

let quantifier_to_extracted : Basics.quantifier -> Sql_query_to_js.quantifier = function
  | Basics.Forall_F -> Sql_query_to_js.Forall_F
  | Basics.Exists_F -> Sql_query_to_js.Exists_F

let typed_aname_to_extracted : Basics.typed_aname -> Sql_query_to_js.Tuple.attribute = function
  | (n, Basics.TInt) -> Obj.magic (Sql_query_to_js.Attr_Z (Util.char_list_of_string n))
  | (n, Basics.TString) -> Obj.magic (Sql_query_to_js.Attr_string (Util.char_list_of_string n))
  | (n, Basics.TBool) -> Obj.magic (Sql_query_to_js.Attr_bool (Util.char_list_of_string n))
  | (n, Basics.TDouble) -> Obj.magic (Sql_query_to_js.Attr_float (Util.char_list_of_string n))

let typed_attribute_name_to_extracted : Basics.typed_attribute_name -> Sql_query_to_js.Tuple.attribute =
  fun ((r, n), ty) ->
  let n = match r with Some r -> r^"."^n | None -> n in
  typed_aname_to_extracted (n, ty)

let symb_to_extracted : Basics.symb -> int -> Basics.coltype -> Sql_query_to_js.Tuple.symbol =
  fun s a t -> Obj.magic (Sql_query_to_js.Symbol (Util.char_list_of_string (Basics.string_of_symb s a t)))

let predicate_to_extracted : Basics.predicate -> Sql_query_to_js.aggterm list -> Basics.coltype -> Sql_query_to_js.Tuple.predicate * Sql_query_to_js.aggterm list =
  fun p l t ->
  let (p, l) =
    (* TODO: remove when predicates are not exported to binops but to
       any NRAEnv expression *)
    match Basics.string_of_predicate p t with
      | "<" -> ("<", l)
      | "<." -> ("<.", l)
      | "<=" -> ("<=", l)
      | "<=." -> ("<=.", l)
      | ">" -> ("<", List.rev l)
      | ">." -> ("<.", List.rev l)
      | ">=" -> ("<=", List.rev l)
      | ">=." -> ("<=.", List.rev l)
      | "=" -> ("=", l)
      | "<>" -> ("<>", l)
      | p -> (p, l)
  in
  (Obj.magic (Util.char_list_of_string p), l)

let aggregate_to_extracted : Basics.aggregate -> Basics.coltype -> Sql_query_to_js.Tuple.aggregate =
  fun a t -> Obj.magic (Util.char_list_of_string (Basics.string_of_aggregate a t))

let value_to_extracted : Basics.value -> Sql_query_to_js.Tuple.value =
  fun v -> Obj.magic (
               match v with
                 (* We consider NULL values as untyped, and put them into bool values *)
                 | Basics.VNull -> Sql_query_to_js.NullValues.Value_bool None
                 | Basics.VString s -> Sql_query_to_js.NullValues.Value_string (Some (Util.char_list_of_string s))
                 | Basics.VInt i -> Sql_query_to_js.NullValues.Value_Z (Some i)
                 | Basics.VBool b -> Sql_query_to_js.NullValues.Value_bool (Some b)
                 | Basics.VFloat f -> Sql_query_to_js.NullValues.Value_float (Some f)
             )

let rec funterm_to_extracted : Coq_sql_algebra.funterm -> Sql_query_to_js.funterm = function
  | Coq_sql_algebra.F_Constant v -> Sql_query_to_js.F_Constant (value_to_extracted v)
  | Coq_sql_algebra.F_Dot tan -> Sql_query_to_js.F_Dot (typed_attribute_name_to_extracted tan)
  | Coq_sql_algebra.F_Expr (s, (f1::_ as l)) ->
     let t = Coq_sql_algebra.type_of_funterm f1 in
     Sql_query_to_js.F_Expr (symb_to_extracted s (List.length l) t, List.map funterm_to_extracted l)
  | Coq_sql_algebra.F_Expr (_, []) ->
     failwith "Sql_compiler.funterm_to_extracted: symbol applied to nothing"

let rec aggterm_to_extracted : Coq_sql_algebra.aggterm -> Sql_query_to_js.aggterm = function
  | Coq_sql_algebra.A_Expr f -> Sql_query_to_js.A_Expr (funterm_to_extracted f)
  | Coq_sql_algebra.A_agg (a, f) ->
     let t = Coq_sql_algebra.type_of_funterm f in
     Sql_query_to_js.A_agg (aggregate_to_extracted a t, funterm_to_extracted f)
  | Coq_sql_algebra.A_fun (s, (a1::_ as l)) ->
     let t = Coq_sql_algebra.type_of_aggterm a1 in
     Sql_query_to_js.A_fun (symb_to_extracted s (List.length l) t, List.map aggterm_to_extracted l)
  | Coq_sql_algebra.A_fun (_, []) ->
     failwith "Sql_compiler.aggterm_to_extracted: symbol applied to nothing"

let select_to_extracted : Coq_sql_algebra.select -> Sql_query_to_js.select = function
  | Coq_sql_algebra.Select_As (a, n) -> Sql_query_to_js.Select_As (aggterm_to_extracted a, typed_aname_to_extracted n)

let select_item_to_extracted : Coq_sql_algebra.select_item -> Sql_query_to_js.select_item = function
  | Coq_sql_algebra.Select_Star -> Sql_query_to_js.Select_Star
  | Coq_sql_algebra.Select_List l -> Sql_query_to_js.Select_List (List.map select_to_extracted l)

let groub_by_to_extracted : ToCoq.group_by -> Sql_query_to_js.group_by = function
  | ToCoq.Group_By l -> Sql_query_to_js.Group_By (List.map (fun f -> Sql_query_to_js.A_Expr (funterm_to_extracted f)) l)
  | ToCoq.Group_Fine -> Sql_query_to_js.Group_Fine

let att_renaming_to_extracted : ToCoq.att_renaming -> Sql_query_to_js.att_renaming = function
  | ToCoq.Att_As (tan1, tan2) -> Sql_query_to_js.Att_As (typed_attribute_name_to_extracted tan1, typed_attribute_name_to_extracted tan2)

let att_renaming_item_to_extracted : ToCoq.att_renaming_item -> Sql_query_to_js.att_renaming_item = function
  | ToCoq.Att_Ren_Star -> Sql_query_to_js.Att_Ren_Star
  | ToCoq.Att_Ren_List l -> Sql_query_to_js.Att_Ren_List (List.map att_renaming_to_extracted l)

let rec sql_query_to_extracted
    : ToCoq.sql_query -> Sql_query_to_js.relname Sql_query_to_js.sql_query0
  = function
  | ToCoq.Sql_Table r -> Sql_query_to_js.Sql_Table (relname_to_extracted r)
  | ToCoq.Sql_Set (op, q1, q2) -> Sql_query_to_js.Sql_Set (set_op_to_extracted op, sql_query_to_extracted q1, sql_query_to_extracted q2)
  | ToCoq.Sql_Select (select, from, where, gby, having) ->
     Sql_query_to_js.Sql_Select (select_item_to_extracted select, List.map from_item_to_extracted from, sql_formula_to_extracted where, groub_by_to_extracted gby, sql_formula_to_extracted having)

and from_item_to_extracted : ToCoq.from_item -> Sql_query_to_js.relname Sql_query_to_js.sql_from_item = function
  | ToCoq.From_Item (q, ari) -> Sql_query_to_js.From_Item (sql_query_to_extracted q, att_renaming_item_to_extracted ari)

and sql_formula_to_extracted : ToCoq.sql_query Coq_sql_algebra.sql_formula -> Sql_query_to_js.relname Sql_query_to_js.sql_query0 Sql_query_to_js.sql_formula = function
  | Coq_sql_algebra.Sql_Conj (ao, f1, f2) -> Sql_query_to_js.Sql_Conj (and_or_to_extracted ao, sql_formula_to_extracted f1, sql_formula_to_extracted f2)
  | Coq_sql_algebra.Sql_Not f -> Sql_query_to_js.Sql_Not (sql_formula_to_extracted f)
  | Coq_sql_algebra.Sql_True -> Sql_query_to_js.Sql_True
  | Coq_sql_algebra.Sql_Pred (p, (a1::_ as l)) ->
     let t = Coq_sql_algebra.type_of_aggterm a1 in
     let (p, l) = predicate_to_extracted p (List.map aggterm_to_extracted l) t in
     Sql_query_to_js.Sql_Pred (p, l)
  | Coq_sql_algebra.Sql_Pred (p, []) ->
     failwith "Sql_compiler.sql_formula_to_extracted: predicate applied to nothing"
  | Coq_sql_algebra.Sql_Quant (quant, p, l, q) ->
     (* Currently, only, =, < and <= are correctly extracted *)
     (* TODO: remove when predicates are not exported to binops but to
        any NRAEnv expression *)
     Sql_query_to_js.Sql_Quant (quantifier_to_extracted quant, Obj.magic (Util.char_list_of_string p), List.map aggterm_to_extracted l, sql_query_to_extracted q)
  | Coq_sql_algebra.Sql_In ((Coq_sql_algebra.Select_List l), q) -> Sql_query_to_js.Sql_In (List.map select_to_extracted l, sql_query_to_extracted q)
  | Coq_sql_algebra.Sql_In _ -> assert false
  | Coq_sql_algebra.Sql_Exists q -> Sql_query_to_js.Sql_Exists (sql_query_to_extracted q)


(** Glue for the database schema **)

let schema_to_extracted schema =
  fun r0 ->
  let r = Util.string r0 in
  try
    let attributes = List.assoc r schema in
    let la = List.map (fun (a, ty) ->
                 let a = Util.char_list_of_string ("."^a) in
                 let ra = r0@a in
                 match ty with
                   | Basics.TString -> Sql_query_to_js.Attr_string ra
                   | Basics.TInt -> Sql_query_to_js.Attr_Z ra
                   | Basics.TBool -> Sql_query_to_js.Attr_bool ra
                   | Basics.TDouble -> Sql_query_to_js.Attr_float ra
               ) attributes
    in
    Sql_query_to_js.Fset.mk_set Sql_query_to_js.oAN Sql_query_to_js.fAN la
  with
    | Not_found -> Sql_query_to_js.Fset.empty Sql_query_to_js.oAN Sql_query_to_js.fAN


(** Pretty-printer for SQLAlg queries **)

let pp_list op sep cl elt out l =
  let rec _pp out l =
    match l with
      | [] -> ()
      | [e] -> Printf.fprintf out "%a" elt e
      | e::l -> Printf.fprintf out "%a%s%a" elt e sep _pp l in
  Printf.fprintf out "%s%a%s" op _pp l cl

let pp_list (elt:out_channel -> 'a -> unit) (out:out_channel) (l:'a list) : unit =
  pp_list "[" "; " "]" elt out l

let pp_option elt out = function
  | Some e -> Printf.fprintf out "(Some %a)" elt e
  | None -> Printf.fprintf out "None"

let pp_pair elt1 elt2 out (x,y) = Printf.fprintf out "(%a,%a)" elt1 x elt2 y

let pp_bool out b = Printf.fprintf out "%B" b
let pp_char_list out s = Printf.fprintf out "%s" (Util.string s)
let pp_int out i = Printf.fprintf out "%d" i
let pp_float out f = Printf.fprintf out "%f" f

let pp_set_op out = function
  | Sql_query_to_js.Union -> Printf.fprintf out "Union"
  | Sql_query_to_js.UnionMax -> Printf.fprintf out "UnionMax"
  | Sql_query_to_js.Inter -> Printf.fprintf out "Inter"
  | Sql_query_to_js.Diff -> Printf.fprintf out "Diff"

let pp_tuple_attribute out (n:Sql_query_to_js.Tuple.attribute) =
  match Obj.magic n with
    | Sql_query_to_js.Attr_Z n -> Printf.fprintf out "(Attr_Z \"%s\"%%string)" (Util.string n)
    | Sql_query_to_js.Attr_string n -> Printf.fprintf out "(Attr_string \"%s\"%%string)" (Util.string n)
    | Sql_query_to_js.Attr_bool n -> Printf.fprintf out "(Attr_bool \"%s\"%%string)" (Util.string n)
    | Sql_query_to_js.Attr_float n -> Printf.fprintf out "(Attr_float \"%s\"%%string)" (Util.string n)

let pp_tuple_aggregate out (a:Sql_query_to_js.Tuple.aggregate) =
  Printf.fprintf out "%s" (Util.string (Obj.magic a))

let pp_tuple_value out (v:Sql_query_to_js.Tuple.value) =
  match Obj.magic v with
    | Sql_query_to_js.NullValues.Value_bool v -> Printf.fprintf out "(Value_bool %a)" (pp_option pp_bool) v
    | Sql_query_to_js.NullValues.Value_string v -> Printf.fprintf out "(Value_string %a)" (pp_option pp_char_list) v
    | Sql_query_to_js.NullValues.Value_Z v -> Printf.fprintf out "(Value_Z %a)" (pp_option pp_int) v
    | Sql_query_to_js.NullValues.Value_float v -> Printf.fprintf out "(Value_float %a)" (pp_option pp_float) v

let pp_tuple_symbol out (s:Sql_query_to_js.Tuple.symbol) =
  match Obj.magic s with
    | Sql_query_to_js.Symbol s -> Printf.fprintf out "(Symbol \"%s\"%%string)" (Util.string s)
    | Sql_query_to_js.CstVal v -> Printf.fprintf out "(CstVal %a)" pp_tuple_value v

let pp_tuple_predicate out (p:Sql_query_to_js.Tuple.predicate) =
  Printf.fprintf out "(Predicate \"%s\"%%string)" (Util.string (Obj.magic p))

let rec pp_funterm (out:out_channel) : Sql_query_to_js.funterm -> unit = function
  | Sql_query_to_js.F_Constant v -> Printf.fprintf out "(F_Constant %a)" pp_tuple_value v
  | Sql_query_to_js.F_Dot a -> Printf.fprintf out "(F_Dot %a)" pp_tuple_attribute a
  | Sql_query_to_js.F_Expr (s, l) -> Printf.fprintf out "(F_Expr %a %a)" pp_tuple_symbol s (pp_list pp_funterm) l

let rec pp_aggterm (out:out_channel) : Sql_query_to_js.aggterm -> unit = function
  | Sql_query_to_js.A_Expr f -> Printf.fprintf out "(A_Expr %a)" pp_funterm f
  | Sql_query_to_js.A_agg (a, f) -> Printf.fprintf out "(A_agg %a %a)" pp_tuple_aggregate a pp_funterm f
  | Sql_query_to_js.A_fun (s, l) -> Printf.fprintf out "(A_fun %a %a)" pp_tuple_symbol s (pp_list pp_aggterm) l

let pp_select out = function
  | Sql_query_to_js.Select_As (a, n) ->
     Printf.fprintf out "(Select_As %a %a)" pp_aggterm a pp_tuple_attribute n

let pp__select_list out (s:Sql_query_to_js._select_list) =
  pp_list pp_select out s

let pp_and_or out = function
  | Sql_query_to_js.And_F -> Printf.fprintf out "And_F"
  | Sql_query_to_js.Or_F -> Printf.fprintf out "Or_F"

let pp_quantifier out = function
  | Sql_query_to_js.Forall_F -> Printf.fprintf out "Forall_F"
  | Sql_query_to_js.Exists_F -> Printf.fprintf out "Exists_F"

let rec pp_query out (q:Sql_query_to_js.relname Sql_query_to_js.query0) =
  match q with
    | Sql_query_to_js.Q_Empty_Tuple -> Printf.fprintf out "Q_Empty_Tuple"
    | Sql_query_to_js.Q_Table r -> Printf.fprintf out "(Q_Table (Rel \"%s\"%%string))" (Util.string r)
    | Sql_query_to_js.Q_Set (op, q1, q2) -> Printf.fprintf out "(Q_Set %a %a %a)" pp_set_op op pp_query q1 pp_query q2
    | Sql_query_to_js.Q_NaturalJoin (q1, q2) -> Printf.fprintf out "(Q_NaturalJoin %a %a)" pp_query q1 pp_query q2
    | Sql_query_to_js.Q_Pi (pi, q) -> Printf.fprintf out "(Q_Pi (_Select_List %a) %a)" pp__select_list pi pp_query q
    | Sql_query_to_js.Q_Sigma (f, q) -> Printf.fprintf out "(Q_Sigma %a %a)" pp_sql_formula f pp_query q
    | Sql_query_to_js.Q_Gamma (pi, l, f, q) -> Printf.fprintf out "(Q_Gamma %a %a %a %a)" pp__select_list pi (pp_list pp_aggterm) l pp_sql_formula f pp_query q

and pp_sql_formula out = function
  | Sql_query_to_js.Sql_Conj (ao, f1, f2) -> Printf.fprintf out "(Sql_Conj %a %a %a)" pp_and_or ao pp_sql_formula f1 pp_sql_formula f2
  | Sql_query_to_js.Sql_Not f -> Printf.fprintf out "(Sql_Not %a)" pp_sql_formula f
  | Sql_query_to_js.Sql_True -> Printf.fprintf out "Sql_True"
  | Sql_query_to_js.Sql_Pred (p, l) -> Printf.fprintf out "(Sql_Pred %a %a)" pp_tuple_predicate p (pp_list pp_aggterm) l
  | Sql_query_to_js.Sql_Quant (qu, p, l, q) -> Printf.fprintf out "(Sql_Quant %a %a %a %a)" pp_quantifier qu pp_tuple_predicate p (pp_list pp_aggterm) l pp_query q
  | Sql_query_to_js.Sql_In (s, q) -> Printf.fprintf out "(Sql_In %a %a)" (pp_list pp_select) s pp_query q
  | Sql_query_to_js.Sql_Exists q -> Printf.fprintf out "(Sql_Exists %a)" pp_query q


(** Pretty-printer for NRAe queries **)

let rec pp_data out (d:Sql_query_to_js.data) =
  match d with
    | Sql_query_to_js.Dunit -> Printf.fprintf out "unit"
    | Sql_query_to_js.Dnat i -> Printf.fprintf out "(nat %d)" i
    | Sql_query_to_js.Dfloat f -> Printf.fprintf out "(float %f)" f
    | Sql_query_to_js.Dbool b -> Printf.fprintf out "(bool %B)" b
    | Sql_query_to_js.Dstring s -> Printf.fprintf out "(string %a)" pp_char_list s
    | Sql_query_to_js.Dcoll l -> Printf.fprintf out "(coll %a)" (pp_list pp_data) l
    | Sql_query_to_js.Drec l -> Printf.fprintf out "(rec %a)" (pp_list (pp_pair pp_char_list pp_data)) l
    | Sql_query_to_js.Dleft d -> Printf.fprintf out "(left %a)" pp_data d
    | Sql_query_to_js.Dright d -> Printf.fprintf out "(right %a)" pp_data d
    | Sql_query_to_js.Dbrand (_,d) -> Printf.fprintf out "(brand _ %a)" pp_data d
    | Sql_query_to_js.Dforeign _ -> Printf.fprintf out "(foreign _)"

let pp_nat_arith_binary_op out (op:Sql_query_to_js.nat_arith_binary_op) =
  match op with
    | Sql_query_to_js.NatPlus -> Printf.fprintf out "Plus"
    | Sql_query_to_js.NatMinus -> Printf.fprintf out "Minus"
    | Sql_query_to_js.NatMult -> Printf.fprintf out "Mult"
    | Sql_query_to_js.NatDiv -> Printf.fprintf out "Div"
    | Sql_query_to_js.NatRem -> Printf.fprintf out "Rem"
    | Sql_query_to_js.NatMin -> Printf.fprintf out "Min"
    | Sql_query_to_js.NatMax -> Printf.fprintf out "Max"

let pp_float_arith_binary_op out (op:Sql_query_to_js.float_arith_binary_op) =
  match op with
    | Sql_query_to_js.FloatPlus -> Printf.fprintf out "Plus"
    | Sql_query_to_js.FloatMinus -> Printf.fprintf out "Minus"
    | Sql_query_to_js.FloatMult -> Printf.fprintf out "Mult"
    | Sql_query_to_js.FloatDiv -> Printf.fprintf out "Div"
    | Sql_query_to_js.FloatPow -> Printf.fprintf out "Pow"
    | Sql_query_to_js.FloatMin -> Printf.fprintf out "Min"
    | Sql_query_to_js.FloatMax -> Printf.fprintf out "Max"

let pp_float_compare_binary_op out (op:Sql_query_to_js.float_compare_binary_op) =
  match op with
    | Sql_query_to_js.FloatLt -> Printf.fprintf out "Lt"
    | Sql_query_to_js.FloatLe -> Printf.fprintf out "Le"
    | Sql_query_to_js.FloatGt -> Printf.fprintf out "Gt"
    | Sql_query_to_js.FloatGe -> Printf.fprintf out "Ge"

let pp_binary_op out (op:Sql_query_to_js.binary_op) =
  match op with
    | Sql_query_to_js.OpEqual -> Printf.fprintf out "Equal"
    | Sql_query_to_js.OpRecConcat -> Printf.fprintf out "RecConcat"
    | Sql_query_to_js.OpRecMerge -> Printf.fprintf out "RecMerge"
    | Sql_query_to_js.OpAnd -> Printf.fprintf out "And"
    | Sql_query_to_js.OpOr -> Printf.fprintf out "Or"
    | Sql_query_to_js.OpLt -> Printf.fprintf out "Lt"
    | Sql_query_to_js.OpLe -> Printf.fprintf out "Le"
    | Sql_query_to_js.OpBagUnion -> Printf.fprintf out "BagUnion"
    | Sql_query_to_js.OpBagDiff -> Printf.fprintf out "BagDiff"
    | Sql_query_to_js.OpBagMin -> Printf.fprintf out "BagMin"
    | Sql_query_to_js.OpBagMax -> Printf.fprintf out "BagMax"
    | Sql_query_to_js.OpBagNth -> Printf.fprintf out "BagNth"
    | Sql_query_to_js.OpContains -> Printf.fprintf out "Contains"
    | Sql_query_to_js.OpStringConcat -> Printf.fprintf out "StringConcat"
    | Sql_query_to_js.OpStringJoin -> Printf.fprintf out "StringJoin"
    | Sql_query_to_js.OpNatBinary op -> Printf.fprintf out "(NatBinary %a)" pp_nat_arith_binary_op op
    | Sql_query_to_js.OpFloatBinary op -> Printf.fprintf out "(FloatBinary %a)" pp_float_arith_binary_op op
    | Sql_query_to_js.OpFloatCompare op -> Printf.fprintf out "(FloatCompare %a)" pp_float_compare_binary_op op
    | Sql_query_to_js.OpForeignBinary _ -> Printf.fprintf out "(ForeignBinary _)"

let pp_nat_arith_unary_op out (op:Sql_query_to_js.nat_arith_unary_op) =
  match op with
    | Sql_query_to_js.NatAbs -> Printf.fprintf out "Abs"
    | Sql_query_to_js.NatLog2 -> Printf.fprintf out "Log2"
    | Sql_query_to_js.NatSqrt -> Printf.fprintf out "Sqrt"

let pp_float_arith_unary_op out (op:Sql_query_to_js.float_arith_unary_op) =
  match op with
    | Sql_query_to_js.FloatNeg -> Printf.fprintf out "Neg"
    | Sql_query_to_js.FloatSqrt -> Printf.fprintf out "Sqrt"
    | Sql_query_to_js.FloatExp -> Printf.fprintf out "Exp"
    | Sql_query_to_js.FloatLog -> Printf.fprintf out "Log"
    | Sql_query_to_js.FloatLog10 -> Printf.fprintf out "Log10"
    | Sql_query_to_js.FloatCeil -> Printf.fprintf out "Ceil"
    | Sql_query_to_js.FloatFloor -> Printf.fprintf out "Floor"
    | Sql_query_to_js.FloatAbs -> Printf.fprintf out "Abs"

let pp_unary_op out (op:Sql_query_to_js.unary_op) =
  match op with
    | Sql_query_to_js.OpIdentity -> Printf.fprintf out "Identity"
    | Sql_query_to_js.OpNeg -> Printf.fprintf out "Neg"
    | Sql_query_to_js.OpRec s -> Printf.fprintf out "(Rec %a)" pp_char_list s
    | Sql_query_to_js.OpDot s -> Printf.fprintf out "(Dot %a)" pp_char_list s
    | Sql_query_to_js.OpRecRemove s -> Printf.fprintf out "(RecRemove %a)" pp_char_list s
    | Sql_query_to_js.OpRecProject l -> Printf.fprintf out "(RecProject %a)" (pp_list pp_char_list) l
    | Sql_query_to_js.OpBag -> Printf.fprintf out "Bag"
    | Sql_query_to_js.OpSingleton -> Printf.fprintf out "Singleton"
    | Sql_query_to_js.OpFlatten -> Printf.fprintf out "Flatten"
    | Sql_query_to_js.OpDistinct -> Printf.fprintf out "Distinct"
    | Sql_query_to_js.OpOrderBy _ -> Printf.fprintf out "(OrderBy _)"
    | Sql_query_to_js.OpCount -> Printf.fprintf out "Count"
    | Sql_query_to_js.OpToString -> Printf.fprintf out "ToString"
    | Sql_query_to_js.OpToText -> Printf.fprintf out "ToText"
    | Sql_query_to_js.OpLength -> Printf.fprintf out "Length"
    | Sql_query_to_js.OpSubstring (i, io) -> Printf.fprintf out "(Substring %d %a)" i (pp_option pp_int) io
    | Sql_query_to_js.OpLike s -> Printf.fprintf out "(Like %a)" pp_char_list s
    | Sql_query_to_js.OpLeft -> Printf.fprintf out "Left"
    | Sql_query_to_js.OpRight -> Printf.fprintf out "Right"
    | Sql_query_to_js.OpBrand _ -> Printf.fprintf out "(Brands _)"
    | Sql_query_to_js.OpUnbrand -> Printf.fprintf out "Unbrand"
    | Sql_query_to_js.OpCast _ -> Printf.fprintf out "(Cast _)"
    | Sql_query_to_js.OpNatUnary op -> Printf.fprintf out "(NatUnary %a)" pp_nat_arith_unary_op op
    | Sql_query_to_js.OpNatSum -> Printf.fprintf out "NatSum"
    | Sql_query_to_js.OpNatMin -> Printf.fprintf out "NatMin"
    | Sql_query_to_js.OpNatMax -> Printf.fprintf out "NatMax"
    | Sql_query_to_js.OpNatMean -> Printf.fprintf out "NatMean"
    | Sql_query_to_js.OpFloatOfNat -> Printf.fprintf out "FloatOfNat"
    | Sql_query_to_js.OpFloatUnary op -> Printf.fprintf out "(FloatUnary %a)" pp_float_arith_unary_op op
    | Sql_query_to_js.OpFloatTruncate -> Printf.fprintf out "FloatTruncate"
    | Sql_query_to_js.OpFloatSum -> Printf.fprintf out "FloatSum"
    | Sql_query_to_js.OpFloatMean -> Printf.fprintf out "FloatMean"
    | Sql_query_to_js.OpFloatBagMin -> Printf.fprintf out "FloatBagMin"
    | Sql_query_to_js.OpFloatBagMax -> Printf.fprintf out "FloatBagMax"
    | Sql_query_to_js.OpForeignUnary _ -> Printf.fprintf out "(ForeignUnary _)"

let rec pp_nra out (q:Sql_query_to_js.nraenv) =
  match q with
    | Sql_query_to_js.NRAEnvGetConstant s -> Printf.fprintf out "(GetConstant %a)" pp_char_list s
    | Sql_query_to_js.NRAEnvID -> Printf.fprintf out "ID"
    | Sql_query_to_js.NRAEnvConst d -> Printf.fprintf out "(Const %a)" pp_data d
    | Sql_query_to_js.NRAEnvBinop (op, q1, q2) -> Printf.fprintf out "(Binop %a %a %a)" pp_binary_op op pp_nra q1 pp_nra q2
    | Sql_query_to_js.NRAEnvUnop (op, q) -> Printf.fprintf out "(Unop %a %a)" pp_unary_op op pp_nra q
    | Sql_query_to_js.NRAEnvMap (q1, q2) -> Printf.fprintf out "(Map %a %a)" pp_nra q1 pp_nra q2
    | Sql_query_to_js.NRAEnvMapProduct (q1, q2) -> Printf.fprintf out "(MapProduct %a %a)" pp_nra q1 pp_nra q2
    | Sql_query_to_js.NRAEnvProduct (q1, q2) -> Printf.fprintf out "(Product %a %a)" pp_nra q1 pp_nra q2
    | Sql_query_to_js.NRAEnvSelect (q1, q2) -> Printf.fprintf out "(Select %a %a)" pp_nra q1 pp_nra q2
    | Sql_query_to_js.NRAEnvDefault (q1, q2) -> Printf.fprintf out "(Default %a %a)" pp_nra q1 pp_nra q2
    | Sql_query_to_js.NRAEnvEither (q1, q2) -> Printf.fprintf out "(Either %a %a)" pp_nra q1 pp_nra q2
    | Sql_query_to_js.NRAEnvEitherConcat (q1, q2) -> Printf.fprintf out "(EitherConcat %a %a)" pp_nra q1 pp_nra q2
    | Sql_query_to_js.NRAEnvApp (q1, q2) -> Printf.fprintf out "(App %a %a)" pp_nra q1 pp_nra q2
    | Sql_query_to_js.NRAEnvEnv -> Printf.fprintf out "Env"
    | Sql_query_to_js.NRAEnvAppEnv (q1, q2) -> Printf.fprintf out "(AppEnv %a %a)" pp_nra q1 pp_nra q2
    | Sql_query_to_js.NRAEnvMapEnv q -> Printf.fprintf out "(MapEnv %a)" pp_nra q
    | Sql_query_to_js.NRAEnvFlatMap (q1, q2) -> Printf.fprintf out "(FlatMap %a %a)" pp_nra q1 pp_nra q2
    | Sql_query_to_js.NRAEnvJoin (q1, q2, q3) -> Printf.fprintf out "(Join %a %a %a)" pp_nra q1 pp_nra q2 pp_nra q3
    | Sql_query_to_js.NRAEnvNaturalJoin (q1, q2) -> Printf.fprintf out "(NaturalJoin %a %a)" pp_nra q1 pp_nra q2
    | Sql_query_to_js.NRAEnvProject (l, q) -> Printf.fprintf out "(Project %a %a)" (pp_list pp_char_list) l pp_nra q
    | Sql_query_to_js.NRAEnvGroupBy (s, l, q) -> Printf.fprintf out "(GroupBy %a %a %a)" pp_char_list s (pp_list pp_char_list) l pp_nra q
    | Sql_query_to_js.NRAEnvUnnest (s1, s2, q) -> Printf.fprintf out "(Unnest %a %a %a)" pp_char_list s1 pp_char_list s2 pp_nra q
