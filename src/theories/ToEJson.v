(************************************************************************************)
(**                                                                                 *)
(**                          The SQLToNRACert Library                               *)
(**                                                                                 *)
(**                       LMF, CNRS & Université Paris-Saclay                       *)
(**                                                                                 *)
(**                        Copyright 2016-2022 : FormalData                         *)
(**                                                                                 *)
(**         Authors: Véronique Benzaken                                             *)
(**                  Évelyne Contejean                                              *)
(**                  Mohammed Hachmaoui                                             *)
(**                  Chantal Keller                                                 *)
(**                                                                                 *)
(************************************************************************************)


(*** Full translation ***)

Require Import List NArith String.

Require Import Utils Qcert.Data.DataRuntime Qcert.NRAEnv.NRAEnvRuntime Qcert.Imp.ImpRuntime.
Require Import Qcert.Driver.CompCorrectness.

(* Add LoadPath *)
(*     "../../datacert/plugins/" *)
(*   as Datacert.plugins. *)

From SQLFS Require Import OrderedSet FiniteSet FiniteBag FiniteCollection
        Tree Formula DExpressions ListSort.
From SQLFS Require Import FlatData.
From SQLFS Require Import SqlAlgebra.

From SQLToNRACert Require Import FormulaToNRAEnv QueryToNRAEnv InstanceToNRAEnv.
Import QueryToNRAEnv.

Require Import Qcert.Compiler.Enhanced.EnhancedModel.
Require Import Qcert.Compiler.Component.LoggerComponent.
Require Import String Permutation NArith ZArith.
From SQLFS Require Import ListFacts TuplesImpl.

From SQLFS Require Import GenericInstance SqlSyntax.
From SQLToNRACert Require Import Plugins.

From SQLToNRACert Require Import TnullQN.


Require Import Qcert.Driver.CompLang.
Require Import Qcert.Compiler.Enhanced.EnhancedCompiler.
Require Import Qcert.NRAEnv.Lang.NRAEnv.
Require Import Qcert.TypeSystem.TBrandModel.

(** Translation to JS **)

Global Instance bm : brand_model := EnhancedCompiler.QType.empty_brand_model tt eq_refl.

Definition config := @EnhancedCompiler.QDriver.default_dv_config bm.
Definition source := L_nraenv.
Definition middle := L_imp_ejson.
Definition target := L_javascript.

Definition nraenv2nraenv_opt : query -> query :=
  @EnhancedCompiler.QDriver.compile_from_source_target bm enhanced_foreign_typing config source source.

Definition nraenv2imp_opt : query -> query :=
  @EnhancedCompiler.QDriver.compile_from_source_target bm enhanced_foreign_typing config source middle.

Definition nraenv2imp_no_opt : query -> query :=
  @EnhancedCompiler.QDriver.compile_nraenv_to_imp_ejson_verified bm enhanced_foreign_typing config.

Definition imp2JS : query -> query :=
  @EnhancedCompiler.QDriver.compile_from_source_target bm enhanced_foreign_typing config middle target.

Section ToEJson.

  Variable basesort : relname -> Fset.set FAN.

  Notation query_imp := (@query EnhancedRuntime.compiler_foreign_type bm enhanced_foreign_runtime enhanced_foreign_ejson enhanced_foreign_ejson_runtime enhanced_foreign_reduce_op).
  Notation query_sqlalg := (SqlAlgebra.query TNull relname).
  Notation sql_query_to_alg := (SqlAlgebra.sql_query_to_alg (T:=TNull) basesort).
  Notation query_to_nraenv_top := (@query_to_nraenv_top _ _ tnull_frt tnull_TD tnull_SN tnull_EN tnull_AN tnull_FTN tnull_ATN (tnull_IN basesort) _ tnull_PN (tnull_FN basesort)).

  Notation weak_well_formed_q := (weak_well_formed_q (T:=TNull) basesort).
  Notation is_translatable_q := (@is_translatable_q _ ORN tnull_frt tnull_TD _ tnull_AN (tnull_IN basesort)).
  Notation well_formed_q := (well_formed_q (T:=TNull) basesort nil).
  Notation more_well_formed_q := (@more_well_formed_q _ ORN tnull_frt tnull_TD (tnull_IN basesort) nil).

  Inductive result : Type :=
  | Success (_:javascript)
  | SuccessUpToImp (_:query_imp)
  | Not_weak_well_formed_sqlcoq
  | Not_translatable_sqlalg (_:query_sqlalg)
  | Not_well_formed_sqlalg (_:query_sqlalg)
  | Not_more_well_formed_sqlalg (_:query_sqlalg)
  | Compilation_nra_js_failed
  .

  Definition sql_query_to_imp (opt:bool) qsql :=
    if weak_well_formed_q qsql then
      let qalg := query_optim (sql_query_to_alg qsql) in
      if is_translatable_q qalg then
        if well_formed_q qalg then
          if more_well_formed_q qalg then
            let qnra := query_to_nraenv_top qalg in
            let qimp :=
                if opt
                then nraenv2imp_opt (Q_nraenv qnra)
                else nraenv2imp_no_opt (Q_nraenv qnra)
            in SuccessUpToImp qimp
          else
            Not_more_well_formed_sqlalg qalg
        else
          Not_well_formed_sqlalg qalg
      else
        Not_translatable_sqlalg qalg
    else
      Not_weak_well_formed_sqlcoq
  .

  Definition sql_query_to_js (opt:bool) qsql :=
    match sql_query_to_imp opt qsql with
    | SuccessUpToImp q =>
      match imp2JS q with
      | Q_javascript s => Success s
      | _ => Compilation_nra_js_failed
      end
    | error => error
    end.


  Lemma well_sorted_instance_well_sorted_sql_table i:
    well_sorted_instance (IN:=tnull_IN basesort) i =true ->
    well_sorted_sql_table TNull basesort
      (fun r : relname =>
         match Oset.find ORN r i with
         | Some b => b
         | None => emptysetBE
         end).
  Proof.
    unfold InstanceToNRAEnv.well_sorted_instance, well_sorted_sql_table.
    intros Hws tbl t. case_eq (Oset.find ORN tbl i).
    - intros b Hb Htb. apply Oset.find_some in Hb.
      rewrite Bool.andb_true_iff in Hws.
      destruct Hws as [_ Hws].
      rewrite forallb_forall in Hws.
      assert (Hws := Hws _ Hb).      
      rewrite forallb_forall in Hws.
      rewrite Febag.mem_unfold in Htb.
      rewrite Oeset.mem_bool_true_iff in Htb.
      destruct Htb as [t' [Heq Ht']].
      rewrite (Fset.equal_eq_1
                       _ _ _ _
                       (tuple_eq_labels _ _ _ Heq)).
      apply (Hws _ Ht').
    - rewrite Febag.empty_spec_weak. discriminate.
  Qed.

  Notation eval_sql_query_top i :=
    (eval_sql_query (T:=TNull) basesort
       (fun r => match Oset.find ORN r i with
                 | Some b => b
                 | None => emptysetBE
                 end) Bool3.unknown3 contains_nulls nil).

  Notation eval_query_top i :=
    (SqlAlgebra.eval_query (T:=TNull) basesort
       (fun r => match Oset.find ORN r i with
                 | Some b => b
                 | None => emptysetBE
                 end) Bool3.unknown3 contains_nulls nil).

  Notation well_sorted_instance := (InstanceToNRAEnv.well_sorted_instance (IN:=tnull_IN basesort)).
  Notation is_complete_instance_q := (@is_complete_instance_q _ ORN _ tnull_TD).
  Notation instance_to_bindings := (InstanceToNRAEnv.instance_to_bindings (IN:=tnull_IN basesort)).

  Theorem sql_query_to_imp_no_opt_is_sound qsql :
    match sql_query_to_imp false qsql with
    | SuccessUpToImp (Q_imp_ejson qimp) =>
      let qalg := query_optim (sql_query_to_alg qsql) in
      forall i,
        well_sorted_instance i = true ->
        well_typed_instance i = true ->
        is_complete_instance_q i qalg = true ->
        match imp_ejson_eval_top brand_relation_brands (instance_to_bindings i) qimp with
        | Some (dcoll dq) => Permutation (bagT_to_listD (eval_sql_query_top i qsql)) dq
        | _ => False
        end
    | SuccessUpToImp _ => False
    | Not_weak_well_formed_sqlcoq => weak_well_formed_q qsql = false
    | Not_translatable_sqlalg _ => is_translatable_q (query_optim (sql_query_to_alg qsql)) = false
    | Not_well_formed_sqlalg _ => well_formed_q (query_optim (sql_query_to_alg qsql)) = false (* TODO: should be ensured by translation SQL -> Alg *)
    | Not_more_well_formed_sqlalg _ => more_well_formed_q (query_optim (sql_query_to_alg qsql)) = false
    | Compilation_nra_js_failed => False
    | Success _ => False
    end.
  Proof.
    unfold sql_query_to_imp.
    case_eq (weak_well_formed_q qsql); intro HwfSQL; try reflexivity.
    set (qalg2 := sql_query_to_alg qsql).
    set (qalg := query_optim qalg2).
    case_eq (is_translatable_q qalg); intro Htrans; try reflexivity.
    case_eq (well_formed_q qalg); intro HwfAlg; try reflexivity.
    case_eq (more_well_formed_q qalg); intro HmwfAlg; try reflexivity.
    set (qnra := query_to_nraenv_top qalg).
    generalize
      (compile_nraenv_to_imp_ejson_verified_correct
         config qnra
         (CompCustom.compile_nraenv_to_imp_ejson_verified config (Q_nraenv qnra))
         eq_refl); intros.
    destruct H as [x H0].
    destruct H0 as [H H1].
    replace (nraenv2imp_no_opt (Q_nraenv qnra)) with (Q_imp_ejson x); auto.
    intros i Hi Ti Tc.
    generalize (@query_to_nraenv_top_is_sound _ _ tnull_frt tnull_TD tnull_SN tnull_EN tnull_AN tnull_FTN tnull_ATN (tnull_IN basesort) _ tnull_PN (tnull_FN basesort) brand_relation_brands i Hi Ti qalg Tc Htrans HwfAlg HmwfAlg).
    fold qnra.
    rewrite <- (nraenv_to_imp_ejson_correct config qnra x H).
    assert (@nraenv_eval_top enhanced_foreign_runtime
            (@brand_relation_brands (@brand_model_relation enhanced_foreign_type bm)) qnra
            (@InstanceToNRAEnv.instance_to_bindings relname ORN enhanced_foreign_runtime tnull_TD
               (tnull_IN basesort) i) =
            @nraenv_eval_top enhanced_foreign_runtime (@CompEval.h EnhancedRuntime.compiler_foreign_type bm) qnra
      (@InstanceToNRAEnv.instance_to_bindings relname ORN enhanced_foreign_runtime tnull_TD (tnull_IN basesort) i)) by reflexivity.
    rewrite <- H0; clear H0.
    case_eq (@nraenv_eval_top enhanced_foreign_runtime
            (@brand_relation_brands (@brand_model_relation enhanced_foreign_type bm)) qnra
            (@InstanceToNRAEnv.instance_to_bindings relname ORN enhanced_foreign_runtime tnull_TD
                                                    (tnull_IN basesort) i)).
    intros [ | | | | |dq| | | | | ]; auto.
    intros HNRAeval. apply Permutation_trans, permutation_bagT_eq, Febag.equal_sym.
    erewrite Febag.equal_eq_1; [ |apply query_optim_is_sound, contains_nulls_eq].
    apply sql_query_to_alg_is_sound; auto.
    - apply contains_nulls_eq.
    - now apply well_sorted_instance_well_sorted_sql_table.
    - intros; contradiction.
  Qed.


  (* DEBUG: used to inspect possible optimizations at the NRAe level *)
  Definition sql_query_to_nra qsql :=
    if weak_well_formed_q qsql then
      let qalg := query_optim (sql_query_to_alg qsql) in
      if is_translatable_q qalg then
        if well_formed_q qalg then
          if more_well_formed_q qalg then
            let qnra := query_to_nraenv_top qalg in
            match nraenv2nraenv_opt (Q_nraenv qnra) with
            | Q_nraenv qnra_opt => Some (qalg, qnra, qnra_opt)
            | _ => None
            end
          else
            None
        else
          None
      else
        None
    else
      None
  .

End ToEJson.
