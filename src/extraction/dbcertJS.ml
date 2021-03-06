open Js_of_ocaml

open Compiler

(** Putting everything together **)

let input_files = ref []
let runtime = ref ""

let _ =
  Arg.parse args_list (anon_args input_files) usage;
  if (List.length !input_files <> 1) then
    Printf.printf "Argument required: file with create and select queries\n"
  else
    let fileSQL = List.nth !input_files 0 in
    let chanSQL = open_in fileSQL in
    let bufSQL = Lexing.from_channel chanSQL in
    let index = ref 0 in (* File counter *)
    begin try
      while true do
        let qSQL = Sql_parser.line Sql_lexer.token bufSQL in
        begin match qSQL with
        | Sql_ast.SQL_Create (t, (cols, _)) -> Hashtbl.add ToCoq.tables t cols
        | Sql_ast.SQL_Query qselect ->
            begin
              let ctxt = ToCoq.init_context () in
              let (schema, _, _) = ctxt in
              let qSQLCoq = ToCoq.select ctxt qselect in
              Printf.printf "%s" dash72;
              Printf.printf "SQLCoq query: %s\n\n" (ToCoq.string_of_sql_query schema qSQLCoq);
              let qSQLCoqJS = sql_query_to_extracted qSQLCoq in
              let schemaJS = schema_to_extracted schema in
              let qJS = Sql_query_to_js.sql_query_to_js schemaJS true qSQLCoqJS in (* XXX optimization on *)
              (match qJS with
              | Sql_query_to_js.Success q ->
                  let content = !runtime ^ (Util.string q) ^ exports in
                  begin match !output with
                  | None -> Printf.printf "Corresponding JS query: %s\n" content
                  | Some file ->
                      let base = Filename.remove_extension file in
                      let postfix = if !index > 0 then "_" ^ (string_of_int !index) else "" in
                      let output_file = base ^ postfix ^ ".js" in 
                      Printf.printf "Corresponding JS query generated in: %s\n" output_file;
                      Util.make_file output_file content
                  end
              | Sql_query_to_js.SuccessUpToImp _ -> Printf.printf "Compilation from Imp to JS failed\n"
              | Sql_query_to_js.Not_weak_well_formed_sqlcoq -> Printf.printf "Compilation failed: SQLCoq query is weak ill formed\n"
              | Sql_query_to_js.Not_translatable_sqlalg qalg ->
                  Printf.printf "Compilation failed: SQLAlg query is not translatable:\n\t%a\n" pp_query qalg
              | Sql_query_to_js.Not_well_formed_sqlalg qalg ->
                  Printf.printf "Compilation failed: SQLAlg query is ill formed (case 1):\n\t%a\n" pp_query qalg
              | Sql_query_to_js.Not_more_well_formed_sqlalg qalg ->
                  Printf.printf "Compilation failed: SQLAlg query is ill formed (case 2):\n\t%a\n" pp_query qalg
              | Sql_query_to_js.Compilation_nra_js_failed -> Printf.printf "Compilation failed: NRA to JS did not output JS\n");
              incr index;
              flush stdout;
            end
        | _ -> raise (Sql_lexer.Error "Expecting CREATE or SELECT query")
        end;
      done;
    with
    | Sql_lexer.Error msg -> failwith ("Toto : "^msg)
    | Sql_parser.Error -> failwith ("At offset "^(string_of_int (Lexing.lexeme_start bufSQL))^": syntax error.\n")
    | Sql_lexer.Eof -> Printf.printf "Translation to JavaScript finished\n";
    end;
    close_in chanSQL

open Util
open DataUtil
open QcertUtil
open QcertConfig
open QcertCompiler.EnhancedCompiler


(**********************************)
(* Configuration support          *)
(**********************************)

(* XXX g is applied to json value if it exists, f is the configuration setter, taking the result of g XXX *)
let apply_gen gconf f g o = Js.Optdef.iter o (fun j -> f gconf (g j))
let apply gconf f o = apply_gen gconf f Js.to_string o
let apply_int gconf f o = apply_gen gconf f (fun x ->  int_of_float (Js.float_of_number x)) o
let iter_array_gen gconf f o = Js.Optdef.iter o (fun a -> f gconf a)
let iter_array gconf f o =
  iter_array_gen gconf
    (fun gconf a ->
      let a = Js.str_array a in
      ignore (Js.array_map (fun s -> f gconf (Js.to_string s)) a)) o
let map_array_gen gconf f o =
  Js.Optdef.map o
    (fun a -> f gconf a)

(**********************************)
(* Optim. configuration support   *)
(**********************************)

let optim_config_from_json s : QDriver.optim_config =
  let optim_json = ParseString.parse_json_from_string s in
  DataUtil.build_optim_config optim_json

(**********************************)
(* Equivalent to qcert cmd        *)
(**********************************)

let global_config_of_json j =
  let gconf =
    { gconf_qname = None;
      gconf_source = QcertCompiler.L_camp_rule;
      gconf_target = QcertCompiler.L_javascript;
      gconf_path = [];
      gconf_exact_path = false;
      gconf_dir = None;
      gconf_dir_target = None;
      gconf_schema = TypeUtil.empty_schema;
      gconf_input = [];
      gconf_output = QData.dunit;
      gconf_io = None;
      gconf_emit_all = false;
      gconf_emit_sexp = false;
      gconf_emit_sexp_all = false;
      gconf_eval = false;
      gconf_eval_all = false;
      gconf_eval_debug = false;
      gconf_eval_validate = false;
      gconf_source_sexp = false;
      gconf_pretty_config = PrettyCommon.default_pretty_config ();
      gconf_java_imports = "";
      gconf_mr_vinit = "init";
      gconf_stat = false;
      gconf_stat_all = false;
      gconf_stat_tree = false;
      gconf_optim_config_file = None;
      gconf_emit_optim_config = false;
      gconf_optim_config = [];
      gconf_prefix = ""; }
  in
  (* Specialize apply/iter for this given gconf *)
  let apply = apply gconf in
  let iter_array = iter_array gconf in
  (* Source/Target *)
  apply QcertArg.set_qname j##.qname;
  apply QcertArg.set_source j##.source;
  apply QcertArg.set_target j##.target;
  (* Compilation path *)
  iter_array QcertArg.add_path j##.path;
  Js.Optdef.iter j##.exactpath (fun b -> gconf.gconf_exact_path <- Js.to_bool b);
  (* Target directory -- XXX is that used? XXX *)
  apply QcertArg.set_dir j##.dir;
  apply QcertArg.set_dir j##.dirtarget;
  (* I/O *)
  apply QcertArg.set_schema_content j##.schema;
  apply QcertArg.set_input_content j##.input;
  (* Cloudant options *)
  Js.Optdef.iter j##.jsruntime (fun b -> if b then QcertArg.set_link_js_runtime gconf ());
  Js.Optdef.iter j##.cld_prefix
    (fun s -> QcertArg.set_prefix gconf (Js.to_string s));
  (* Emit options *)
  Js.Optdef.iter j##.emitall (fun b -> gconf.gconf_emit_all <- Js.to_bool b);
  Js.Optdef.iter j##.emitsexp (fun b -> gconf.gconf_emit_sexp <- Js.to_bool b);
  Js.Optdef.iter j##.emitsexpall (fun b -> gconf.gconf_emit_sexp_all <- Js.to_bool b);
  (* Eval options *)
  Js.Optdef.iter j##.eval (fun b -> gconf.gconf_eval <- Js.to_bool b);
  Js.Optdef.iter j##.evalall (fun b -> gconf.gconf_eval_all <- Js.to_bool b);
  (* Source options *)
  Js.Optdef.iter j##.sourcesexp (fun b -> gconf.gconf_source_sexp <- Js.to_bool b);
  (* Pretty-printing options *)
  Js.Optdef.iter j##.ascii
    (fun b -> if Js.to_bool b then
      PrettyCommon.set_ascii gconf.gconf_pretty_config ()
    else
      PrettyCommon.set_greek gconf.gconf_pretty_config ());
  Js.Optdef.iter j##.margin
    (fun num ->
      let n = int_of_float (Js.float_of_number num) in
      PrettyCommon.set_margin gconf.gconf_pretty_config n);
  (* Java options *)
  apply QcertArg.set_java_imports j##.javaimports;
  (* NNRCMR options *)
  apply QcertArg.set_vinit j##.vinit;
  (* Optimization configuration *)
  apply (fun gconf o -> QcertArg.set_optims gconf (optim_config_from_json o)) j##.optims;
  (* Return configuration after applying self-consistency constraints *)
  complete_configuration gconf

let wrap_all wrap_f l =
  let a = new%js Js.array_empty in
  List.iter (fun x -> ignore (a##push (wrap_f x))) l;
  a

let json_of_result res =
  let wrap x =
      object%js
        val file = Js.string x.QcertCore.res_file
        val lang = Js.string x.QcertCore.res_lang
        val value = Js.string x.QcertCore.res_content
      end
  in
  object%js
    val emit = Js.def (wrap res.QcertCore.res_emit)
    val emitall = Js.def (wrap_all wrap res.QcertCore.res_emit_all)
    val emitsexp = Js.def (wrap res.QcertCore.res_emit_sexp)
    val emitsexpall = Js.def (wrap_all wrap res.QcertCore.res_emit_sexp_all)
    val result = Js.string res.QcertCore.res_emit.QcertCore.res_content
    val eval = Js.string res.QcertCore.res_eval.QcertCore.res_content
  end

let json_of_error msg =
  object%js
    val emit = Js.undefined
    val emitall = Js.undefined
    val emitsexp = Js.undefined
    val emitsexpall = Js.undefined
    val result = Js.string msg
    val eval = Js.string msg
  end

let json_of_exported_languages exported_languages =
  let wrap x =
    let ((((_,id),_),lab),desc) = x in
    object%js
      val langid = Js.string (Util.string_of_char_list id)
      val label = Js.string (Util.string_of_char_list lab)
      val description = Js.string (Util.string_of_char_list desc)
    end
  in
  object%js
    val frontend = Js.def (wrap_all wrap exported_languages.QcertCompiler.frontend)
    val core = Js.def (wrap_all wrap exported_languages.QcertCompiler.coreend)
    val distributed = Js.def (wrap_all wrap exported_languages.QcertCompiler.distrend)
    val backend =  Js.def (wrap_all wrap exported_languages.QcertCompiler.backend)
  end
let language_specs () =
  let exported_languages = QLang.export_language_descriptions  in
  json_of_exported_languages exported_languages

let json_of_source_to_target_path j =
  let source_lang = language_of_name (Js.to_string j##.source) in
  let target_lang = language_of_name (Js.to_string j##.target) in
  let path_lang = QDriver.get_path_from_source_target source_lang target_lang in
  let path = List.map name_of_language path_lang in
  let wrap x = Js.string x in
  object%js
    val path = Js.def (wrap_all wrap path)
  end

let rec unsafe_json_to_js (j:QData.json) =
  match j with
  | QcertCompiler.Jnull -> Js.Unsafe.inject (Js.null)
  | QcertCompiler.Jnumber n -> Js.Unsafe.inject (Js.number_of_float n)
  | QcertCompiler.Jbool b -> Js.Unsafe.inject (Js.bool b)
  | QcertCompiler.Jstring str -> Js.Unsafe.inject (Js.string (Util.string_of_char_list str))
  | QcertCompiler.Jarray a -> Js.Unsafe.inject (wrap_all unsafe_json_to_js a)
  | QcertCompiler.Jobject l ->
     Js.Unsafe.inject (Js.Unsafe.obj (Array.of_list (List.map (fun (str,y) -> ((Util.string_of_char_list str, unsafe_json_to_js y))) l)))
  

let json_of_optim_list() = unsafe_json_to_js QDriver.optim_config_list_to_json

let json_of_optim_default() =
  object%js
    val optims = unsafe_json_to_js QDriver.optim_config_default_to_json
  end
  
let qcert_compile input =
  begin try
    let gconf =
      begin try
	global_config_of_json input
      with exn ->
        raise (Qcert_Error ("[Couldn't load configuration: "^(Printexc.to_string exn)^"]"))
      end
    in
    let q_s =
      begin try
       Js.to_string input##.query
      with exn ->
        raise (Qcert_Error ("[Couldn't load query: "^(Printexc.to_string exn)^"]"))
      end
    in
    let res =
      begin try
        QcertCore.main gconf ("Query.string", q_s)
      with Qcert_Error err -> raise (Qcert_Error ("[Compilation error: "^err^"]"))
      | exn -> raise (Qcert_Error ("[Compilation error: "^(Printexc.to_string exn)^"]"))
      end
    in
    json_of_result res
  with
  | Qcert_Error msg -> json_of_error msg
  | exn -> json_of_error ("[Main error: "^(Printexc.to_string exn)^"]")
  end

let _ =
  Js.export "Qcert" (object%js
    val languages = Js.wrap_callback language_specs;
    val languagesPath = Js.wrap_callback json_of_source_to_target_path
    val optimList = Js.wrap_callback json_of_optim_list
    val optimDefaults = Js.wrap_callback json_of_optim_default
    val compile = Js.wrap_callback qcert_compile
   end)
