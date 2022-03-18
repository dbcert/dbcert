open Sql_compiler

(** Putting everything together **)

let dash72 = "------------------------------------------------------------------------\n"
let exports = "module.exports = { query };\n"

let output = ref None
let input_files = ref []
let runtime = ref ""
let verbose = ref false
let optim = ref false

let usage =
  "DBCert - SQL to JavaScript compiler\n"^
  "Usage: "^Sys.argv.(0)^" [-output <file>] query"

let args_list =
  Arg.align
    [ ("-output", Arg.String (fun s -> output := Some s),
       "<file> Generated JavaScript File");
      ("-link", Arg.Unit (fun () -> runtime := Js_runtime.runtime),
       "Link the JavaScript runtime");
      ("-optim", Arg.Unit (fun () -> optim := true),
       "With optimizations");
      ("-verbose", Arg.Unit (fun () -> verbose := true),
       "Verbose output");
    ]

let anon_args input_files f = input_files := f :: !input_files

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
              if !verbose then (
                match Sql_query_to_js.sql_query_to_nra schemaJS qSQLCoqJS with
                  | Some ((qalg, qNRA), qNRAopt) ->
                     (Printf.printf "Compilation to SQLAlg succeeded: %a\n\n" pp_query qalg;
                      Printf.printf "Compilation to NRAe succeeded: %a\n\n" pp_nra qNRA;
                      Printf.printf "The optimized NRAe query is: %a\n\n" pp_nra qNRAopt)
                  | None -> Printf.printf "Compilation to NRAe failed"
              );
              let qJS = Sql_query_to_js.sql_query_to_js schemaJS !optim qSQLCoqJS in (* XXX optimization on *)
              (match qJS with
              | Sql_query_to_js.Success q ->
                  let content = !runtime ^ q ^ exports in
                  if !verbose then Printf.printf "Corresponding JS query: %s\n" content;
                  let base =
                    begin match !output with
                    | None -> Filename.remove_extension fileSQL
                    | Some file -> Filename.remove_extension file
                    end
                  in
                  let postfix = if !index > 0 then "_" ^ (string_of_int !index) else "" in
                  let output_file = base ^ postfix ^ ".js" in
                  Printf.printf "Corresponding JS query generated in: %s\n" output_file;
                  Util.make_file output_file content
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
    | Sql_lexer.Error msg -> failwith ("Lexing error: "^msg)
    | Sql_parser.Error -> failwith ("At offset "^(string_of_int (Lexing.lexeme_start bufSQL))^": syntax error.\n")
    | Sql_lexer.Eof -> Printf.printf "Compilation to JavaScript finished\n";
    end;
    close_in chanSQL

(* Current test:
   ./compiler test.sql
 *)
