(* COMP 360H Project 1:  an interpreter for an imperative language.
 *
 * Kevin Angulo, Valery Corral, Vicky Gong
 *)

 module E = Ast.Expression
 module S = Ast.Stm
 module P = Ast.Program
 
 (* 'a IdentMap.t:  the type of maps from identifiers to 'a.
  *)
 module IdentMap = Map.Make(Ast.Id)
 
 (* MultipleDeclaration x is raised when x is declared more than once in a
  * block.
  *)
 exception MultipleDeclaration of Ast.Id.t
 
 (* UnboundVariable x is raised when x is used but not declared.
  *)
 exception UnboundVariable of Ast.Id.t
 
 (* UndefinedFunction f is raised when f is called but has not been defined.
  *)
 exception UndefinedFunction of Ast.Id.t
 
 (* TypeError s is raised when an operator or function is applied to operands
  * of the incorrect type.  s is any (hopefuly useful) message.
  *)
 exception TypeError of string
 
 exception ReturnFrameInvdec
 
 (* Values.
  *)
 module Value = struct
   type t = 
     | V_Undefined
     | V_None
     | V_Int of int
     | V_Bool of bool
     | V_Str of string
     [@@deriving show]
 
   (* to_string v = a string representation of v (more human-readable than
    * `show`.
    *)
   let to_string (v : t) : string =
     match v with
     | V_Undefined -> "?"
     | V_None -> "None"
     | V_Int n -> Int.to_string n
     | V_Bool b -> Bool.to_string b
     | V_Str s -> s
 end
 
 (* An implementation of the I/O API.  This is a little bit complex, because
  * this one implementation allows for a few variations:
  * - The input and output channel can be set by the client (default to
  *   standard input and output).
  * - The display of prompts (for the prompt_* functions) can be turned off
  *   (default on).
  * These variations let us use this module for interactive use (use the
  * defaults) and testing (redirect the i/o channels to a programmatic stream
  * and turn off the display of prompts.
  *
  * A client makes changes to the defaults by setting `in_channel`,
  * `out_channel`, and `show_prompts`.
  *)
 
 
 module Api = struct
 
   (* Raised when a function is invoked that is not in the API.
    *)
   exception ApiError of string
 
   (* in_channel:  input channel (for get_*, prompt_* ).
    *)
   let in_channel : Scanf.Scanning.in_channel ref = 
     ref Scanf.Scanning.stdin
 
   (* out_channel:  output channel (for print_*, prompt_* when prompts are
    * displayed).
    *)
   let out_channel : Out_channel.t ref = ref Out_channel.stdout
 
   (* show_prompts:  true to display prompts, false to not display.
    *)
   let show_prompts : bool ref = ref true
 
   (* output oc s:  output `s` to `oc` and flush `oc`.
    *)
   let output (oc : Out_channel.t) (s : string) : unit =
     Out_channel.output_string oc s ;
     Out_channel.flush oc
 
   (* outputnl oc s = output `s ^ '\n'` to `oc` and flush `oc`.
    *)
   let outputnl (oc : Out_channel.t) (s : string) : unit =
     output oc (s ^ "\n")
 
   (* The API definition.  The API is specified by a
    * (string*(Value.t->Value.t)) list.  Each element names an API function
    * and provides the code to be executed when the function is called.
    *)
   let api : (Value.t list -> Value.t) IdentMap.t =
     [
       ("print_bool", fun vs ->
         match vs with
         | [Value.V_Bool n] -> 
           outputnl (!out_channel) (Bool.to_string n) ; Value.V_None
         | _ -> raise @@ TypeError "Bad argument type for print_bool"
       )
     ; ("get_bool", fun vs ->
         match vs with
         | [] -> Value.V_Bool (Scanf.bscanf !in_channel " %B" (fun b -> b))
         | _ -> raise @@ TypeError "Bad argument type for get_bool"
       )
     ; ("prompt_bool", fun vs ->
         match vs with
         | [Value.V_Str s] ->
           if !show_prompts then output (!out_channel) s else () ;
             Value.V_Bool (Scanf.bscanf !in_channel " %B" (fun b -> b))
         | _ -> raise @@ TypeError "Bad argument type for prompt_bool"
       )
     ; ("print_int", fun vs ->
         match vs with
         | [Value.V_Int n] -> 
           outputnl (!out_channel) (Int.to_string n) ; Value.V_None
         | _ -> raise @@ TypeError "Bad argument type for print_int"
       )
     ; ("get_int", fun vs ->
         match vs with
         | [] -> Value.V_Int (Scanf.bscanf !in_channel " %d" (fun n -> n))
         | _ -> raise @@ TypeError "Bad argument type for get_int"
       )
     ; ("prompt_int", fun vs ->
         match vs with
         | [Value.V_Str s] ->
           if !show_prompts then output (!out_channel) s else () ;
             Value.V_Int (Scanf.bscanf !in_channel " %d" (fun n -> n))
         | _ -> raise @@ TypeError "Bad argument type for prompt_int"
       )
     ; ("print_str", fun vs ->
          match vs with
          | [Value.V_Str s] -> 
            outputnl (!out_channel) s ; Value.V_None
          | _ -> raise @@ TypeError "Bad argument type for print_s"
       )
     ; ("get_str", fun vs ->
         match vs with
         | [] -> Value.V_Str (Scanf.bscanf !in_channel "%s" (fun s -> s))
         | _ -> raise @@ TypeError "Bad argument type for get_str"
       )
     ; ("prompt_str", fun vs ->
         match vs with
         | [Value.V_Str s] ->
           if !show_prompts then output (!out_channel) s else () ;
             Value.V_Str (Scanf.bscanf !in_channel " %s" (fun s -> s))
         | _ -> raise @@ TypeError "Bad argument type for prompt_str"
       )
     ] |> List.to_seq |> IdentMap.of_seq
 
   (* do_call f vs invokes the API function corresponding to `f` with argument
    * list `vs`.
    *
    * Raises ApiError f: if f is not an API function.
    *)
   let do_call (f : string) (vs : Value.t list) : Value.t =
     try
       IdentMap.find f api vs
     with
     | Not_found -> raise @@ ApiError f
 
 end
 
module Frame = struct
  type env = (Ast.Id.t * Value.t) list
  type t = 
    | Env of env list
    | Return of Value.t 
  (* vdec declares a variable in a given frame with a given value *)
  let vdec (frame : t) (x : Ast.Id.t) (v : Value.t) : t =
    match frame with
    (* goes through the list of Env declaring vars *)
    | Env [] -> Env [ [(x, v)] ]
    | Env (env :: rest) ->
        if List.mem_assoc x env then
            raise (MultipleDeclaration x)
        else
            Env (( (x, v) :: env) :: rest)
    | _ -> failwith "Frame.vdec applied to a non-environment frame"
  (* vlookup looks up x in a given frame. Frame is a list of Env therefore we have to iterate through the list *)
  let rec vlookup (frame : t) (x : Ast.Id.t) : Value.t =
    match frame with
    | Env [] -> raise (UnboundVariable x)
    | Env (env :: rest) ->
        begin
          try List.assoc x env
          with Not_found -> vlookup (Env rest) x
        end
    | _ -> failwith "Frame.vlookup applied to a non-environment frame"
  (* vupdate maps the new value to the variable already in the list *)
  let rec vupdate (frame: t) (x: Ast.Id.t) (v: Value.t): t =
    match frame with
    | Env [] -> raise (UnboundVariable x)  (* If the environment is empty, the variable is unbound *)
    | Env (env :: rest) ->
      if List.mem_assoc x env then
        (* If the variable is found in the current environment, update its value *)
        Env ((List.map (fun (key, value) -> if key = x then (key, v) else (key, value)) env) :: rest)
      else
        (* If the variable is not found in the current environment, try updating in the outer environment *)
        let updated_rest = match vupdate (Env rest) x v with
          | Env updated_envs -> updated_envs
          | _ -> rest  (* Keep the outer environments as they were if update was not possible *)
        in
        Env (env :: updated_rest)
  | Return _ -> frame  

  let return (frame : t) (v : Value.t) : t =
    match frame with
    | Env _ -> Return v 
    | _ -> failwith "Frame.return applied to a non-environment frame"
  let new_env (frame: t): t = 
    match frame with
    | Env envs -> Env ([] :: envs)  (* Add a new, empty environment on top *)
    | _ -> failwith "new_env applied to a non-environment frame"

  let discard_env (frame: t): t = 
    match frame with
    | Env (_ :: rest) -> Env rest  (* Remove the top environment, returning to the previous one *)
    | _ -> failwith "discard_env applied to a non-environment frame or empty frame"

  let extract_return_value (frame: t): Value.t option =
    match frame with
    | Return v -> Some v  (*Returns a the value given there is one *)
    | _ -> None  (* or appropriate handling for non-return frames *)

end
 
 
 (* expressions *)
let binop (op : E.binop) (v : Value.t) (v' : Value.t) : Value.t =
   match (op, v, v') with
   | (E.Plus, Value.V_Int n, Value.V_Int n') -> Value.V_Int (n + n')
   | (E.Minus, Value.V_Int n, Value.V_Int n') -> Value.V_Int (n - n')
   | (E.Times, Value.V_Int n, Value.V_Int n') -> Value.V_Int (n * n')
   | (E.Div, Value.V_Int n, Value.V_Int n') -> Value.V_Int (n / n')
   | (E.Mod, Value.V_Int n, Value.V_Int n') -> Value.V_Int (n mod n')    (* Mod operator for integers *)
   | (E.And, Value.V_Bool b, Value.V_Bool b') -> Value.V_Bool (b && b')  (* Logical AND for booleans *)
   | (E.Or, Value.V_Bool b, Value.V_Bool b') -> Value.V_Bool (b || b')   (* Logical OR for booleans, corrected *)
   | (E.Eq, Value.V_Int n, Value.V_Int n') -> Value.V_Bool (n = n')      (* Equality check for integers *)
   | (E.Ne, Value.V_Int n, Value.V_Int n') -> Value.V_Bool (n <> n')     (* Inequality check for integers *)
   | (E.Lt, Value.V_Int n, Value.V_Int n') -> Value.V_Bool (n < n')      (* Less than for integers *)
   | (E.Le, Value.V_Int n, Value.V_Int n') -> Value.V_Bool (n <= n')     (* Less than or equal for integers *)
   | (E.Gt, Value.V_Int n, Value.V_Int n') -> Value.V_Bool (n > n')      (* Greater than for integers *)
   | (E.Ge, Value.V_Int n, Value.V_Int n') -> Value.V_Bool (n >= n')     (* Greater than or equal for integers *)
   | _ -> raise (TypeError "Invalid operands or operator for binary operation")  (* Handling invalid cases *)
 
 
 (* statments *)
 let rec eval (frame : Frame.t) (e : E.t)(p : Ast.Program.t) : Value.t * Frame.t =
   match e with
   | E.Var x -> (Frame.vlookup frame x, frame)
   | E.Num n -> (Value.V_Int n, frame)
   | E.Bool b -> (Value.V_Bool b, frame)
   | E.Str s -> (Value.V_Str s, frame)
   | E.Binop (op, e1, e2) ->
       let v1, frame1 = eval frame e1 p in
       let v2, frame2 = eval frame1 e2 p in
       (binop op v1 v2, frame2)
       (* asigns a variable a value *)
   | E.Assign (x, e) ->
       let v, frame' = eval frame e p in
       (v, Frame.vupdate frame' x v)
   | E.Not e -> (* not of a value  *)
       let v, frame' = eval frame e p in
       (match v with
        | Value.V_Bool b -> (Value.V_Bool (not b), frame')
        | _ -> failwith "TypeError: Not operation requires a boolean")
   | E.Neg e ->
       let v, frame' = eval frame e p in
       (match v with
        | Value.V_Int n -> (Value.V_Int (-n), frame')
        | _ -> failwith "TypeError: Neg operation requires an integer")

    | E.Call (f_name, args) ->
      begin
        try
        (* check the api function and sees if the function arguments is in there *)
          let api_function = Api.do_call f_name (List.map (fun arg -> let value, _ = eval frame arg p in value) args) in
          (api_function, frame)
        with
        | Api.ApiError _ ->
        (* if the function is not in the Api it runs the program iterating through all the functiond until it finds the one it is looking for *)
          match p with
          | Ast.Program.Pgm fundefs ->
              let fun_opt = List.find_opt (fun (Ast.Program.FunDef (name, _, _)) -> name = f_name) fundefs in (*finds name of the function* *)
              begin
                match fun_opt with
                | Some(Ast.Program.FunDef (_, param_names, body)) ->
                    let evaluated_args = List.map (fun arg -> fst (eval frame arg p)) args in
                    let new_frame = Frame.new_env frame in  (* Create a new environment frame *)
                    let new_frame_with_params = List.fold_left2 (fun fr param arg_val -> Frame.vdec fr param arg_val) new_frame param_names evaluated_args in
                    let final_frame = exec_stmList body new_frame_with_params p in  (* Execute the function body *)
                    (match Frame.extract_return_value final_frame with
                    | Some v -> (v, frame)  (* Return the value from the function, keeping the original frame intact *)
                    | None -> (Value.V_None, frame))  (* In case there is no return value *)
                | None -> raise (UndefinedFunction f_name)
              end
      end
        
      
    
    
        
   
 (* Evaluate a single statement *)
and exec_stm (stm : Ast.Stm.t) (frame : Frame.t) (p : Ast.Program.t) : Frame.t =
  match stm with
    | S.Skip -> 
        frame  (* Do nothing and return the current frame *)
    | S.VarDec decls ->
        List.fold_left (fun fr (x, opt_e) ->
          match opt_e with
          | None -> Frame.vdec fr x Value.V_Undefined  (* Declare uninitialized variable *)
          | Some e ->
              let v, fr' = eval fr e p in  (* Evaluate the initialization expression *)
              Frame.vdec fr' x v  (* Update the frame with the new variable value *)
        ) frame decls
    | S.Expr e ->
        let _, frame' = eval frame e p in  (* Evaluate the expression, but only use the updated frame *)
        frame'
    | S.Block stms ->
      let new_frame = Frame.new_env frame in  (* Create a new frame/environment for the block *)
      let final_frame = exec_stmList stms new_frame p in  (* Execute the block with the new frame *)
      Frame.discard_env final_frame  (* Discard the new frame after the block and return the updated original frame *)
    (* Execute a list of statements *)
    | S.If (e, s1, s2) ->
        let v, frame' = eval frame e p in  (* Evaluate the condition *)
        (match v with
        | Value.V_Bool true -> exec_stm s1 frame' p  (* Execute the 'then' branch *)
        | Value.V_Bool false -> exec_stm s2 frame' p  (* Execute the 'else' branch *)
        | _ -> failwith "TypeError: If condition is not boolean"
        )
    | S.While (e, s) ->
        let rec loop fr =
          let v, fr' = eval fr e p in  (* Evaluate the condition within the loop *)
          match v with
          | Value.V_Bool true -> loop (exec_stm s fr' p)  (* Continue the loop if condition is true *)
          | Value.V_Bool false -> fr'  (* Exit the loop if condition is false *)
          | _ -> failwith "TypeError: While condition is not boolean"
        in loop frame
    | S.Return opt_e ->
      let v = match opt_e with
        | None -> Value.V_None
        | Some e -> fst (eval frame e p)
      in
      Frame.return frame v
 
   (* Evaluate a list of statements *)
 and exec_stmList (stms : Ast.Stm.t list) (frame : Frame.t) (p : Ast.Program.t) : Frame.t =
     List.fold_left (fun fr s -> exec_stm s fr p) frame stms
 
 
 (* exec p :  execute the program p according to the operational semantics
  * provided as a handout.
  *)
 let exec (p : Ast.Program.t) : unit =
   let initial_frame = Frame.Env [] in
     ignore (eval initial_frame (E.Call("main", [])) p);
     