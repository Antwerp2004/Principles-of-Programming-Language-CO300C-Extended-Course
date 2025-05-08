boolean(true).
boolean(false).
is_any_builtin(Name) :- is_builtin(Name, _).


% Program structure: [GlobalVars, FuncsProcs, Statements]
reduce_prog([Var, FuncsProcs, Body]) :-
		create_env(Var, env([[]], [], false), GlobalVarEnv),
		process_func_proc_decls(FuncsProcs, GlobalVarEnv, FinalEnv),
		reduce_stmt(config(Body, FinalEnv), _).


% check if X has been declared in a specific scope
% has_declared(Name, ScopeList, Declaration)
has_declared(Name, [id(Name, Kind, Type, Value)|_], id(Name, Kind, Type, Value)) :- !.
has_declared(Name, [_|Rest], Declaration) :- has_declared(Name, Rest, Declaration).

% check if X has been declared in a list of func/proc declarations
find_func_proc_decl(Name, [id(Name, Kind, Type, params_body(Params, Body))|_], id(Name, Kind, Type, params_body(Params, Body))) :- member(Kind, [func, proc]), !.
find_func_proc_decl(Name, [_|Rest], Declaration) :- find_func_proc_decl(Name, Rest, Declaration).


% create_env for Global Declarations
create_env([], Env, Env).
% Var check
create_env([var(Name, Type)|Rest], env([GlobalScope|OtherScopes], FuncProcs, LoopState), FinalEnv) :-
    (is_any_builtin(Name) -> throw(redeclare_identifier(var(Name, Type))) ; true),
    % Check if Name is already in current scope (GlobalScope)
    (has_declared(Name, GlobalScope, _) -> throw(redeclare_identifier(var(Name, Type))) ; true),
    % Add variable to scope with initial 'undef' value
    create_env(Rest, env([[id(Name, var, Type, undef)|GlobalScope]|OtherScopes], FuncProcs, LoopState), FinalEnv).

% Const check
create_env([const(Name, Expr)|Rest], env([GlobalScope|OtherScopes], FuncProcs, LoopState), FinalEnv) :-
    (is_any_builtin(Name) -> throw(redeclare_identifier(const(Name, Expr))) ; true),
    (has_declared(Name, GlobalScope, _) -> throw(redeclare_identifier(const(Name, Expr))) ; true),
    % Evaluate constant expression
    (eval_literal(Expr, Value, Type) -> true ; throw(invalid_expression(Expr))),
    % Add constant to scope with its value
    create_env(Rest, env([[id(Name, const, Type, Value)|GlobalScope]|OtherScopes], FuncProcs, LoopState), FinalEnv).


% process_func_proc_decls(DeclarationList, EnvIn, EnvOut)
process_func_proc_decls([], Env, Env) :- !. % Base case: empty list
% Process a function declaration
process_func_proc_decls([func(Name, Params, ReturnType, Body)|Rest], env(Scopes, CurrentFuncProcs, LoopState), FinalEnv) :-
	atom(Name),
	(is_any_builtin(Name) -> throw(redeclare_function(Name)) ; true),
	% Check redeclaration against global variables/constants (only need to check the global scope, which is the last one in the list after reversing)
	(last(Scopes, GlobalScope) ->
		((has_declared(Name, GlobalScope, id(_, VarKind, _, _)), member(VarKind, [var, const])) -> throw(redeclare_function(Name)) ; true) % Check global vars/consts
	; true
	),
	% Check redeclaration against already processed functions/procedures
    (find_func_proc_decl(Name, CurrentFuncProcs, _) -> throw(redeclare_function(Name)) ; true),
	% Check redeclaration within parameters
	check_param_redeclaration(Params, Name), % Name needed for error message
	% Add function declaration to the list
    process_func_proc_decls(Rest, env(Scopes, [id(Name, func, ReturnType, params_body(Params, Body))|CurrentFuncProcs], LoopState), FinalEnv).

% Process a procedure declaration
process_func_proc_decls([proc(Name, Params, Body)|Rest], env(Scopes, CurrentFuncProcs, LoopState), FinalEnv) :-
    atom(Name),
    % Check redeclaration against built-ins
    (is_any_builtin(Name) -> throw(redeclare_procedure(Name)) ; true),
    % Check redeclaration against global variables/constants
	(last(Scopes, GlobalScope) ->
		((has_declared(Name, GlobalScope, id(_, VarKind, _, _)), member(VarKind, [var, const])) -> throw(redeclare_procedure(Name)) ; true)
	; true
	),
    % Check redeclaration against already processed functions/procedures
    (find_func_proc_decl(Name, CurrentFuncProcs, _) -> throw(redeclare_procedure(Name)) ; true),
	% Check redeclaration within parameters
	check_param_redeclaration(Params, Name), % Name needed for error message
    % Add procedure declaration to the list
    process_func_proc_decls(Rest, env(Scopes, [id(Name, proc, void, params_body(Params, Body))|CurrentFuncProcs], LoopState), FinalEnv).


% Helper to convert par terms to id terms for has_declared check
% Convert [par(a,int), par(b,float)] to [id(a,par,int,undef), id(b,par,float,undef)]
pars_to_ids([], []).
pars_to_ids([par(Name, Type)|Rest], [id(Name, par, Type, undef)|IdsRest]) :-
    pars_to_ids(Rest, IdsRest).

% Corrected check_param_redeclaration using pars_to_ids
check_param_redeclaration(Params, _) :-
    check_params_acc(Params, []).

check_params_acc([], _).
check_params_acc([par(Name, Type)|RestParams], SeenParams) :-
    % Check if the current Name exists in the rest of the list
    (member(id(Name, _, _), SeenParams) ->
        throw(redeclare_identifier(par(Name, Type)))
    ;
		check_params_acc(RestParams, [id(Name, par, Type)|SeenParams])
    ).


% Evaluate literals and determine type
eval_literal(I, I, integer) :- integer(I), !.
eval_literal(F, F, float) :- float(F), !.
eval_literal(true, true, boolean) :- !.
eval_literal(false, false, boolean) :- !.
eval_literal(S, S, string) :- string(S), !.


% Identifier Lookup: lookup_id(Name, Env, Declaration)
lookup_id(Name, env([CurrentScope|OtherScopes], _, LoopState), Declaration) :-
    (has_declared(Name, CurrentScope, Declaration) -> true % Found in current scope, stop searching
	;
    	% Not in current scope, try outer scopes
		OtherScopes \== [],
    	lookup_id(Name, env(OtherScopes, _, LoopState), Declaration)
	),
	!.

% lookup_name(Name, Env, Declaration)
lookup_name(Name, env(Scopes, _, _), Declaration) :-
% Try looking up in scopes first (for vars, consts, pars)
	lookup_id(Name, env(Scopes, _, false), Declaration),
	!.
lookup_name(Name, env(_, FuncProcs, _), Declaration) :-
% If not found in scopes, look in the global func/proc list
	find_func_proc_decl(Name, FuncProcs, Declaration).


% Expression Evaluation (reduce, reduce_all)
% Base Cases for reduce_all
reduce_all(config(V,Env),config(V,Env)):- number(V), !.
reduce_all(config(V,Env),config(V,Env)):- member(V, [true, false]), !. % For boolean
reduce_all(config(V,Env),config(V,Env)):- string(V), !.
% Recursive case
reduce_all(config(E, Env), config(E2, Env)):-
	reduce(config(E, Env), config(E1, Env)), !,
	reduce_all(config(E1, Env), config(E2, Env)).
% If reduce/2 fails to make progress -> invalid expression
reduce_all(config(E, _), _) :-
    throw(invalid_expression(E)).


% Reduce the atom Name to Value or throws an error.
reduce_atom(Name, Env, Value) :-
    atom(Name),
    (lookup_name(Name, Env, id(_, Kind, _, FoundValue)) ->
        % Case 1: Identifier is DECLARED in the current environment Env
        (   member(Kind, [var, const, par]) ->
            % Case 1a: Declared as var/const/par
            (   FoundValue \== undef ->
                % Found and initialized
                Value = FoundValue
            ;
                % Found but uninitialized (var/par)
                throw(invalid_expression(Name))
            )
        ;
            % Case 1b: Declared, but NOT var/const/par (must be func/proc)
            throw(invalid_expression(Name))
        )
    ;
        % Case 2: Identifier is NOT DECLARED in Env (lookup_id failed)
        (   is_any_builtin(Name) ->
            % Case 2a: A built-in name (used directly as expression)
            throw(invalid_expression(Name))
        ;
            % Case 2b: Neither declared NOR built-in
            throw(undeclare_identifier(Name))
        )
    ),
    !.


% reduce for Numerical Operators
get_type(V, integer) :- integer(V).
get_type(V, float) :- float(V).
get_type(V, boolean) :- boolean(V).
get_type(V, string) :- string(V).


promote_types(V1, Type1, V2, Type2, PromotedV1, PromotedType, PromotedV2) :-
	(Type1 == float ; Type2 == float) -> (
        PromotedType = float,
        (Type1 == integer -> PromotedV1 is float(V1) ; PromotedV1 = V1),
		(Type2 == integer -> PromotedV2 is float(V2) ; PromotedV2 = V2)
    ) ; ( % Both integer
		PromotedType = integer,
		PromotedV1 = V1,
		PromotedV2 = V2
	).

% Type check for numeric operations
check_numeric(Type1, Type2, Expr) :-
    (member(Type1, [integer, float]), member(Type2, [integer, float])) -> true ;
    throw(type_mismatch(Expr)).

check_boolean(Value, Expr) :-
	(member(Value, [true, false]) -> true ; throw(type_mismatch(Expr))).

check_equality(Type1, Type2, Expr) :-
	(Type1 == Type2, member(Type1, [integer, boolean]) -> true ;
	throw(type_mismatch(Expr))).

eval_binary_numeric_operands(Expr, E1, E2, Env, V1, Type1, V2, Type2) :-
	reduce_all(config(E1, Env), config(V1, Env)), % Evaluate first operand
	reduce_all(config(E2, Env), config(V2, Env)), % Evaluate second operand
	get_type(V1, Type1),
	get_type(V2, Type2),
	check_numeric(Type1, Type2, Expr).

% Rule for Identifiers (Atoms) - Delegate to reduce_atom
reduce(config(Atom, Env), config(Value, Env)) :-
    atom(Atom),
    reduce_atom(Atom, Env, Value),
    !.


% Addition
reduce(config(add(E1, E2), Env), config(Result, Env)) :-  
	eval_binary_numeric_operands(add(E1, E2), E1, E2, Env, V1, Type1, V2, Type2),
	promote_types(V1, Type1, V2, Type2, PV1, _, PV2),
	Result is PV1 + PV2,
	!.

% Binary Subtraction
reduce(config(sub(E1, E2), Env), config(Result, Env)) :-  
	eval_binary_numeric_operands(sub(E1, E2), E1, E2, Env, V1, Type1, V2, Type2),
	promote_types(V1, Type1, V2, Type2, PV1, _, PV2),
	Result is PV1 - PV2,
	!.

% Multiplication
reduce(config(times(E1, E2), Env), config(Result, Env)) :-  
	eval_binary_numeric_operands(times(E1, E2), E1, E2, Env, V1, Type1, V2, Type2),
	promote_types(V1, Type1, V2, Type2, PV1, _, PV2),
	Result is PV1 * PV2,
	!.

% Division
reduce(config(rdiv(E1, E2), Env), config(Result, Env)) :-  
	eval_binary_numeric_operands(rdiv(E1, E2), E1, E2, Env, V1, Type1, V2, Type2),
	promote_types(V1, Type1, V2, Type2, PV1, _, PV2),
	(PV2 =:= 0 -> % Use =:= for numeric equality comparison
		throw(invalid_expression(rdiv(E1, E2))) % Check divide for 0
	;
	(Type1 == integer, Type2 == integer) ->
		Result is PV1 // PV2
	; 
		Result is PV1 / PV2 % Float division otherwise
	),
	!.

% Integer Division
reduce(config(idiv(E1, E2), Env), config(Result, Env)) :-
	reduce_all(config(E1, Env), config(V1, Env)),
	reduce_all(config(E2, Env), config(V2, Env)),
	(integer(V1), integer(V2) ->
		(V2 =:= 0 -> % Division by zero check
			throw(invalid_expression(idiv(E1, E2))) ;
			Result is V1 // V2
		)
	;
		throw(type_mismatch(idiv(E1, E2)))
	),
	!.

% Modulo
reduce(config(imod(E1, E2), Env), config(Result, Env)) :-
	reduce_all(config(E1, Env), config(V1, Env)),
	reduce_all(config(E2, Env), config(V2, Env)),
	(integer(V1), integer(V2) ->
		(V2 =:= 0 -> % Modulo zero check
			throw(invalid_expression(imod(E1, E2))) ;
			Result is V1 mod V2
		)
	;
		throw(type_mismatch(imod(E1, E2)))
	),
	!.

% Unary Subtraction
reduce(config(sub(E1), Env), config(Result, Env)) :-
    reduce_all(config(E1, Env), config(V1, Env)),
    get_type(V1, Type1),
    (member(Type1, [integer, float]) ->
        Result is -V1
    ;
        throw(type_mismatch(sub(E1)))
    ),
    !.

% Logical NOT
reduce(config(bnot(E), Env), config(Result, Env)) :-
    reduce_all(config(E, Env), config(V, Env)),
    check_boolean(V, bnot(E)), % Ensure operand is boolean
    (V == true -> Result = false ; Result = true),
    !.

% Logical AND (short-circuit)
reduce(config(band(E1, E2), Env), config(Result, Env)) :-
    reduce_all(config(E1, Env), config(V1, Env)),
    check_boolean(V1, band(E1, E2)), % Ensure E1 is boolean
    (   V1 == false -> Result = false % Short-circuit
    ;   % V1 is true, evaluate E2
        reduce_all(config(E2, Env), config(V2, Env)),
        check_boolean(V2, band(E1, E2)), % Ensure E2 is boolean
        Result = V2 % Result is V2's value (which must be true here)
    ),
    !.

% Logical OR (short-circuit)
reduce(config(bor(E1, E2), Env), config(Result, Env)) :-
    reduce_all(config(E1, Env), config(V1, Env)),
    check_boolean(V1, bor(E1, E2)), % Ensure E1 is boolean
    (   V1 == true -> Result = true % Short-circuit
    ;   % V1 is false, evaluate E2
        reduce_all(config(E2, Env), config(V2, Env)),
        check_boolean(V2, bor(E1, E2)), % Ensure E2 is boolean
        Result = V2 % Result is V2's value
    ),
    !.

% Relational Operators (>, >=, <, <=)
reduce(config(greater(E1, E2), Env), config(Result, Env)) :-
    eval_binary_numeric_operands(greater(E1, E2), E1, E2, Env, V1, Type1, V2, Type2),
    promote_types(V1, Type1, V2, Type2, PV1, _, PV2),
    (PV1 > PV2 -> Result = true ; Result = false),
    !.

reduce(config(ge(E1, E2), Env), config(Result, Env)) :-
    eval_binary_numeric_operands(ge(E1, E2), E1, E2, Env, V1, Type1, V2, Type2),
    promote_types(V1, Type1, V2, Type2, PV1, _, PV2),
    (PV1 >= PV2 -> Result = true ; Result = false),
    !.

reduce(config(less(E1, E2), Env), config(Result, Env)) :-
    eval_binary_numeric_operands(less(E1, E2), E1, E2, Env, V1, Type1, V2, Type2),
    promote_types(V1, Type1, V2, Type2, PV1, _, PV2),
    (PV1 < PV2 -> Result = true ; Result = false),
    !.

reduce(config(le(E1, E2), Env), config(Result, Env)) :-
    eval_binary_numeric_operands(le(E1, E2), E1, E2, Env, V1, Type1, V2, Type2),
    promote_types(V1, Type1, V2, Type2, PV1, _, PV2),
    (PV1 =< PV2 -> Result = true ; Result = false), % Note: =< for less than or equal
    !.

% Equality Operators (==, !=)
reduce(config(eql(E1, E2), Env), config(Result, Env)) :-
    reduce_all(config(E1, Env), config(V1, Env)),
    reduce_all(config(E2, Env), config(V2, Env)),
    get_type(V1, Type1),
    get_type(V2, Type2),
    check_equality(Type1, Type2, eql(E1, E2)), % Check types are same AND int/boolean
    (V1 == V2 -> Result = true ; Result = false), % Use Prolog's term equality
    !.

reduce(config(ne(E1, E2), Env), config(Result, Env)) :-
    reduce_all(config(E1, Env), config(V1, Env)),
    reduce_all(config(E2, Env), config(V2, Env)),
    get_type(V1, Type1),
    get_type(V2, Type2),
    check_equality(Type1, Type2, ne(E1, E2)), % Check types are same AND int/boolean
    (V1 \== V2 -> Result = true ; Result = false), % Use Prolog's term inequality
    !.


% Function Call Expression
reduce(config(call(Name, ArgsExprList), EnvIn), config(ResultValue, EnvIn)) :-
    atom(Name),
    EnvIn = env(OuterScopes, FuncProcs, _),

    % Case 1: Built-in Function Call (readInt, readReal, readBool)
    (   is_builtin(Name, func) ->
        length(ArgsExprList, ActualArity),
        builtin_arity(Name, ExpectedArity),
        (ActualArity == ExpectedArity -> true ; throw(wrong_number_of_argument(call(Name, ArgsExprList)))),
        % Built-in funcs have no args to evaluate
        p_call_builtin(Name, ResultValue) % Call built-in, ResultValue gets unified

    % Case 2: User-defined Function Call
    ;   find_func_proc_decl(Name, FuncProcs, id(Name, func, ReturnType, params_body(Params, Body))) ->
        % Found user-defined function declaration
        length(ArgsExprList, ActualArity),
        length(Params, ExpectedArity),
        (ActualArity == ExpectedArity -> true ; throw(wrong_number_of_argument(call(Name, ArgsExprList)))),
        reduce_args(ArgsExprList, EnvIn, ArgValues),
        % Create parameter scope (checks arg types against param types)
        map_args_params(ArgValues, Params, ParamIdList, call(Name, ArgsExprList)),
        % Add the function's own name as a variable for return value assignment
        ParamScope = [id(Name, var, ReturnType, undef)|ParamIdList], % Store as 'var' for assign
        % Create environment for function body execution
        FuncEnv = env([ParamScope|OuterScopes], FuncProcs, false), % LoopState starts as false
        % Execute function body
        reduce_stmt(config(Body, FuncEnv), config(_, EnvAfterBody)),
        % Retrieve the final parameter scope
        EnvAfterBody = env([FinalParamScope|_], _, _),
        % Look up the return value (assigned to function name's var entry)
        (   has_declared(Name, FinalParamScope, id(_, var, _, ReturnValue)) ->
            % Check if return value was assigned
            (   ReturnValue \== undef -> ResultValue = ReturnValue
            ;   throw(invalid_expression(call(Name, ArgsExprList))) % Function finished without assigning return value
            ),
            % Check if return value type matches function's declared return type
            get_type(ReturnValue, ActualReturnType),
            ActualReturnType == ReturnType -> true
            % ResultValue is the value found
        ;
            throw(type_mismatch(call(Name, ArgsExprList))) % Failed to find return value slot
        )

    % Case 3: Not a built-in function or declared function
    ;   % Could be undeclared, or a procedure/var/const called as function
        ( is_builtin(Name, proc) ; find_func_proc_decl(Name, FuncProcs, id(_, proc, _, _)) ) ->
            throw(type_mismatch(call(Name, ArgsExprList))) % Calling a procedure as a function
        ; lookup_name(Name, EnvIn, id(_, Kind, _, _)), member(Kind, [var, const, par]) ->
            throw(type_mismatch(call(Name, ArgsExprList))) % Calling var/const/par as function
        ;   % Otherwise, truly undeclared
            throw(undeclare_function(call(Name, ArgsExprList))
        )
    ),
    !. % Cut for the entire call/2 rule


% --- Helpers for Function Calls ---

% map_args_params(ArgValues, ParamDecls, ParamScopeList)
% Creates the list of id(ParamName, par, ParamType, ArgValue) terms for the parameter scope.
map_args_params([], [], [], _). % Base case
map_args_params([ArgValue|ArgRest], [par(ParamName, ParamType)|ParamRest], [id(ParamName, par, ParamType, ArgValue)|ScopeRest], CallFunc) :-
    get_type(ArgValue, ArgType),
    % Strict type check for argument passing
    (ArgType == ParamType -> true ; throw(type_mismatch(CallFunc))),
    map_args_params(ArgRest, ParamRest, ScopeRest, CallFunc).


% Statement Execution
% Status can be 'normal', 'break', 'continue', 'return(Value)'

% Base case: Empty stmt list
reduce_stmt(config([], Env), config(normal, Env)).
% Recursive case
reduce_stmt(config([S|Ss], Env), config(ResultStatus, FinalEnv)) :-
	% Head stmt
	reduce_stmt(config(S, Env), config(S_Status, IntermediateEnv)),
	% If head exec normally then exec the rest
	( S_Status == normal ->
        reduce_stmt(config(Ss, IntermediateEnv), config(ResultStatus, FinalEnv))
	;
		member(S_Status, [break, continue]) ->
		% Otherwise, stop processing the list and propagate status
		ResultStatus = S_Status,
		FinalEnv = IntermediateEnv % Stop processing
	).


% Call statement
reduce_stmt(config(call(Name, ArgsExprList), Env), config(normal, Env)) :-
	execute_procedure_call(call(Name, ArgsExprList), Env, normal, Env).


% Assignment Statement
reduce_stmt(config(assign(Name, Expr), EnvIn), config(normal, EnvOut)) :-
    atom(Name),
    (lookup_name(Name, EnvIn, id(_, Kind, _, _)) ->
        (member(Kind, [var, par]) ->
            % Evaluate RHS expression
            reduce_all(config(Expr, EnvIn), config(Value, EnvIn)),
            % Update environment (checks type, assignability internally)
            % update_env will fail if Name not found in SCOPES (lookup_name might find func/proc)
            update_env(Name, Value, EnvIn, EnvOut)
        ; Kind == const ->
            throw(cannot_assign(assign(Name, Expr)))
        ; % Found, but it's func/proc
            throw(undeclare_identifier(Name))
        )
    ;   % lookup_name failed
        throw(undeclare_identifier(Name))
    ).


% Block Statement
reduce_stmt(config(block(LocalDecls, Stmts), EnvIn), config(BlockStatus, FinalOuterEnv)) :-
    EnvIn = env(OuterScopes, FuncProcs, LoopState),
    % 1. Push new scope
    EnvWithNewScope = env([[]|OuterScopes], FuncProcs, LoopState),
    % 2. Process local declarations into the new scope
    process_local_decls(LocalDecls, EnvWithNewScope, EnvAfterDecls),
    % 3. Execute statements in the block's environment
    reduce_stmt(config(Stmts, EnvAfterDecls), config(BlockStatus, EnvAfterBlockExec)),
    % 4. Pop the local scope
    EnvAfterBlockExec = env([_|FinalOuterScopes], _, _), % Deconstruct to get outer scopes *after* execution
    FinalOuterEnv = env(FinalOuterScopes, FuncProcs, LoopState). % Reconstruct final env without the block's scope


% If Statement (with else)
reduce_stmt(config(if(CondExpr, ThenStmt, ElseStmt), EnvIn), config(FinalStatus, FinalEnv)) :-
    % Condition
    reduce_all(config(CondExpr, EnvIn), config(CondValue, EnvIn)),
    % Check condition type
    (member(CondValue, [true, false]) -> true ; throw(type_mismatch(if(CondExpr, ThenStmt, ElseStmt)))),
    % Branch based on value
    (   CondValue == true ->
        % Execute Then branch
        reduce_stmt(config(ThenStmt, EnvIn), config(FinalStatus, FinalEnv))
    ;   % CondValue == false -> Execute Else branch
        reduce_stmt(config(ElseStmt, EnvIn), config(FinalStatus, FinalEnv))
    ).

% If Statement (without else)
reduce_stmt(config(if(CondExpr, ThenStmt), EnvIn), config(FinalStatus, FinalEnv)) :-
    % Evaluate condition
    reduce_all(config(CondExpr, EnvIn), config(CondValue, EnvIn)),
    % Check condition type
    (member(CondValue, [true, false]) -> true ; throw(type_mismatch(if(CondExpr, ThenStmt)))),
    % Branch based on value
    (   CondValue == true ->
        % Execute Then branch
        reduce_stmt(config(ThenStmt, EnvIn), config(FinalStatus, FinalEnv))
    ;   % CondValue == false -> Do nothing
        FinalStatus = normal,
        FinalEnv = EnvIn
    ).


% Break Statement
reduce_stmt(config(break(null), Env), config(break, Env)) :-
	Env = env(_, _, LoopState),
	(LoopState == true -> true ; throw(break_not_in_loop(break(null)))).


% Continue Statement
reduce_stmt(config(continue(null), Env), config(continue, Env)) :-
    Env = env(_, _, LoopState), % Extract LoopState
    (LoopState == true -> true ; throw(continue_not_in_loop(continue(null)))).


% Loop Statement
% While Statement
reduce_stmt(config(while(CondExpr, BodyStmt), EnvIn), config(normal, FinalEnv)) :-
    reduce_while(CondExpr, BodyStmt, EnvIn, FinalEnv).

% Do-While Statement
reduce_stmt(config(do(BodyStmts, CondExpr), EnvIn), config(normal, FinalEnv)) :-
    % Evaluate condition FIRST only to check its type
    reduce_all(config(CondExpr, EnvIn), config(InitialCondValue, _)),
    check_boolean(InitialCondValue, do(BodyStmts, CondExpr)),
    % If type check passed, proceed to execute body at least once
    reduce_do(BodyStmts, CondExpr, EnvIn, FinalEnv).

% Loop N Times Statement
reduce_stmt(config(loop(CountExpr, BodyStmt), EnvIn), config(normal, FinalEnv)) :-
    % Evaluate count expression ONCE
    reduce_all(config(CountExpr, EnvIn), config(N, _)),
    (integer(N) -> true ; throw(type_mismatch(loop(CountExpr, BodyStmt)))),
    % Check if N > 0 before starting loop helper
    ( N =< 0 -> FinalEnv = EnvIn % If N <= 0, loop does nothing
    ; reduce_loop_n(N, BodyStmt, EnvIn, FinalEnv) % Call helper if N > 0
    ).


% --- Helper for Call Statement ---

% Evaluate a list of expressions (arguments)
% reduce_args(ExpressionList, Env, ValueList)
reduce_args([], _, []).
reduce_args([E|Es], Env, [V|Vs]) :-
	reduce_all(config(E, Env), config(V, Env)),
	reduce_args(Es, Env, Vs).


% Expected number of arguments for built-in procedures
builtin_arity(writeInt, 1).
builtin_arity(writeIntLn, 1).
builtin_arity(readInt, 0). % Function
builtin_arity(writeReal, 1).
builtin_arity(writeRealLn, 1).
builtin_arity(readReal, 0). % Function
builtin_arity(writeBool, 1).
builtin_arity(writeBoolLn, 1).
builtin_arity(readBool, 0). % Function
builtin_arity(writeStr, 1).
builtin_arity(writeStrLn, 1).
builtin_arity(writeLn, 0).


% Handles built-in procedures and checks arity/undeclared/invalid types
execute_procedure_call(call(Name, ArgsExprList), EnvIn, normal, FinalEnv) :-
	atom(Name),
    EnvIn = env(OuterScopes, FuncProcs, LoopState),
    CallTerm = call(Name, ArgsExprList),
	(lookup_name(Name, EnvIn, Decl) -> % Use lookup_name
        % Case 1: Name is Declared
		(Decl = id(Name, Kind, _, DeclInfo) ->
			(Kind == proc ->
				% Case 1a: User-defined Procedure Call
				DeclInfo = params_body(Params, Body),
                length(ArgsExprList, ActualArity),
                length(Params, ExpectedArity),
                (ActualArity == ExpectedArity -> true; throw(wrong_number_of_argument(CallTerm))),
                % Evaluate Arguments
				reduce_args(ArgsExprList, EnvIn, ArgValues),
				% Create Parameter Scope (checks types)
				map_args_params(ArgValues, Params, ParamScope, CallTerm),
                ProcEnv = env([ParamScope|OuterScopes], FuncProcs, false),
                % Execute Procedure Body
				reduce_stmt(config(Body, ProcEnv), config(_, EnvAfterProc)),
                % Pop Procedure Scope and return modified Outer Scopes
				EnvAfterProc = env([_|FinalOuterScopes], _, _),
				FinalEnv = env(FinalOuterScopes, FuncProcs, LoopState)
			; Kind == func ->
				% Case 1b: Declared as a user-defined function
				throw(invalid_expression(CallTerm))
			; % Kind is var, const, or par
                throw(invalid_expression(CallTerm))
			)
		;
            throw(invalid_expression(call(Name, ArgsExprList)))
        )
	;
		% Name is NOT declared (could be built-in or truly undeclared)
		(is_builtin(Name, proc) ->
			% Case 2a: It's a built-in procedure
			% Check number of arguments
			length(ArgsExprList, ActualArity),
			builtin_arity(Name, ExpectedArity),
			(ActualArity == ExpectedArity ->
				% Arity matches, evaluate arguments
				reduce_args(ArgsExprList, EnvIn, ArgValues),
				p_call_builtin(Name, ArgValues)
			;
				% Wrong number of arguments
				throw(wrong_number_of_argument(CallTerm))
			),
            FinalEnv = EnvIn
		; is_builtin(Name, func) ->
			% Case 2b: A built-in function -> Cannot call function as procedure
			throw(invalid_expression(CallTerm))
		;
			% Case 5: Neither declared nor built-in -> Undeclared Procedure
			throw(undeclare_procedure(CallTerm))
		)
	),
	!.


% --- Helper for Assignment Statement ---

% update_scope(Name, NewValue, DeclaredType, ScopeIn, ScopeOut)
% Base case: Not found in this scope (empty list)
update_scope(_, _, [], []) :- !, fail.

update_scope(Name, NewValue, [id(Name, Kind, DeclaredType, _)|Rest], [id(Name, Kind, DeclaredType, NewValue)|Rest]) :-
    !, % Found Name, commit to this clause
    % Check assignability
    (Kind == const -> throw(cannot_assign(assign(Name, NewValue))) ; true),
    (member(Kind, [var, par]) -> % Must be var or par to be assignable
		get_type(NewValue, ValueType), % Get type of the value being assigned
		(ValueType == DeclaredType -> true ; throw(type_mismatch(assign(Name, NewValue))))
	;
		% Trying to assign to something not var/par
		throw(undeclare_identifier(Name))
    ).

update_scope(Name, NewValue, [OtherDecl|RestIn], [OtherDecl|RestOut]) :-
    % Name NOT matched in head, recurse on tail
    update_scope(Name, NewValue, RestIn, RestOut).


% update_env(Name, NewValue, EnvIn, EnvOut)
% Finds Name in EnvIn scope stack and returns EnvOut with its value updated.
% Fails if Name is not found in any scope.
update_env(Name, NewValue, env([CurrentScope|OuterScopes], FuncProcs, LoopState), env([NewCurrentScope|OuterScopes], FuncProcs, LoopState)) :-
    update_scope(Name, NewValue, CurrentScope, NewCurrentScope), % Try updating innermost scope using /4
    !. % Found and updated in CurrentScope, cut

update_env(Name, NewValue, env([Scope|OuterScopes], FuncProcs, LoopState), env([Scope|NewOuterScopes], FuncProcs, LoopState)) :-
    OuterScopes \== [], % Make sure there are outer scopes to check
    update_env(Name, NewValue, env(OuterScopes, FuncProcs, LoopState), env(NewOuterScopes, FuncProcs, LoopState)). % Recurse on outer scopes using /4


% --- Helper for Block Statement ---

% process_local_decls(DeclarationList, EnvIn, EnvOut)
% Processes declarations in L1 of block(L1,L2), adds them to the innermost scope.
process_local_decls([], Env, Env).

process_local_decls([var(Name, Type)|RestDecls], env([CurrentScope|OuterScopes], FuncProcs, LoopState), FinalEnv) :-
    (is_any_builtin(Name) -> throw(redeclare_identifier(var(Name, Type))) ; true),
    % Check for redeclaration in the current scope
    (has_declared(Name, CurrentScope, _) -> throw(redeclare_identifier(var(Name, Type))) ; true),
    % Add to current scope and recurse
    process_local_decls(RestDecls, env([[id(Name, var, Type, undef)|CurrentScope]|OuterScopes], FuncProcs, LoopState), FinalEnv).

process_local_decls([const(Name, Expr)|RestDecls], env([CurrentScope|OuterScopes], FuncProcs, LoopState), FinalEnv) :-
    (is_any_builtin(Name) -> throw(redeclare_identifier(const(Name, Expr))) ; true),
    % Check for redeclaration in the current scope
    (has_declared(Name, CurrentScope, _) -> throw(redeclare_identifier(const(Name, Expr))) ; true),
    % Evaluate constant
    (eval_literal(Expr, Value, Type) -> true ; throw(invalid_expression(Expr))),
    % Add to current scope and recurse
    process_local_decls(RestDecls, env([[id(Name, const, Type, Value)|CurrentScope]|OuterScopes], FuncProcs, LoopState), FinalEnv).


% --- Helpers for Loops ---

% Set LoopState in environment
set_loop_state(env(Scopes, FuncProcs, _), NewLoopState, env(Scopes, FuncProcs, NewLoopState)).

% Helper for While loop
reduce_while(CondExpr, BodyStmt, EnvIn, FinalEnv) :-
    set_loop_state(EnvIn, true, EnvLoop),
    % Evaluate condition
    reduce_all(config(CondExpr, EnvLoop), config(CondValue, _)),
    check_boolean(CondValue, while(CondExpr, BodyStmt)),
    (   CondValue == true ->
        % Execute body
        reduce_stmt(config(BodyStmt, EnvLoop), config(BodyStatus, EnvAfterBody)),
        % Handle body status
        (   member(BodyStatus, [normal, continue]) -> % Continue looping
            reduce_while(CondExpr, BodyStmt, EnvAfterBody, FinalEnv) % Loop with env after body exec
        ;   BodyStatus == break ->
            FinalEnv = EnvAfterBody
        )
    ;   % Condition is false, loop terminates
        FinalEnv = EnvIn
    ).

% Helper for Do-While loop
reduce_do(BodyStmts, CondExpr, EnvIn, FinalEnv) :-
    set_loop_state(EnvIn, true, EnvLoop), % Environment with LoopState=true for body/condition
    % Execute body first
    reduce_stmt(config(BodyStmts, EnvLoop), config(BodyStatus, EnvAfterBody)),
    % Handle body status
    (   BodyStatus == break ->
        FinalEnv = EnvAfterBody
    ;   BodyStatus == continue ->
        reduce_do_cond(BodyStmts, CondExpr, EnvAfterBody, FinalEnv)
    ;   BodyStatus == normal ->
        reduce_do_cond(BodyStmts, CondExpr, EnvAfterBody, FinalEnv)
    ).

% Helper for Do-While loop condition check (called after body execution)
reduce_do_cond(BodyStmts, CondExpr, CurrentEnv, FinalEnv) :-
    set_loop_state(CurrentEnv, true, EnvLoop),
    % Evaluate condition
    reduce_all(config(CondExpr, EnvLoop), config(CondValue, _)),
    check_boolean(CondValue, do(BodyStmts, CondExpr)),
    (   CondValue == true ->
        reduce_do(BodyStmts, CondExpr, CurrentEnv, FinalEnv)
    ;   % Condition false, loop terminates
        FinalEnv = CurrentEnv
    ).


% Helper for Loop N times statement
reduce_loop_n(0, _, EnvIn, EnvIn) :- !. % Base case: Counter is 0, loop done
reduce_loop_n(N, BodyStmt, EnvIn, FinalEnv) :-
    N > 0,
    set_loop_state(EnvIn, true, EnvLoop), % Environment with LoopState=true for body
    % Execute body
    reduce_stmt(config(BodyStmt, EnvLoop), config(BodyStatus, EnvAfterBody)),
    (   BodyStatus == break ->
        FinalEnv = EnvAfterBody
    ;   member(BodyStatus, [normal, continue]) -> % Continue to next iteration
        N1 is N - 1,
        reduce_loop_n(N1, BodyStmt, EnvAfterBody, FinalEnv) % Recurse with decremented counter and env after body
    ).