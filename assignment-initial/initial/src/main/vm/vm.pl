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
has_declared_func_proc(Name, [id(Name, Kind, _, _)|_]) :- member(Kind, [func, proc]), !.
has_declared_func_proc(Name, [_|Rest]) :- has_declared_func_proc(Name, Rest).


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
process_func_proc_decls([], Env, Env). % Base case: empty list
% Process a function declaration
process_func_proc_decls([func(Name, Params, ReturnType, Body)|Rest], env(Scopes, CurrentFuncProcs, LoopState), FinalEnv) :-
	atom(Name),
	(is_any_builtin(Name) -> throw(redeclare_function(Name)) ; true),
	% Check redeclaration against global variables/constants (only need to check the global scope, which is the last one in the list after reversing)
	env(GlobalScopeList, _, _ ) = env(Scopes, _, _),
	(GlobalScopeList = [LastGlobalScope] ->
		(has_declared(Name, LastGlobalScope, id(_, VarKind, _, _)), member(VarKind, [var, const])) -> throw(redeclare_function(Name)) ; true % Check global vars/consts
	; true % If scopes are more complex (shouldn't be at global level initially), assume no conflict there for now
	),
	% Check redeclaration against already processed functions/procedures
    (has_declared_func_proc(Name, CurrentFuncProcs) -> throw(redeclare_function(Name)) ; true),
	% Check redeclaration within parameters
	check_param_redeclaration(Params, Name), % Name needed for error message
	% Add function declaration to the list
    process_func_proc_decls(Rest, env(Scopes, [id(Name, func, ReturnType, {Params, Body})|CurrentFuncProcs], LoopState), FinalEnv).

% Process a procedure declaration
process_func_proc_decls([proc(Name, Params, Body)|Rest], env(Scopes, CurrentFuncProcs, LoopState), FinalEnv) :-
    atom(Name),
    % Check redeclaration against built-ins
    (is_any_builtin(Name) -> throw(redeclare_procedure(Name)) ; true),
    % Check redeclaration against global variables/constants
	env(GlobalScopeList, _, _ ) = env(Scopes, _, _),
	(GlobalScopeList = [LastGlobalScope] ->
		(has_declared(Name, LastGlobalScope, id(_, VarKind, _, _)), member(VarKind, [var, const])) -> throw(redeclare_procedure(Name)) ; true
	; true
	),
    % Check redeclaration against already processed functions/procedures
    (has_declared_func_proc(Name, CurrentFuncProcs) -> throw(redeclare_procedure(Name)) ; true),
	% Check redeclaration within parameters
	check_param_redeclaration(Params, Name), % Name needed for error message
    % Add procedure declaration to the list
    process_func_proc_decls(Rest, env(Scopes, [id(Name, proc, void, {Params, Body})|CurrentFuncProcs], LoopState), FinalEnv).


% Check for redeclaration WITHIN a parameter list
% check_param_redeclaration(ParameterList, FuncProcName)
check_param_redeclaration([], _). % Base case: empty list
check_param_redeclaration([par(Name, Type)|Rest], FuncProcName) :-
    atom(Name),
    % Check if Name appears in the rest of the list
    (has_declared(Name, [par(PName, PType) :> id(PName, par, PType, undef) || (par(PName, PType))] , _) ->
        throw(redeclare_identifier(par(Name, Type))) % Redeclared parameter
    ;
        % Continue checking the rest of the list
        check_param_redeclaration(Rest, FuncProcName)
    ).

% Helper to convert par terms to id terms for has_declared check
% Convert [par(a,int), par(b,float)] to [id(a,par,int,undef), id(b,par,float,undef)]
pars_to_ids([], []).
pars_to_ids([par(Name, Type)|Rest], [id(Name, par, Type, undef)|IdsRest]) :-
    pars_to_ids(Rest, IdsRest).

% Corrected check_param_redeclaration using pars_to_ids
check_param_redeclaration(Params, _) :-
    pars_to_ids(Params, ParamIds),
    check_param_redeclaration_ids(ParamIds).

check_param_redeclaration_ids([]).
check_param_redeclaration_ids([id(Name, Kind, Type, Value)|RestIds]) :-
    % Check if the current Name exists in the rest of the list
    (has_declared(Name, RestIds, _) ->
        throw(redeclare_identifier(id(Name, Kind, Type, Value))) % Or throw redeclare_identifier(par(Name,Type)) if you want the original syntax in error
    ;
        % Continue checking the rest of the list
        check_param_redeclaration_ids(RestIds)
    ).


% Evaluate literals and determine type
eval_literal(I, I, integer) :- integer(I), !.
eval_literal(F, F, float) :- float(F), !.
eval_literal(true, true, boolean) :- !.
eval_literal(false, false, boolean) :- !.
eval_literal(S, S, string) :- string(S), !.


% Identifier Lookup: lookup_id(Name, Env, Declaration)
lookup_id(Name, env([CurrentScope|OtherScopes], LoopState), Declaration) :-
    (has_declared(Name, CurrentScope, Declaration) ->
		true % Found in current scope, stop searching
	;
    	% Not in current scope, try outer scopes
		OtherScopes \== [],
    	lookup_id(Name, env(OtherScopes, LoopState), Declaration)
	),
	!.


% Expression Evaluation (reduce, reduce_all)
% Base Cases for reduce_all
reduce_all(config(V,Env),config(V,Env)):- number(V), !.
reduce_all(config(V,Env),config(V,Env)):- boolean(V), !. % For boolean
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
    (lookup_id(Name, Env, id(_, Kind, _, FoundValue)) ->
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

% Rule for Identifiers (Atoms) - Delegate to reduce_atom
reduce(config(Atom, Env), config(Value, Env)) :-
    atom(Atom),
    reduce_atom(Atom, Env, Value),
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


eval_binary_numeric_operands(Expr, E1, E2, Env, V1, Type1, V2, Type2) :-
	reduce_all(config(E1, Env), config(V1, Env)), % Evaluate first operand
	reduce_all(config(E2, Env), config(V2, Env)), % Evaluate second operand
	get_type(V1, Type1), % Get type of first operand
	get_type(V2, Type2), % Get type of second operand
	check_numeric(Type1, Type2, Expr).

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
		% Otherwise, stop processing the list and propagate status
		ResultStatus = S_Status,
		FinalEnv = IntermediateEnv % Stop processing
	).

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
execute_procedure_call(call(Name, ArgsExprList), Env, normal, Env) :-
	atom(Name),
	(is_builtin(Name, proc) ->
		% Check number of arguments
		length(ArgsExprList, ActualArity),
		builtin_arity(Name, ExpectedArity),
		(ActualArity == ExpectedArity ->
			reduce_args(ArgsExprList, Env, ArgValues),
			p_call_builtin(Name, ArgValues) % p_call_builtin handles arg type checks and throws type_mismatch
		;
			throw(wrong_number_of_argument(call(Name, ArgsExprList)))
		)
	;
		% Not a built-in procedure
		% TODO: Add lookup for user-defined procedures in FuncsProcs list here later
		% For now, check if it's a built-in function or just undeclared/wrong kind
		(is_builtin(Name, func) ->
			% Calling a built-in function as a procedure
			throw(invalid_expression(call(Name, ArgsExprList)))
		;
			% Not a built-in function or procedure (implies undeclared or wrong kind like var/const used as callee)
			(lookup_id(Name, Env, id(_, Kind, _, _)), member(Kind, [var, const, par]) ->
				throw(invalid_expression(call(Name, ArgsExprList))) % Cannot call var/const/par
			;
				throw(undeclare_procedure(call(Name, ArgsExprList))) % Undeclared or declared but not func/proc
			)
		)
	),
	!.


% reduce_stmt for call statement
% Check if Name is a built-in procedure
reduce_stmt(config(call(Name, ArgsExprList), Env), config(normal, Env)) :-
	execute_procedure_call(call(Name, ArgsExprList), Env, normal, Env).