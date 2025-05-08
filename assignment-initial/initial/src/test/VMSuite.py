import unittest
from TestUtils import TestVM


class VMSuite(unittest.TestCase):

    # Test 00: Simple arithmetic with nesting
    def test_00(self):
        input = """[[],[],[call(writeInt,[add(times(2,3),sub(10,5))])]]."""
        expect = "11" # (2*3) + (10-5) = 6 + 5 = 11
        self.assertTrue(TestVM.test(input, expect, 0))

    # Test 01: Mixed integer and float arithmetic, requires writeReal
    def test_01(self):
        input = """[[],[],[call(writeReal,[add(5,2.5)])]]."""
        expect = "7.5" # 5.0 + 2.5 = 7.5 (float promotion)
        self.assertTrue(TestVM.test(input, expect, 1))

    # Test 02: Float arithmetic
    def test_02(self):
        input = """[[],[],[call(writeReal,[times(10.0,sub(5.5,1.2))])]]."""
        expect = "43.0" # 10.0 * (5.5 - 1.2) = 10.0 * 4.3 = 43.0
        self.assertTrue(TestVM.test(input, expect, 2))

    # Test 03: Integer division (idiv)
    def test_03(self):
        input = """[[],[],[call(writeInt,[idiv(20,3)])]]."""
        expect = "6" # 20 div 3 = 6
        self.assertTrue(TestVM.test(input, expect, 3))

    # Test 04: Modulo (imod)
    def test_04(self):
        input = """[[],[],[call(writeInt,[imod(20,3)])]]."""
        expect = "2" # 20 mod 3 = 2
        self.assertTrue(TestVM.test(input, expect, 4))

    # Test 05: Integer division by zero error
    def test_05(self):
        input = """[[],[],[call(writeInt,[idiv(10,0)])]]."""
        expect = "Invalid expression: idiv(10,0)"
        self.assertTrue(TestVM.test(input, expect, 5))

    # Test 06: Float division by zero error (rdiv)
    def test_06(self):
        input = """[[],[],[call(writeReal,[rdiv(10.0,0)])]]."""
        expect = "Invalid expression: 10.0 rdiv 0"
        self.assertTrue(TestVM.test(input, expect, 6))

    # Test 07: Unary minus on float, requires writeReal
    def test_07(self):
        input = """[[],[],[call(writeReal,[sub(15.99)])]]."""
        expect = "-15.99"
        self.assertTrue(TestVM.test(input, expect, 7))

    # Test 08: Using a global constant (integer)
    def test_08(self):
        input = """[[const(x,100)],[],[call(writeInt,[add(x,50)])]]."""
        expect = "150" # 100 + 50 = 150
        self.assertTrue(TestVM.test(input, expect, 8))

    # Test 09: Using a global constant (float), requires writeReal
    def test_09(self):
        input = """[[const(pi,3.14)],[],[call(writeReal,[times(pi,2)])]]."""
        expect = "6.28" # 3.14 * 2.0 = 6.28
        self.assertTrue(TestVM.test(input, expect, 9))

    # Test 10: Using a global constant (boolean), requires writeBool
    def test_10(self):
        input = """[[const(t,true)],[],[call(writeBool,[t])]]."""
        expect = "true"
        self.assertTrue(TestVM.test(input, expect, 10))

    # Test 11: Undeclared identifier in expression error
    def test_11(self):
        input = """[[],[],[call(writeInt,[add(a,10)])]]."""
        expect = "Undeclared identifier: a"
        self.assertTrue(TestVM.test(input, expect, 11))

    # Test 12: Type mismatch in arithmetic operation (int + bool) error
    def test_12(self):
        input = """[[],[],[call(writeInt,[add(1,true)])]]."""
        expect = "Type mismatch: add(1,true)"
        self.assertTrue(TestVM.test(input, expect, 12))

    # Test 13: Wrong number of arguments for built-in procedure error
    def test_13(self):
        input = """[[],[],[call(writeInt,[1,2])]].""" # writeInt expects 1 argument
        expect = "Wrong number of arguments: call(writeInt,[1,2])"
        self.assertTrue(TestVM.test(input, expect, 13))

    # Test 14: Type mismatch for built-in procedure argument (writeInt expects integer) error
    def test_14(self):
        input = """[[],[],[call(writeInt,[3.14])]].""" # writeInt expects integer
        expect = "Type mismatch: call(writeInt,[3.14])"
        self.assertTrue(TestVM.test(input, expect, 14))


    # --- Tests for Phase 1: Declarations and Lookup ---

    # Test 15: Redeclared function name vs global variable
    def test_15(self):
        input = """[[var(a,integer)],[func(a,[],integer,[])],[]]."""
        expect = "Redeclared function: a"
        self.assertTrue(TestVM.test(input, expect, 15))

    # Test 16: Redeclared procedure name vs global constant
    def test_16(self):
        input = """[[const(b,true)],[proc(b,[],[])],[]]."""
        expect = "Redeclared procedure: b"
        self.assertTrue(TestVM.test(input, expect, 16))

    # Test 17: Redeclared function name vs built-in function name
    def test_17(self):
        input = """[[],[func(readInt,[],integer,[])],[]]."""
        expect = "Redeclared function: readInt"
        self.assertTrue(TestVM.test(input, expect, 17))

    # Test 18: Redeclared procedure name vs built-in procedure name
    def test_18(self):
        input = """[[],[proc(writeInt,[],[])],[]]."""
        expect = "Redeclared procedure: writeInt"
        self.assertTrue(TestVM.test(input, expect, 18))

    # Test 19: Redeclared function name vs previously declared function
    def test_19(self):
        input = """[[],[func(foo,[],integer,[]), func(foo,[],float,[])],[]]."""
        expect = "Redeclared function: foo"
        self.assertTrue(TestVM.test(input, expect, 19))

    # Test 20: Redeclared procedure name vs previously declared procedure
    def test_20(self):
        input = """[[],[proc(bar,[],[]), proc(bar,[],[])],[]]."""
        expect = "Redeclared procedure: bar"
        self.assertTrue(TestVM.test(input, expect, 20))

    # Test 21: Redeclared procedure name vs previously declared function (same name)
    def test_21(self):
        input = """[[],[func(baz,[],integer,[]), proc(baz,[],[])],[]]."""
        expect = "Redeclared procedure: baz" # proc declared second, redefines func
        self.assertTrue(TestVM.test(input, expect, 21))

    # Test 22: Redeclared parameter within a function's parameter list
    def test_22(self):
        input = """[[],[func(testFunc,[par(x,integer),par(y,float),par(x,real)],integer,[])],[]]."""
        expect = "Redeclared identifier: par(x,real)"
        self.assertTrue(TestVM.test(input, expect, 22))

    # Test 23: Redeclared parameter within a procedure's parameter list
    def test_23(self):
        input = """[[],[proc(testProc,[par(a,boolean),par(b,string),par(a,integer)],[])],[]]."""
        expect = "Redeclared identifier: par(a,integer)"
        self.assertTrue(TestVM.test(input, expect, 23))

    # Test 24: Using a declared function name directly as an expression (invalid context)
    def test_24(self):
        input = """[[],[func(myFunc,[],integer,[])],[call(writeInt,[myFunc])]]."""
        expect = "Invalid expression: myFunc"
        self.assertTrue(TestVM.test(input, expect, 24))

    # Test 25: Using a declared procedure name directly as an expression (invalid context)
    def test_25(self):
        input = """[[],[proc(myProc,[],[])],[call(writeInt,[myProc])]]."""
        expect = "Invalid expression: myProc"
        self.assertTrue(TestVM.test(input, expect, 25))

    # Test 26: Calling a global variable as if it were a procedure
    def test_26(self):
        input = """[[var(myVar,integer)],[],[call(myVar,[])]]."""
        expect = "Invalid expression: call(myVar,[])"
        self.assertTrue(TestVM.test(input, expect, 26))

    # Test 27: Calling a global constant as if it were a procedure
    def test_27(self):
        input = """[[const(myConst,true)],[],[call(myConst,[])]]."""
        expect = "Invalid expression: call(myConst,[])"
        self.assertTrue(TestVM.test(input, expect, 27))

    # Test 28: Calling a built-in function as if it were a procedure
    def test_28(self):
        input = """[[],[],[call(readInt,[1])]].""" # readInt is a function, not a procedure
        expect = "Invalid expression: call(readInt,[1])"
        self.assertTrue(TestVM.test(input, expect, 28))

    # Test 29: Calling a truly undeclared name as a procedure
    def test_29(self):
        input = """[[],[],[call(nonExistentProc,[])]]."""
        expect = "Undeclared procedure: call(nonExistentProc,[])"
        self.assertTrue(TestVM.test(input, expect, 29))


    # --- Tests for Phase 2: Assign, Block, If ---

    # Test 30: Simple assignment to global variable and printing it
    def test_30(self):
        input = """[[var(x,integer)],[],[assign(x,100), call(writeInt,[x])]]."""
        expect = "100"
        self.assertTrue(TestVM.test(input, expect, 30))

    # Test 31: Type mismatch on assignment (strict types required) - float to int var
    def test_31(self):
        input = """[[var(i,integer)],[],[assign(i, 5.5)]]."""
        expect = "Type mismatch: assign(i,5.5)"
        self.assertTrue(TestVM.test(input, expect, 31))

    # Test 32: Assign result of an expression to a variable
    def test_32(self):
        input = """[[var(res,integer)],[],[assign(res, add(times(5,3), idiv(10,2))), call(writeIntLn,[res])]]."""
        expect = "20\n" # (5*3) + (10 div 2) = 15 + 5 = 20
        self.assertTrue(TestVM.test(input, expect, 32))

    # Test 33: Error - Assigning to a constant
    def test_33(self):
        input = """[[const(c, 10)],[],[assign(c, 20)]]."""
        expect = "Cannot assign to a constant: assign(c,20)"
        self.assertTrue(TestVM.test(input, expect, 33))

    # Test 34: Error - Assigning to an undeclared identifier
    def test_34(self):
        input = """[[],[],[assign(undeclaredVar, 10)]]."""
        expect = "Undeclared identifier: undeclaredVar"
        self.assertTrue(TestVM.test(input, expect, 34))

    # Test 35: Simple block with a local variable declaration and assignment
    def test_35(self):
        input = """[[],[],[block([var(a,integer)], [assign(a,5), call(writeInt,[a])])]]."""
        expect = "5"
        self.assertTrue(TestVM.test(input, expect, 35))

    # Test 36: Block shadowing a global variable
    def test_36(self):
        input = """[[var(x,integer)],[],[assign(x,10), block([var(x,integer)], [assign(x,20), call(writeInt,[x])]), call(writeInt,[x])]]."""
        expect = "2010" # Inner x is 20, outer x remains 10
        self.assertTrue(TestVM.test(input, expect, 36))

    # Test 37: Nested blocks with shadowing
    def test_37(self):
        input = """
        [[var(x,integer), var(y,integer)],[],
         [
            assign(x,1), assign(y,10),
            block([var(x,integer)],
                [
                    assign(x,2), assign(y,20), % Assign local x=2, outer y=20
                    block([var(y,integer)],
                        [
                            assign(x,3), assign(y,30), % Assign outer-block x=3, local y=30
                            call(writeInt,[x]), call(writeInt,[y]) % Prints 3, 30
                        ]
                    ),
                    call(writeInt,[x]), call(writeInt,[y]) % Prints 3, 20 (y is outer y)
                ]
            ),
            call(writeInt,[x]), call(writeInt,[y]) % Prints 1, 20 (x is global x)
         ]
        ]."""
        expect = "330320120"
        self.assertTrue(TestVM.test(input, expect, 37))

    # Test 38: Modifying a global variable from inside a block
    def test_38(self):
        input = """[[var(g,integer)],[],[assign(g, 1), block([], [assign(g, add(g, 5))]), call(writeInt,[g])]]."""
        expect = "6" # g becomes 1+5 = 6
        self.assertTrue(TestVM.test(input, expect, 38))

    # Test 39: Error - Redeclaring a variable in the same block scope
    def test_39(self):
        input = """[[],[],[block([var(a,integer), var(b,float), var(a,boolean)], [])]]."""
        expect = "Redeclared identifier: var(a,boolean)"
        self.assertTrue(TestVM.test(input, expect, 39))

    # Test 40: Simple If statement, true condition
    def test_40(self):
        input = """[[],[],[if(true, call(writeInt,[1]))]]."""
        expect = "1"
        self.assertTrue(TestVM.test(input, expect, 40))

    # Test 41: If-Else statement, false condition
    def test_41(self):
        input = """[[],[],[if(false, call(writeInt,[1]), call(writeInt,[2]))]]."""
        expect = "2"
        self.assertTrue(TestVM.test(input, expect, 41))

    # Test 42: If statement, false condition, no else branch (should print nothing)
    def test_42(self):
        input = """[[],[],[if(false, call(writeInt,[1]))]]."""
        expect = ""
        self.assertTrue(TestVM.test(input, expect, 42))

    # Test 43: If condition is an expression using a global constant
    def test_43(self):
        input = """[[const(flag,true)],[],[if(flag, call(writeInt,[10]), call(writeInt,[20]))]]."""
        expect = "10"
        self.assertTrue(TestVM.test(input, expect, 43))

    # Test 44: Error - If condition is not a boolean type
    def test_44(self):
        input = """[[],[],[if(add(1,2), call(writeInt,[1]))]]."""
        expect = "Type mismatch: if(add(1,2),call(writeInt,[1]))"
        self.assertTrue(TestVM.test(input, expect, 44))


    # --- Tests for Phase 3: Logical, Relational, Function Calls ---

    # Test 45: Logical NOT (bnot)
    def test_45(self):
        input = """[[var(b,boolean)],[],[assign(b,true), call(writeBool,[bnot(b)])]]."""
        expect = "false"
        self.assertTrue(TestVM.test(input, expect, 45))

    # Test 46: Logical AND (band) - both true
    def test_46(self):
        input = """[[],[],[call(writeBool,[band(true,true)])]]."""
        expect = "true"
        self.assertTrue(TestVM.test(input, expect, 46))

    # Test 47: Logical OR (bor) - one true
    def test_47(self):
        input = """[[],[],[call(writeBool,[bor(false,true)])]]."""
        expect = "true"
        self.assertTrue(TestVM.test(input, expect, 47))

    # Test 48: Logical AND (band) - short circuit
    def test_48(self):
        # band(false, add(1,true)) should evaluate to false without type error on add
        input = """[[],[],[call(writeBool,[band(false, add(1,true))])]]."""
        expect = "false"
        self.assertTrue(TestVM.test(input, expect, 48))

    # Test 49: Logical OR (bor) - short circuit
    def test_49(self):
        # bor(true, add(1,true)) should evaluate to true without type error on add
        input = """[[],[],[call(writeBool,[bor(true, add(1,true))])]]."""
        expect = "true"
        self.assertTrue(TestVM.test(input, expect, 49))

    # Test 50: Logical expression type mismatch error
    def test_50(self):
        input = """[[],[],[call(writeBool,[band(1, true)])]]."""
        expect = "Type mismatch: band(1,true)"
        self.assertTrue(TestVM.test(input, expect, 50))

    # Test 51: Relational operators (greater, le) with mixed types
    def test_51(self):
        input = """[[],[],[if(greater(5.5, 3), call(writeInt,[1])), if(le(10, 10.0), call(writeInt,[2]))]]."""
        expect = "12" # 5.5 > 3.0 is true, 10.0 <= 10.0 is true
        self.assertTrue(TestVM.test(input, expect, 51))

    # Test 52: Equality operators (eql, ne) - int and boolean allowed
    def test_52(self):
        input = """[[],[],[if(eql(15,15), call(writeInt,[1])), if(ne(true,false), call(writeInt,[2]))]]."""
        expect = "12" # 15 == 15 is true, true != false is true
        self.assertTrue(TestVM.test(input, expect, 52))

    # Test 53: Equality type mismatch error (different types or float)
    def test_53(self):
        input = """[[],[],[if(eql(10, true), call(writeInt,[1]))]].""" # int vs bool
        # input = """[[],[],[if(eql(5.0, 5.0), call(writeInt,[1]))]].""" # float vs float (change if float allowed for eql)
        expect = "Type mismatch: eql(10,true)"
        self.assertTrue(TestVM.test(input, expect, 53))

    # Test 54: Relational operator type mismatch error
    def test_54(self):
        input = """[[],[],[if(less(true, 10), call(writeInt,[1]))]].""" # bool vs int
        expect = "Type mismatch: less(true,10)"
        self.assertTrue(TestVM.test(input, expect, 54))

    # Test 55: Simple user-defined function call (no params, literal return)
    def test_55(self):
        input = """
        [[],
         [func(getFive,[],integer,[assign(getFive,5)])],
         [call(writeInt,[call(getFive,[])])]
        ]."""
        expect = "5"
        self.assertTrue(TestVM.test(input, expect, 55))

    # Test 56: User function with parameters and calculation
    def test_56(self):
        input = """
        [[],
         [func(addTwo,[par(a,integer),par(b,integer)],integer,[assign(addTwo,add(a,b))])],
         [call(writeInt,[call(addTwo,[10,20])])]
        ]."""
        expect = "30"
        self.assertTrue(TestVM.test(input, expect, 56))

    # Test 57: Nested user function calls
    def test_57(self):
        input = """
        [[],
         [func(inner,[],integer,[assign(inner,10)]),
          func(outer,[],integer,[assign(outer,add(3,call(inner,[])))])
         ],
         [call(writeInt,[call(outer,[])])]
        ]."""
        expect = "13" # 3 + inner() = 3 + 10 = 13
        self.assertTrue(TestVM.test(input, expect, 57))

    # Test 58: User function call - argument type mismatch error
    def test_58(self):
        input = """
        [[],
         [func(needsInt,[par(x,integer)],integer,[assign(needsInt,x)])],
         [call(writeInt,[call(needsInt,[true])])]
        ]."""
        expect = "Type mismatch: call(needsInt,[true])" # Error comes from map_args_params check
        self.assertTrue(TestVM.test(input, expect, 58))

    # Test 59: User function call - unassigned return value error
    def test_59(self):
        input = """
        [[],
         [func(forgotAssign,[],integer,[])], % Body is empty, never assigns to forgotAssign
         [call(writeInt,[call(forgotAssign,[])])]
        ]."""
        expect = "Invalid expression: call(forgotAssign,[])" # Error finding return value
        self.assertTrue(TestVM.test(input, expect, 59))


    # --- Tests for Phase 4: Loops and Control Flow ---

    # Test 60: Simple while loop counting down
    def test_60(self):
        input = """
        [[var(i, integer)],[],
         [
            assign(i, 3),
            while(greater(i, 0),
                block([], [call(writeInt,[i]), assign(i, sub(i,1))])
            )
         ]
        ]."""
        expect = "321" # Prints 3, 2, 1
        self.assertTrue(TestVM.test(input, expect, 60))

    # Test 61: While loop condition initially false
    def test_61(self):
        input = """
        [[var(i, integer)],[],
         [
            assign(i, 0),
            while(greater(i, 0),
                call(writeInt,[i]) % This should not execute
            ),
            call(writeInt,[-1]) % To show loop was skipped
         ]
        ]."""
        expect = "-1"
        self.assertTrue(TestVM.test(input, expect, 61))

    # Test 62: While loop condition type mismatch error
    def test_62(self):
        input = """[[],[],[while(1, call(writeInt,[1]))]]."""
        expect = "Type mismatch: while(1,call(writeInt,[1]))"
        self.assertTrue(TestVM.test(input, expect, 62))

    # Test 63: Simple do-while loop
    def test_63(self):
        input = """
        [[var(x, integer)],[],
         [
            assign(x, 5),
            do([call(writeInt,[x]), assign(x, sub(x,1))], greater(x,3))
            % Body runs for x=5 (prints 5), x becomes 4. 4>3 is true.
            % Body runs for x=4 (prints 4), x becomes 3. 3>3 is false. Loop ends.
         ]
        ]."""
        expect = "54"
        self.assertTrue(TestVM.test(input, expect, 63))

    # Test 64: Do-while loop body executes at least once
    def test_64(self):
        input = """
        [[var(x, integer)],[],
         [
            assign(x, 0),
            do([call(writeInt,[x])], false) % Condition is immediately false
         ]
        ]."""
        expect = "0" # Body executes once even if condition is false
        self.assertTrue(TestVM.test(input, expect, 64))

    # Test 65: Do-while loop condition type mismatch error
    def test_65(self):
        input = """[[],[],[do([call(writeInt,[1])], 0)]]."""
        expect = "Type mismatch: do([call(writeInt,[1])],0)"
        self.assertTrue(TestVM.test(input, expect, 65))

    # Test 66: Simple loop N statement
    def test_66(self):
        input = """
        [[],[],
         [
            loop(4, call(writeInt,[1]))
         ]
        ]."""
        expect = "1111"
        self.assertTrue(TestVM.test(input, expect, 66))

    # Test 67: Loop N statement with N=0
    def test_67(self):
        input = """
        [[],[],
         [
            loop(0, call(writeInt,[1])), % Loop body should not execute
            call(writeInt,[0]) % To show loop was skipped
         ]
        ]."""
        expect = "0"
        self.assertTrue(TestVM.test(input, expect, 67))

    # Test 68: Loop N statement with non-integer N error
    def test_68(self):
        input = """[[],[],[loop(true, call(writeInt,[1]))]]."""
        expect = "Type mismatch: loop(true,call(writeInt,[1]))"
        self.assertTrue(TestVM.test(input, expect, 68))

    # Test 69: Break statement inside while loop
    def test_69(self):
        input = """
        [[var(i, integer)],[],
         [
            assign(i, 5),
            while(true, % Infinite loop condition
                block([], [
                    call(writeInt,[i]),
                    assign(i, sub(i,1)),
                    if(eql(i, 2), break(null)) % Break when i becomes 2
                ])
            ),
            call(writeInt,[i]) % Print final value of i after break
         ]
        ]."""
        expect = "5432" # Loop prints 5, 4, 3. i becomes 2. Break. Final i is 2.
        self.assertTrue(TestVM.test(input, expect, 69))

    # Test 70: Continue statement inside loop N
    def test_70(self):
        input = """
        [[var(i, integer)],[],
         [
            assign(i, 0),
            loop(5,
                block([], [
                    assign(i, add(i,1)),
                    if(eql(imod(i,2), 0), continue(null)), % If i is even, continue
                    call(writeInt,[i]) % Only print odd numbers
                ])
            )
         ]
        ]."""
        expect = "135" # Prints 1, skips 2, prints 3, skips 4, prints 5
        self.assertTrue(TestVM.test(input, expect, 70))

    # Test 71: Break statement not inside a loop error
    def test_71(self):
        input = """[[],[],[break(null)]]."""
        expect = "Break not in a loop: break(null)"
        self.assertTrue(TestVM.test(input, expect, 71))

    # Test 72: Continue statement not inside a loop error
    def test_72(self):
        input = """[[],[],[continue(null)]]."""
        expect = "Continue not in a loop: continue(null)"
        self.assertTrue(TestVM.test(input, expect, 72))

    # Test 73: Nested loops with break affecting only inner loop
    def test_73(self):
        input = """
        [[var(i, integer), var(j, integer)],[],
         [
            assign(i, 1),
            while(le(i, 2), % Outer loop i=1, i=2
                block([], [
                    assign(j, 1),
                    while(le(j, 3), % Inner loop j=1, j=2, j=3
                        block([], [
                            call(writeInt,[i]), call(writeStr,[","]), call(writeIntLn,[j]),
                            if(eql(j, 2), break(null)), % Break inner loop when j=2
                            assign(j, add(j,1))
                        ])
                    ), % Inner loop finished
                    call(writeStrLn,["---"]),
                    assign(i, add(i,1))
                ])
            ) % Outer loop finished
         ]
        ]."""
        # i=1: prints 1,1 -> prints 1,2 -> breaks inner -> prints ---
        # i=2: prints 2,1 -> prints 2,2 -> breaks inner -> prints ---
        expect = "1,1\n1,2\n---\n2,1\n2,2\n---\n"
        self.assertTrue(TestVM.test(input, expect, 73))

    # Test 74: Loop with function call and assignment
    def test_74(self):
        input = """
        % Reworking Test 74 logic to fit loop N
        [[var(sum, integer), var(i, integer)],
         [func(square,[par(x,integer)],integer,[assign(square,times(x,x))])],
         [
             assign(sum, 0), assign(i, 1),
             loop(3, % Execute body 3 times
                 block([], [
                     assign(sum, add(sum, call(square,[i]))), % sum = sum + i*i
                     assign(i, add(i,1)) % i becomes 2, then 3, then 4
                 ])
             ),
             call(writeInt,[sum]) % Iteration 1: sum=0+1*1=1, i=2. Iter 2: sum=1+2*2=5, i=3. Iter 3: sum=5+3*3=14, i=4. Final sum=14.
         ]
        ]."""
        expect = "14"
        self.assertTrue(TestVM.test(input, expect, 74))

    
    # --- Tests for Phase 5: User-defined Procedures and Integration ---

    # Test 75: Simple procedure call (no params)
    def test_75(self):
        input = """
        [[],
         [proc(p,[],[call(writeInt,[1])])],
         [call(p,[])]
        ]."""
        expect = "1"
        self.assertTrue(TestVM.test(input, expect, 75))

    # Test 76: Procedure with one integer parameter
    def test_76(self):
        input = """
        [[],
         [proc(inc,[par(a,integer)],[call(writeInt,[add(a,1)])])],
         [call(inc,[10])]
        ]."""
        expect = "11"
        self.assertTrue(TestVM.test(input, expect, 76))

    # Test 77: Procedure modifying a global variable
    def test_77(self):
        input = """
        [[var(g,integer)],
         [proc(modifyG,[],[assign(g,100)])],
         [assign(g,0), call(modifyG,[]), call(writeInt,[g])]
        ]."""
        expect = "100"
        self.assertTrue(TestVM.test(input, expect, 77))

    # Test 78: Procedure with multiple parameters (different types)
    def test_78(self):
        input = """
        [[],
         [proc(printAll,[par(i,integer),par(b,boolean),par(s,string)],
                [call(writeInt,[i]), call(writeBool,[b]), call(writeStr,[s])])
         ],
         [call(printAll,[5,true,"Hello"])]
        ]."""
        expect = "5trueHello"
        self.assertTrue(TestVM.test(input, expect, 78))

    # Test 79: Procedure using local block/variables shadowing parameters
    def test_79(self):
        input = """
        [[],
         [proc(p,[par(x,integer)],
              [ block([var(x,integer)], % Local x shadows param x
                  [assign(x,10), call(writeInt,[x])]) % Prints local x=10
              ])
         ],
         [call(p,[1])]
        ]."""
        expect = "10" # Parameter x=1 is shadowed by local x=10
        self.assertTrue(TestVM.test(input, expect, 79))

    # Test 80: Nested procedure calls
    def test_80(self):
        input = """
        [[],
         [proc(p2,[],[call(writeInt,[2])]),
          proc(p1,[],[call(writeInt,[1]), call(p2,[])])
         ],
         [call(p1,[])]
        ]."""
        expect = "12"
        self.assertTrue(TestVM.test(input, expect, 80))

    # Test 81: Procedure calling a user-defined function
    def test_81(self):
        input = """
        [[],
         [func(f,[],integer,[assign(f,5)]),
          proc(p,[],[call(writeInt,[call(f,[])])])
         ],
         [call(p,[])]
        ]."""
        expect = "5"
        self.assertTrue(TestVM.test(input, expect, 81))

    # Test 82: Function calling a procedure
    def test_82(self):
        input = """
        [[],
         [proc(p,[],[call(writeInt,[10])]),
          func(f,[],integer,[call(p,[]), assign(f,1)]) % p prints 10, f returns 1
         ],
         [call(writeInt,[call(f,[])])] % Prints 10, then prints 1
        ]."""
        expect = "101"
        self.assertTrue(TestVM.test(input, expect, 82))

    # Test 83: Procedure calling a function with parameters from procedure
    def test_83(self):
        input = """
        [[],
         [func(add1,[par(x,integer)],integer,[assign(add1,add(x,1))]),
          proc(p,[par(y,integer)],[call(writeInt,[call(add1,[y])])])
         ],
         [call(p,[9])]
        ]."""
        expect = "10"
        self.assertTrue(TestVM.test(input, expect, 83))

    # Test 84: Procedure with internal loop
    def test_84(self):
        input = """
        [[],
         [proc(loopP,[par(n,integer)],
            [ block([var(i,integer)],
                [ assign(i,n),
                  while(greater(i,0),
                    block([], [call(writeInt,[i]), assign(i,sub(i,1))])
                  )
                ])
            ])
         ],
         [call(loopP,[3])]
        ]."""
        expect = "321"
        self.assertTrue(TestVM.test(input, expect, 84))

    # Test 85: Procedure with internal loop + break (break local to proc)
    def test_85(self):
        input = """
        [[],
         [proc(p,[],
            [ block([var(i,integer)],
                [ assign(i,0),
                  while(true,
                    block([], [
                        assign(i,add(i,1)), call(writeInt,[i]),
                        if(eql(i,3), break(null))
                    ])
                  ) % End while
                ])
            ])
         ],
         [call(p,[]), call(writeInt,[99])] % 99 should print after p finishes
        ]."""
        expect = "12399"
        self.assertTrue(TestVM.test(input, expect, 85))

    # Test 86: Procedure with internal loop + continue
    def test_86(self):
        input = """
        [[],
         [proc(p,[],
            [ block([var(i,integer)],
                [ assign(i,0),
                  loop(5,
                    block([], [
                        assign(i,add(i,1)),
                        if(eql(imod(i,2), 0), continue(null)), % Skip even i
                        call(writeInt,[i])
                    ])
                  ) % End loop
                ])
            ])
         ],
         [call(p,[])] % Should print 1, 3, 5
        ]."""
        expect = "135"
        self.assertTrue(TestVM.test(input, expect, 86))

    # Test 87: Calling procedure from within a loop in main
    def test_87(self):
        input = """
        [[],
         [proc(p,[par(i,integer)],[call(writeInt,[i])])],
         [loop(3, call(p,[5]))] % Call p(5) three times
        ]."""
        expect = "555"
        self.assertTrue(TestVM.test(input, expect, 87))

    # Test 88: Error - Calling procedure with too few arguments
    def test_88(self):
        input = """
        [[],
         [proc(p,[par(a,integer),par(b,integer)],[])],
         [call(p,[1])]
        ]."""
        expect = "Wrong number of arguments: call(p,[1])"
        self.assertTrue(TestVM.test(input, expect, 88))

    # Test 89: Error - Calling procedure with too many arguments
    def test_89(self):
        input = """
        [[],
         [proc(p,[par(a,integer)],[])],
         [call(p,[1,2])]
        ]."""
        expect = "Wrong number of arguments: call(p,[1,2])"
        self.assertTrue(TestVM.test(input, expect, 89))

    # Test 90: Error - Calling procedure with wrong argument type
    def test_90(self):
        input = """
        [[],
         [proc(p,[par(a,boolean)],[])],
         [call(p,[1])]
        ]."""
        # The error message needs to match the format produced when map_args_params throws
        expect = "Type mismatch: call(p,[1])"
        self.assertTrue(TestVM.test(input, expect, 90))

    # Test 91: Procedure body attempts to assign to constant
    def test_91(self):
        input = """
        [[const(c,1)],
         [proc(p,[],[assign(c,2)])],
         [call(p,[])]
        ]."""
        expect = "Cannot assign to a constant: assign(c,2)"
        self.assertTrue(TestVM.test(input, expect, 91))

    # Test 92: Procedure body uses undeclared variable
    def test_92(self):
        input = """
        [[],
         [proc(p,[],[call(writeInt,[x])])], % x is not param or local or global
         [call(p,[])]
        ]."""
        expect = "Undeclared identifier: x" # Error from reduce_atom inside p
        self.assertTrue(TestVM.test(input, expect, 92))

    # Test 93: Integration - globals, func, proc, assign, block, if, loop
    def test_93(self):
        input = """
        [[var(g,integer)],
         [func(f,[par(x,integer)],integer,[assign(f,times(x,2))]),
          proc(p,[par(y,integer)],[assign(g,add(g,y))])
         ],
         [
             assign(g,1),
             loop(3,
                 block([var(temp,integer)],
                     [
                         assign(temp, call(f,[g])), % temp = g*2
                         if(greater(temp, 5),
                             call(p,[1]), % g = g + 1
                             call(p,[0])  % g = g + 0
                         )
                     ]
                 )
             ),
             call(writeInt,[g]) % Iter 1: g=1, temp=2, temp<5, p(0) called, g=1.
                                % Iter 2: g=1, temp=2, temp<5, p(0) called, g=1.
                                % Iter 3: g=1, temp=2, temp<5, p(0) called, g=1. Something's wrong?
                                % Let's trace again:
                                % g=1 -> loop starts
                                % Iter 1: temp=f(1)=2. 2>5 is false. p(0) called. g=1+0=1.
                                % Iter 2: temp=f(1)=2. 2>5 is false. p(0) called. g=1+0=1.
                                % Iter 3: temp=f(1)=2. 2>5 is false. p(0) called. g=1+0=1. Final g=1. Expected: 1
         ]
        ]."""
        expect = "1"
        self.assertTrue(TestVM.test(input, expect, 93))

    # Test 94: Integration - Type mismatch deep inside calls
    def test_94(self):
        input = """
        [[],
         [func(f,[par(x,integer)],boolean,[assign(f,true)]),
          proc(p,[par(y,integer)],[call(writeBool,[call(f,[y])])])
         ],
         [call(p,[false])] % Error: passing bool to p which expects int (y)
        ]."""
        expect = "Type mismatch: call(p,[false])"
        self.assertTrue(TestVM.test(input, expect, 94))

    # Test 95: Integration - Complex expression evaluation order
    def test_95(self):
        input = """
        [[],
         [func(f1,[],integer,[assign(f1, 5)]),
          func(f2,[],integer,[assign(f2, 10)])
         ],
         [call(writeInt,[add(sub(call(f1,[]), 1), times(call(f2,[]), 2))])] % (5-1) + (10*2) = 4 + 20 = 24
        ]."""
        expect = "24"
        self.assertTrue(TestVM.test(input, expect, 95))

    # Test 96: Integration - Redeclaration check order (global, param, local)
    def test_96(self):
        input = """
        [[var(x,integer)],
         [proc(p,[par(x,integer)],[ % Param x is OK (different scope)
             block([var(x,integer)],[]) % Local x is OK (different scope)
             ])],
         []
        ]."""
        expect = "" # Should succeed with no output
        self.assertTrue(TestVM.test(input, expect, 96))

    # Test 97: Integration - Using boolean expressions and short-circuiting with calls
    def test_97(self):
        input = """
        [[],
         [proc(p,[],[call(writeInt,[1])])], % Procedure that prints 1
         [
             if(bor(true, call(p,[])), call(writeInt,[2])), % Should print 2 (short-circuits)
             if(band(false, call(p,[])), call(writeInt,[3])) % Should print nothing (short-circuits)
         ]
        ]."""
        expect = "2" # Only the first if's body runs
        self.assertTrue(TestVM.test(input, expect, 97))

    # Test 98: Integration - Assign function result to variable
    def test_98(self):
        input = """
        [[var(r, integer)],
         [func(calc,[],integer,[assign(calc, 123)])],
         [assign(r, call(calc,[])), call(writeInt,[r])]
        ]."""
        expect = "123"
        self.assertTrue(TestVM.test(input, expect, 98))

    # Test 99: Integration - Return value requires assignment to function name
    def test_99(self):
        input = """
        [[],
         [func(calc,[par(x,integer)],integer,[assign(x,add(x,1))])], % Assigns to param x, not func name calc
         [call(calc,[5])] % Call should cause invalid_expression (unassigned return)
        ]."""
        expect = "Invalid expression: call(calc,[5])"
        self.assertTrue(TestVM.test(input, expect, 99))