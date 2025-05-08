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
         [call(needsInt,[true])]
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