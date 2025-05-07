import unittest
from TestUtils import TestVM


class VMSuite(unittest.TestCase):

    # # Test 00: Simple arithmetic with nesting
    # def test_00(self):
    #     input = """[[],[],[call(writeInt,[add(times(2,3),sub(10,5))])]]."""
    #     expect = "11" # (2*3) + (10-5) = 6 + 5 = 11
    #     self.assertTrue(TestVM.test(input, expect, 0))

    # # Test 01: Mixed integer and float arithmetic, requires writeReal
    # def test_01(self):
    #     input = """[[],[],[call(writeReal,[add(5,2.5)])]]."""
    #     expect = "7.5" # 5.0 + 2.5 = 7.5 (float promotion)
    #     self.assertTrue(TestVM.test(input, expect, 1))

    # # Test 02: Float arithmetic
    # def test_02(self):
    #     input = """[[],[],[call(writeReal,[times(10.0,sub(5.5,1.2))])]]."""
    #     expect = "43.0" # 10.0 * (5.5 - 1.2) = 10.0 * 4.3 = 43.0
    #     self.assertTrue(TestVM.test(input, expect, 2))

    # # Test 03: Integer division (idiv)
    # def test_03(self):
    #     input = """[[],[],[call(writeInt,[idiv(20,3)])]]."""
    #     expect = "6" # 20 div 3 = 6
    #     self.assertTrue(TestVM.test(input, expect, 3))

    # # Test 04: Modulo (imod)
    # def test_04(self):
    #     input = """[[],[],[call(writeInt,[imod(20,3)])]]."""
    #     expect = "2" # 20 mod 3 = 2
    #     self.assertTrue(TestVM.test(input, expect, 4))

    # # Test 05: Integer division by zero error
    # def test_05(self):
    #     input = """[[],[],[call(writeInt,[idiv(10,0)])]]."""
    #     expect = "Invalid expression: idiv(10,0)"
    #     self.assertTrue(TestVM.test(input, expect, 5))

    # # Test 06: Float division by zero error (rdiv)
    # def test_06(self):
    #     input = """[[],[],[call(writeReal,[rdiv(10.0,0)])]]."""
    #     expect = "Invalid expression: 10.0 rdiv 0"
    #     self.assertTrue(TestVM.test(input, expect, 6))

    # # Test 07: Unary minus on float, requires writeReal
    # def test_07(self):
    #     input = """[[],[],[call(writeReal,[sub(15.99)])]]."""
    #     expect = "-15.99"
    #     self.assertTrue(TestVM.test(input, expect, 7))

    # # Test 08: Using a global constant (integer)
    # def test_08(self):
    #     input = """[[const(x,100)],[],[call(writeInt,[add(x,50)])]]."""
    #     expect = "150" # 100 + 50 = 150
    #     self.assertTrue(TestVM.test(input, expect, 8))

    # # Test 09: Using a global constant (float), requires writeReal
    # def test_09(self):
    #     input = """[[const(pi,3.14)],[],[call(writeReal,[times(pi,2)])]]."""
    #     expect = "6.28" # 3.14 * 2.0 = 6.28
    #     self.assertTrue(TestVM.test(input, expect, 9))

    # # Test 10: Using a global constant (boolean), requires writeBool
    # def test_10(self):
    #     input = """[[const(t,true)],[],[call(writeBool,[t])]]."""
    #     expect = "true"
    #     self.assertTrue(TestVM.test(input, expect, 10))

    # # Test 11: Undeclared identifier in expression error
    # def test_11(self):
    #     input = """[[],[],[call(writeInt,[add(a,10)])]]."""
    #     expect = "Undeclared identifier: a"
    #     self.assertTrue(TestVM.test(input, expect, 11))

    # # Test 12: Type mismatch in arithmetic operation (int + bool) error
    # def test_12(self):
    #     input = """[[],[],[call(writeInt,[add(1,true)])]]."""
    #     expect = "Type mismatch: add(1,true)"
    #     self.assertTrue(TestVM.test(input, expect, 12))

    # # Test 13: Wrong number of arguments for built-in procedure error
    # def test_13(self):
    #     input = """[[],[],[call(writeInt,[1,2])]].""" # writeInt expects 1 argument
    #     expect = "Wrong number of arguments: call(writeInt,[1,2])"
    #     self.assertTrue(TestVM.test(input, expect, 13))

    # # Test 14: Type mismatch for built-in procedure argument (writeInt expects integer) error
    # def test_14(self):
    #     input = """[[],[],[call(writeInt,[3.14])]].""" # writeInt expects integer
    #     expect = "Type mismatch: call(writeInt,[3.14])"
    #     self.assertTrue(TestVM.test(input, expect, 14))

    # # --- Tests for Phase 1: Declarations and Lookup ---

    # # Test 15: Redeclared function name vs global variable
    # def test_15(self):
    #     input = """[[var(a,integer)],[func(a,[],integer,[])],[]]."""
    #     expect = "Redeclared function: a"
    #     self.assertTrue(TestVM.test(input, expect, 15))

    # # Test 16: Redeclared procedure name vs global constant
    # def test_16(self):
    #     input = """[[const(b,true)],[proc(b,[],[])],[]]."""
    #     expect = "Redeclared procedure: b"
    #     self.assertTrue(TestVM.test(input, expect, 16))

    # # Test 17: Redeclared function name vs built-in function name
    # def test_17(self):
    #     input = """[[],[func(readInt,[],integer,[])],[]]."""
    #     expect = "Redeclared function: readInt"
    #     self.assertTrue(TestVM.test(input, expect, 17))

    # # Test 18: Redeclared procedure name vs built-in procedure name
    # def test_18(self):
    #     input = """[[],[proc(writeInt,[],[])],[]]."""
    #     expect = "Redeclared procedure: writeInt"
    #     self.assertTrue(TestVM.test(input, expect, 18))

    # Test 19: Redeclared function name vs previously declared function
    def test_19(self):
        input = """[[],[func(foo,[],integer,[]), func(foo,[],float,[])],[]]."""
        expect = "Redeclared function: foo"
        self.assertTrue(TestVM.test(input, expect, 19))

    # # Test 20: Redeclared procedure name vs previously declared procedure
    # def test_20(self):
    #     input = """[[],[proc(bar,[],[]), proc(bar,[],[])],[]]."""
    #     expect = "Redeclared procedure: bar"
    #     self.assertTrue(TestVM.test(input, expect, 20))

    # # Test 21: Redeclared procedure name vs previously declared function (same name)
    # def test_21(self):
    #     input = """[[],[func(baz,[],integer,[]), proc(baz,[],[])],[]]."""
    #     expect = "Redeclared procedure: baz" # proc declared second, redefines func
    #     self.assertTrue(TestVM.test(input, expect, 21))

    # # Test 22: Redeclared parameter within a function's parameter list
    # def test_22(self):
    #     input = """[[],[func(testFunc,[par(x,integer),par(y,float),par(x,real)],integer,[])],[]]."""
    #     expect = "Redeclared identifier: par(x,real)"
    #     self.assertTrue(TestVM.test(input, expect, 22))

    # # Test 23: Redeclared parameter within a procedure's parameter list
    # def test_23(self):
    #     input = """[[],[proc(testProc,[par(a,boolean),par(b,string),par(a,integer)],[])],[]]."""
    #     expect = "Redeclared identifier: par(a,integer)"
    #     self.assertTrue(TestVM.test(input, expect, 23))

    # # Test 24: Using a declared function name directly as an expression (invalid context)
    # def test_24(self):
    #     input = """[[],[func(myFunc,[],integer,[])],[call(writeInt,[myFunc])]]."""
    #     expect = "Invalid expression: myFunc"
    #     self.assertTrue(TestVM.test(input, expect, 24))

    # # Test 25: Using a declared procedure name directly as an expression (invalid context)
    # def test_25(self):
    #     input = """[[],[proc(myProc,[],[])],[call(writeInt,[myProc])]]."""
    #     expect = "Invalid expression: myProc"
    #     self.assertTrue(TestVM.test(input, expect, 25))

    # # Test 26: Calling a global variable as if it were a procedure
    # def test_26(self):
    #     input = """[[var(myVar,integer)],[],[call(myVar,[])]]."""
    #     expect = "Invalid expression: call(myVar,[])"
    #     self.assertTrue(TestVM.test(input, expect, 26))

    # # Test 27: Calling a global constant as if it were a procedure
    # def test_27(self):
    #     input = """[[const(myConst,true)],[],[call(myConst,[])]]."""
    #     expect = "Invalid expression: call(myConst,[])"
    #     self.assertTrue(TestVM.test(input, expect, 27))

    # # Test 28: Calling a built-in function as if it were a procedure
    # def test_28(self):
    #     input = """[[],[],[call(readInt,[1])]].""" # readInt is a function, not a procedure
    #     expect = "Invalid expression: call(readInt,[1])"
    #     self.assertTrue(TestVM.test(input, expect, 28))

    # # Test 29: Calling a truly undeclared name as a procedure
    # def test_29(self):
    #     input = """[[],[],[call(nonExistentProc,[])]]."""
    #     expect = "Undeclared procedure: call(nonExistentProc,[])"
    #     self.assertTrue(TestVM.test(input, expect, 29))