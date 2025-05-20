import unittest
from TestUtils import TestVM


class VMSuite(unittest.TestCase):
    def test_01(self):        
        input = """[[],[],[call(writeInt,[3])]]."""
        expect = "3"
        self.assertTrue(TestVM.test(input, expect, 401))

    def test_02(self):        
        input = """[[],[],[call(writeInt,[add(3,1)])]]."""
        expect = "4"
        self.assertTrue(TestVM.test(input, expect, 402))

    def test_03(self):        
        input = """[[var(a,integer),var(b,integer),var(a,float)],[],[call(writeInt,[1])]]."""
        expect = "Redeclared identifier: var(a,float)"
        self.assertTrue(TestVM.test(input, expect, 403))

    def test_04(self):        
        input = """[[],[],[call(writeInt,[sub(3,1)])]]."""
        expect = "2"
        self.assertTrue(TestVM.test(input, expect, 404))

    def test_05(self):        
        input = """[[],[],[call(writeInt,[add(4,sub(3,1))])]]."""
        expect = "6"
        self.assertTrue(TestVM.test(input, expect, 405))
    def test_06(self):
        input = """[[var(a,integer)],[func(foo,[par(a,integer),par(b,integer)],integer,
[assign(a,add(a,b)),assign(foo,a)])],
[assign(a,3),call(writeIntLn,[call(foo,[a,3])]),call(writeIntLn,[a])]]. 
"""
        expect = "6\n3\n"
        self.assertTrue(TestVM.test(input, expect, 406))
    def test_07(self):
        input = """[[],[func(foo,[],integer,[assign(foo,3)])],[call(writeIntLn,[call(foo,[])])]]."""
        expect = "3\n"
        self.assertTrue(TestVM.test(input, expect, 407))
    def test_08(self):
        input = """[[var(a,integer)],[],[assign(a,3),call(writeIntLn,[a])]]."""
        expect = "3\n"
        self.assertTrue(TestVM.test(input, expect, 408))
    def test_09(self):
        input = """[[var(a,integer),var(b,float),var(a,boolean)],[],[]]."""
        expect = "Redeclared identifier: var(a,boolean)"
        self.assertTrue(TestVM.test(input, expect, 409))
    def test_10(self):
        input = """[[],[proc(foo,[par(a,integer),par(b,integer),par(a,float)],[])],[]]."""
        expect = "Redeclared identifier: par(a,float)"
        self.assertTrue(TestVM.test(input, expect, 410))
    def test_11(self):
        input = """[[],[proc(foo,[],[block([var(a,float)],[block([var(a,integer)],[])])])],[call(writeIntLn,[3])]]."""
        expect = "3\n"
        self.assertTrue(TestVM.test(input, expect, 411))
    def test_12(self):
        input = """[[],[func(readInt,[],float,[])],[]]."""
        expect = "Redeclared function: readInt"
        self.assertTrue(TestVM.test(input, expect, 412))
    def test_13(self):
        input = """[[],[func(foo,[],integer,[]),proc(foo,[a],[])],[]]."""
        expect = "Redeclared procedure: foo"
        self.assertTrue(TestVM.test(input, expect, 413))
    def test_14(self):
        input = """[[var(foo,integer)],[proc(foo,[a],[])],[]]."""
        expect = "Redeclared procedure: foo"
        self.assertTrue(TestVM.test(input, expect, 414))
    def test_15(self):
        input = """[[],[],[call(writeInt,[add(10,true)])]]."""
        expect = """Type mismatch: add(10,true)"""
        self.assertTrue(TestVM.test(input, expect, 415))
    def test_16(self):
        input = """[[],[],[call(writeBool,[add(10,3)])]]."""
        expect = """Type mismatch: call(writeBool,[add(10,3)])"""
        self.assertTrue(TestVM.test(input, expect, 416))
    def test_17(self):
        input = """[[var(a,integer)],[],[assign(a,3),if(a,call(writeInt,[3]))]]."""
        expect = """Type mismatch: if(a,call(writeInt,[3]))"""
        self.assertTrue(TestVM.test(input, expect, 417))
    def test_18(self):
        input = """[[],[func(foo,[],float,[assign(foo,3.0)])],
[call(writeInt,[call(foo,[])])]]. """
        expect = """Type mismatch: call(writeInt,[call(foo,[])])"""
        self.assertTrue(TestVM.test(input, expect, 418))
    def test_19(self):
        input = """[[var(a,integer)],[],[assign(a,add(b,1)),call(writeIntLn,[a])]]."""
        expect = """Undeclared identifier: b"""
        self.assertTrue(TestVM.test(input, expect, 419))
    def test_20(self):
        input = """[[var(a,integer)],[proc(foo,[],[block([var(b,integer)],[assign(b,1),assign(a,b)])])],
[call(writeIntLn,[b])]].   """
        expect = """Undeclared identifier: b"""
        self.assertTrue(TestVM.test(input, expect, 420))
    def test_21(self):
        input = """[[],[proc(foo,[],[])],[call(foo,[1])]]."""
        expect = """Wrong number of arguments: call(foo,[1])"""
        self.assertTrue(TestVM.test(input, expect, 421))
    def test_22(self):
        input = """[[],[proc(foo,[par(a,integer)],[])],[call(foo,[])]]."""
        expect = """Wrong number of arguments: call(foo,[])"""
        self.assertTrue(TestVM.test(input, expect, 422))
    def test_23(self):
        input = """[[var(a,integer)],[func(foo,[],integer,[])],[assign(a,call(foo,[]))]]."""
        expect = """Invalid expression: call(foo,[])"""
        self.assertTrue(TestVM.test(input, expect, 423))
    def test_24(self):
        input = """[[var(a,integer)],[],[call(writeInt,[a])]]."""
        expect = """Invalid expression: a"""
        self.assertTrue(TestVM.test(input, expect, 424))
    def test_25(self):
        input = """[[var(a,integer)],[],[assign(a,call(foo,[]))]]."""
        expect = """Undeclared function: call(foo,[])"""
        self.assertTrue(TestVM.test(input, expect, 425))
    def test_26(self):
        input = """[[],[],[call(foo,[])]].     """
        expect = """Undeclared procedure: call(foo,[])"""
        self.assertTrue(TestVM.test(input, expect, 426))
    def test_27(self):
        input = """[[],[],[break(null)]].     """
        expect = """Break not in a loop: break(null)"""
        self.assertTrue(TestVM.test(input, expect, 427))
    def test_28(self):
        input = """[[],[proc(foo,[],[break(null)])],[do([call(foo,[])],false)]]."""
        expect = """Break not in a loop: break(null)"""
        self.assertTrue(TestVM.test(input, expect, 428))
    def test_29(self):
        input = """[[],[],[continue(null)]].     """
        expect = """Continue not in a loop: continue(null)"""
        self.assertTrue(TestVM.test(input, expect, 429))
    def test_30(self):
        input = """[[],[proc(foo,[],[continue(null)])],[do([call(foo,[])],false)]]."""
        expect = """Continue not in a loop: continue(null)"""
        self.assertTrue(TestVM.test(input, expect, 430))
    def test_31(self):
        input = """[[const(a,7)],[],[assign(a,add(4,2))]]."""
        expect = """Cannot assign to a constant: assign(a,add(4,2))"""
        self.assertTrue(TestVM.test(input, expect, 431))
    def test_32(self):        
        input = """[[],[],[call(writeBool,[band(false,1)])]]."""
        expect = "false"
        self.assertTrue(TestVM.test(input, expect, 432))
    def test_33(self):        
        input = """[[],[],[call(writeReal,[rdiv(1.0,2)])]]."""
        expect = "0.5"
        self.assertTrue(TestVM.test(input, expect, 433))
    def test_34(self):        
        input = """[[],[],[call(writeInt,[idiv(3,2)])]]."""
        expect = "1"
        self.assertTrue(TestVM.test(input, expect, 434))
    def test_35(self):        
        input = """[[],[func(foo,[],integer,[])],[call(writeInt,[imod(5,3)])]]."""
        expect = "2"
        self.assertTrue(TestVM.test(input, expect, 435))
    def test_36(self):
        input = """[[var(a,integer),var(a,integer)],[],[assign(a,8)]]."""
        expect = """Redeclared identifier: var(a,integer)"""
        self.assertTrue(TestVM.test(input, expect, 436))
    def test_37(self):
        input = """[[var(c,integer),var(b,integer)],[func(a,[par(b,integer),par(d,integer)],integer,[assign(c,sub(c,5)),assign(a,add(b,d))])],[assign(b,5),assign(c,2),call(writeInt,[add(call(a,[b,c]),c)])]].""" 
        expect = """9"""
        self.assertTrue(TestVM.test(input, expect, 437))
    def test_38(self):
        input = """[[const(a,5),var(b,integer)],[],[assign(b,a),call(writeInt,[b])]].""" 
        expect = """5"""
        self.assertTrue(TestVM.test(input, expect, 438))
    def test_39(self):
        input = """[[var(b,integer)],[func(foo,[],integer,[assign(b,foo)])],[call(writeInt,[imod(5,3)])]].""" 
        expect = """2"""
        self.assertTrue(TestVM.test(input, expect, 439))
    def test_40(self):
        input = """[[const(a,1),const(b,add(a,1))],[],[call(writeInt,[b])]].""" 
        expect = """Invalid expression: add(a,1)"""
        self.assertTrue(TestVM.test(input, expect, 440))
    def test_41(self):
        input = """[[var(a,integer)],[],[assign(a,10),loop(a,block([],[assign(a,sub(a,1)),call(writeIntLn,[a])]))]].""" 
        expect = """9\n8\n7\n6\n5\n4\n3\n2\n1\n0\n"""
        self.assertTrue(TestVM.test(input, expect, 441))
    def test_42(self):
        input = """[[var(a,integer)],[],[assign(a,0),do([assign(a,add(a,1)),call(writeInt,[a])],less(a,5))]].""" 
        expect = """12345"""
        self.assertTrue(TestVM.test(input, expect, 442))
    def test_43(self):
        input = """ [[const(a,5),var(b,integer)],
    [func(foo,[par(n,integer)],
    integer,
    [block(
        [var(b,integer)],
        [
        assign(b,5),
        loop(n,block([],[call(writeIntLn,[b]),break(null),assign(b,add(b,1))])
                )
        ]
    ),
    assign(foo,0)                                         
    ])],[assign(b,0),call(writeInt,[call(foo,[100])]),call(writeIntLn,[b])]].""" 
        expect = """5\n00\n"""
        self.assertTrue(TestVM.test(input, expect, 443))
    def test_44(self):
        input = """[[var(a,integer)],[func(foo,[],integer,[assign(a,add(a,1)),assign(foo,a)])],[assign(a,1),while(true,block([],[assign(a,call(foo,[])),if(greater(a,5),break(null),call(writeInt,[a]))]))]].""" 
        expect = """2345"""
        self.assertTrue(TestVM.test(input, expect, 444))
    def test_45(self):
        input = """[[var(a,integer)],[proc(foo,[],[do([if(greater(a,5),break(null)),assign(a,add(a,1)),call(writeInt,[a])],true)])],[assign(a,1),call(foo,[]),call(writeInt,[a])]].""" 
        expect = """234566"""
        self.assertTrue(TestVM.test(input, expect, 445))
    def test_46(self):
        input = """[[var(a,integer)],[func(foo,[],integer,[assign(a,add(a,1)),assign(foo,a)])],[assign(a,1),do([if(greater(a,5),break(null)),assign(a,add(a,1)),call(writeInt,[a])],true)]].""" 
        expect = """23456"""
        self.assertTrue(TestVM.test(input, expect, 446))
    def test_47(self):
        input = """[[],[],[loop(sub(add(10,7),times(5,3)),call(writeStrLn,["in loop"]))]].""" 
        expect = """in loop\nin loop\n"""
        self.assertTrue(TestVM.test(input, expect, 447))
    def test_48(self):
        input = """[[],[],[call(writeBool,
        [bor(
            true,
            idiv(1,0)
        )])]].""" 
        expect = """true"""
        self.assertTrue(TestVM.test(input, expect, 448))
    def test_49(self):
        input = """[[var(a,string)],[],[assign(a,"hello"),call(writeStr,[a])]].""" 
        expect = """hello"""
        self.assertTrue(TestVM.test(input, expect, 449))
    def test_50(self):
        input = """[[],[],[
            call(writeInt,[call(writeInt,[1])])
            
            ]].""" 
        expect = """Type mismatch: call(writeInt,[1])"""
        self.assertTrue(TestVM.test(input, expect, 450))
    def test_51(self):
        input = """[[var(a,integer)],[],[assign(a,1),
            while(bnot(greater(a,5)),break(null)),call(writeInt,[a])]].""" 
        expect = """1"""
        self.assertTrue(TestVM.test(input, expect, 451))
    def test_arithmetic_add(self):
        input = "[[var(a,integer),var(b,integer)],[],[assign(a,add(2,3)),call(writeIntLn,[a])]]."
        expect = "5\n"
        self.assertTrue(TestVM.test(input, expect, 1))

    def test_arithmetic_nested(self):
        input = "[[var(x,integer)],[],[assign(x,times(add(1,2),sub(10,4))),call(writeIntLn,[x])]]."
        expect = "18\n"
        self.assertTrue(TestVM.test(input, expect, 2))

    def test_relational(self):
        input = "[[var(f,boolean)],[],[assign(f,greater(5,3)),call(writeBoolLn,[f])]]."
        expect = "true\n"
        self.assertTrue(TestVM.test(input, expect, 3))

    def test_logical_short_circuit(self):
        input = "[[var(b,boolean)],[],[assign(b,band(false,idiv(1,0))),call(writeBoolLn,[b])]]."
        expect = "false\n"
        self.assertTrue(TestVM.test(input, expect, 4))

    def test_if_else(self):
        input = "[[var(n,integer),var(out,integer)],[],[assign(n,3),if(greater(n,0),assign(out,1),assign(out,0)),call(writeIntLn,[out])]]."
        expect = "1\n"
        self.assertTrue(TestVM.test(input, expect, 5))

    def test_while_loop(self):
        input = "[[var(i,integer),var(sum,integer)],[],[assign(i,1),assign(sum,0),while(le(i,5),block([], [assign(sum,add(sum,i)),assign(i,add(i,1))])),call(writeIntLn,[sum])]]."
        expect = "15\n"
        self.assertTrue(TestVM.test(input, expect, 6))

    def test_do_loop(self):
        input = "[[var(i,integer),var(count,integer)],[],[assign(i,0),assign(count,0),do([assign(i,add(i,1)),assign(count,add(count,2))],less(i,3)),call(writeIntLn,[count])]]."
        expect = "6\n"
        self.assertTrue(TestVM.test(input, expect, 7))

    def test_loop_times(self):
        input = "[[var(i,integer)],[],[assign(i,0),loop(4,assign(i,add(i,1))),call(writeIntLn,[i])]]."
        expect = "4\n"
        self.assertTrue(TestVM.test(input, expect, 8))

    def test_break_continue(self):
        input = "[[var(i,integer),var(sum,integer)],[],[assign(i,0),assign(sum,0),while(less(i,5),block([], [assign(i,add(i,1)),if(eql(i,3),break(null)),assign(sum,add(sum,i))])),call(writeIntLn,[sum])]]."
        expect = "3\n"
        self.assertTrue(TestVM.test(input, expect, 9))

    def test_function_call(self):
        input = "[[var(a,integer),var(b,integer),var(c,integer)],[func(foo,[par(a,integer),par(b,integer)],integer,[assign(foo,add(a,b))])],[assign(a,10),assign(b,20),assign(c,call(foo,[a,b])),call(writeIntLn,[c])]]."
        expect = "30\n"
        self.assertTrue(TestVM.test(input, expect, 10))

    def test_undeclare_identifier(self):
        input = "[[var(a,integer)],[],[assign(a,add(a,b))]]."
        expect = "Invalid expression: a"
        self.assertTrue(TestVM.test(input, expect, 11))

    def test_type_mismatch(self):
        input = "[[],[],[call(writeInt,[add(10,true)])]]."
        expect = "Type mismatch: add(10,true)"
        self.assertTrue(TestVM.test(input, expect, 12))

    def test_wrong_number_arguments(self):
        input = "[[],[proc(p,[par(x,integer)],[])],[call(p,[])]]."
        expect = "Wrong number of arguments: call(p,[])"
        self.assertTrue(TestVM.test(input, expect, 13))

    def test_redeclare_identifier(self):
        input = "[[var(a,integer),var(a,boolean)],[],[]]."
        expect = "Redeclared identifier: var(a,boolean)"
        self.assertTrue(TestVM.test(input, expect, 14))

    def test_undeclare_function(self):
        input = "[[var(a,integer)],[],[assign(a,call(foo,[]))]]."
        expect = "Undeclared function: call(foo,[])"
        self.assertTrue(TestVM.test(input, expect, 15))
    def test_assignment_boolean(self):
        input = "[[var(b,boolean)],[],[assign(b,bor(true,false)),call(writeBoolLn,[b])]]."
        expect = "true\n"
        self.assertTrue(TestVM.test(input, expect, 16))

    def test_nested_if(self):
        input = "[[var(a,integer),var(b,integer),var(res,integer)],[],[assign(a,5),assign(b,10),if(greater(b,a),if(eql(a,5),assign(res,100),assign(res,200)),assign(res,300)),call(writeIntLn,[res])]]."
        expect = "100\n"
        self.assertTrue(TestVM.test(input, expect, 17))

    def test_nested_loop(self):
        input = "[[var(i,integer),var(j,integer),var(count,integer)],[],[assign(i,1),assign(count,0),while(le(i,2),block([], [assign(j,1),while(le(j,2),block([], [assign(count,add(count,1)),assign(j,add(j,1))])),assign(i,add(i,1))])),call(writeIntLn,[count])]]."
        expect = "4\n"
        self.assertTrue(TestVM.test(input, expect, 18))

    def test_and_operator(self):
        input = "[[var(r,boolean)],[],[assign(r,band(true,true)),call(writeBoolLn,[r])]]."
        expect = "true\n"
        self.assertTrue(TestVM.test(input, expect, 19))

    def test_or_operator(self):
        input = "[[var(r,boolean)],[],[assign(r,bor(false,false)),call(writeBoolLn,[r])]]."
        expect = "false\n"
        self.assertTrue(TestVM.test(input, expect, 20))

    def test_complex_program(self):
        input = """[[
            var(a,integer), var(b,integer), var(c,integer), var(d,integer), var(e,integer), 
            var(f,integer), var(g,integer), var(h,integer), var(i,integer), var(j,integer), 
            var(result,integer), var(flag,boolean)
        ],
        [
            func(max3, [par(x,integer), par(y,integer), par(z,integer)], integer, [
                if(greater(x,y),
                    if(greater(x,z),assign(max3,x), assign(max3,z)), 
                    if(greater(y,z),assign(max3,y), assign(max3,z))
                   
                )
            ]),
            func(factorial, [par(n,integer)], integer, [
                if(le(n,1), assign(factorial,1), 
                    assign(factorial, times(n, call(factorial, [sub(n,1)])))
                )
            ])
        ],
        [
            assign(a,3), assign(b,4), assign(c,5),
            assign(d,call(max3,[a,b,c])),
            assign(e,call(factorial,[d])),
            assign(f,0), assign(g,1),
            loop(5, block([], [
                assign(h,add(f,g)),
                assign(f,g), assign(g,h)
            ])),
            assign(i,add(f,g)),
            assign(j,sub(i,e)),
            assign(flag,greater(j,0)),
            if(flag, assign(result, j), assign(result, e)),
            call(writeIntLn,[result])
        ]]."""
        expect = "120\n"
        self.assertTrue(TestVM.test(input, expect, 100))
    def test_full_feature_program(self):
        input = """[[
            var(x,integer), var(y,integer), var(z,integer), var(w,integer),
            var(res1,integer), var(res2,integer), var(res3,integer), var(res4,integer),
            var(flag1,boolean), var(flag2,boolean), var(counter,integer), var(sum,integer),
            var(finalResult,integer)
        ],
        [
            func(max2, [par(a,integer), par(b,integer)], integer, [
                if(greater(a,b), assign(max2,a), assign(max2,b))
            ]),
            func(min2, [par(a,integer), par(b,integer)], integer, [
                if(less(a,b), assign(min2,a), assign(min2,b))
            ]),
            func(pow2, [par(n,integer)], integer, [
                if(le(n,0), assign(pow2,1), assign(pow2, times(2, call(pow2, [sub(n,1)]))))
            ]),
            func(sumToN, [par(n,integer)], integer, [
                if(le(n,1), assign(sumToN,n), assign(sumToN, add(n, call(sumToN,[sub(n,1)]))))
            ])
        ],
        [
            assign(x,5), assign(y,8), assign(z,3), assign(w,10),

            assign(res1, call(max2,[x,y])),          
            assign(res2, call(min2,[z,w])),          
            assign(res3, call(pow2,[3])),         
            assign(res4, call(sumToN,[4])),           

            assign(flag1,greater(res1,res2)),    
            assign(flag2,less(res3,res4)),          

            assign(counter,0), assign(sum,0),      
            loop(5, block([], [                      
                assign(sum, add(sum, counter)),       
                assign(counter, add(counter, 1))     
            ])),

            if(flag1,
                if(flag2,
                    assign(finalResult, add(sum, res4)),     
                    assign(finalResult, res4)),
                assign(finalResult, res2)                    
            ),

            call(writeIntLn,[finalResult])
        ]]."""
        expect = "20\n"
        self.assertTrue(TestVM.test(input, expect, 101))
    def test_complex_program_with_proc_and_control_flow(self):
        input = """[[
            const(limit, 5),
            var(i, integer), var(j, integer), var(sum, integer), var(temp, integer),
            var(flag, boolean)
        ],
        [
            func(addMul, [par(a, integer), par(b, integer)], integer, [
                assign(addMul, times(add(a, b), 2))
            ]),

            proc(processLoop, [par(x, integer)], [
                assign(i, 0),
                assign(sum, 0),
                while(bnot(eql(i, x)),
                    block([],[
                        if(eql(imod(i, 2), 0),
                            assign(temp, call(addMul, [i, x]))
                        ),
                        assign(sum, add(sum, temp)),
                        if(greater(sum, 50), break(null)),
                        assign(i, add(i, 1))
                    ])
                )
            ])
        ],
        [
            
            assign(j, 4),
            call(processLoop, [j]),
            call(writeIntLn, [sum]),
            assign(flag, greater(sum, 10)),
            if(flag,
                call(writeIntLn, [1]),
                call(writeIntLn, [0])
            ),
            do([
                assign(j, add(j, 1))
            ], less(j, 6))
        ]]."""
        expect = "40\n1\n"
        self.assertTrue(TestVM.test(input, expect, 202))



    # def test_undeclare_function(self):
    #         input = "[[const(a,1)],[],[call(writeInt,[1]),assign(a,1)]]."
    #         expect = ""
    #         self.assertTrue(TestVM.test(input, expect, 15))