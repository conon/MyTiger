structure Arm : CODEGEN =
struct
    structure A = Assem
    structure T = Tree
    structure Frame = ArmFrame
    exception TestStm of string
    exception TestExp of string

    fun i2s i =
        if i < 0 then "-" ^ Int.toString (~i) else Int.toString i

    fun codegen frame (stm: Tree.stm) : Assem.instr list =
        let val ilist = ref (nil: A.instr list)
            val esc = ref nil
	    fun emit x = 
	        ilist := x :: (!ilist)
        fun result gen =
	        let val t = Temp.newtemp()
		    in gen t;
		       t
		    end

	    fun munchStm (T.SEQ(a,b)) =
	        (munchStm a; munchStm b)
          | munchStm (T.EXP exp) =
            (munchExp exp; ())
	      | munchStm (T.LABEL lab) =
	        emit(A.LABEL{assem=Symbol.name(lab) ^ ":\n", lab=lab})
          | munchStm (T.JUMP(exp,labelList)) =
            emit(A.OPER{assem="b `j0\n",src=[],dst=[],jump=SOME labelList})

            
          | munchStm(T.MOVE(T.MEM(T.BINOP(T.PLUS,T.TEMP t,T.BINOP(T.MUL,T.CONST i1,
                     T.CONST i2))), e)) =
            emit(A.OPER{assem="str `s0, [`d0, #"^i2s(i1*i2)^"]"^"\n",
                        src=[munchExp e], dst=[t], jump=NONE})
          | munchStm(T.MOVE(T.MEM(T.BINOP(T.PLUS,T.TEMP t,T.CONST i)), e)) =
            emit(A.OPER{assem="str `s0, [`d0, #"^i2s(i)^"]"^"\n", 
                        src=[munchExp e], dst=[t], jump=NONE})
          | munchStm(T.MOVE(T.TEMP rv, T.CALL(T.NAME name,args))) =
            (* NOTE rv is not used *)
            let val a = munchArgs(0,args)
            in
                (* force the usage of calling arguments with Frame.callargs *)
                (emit(A.OPER{assem="bl "^Symbol.name(name)^"\n", src=Frame.callargs, dst=[], jump=NONE});
                 emit(A.MOVE{assem="mov `d0, `s0"^"\n", src=Frame.RV, dst=rv}))
            end
          | munchStm(T.MOVE(T.TEMP t1, T.TEMP t2)) =
            emit(A.MOVE{assem="mov `d0, `s0\n",src=t2, dst=t1})
          | munchStm(T.MOVE(T.TEMP t, e)) =
            emit(A.MOVE{assem="mov `d0, `s0\n", src=munchExp e, dst=t})
          | munchStm(T.MOVE(T.MEM e1,e2)) =
            emit(A.MOVE{assem="str `s0, `d0, #4\n", src=munchExp e2, dst=munchExp e1})
          | munchStm (T.MOVE(e1,e2)) =
            emit(A.MOVE{assem="ldr `d0, `s0\n", dst=munchExp e1, src=munchExp e2}) 


          | munchStm(T.CJUMP(T.EQ,e1,e2,lt,lf)) =
            emit(A.OPER{assem="cmp `s0, `s1" ^ "\n" ^ "beq `j0\n",
            src=[munchExp e1,munchExp e2], dst=[], jump=SOME [lt,lf]})
          | munchStm(T.CJUMP(T.NE,e1,e2,lt,lf)) =
            emit(A.OPER{assem="cmp `s0, `s1" ^ "\n" ^ "bne `j0\n",
            src=[munchExp e1,munchExp e2], dst=[], jump=SOME [lt,lf]})
          | munchStm(T.CJUMP(T.LT,e1,e2,lt,lf)) =
            emit(A.OPER{assem="cmp `s0, `s1" ^ "\n" ^ "blt `j0\n",
            src=[munchExp e1,munchExp e2], dst=[], jump=SOME [lt,lf]})
          | munchStm(T.CJUMP(T.GT,e1,e2,lt,lf)) =
            emit(A.OPER{assem="cmp `s0, `s1" ^ "\n" ^ "bgt `j0\n",
            src=[munchExp e1,munchExp e2], dst=[], jump=SOME [lt,lf]})
          | munchStm(T.CJUMP(T.LE,e1,e2,lt,lf)) =
            emit(A.OPER{assem="cmp `s0, `s1" ^ "\n" ^ "ble `j0\n",
            src=[munchExp e1,munchExp e2], dst=[], jump=SOME [lt,lf]})
          | munchStm(T.CJUMP(T.GE,e1,e2,lt,lf)) =
            emit(A.OPER{assem="cmp `s0, `s1" ^ "\n" ^ "bge `j0\n",
            src=[munchExp e1,munchExp e2], dst=[], jump=SOME [lt,lf]})
	      | munchStm _ = raise TestStm "Out of statements\n"


	    and munchExp (T.BINOP(T.PLUS,e1,e2)) =
            result (fn r => emit(A.OPER{assem="add `d0, `s0, `s1\n",
                                src=[munchExp e1,munchExp e2], dst=[r], jump=NONE}))
          | munchExp (T.BINOP(T.MINUS,e1,e2)) =
            result (fn r => emit(A.OPER{assem="sub `d0, `s0, `s1\n",
                                src=[munchExp e1,munchExp e2], dst=[r], jump=NONE}))
          | munchExp (T.BINOP(T.MUL,e1,e2)) =
            result (fn r => emit(A.OPER{assem="mul `d0, `s0, `s1\n",
                                src=[munchExp e1,munchExp e2], dst=[r], jump=NONE}))
          | munchExp (T.BINOP(T.DIV,e1,e2)) =
            result (fn r => emit(A.OPER{assem="sdiv `d0, `s0, `s1\n",
                                src=[munchExp e1,munchExp e2], dst=[r], jump=NONE}))
          | munchExp (T.CONST i) =
            result (fn r => emit(A.OPER{assem="ldr `d0, =" ^ i2s(i) ^ "\n", 
                                src=[], dst=[r], jump=NONE}))
          | munchExp(T.CALL(T.NAME name,args)) =
             (* NOTE r is not used *)
            let val a = munchArgs(0,args)
            in
                (* force the usage of calling arguments with Frame.callargs *)
                result (fn r => emit(A.OPER{assem="bl "^Symbol.name(name)^"\n", src=Frame.callargs, dst=[], jump=NONE}))
            end
          | munchExp(T.NAME n) =
            result (fn r => emit(A.OPER{assem="adr `d0, "^Symbol.name(n)^"\n", src=[], dst=[r], jump=NONE}))
          | munchExp(T.MEM(T.BINOP(T.PLUS,e,T.CONST i))) =
            result (fn r => emit(A.MOVE{assem="ldr `d0, [`s0, #"^i2s(i)^"]"^"\n", 
                                        src=munchExp e, dst=r}))
          | munchExp(T.MEM e) =
            result (fn r => emit(A.MOVE{assem="ldr `d0, [`s0]"^"\n", src=munchExp e, dst=r}))
	      | munchExp (T.TEMP t) = t

	      | munchExp _ = raise TestExp "Out os expressions\n"

       and munchArgs (i,args) =
           let val (escs,_) = Frame.getEsc()
               val esc = ref escs
               fun iter (i,args) =
               let val r = List.nth(Frame.registerTemps,i)
                   (*val _ = print("TEST register: "^Temp.makestring(r)^"\n")*)
               in
                  case args of
                      arg::args => let val res = r::iter(i+1,args) 
                                       val _ = emit(A.MOVE{assem="mov `d0, `s0"^"\n", 
                                              src=munchExp arg, dst=r})
                                       val t1 = i >= Frame.K
                                       val t2 = List.exists (fn n => if n = i then true else false) (!esc)
                                   in
                                       if t1 andalso not t2
                                       then (esc := i::(!esc);res)
                                       else res
                                   end
                    | nil => nil
               end
               fun regs esclst =
                  case esclst of
                      e::nil => emit(A.OPER{assem="str r"^i2s(e)^", [sp, #4]"^"\n", src=[], dst=[], jump=NONE})
                    | e::es => (emit(A.OPER{assem="str r"^i2s(e)^", [sp, #4]"^"\n", src=[], dst=[], jump=NONE}); 
                                regs(es))
                    | nil => ()

               val nl = iter(0,args)
               val regstr = regs (!esc)
           in nl end
	in
	    munchStm stm;
	    rev(!ilist)
	end
end
