structure Arm : CODEGEN =
struct
    structure A = Assem
    structure T = Tree
    structure Frame = ArmFrame
    exception TestStm of string
    exception TestExp of string
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
	      | munchStm (T.LABEL lab) =
	        emit(A.LABEL{assem=Symbol.name(lab) ^ ":\n", lab=lab})
          | munchStm (T.JUMP(exp,labelList)) =
            emit(A.OPER{assem="b `j0\n",src=[],dst=[],jump=SOME labelList})
          | munchStm(T.MOVE(T.TEMP t, e)) =
            emit(A.MOVE{assem="mov `d0, `s0\n", src=munchExp e, dst=t})
          | munchStm(T.MOVE(T.MEM e1,e2)) =
            emit(A.MOVE{assem="str `s0, `d0, #4\n", src=munchExp e2, dst=munchExp e1})
          | munchStm (T.MOVE(e1,e2)) =
            emit(A.MOVE{assem="ldr `d0, `s0\n", dst=munchExp e1, src=munchExp e2}) 
          | munchStm (T.EXP exp) =
            (munchExp exp; ())
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
            result (fn r => emit(A.OPER{assem="ldr `d0, =" ^ Int.toString i ^ "\n", 
                                src=[], dst=[r], jump=NONE}))
          | munchExp(T.CALL(name,args)) =
            let val a = munchArgs(0,args)
                val n = munchExp name
            in
            result (fn r => emit(A.OPER{assem="",
                  src=a,dst=[], jump=NONE}))
            end
          | munchExp(T.NAME n) =
            result (fn r => emit(A.LABEL{assem="b " ^ Symbol.name(n)^"\n", lab=n}))
          | munchExp(T.MEM e) =
            result (fn r => emit(A.MOVE{assem="ldr `d0, `s0\n", src=munchExp e, dst=r}))
	      | munchExp (T.TEMP t) = t

	      | munchExp _ = raise TestExp "Out os expressions\n"

       and munchArgs(i,args) =
           let fun iter(i,args)  =
                  case args of
                      arg::args => if i >= Frame.K
                                   then (esc := i::(!esc);
                                         emit(A.MOVE{assem="mov r"^Int.toString(i)^", `s0\n", 
                                                     src=munchExp arg, dst=i});
                                         i::iter(i+1,args))
                                   else (emit(A.MOVE{assem="mov r"^Int.toString(i)^", `s0\n", 
                                                src=munchExp arg, dst=i});
                                        i::iter(i+1,args))
                   | nil => nil
              fun regs esclst =
                  case esclst of
                      e::nil => "r"^Int.toString(e)
                    | e::es => "r"^Int.toString(e)^","^regs(es)
                    | nil => ""
              val nl = iter(i,args)
              val regstr = regs(!esc)
              val _ = if List.null(!esc) then () else emit(A.OPER{assem="stmia sp, {"^regstr^"}\n", src=[], dst=[], jump=NONE})
          in nl end
	in
	    munchStm stm;
	    rev(!ilist)
	end
end
