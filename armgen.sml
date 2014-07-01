structure Arm : CODEGEN =
struct
    structure A = Assem
    structure T = Tree
    structure Frame = ArmFrame
    exception TestStm of string
    exception TestExp of string
    fun codegen frame (stm: Tree.stm) : Assem.instr list =
        let val ilist = ref (nil: A.instr list)
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
          | munchStm (T.MOVE(e1,e2)) =
            emit(A.MOVE{assem="ldr 'd0, 's0\n", dst=munchExp e1, src=munchExp e2}) 
          | munchStm (T.EXP exp) =
            (munchExp exp; ())
          | munchStm(T.CJUMP(T.GT,e1,e2,lt,lf)) =
            emit(A.OPER{assem="cmp 's0, 's1" ^ "\n" ^ "bge `j0\n",
            src=[munchExp e1,munchExp e2], dst=[], jump=SOME [lt,lf]})
	      | munchStm _ = raise TestStm "Out of statements\n"

	    and munchExp (T.BINOP(T.PLUS,e1,e2)) =
            result (fn r => emit(A.OPER{assem="add 'd0, 's0, 's1\n",
                                src=[munchExp e1,munchExp e2], dst=[r], jump=NONE}))
          | munchExp (T.BINOP(T.MINUS,e1,e2)) =
            result (fn r => emit(A.OPER{assem="sub 'd0, 's0, 's1\n",
                                src=[munchExp e1,munchExp e2], dst=[r], jump=NONE}))
          | munchExp (T.BINOP(T.MUL,e1,e2)) =
            result (fn r => emit(A.OPER{assem="mul 'd0, 's0, 's1\n",
                                src=[munchExp e1,munchExp e2], dst=[r], jump=NONE}))
          | munchExp (T.BINOP(T.DIV,e1,e2)) =
            result (fn r => emit(A.OPER{assem="sdiv 'd0, 's0, 's1\n",
                                src=[munchExp e1,munchExp e2], dst=[r], jump=NONE}))
          | munchExp (T.CONST i) =
            result (fn r => emit(A.OPER{assem="ldr 'd0, =" ^ Int.toString i ^ "\n", 
                                src=[], dst=[r], jump=NONE}))
	      | munchExp (T.TEMP t) = t

	      | munchExp _ = raise TestExp "Out os expressions\n"
	     
	in
	    munchStm stm;
	    rev(!ilist)
	end
end
