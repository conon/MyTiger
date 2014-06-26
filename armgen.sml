structure ARM : CODEGEN =
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
	      | munchStm (T.MOVE(T.MEM(T.BINOP(T.PLUS,e1,T.CONST i)),e2)) =
	        emit(A.OPER{assem="STORE M['s0+" ^ Int.toString i ^ "] <- 's1\n",
		            src=[munchExp e1, munchExp e2],
			    dst=[], jump=NONE})
	      | munchStm (T.MOVE(T.MEM(T.BINOP(T.PLUS,T.CONST i,e1)),e2)) =
	        emit(A.OPER{assem="STORE M['s0+" ^ Int.toString i ^ "] <- 's1\n",
		            src=[munchExp e1, munchExp e2],
			    dst=[], jump=NONE})
	      | munchStm (T.MOVE(T.MEM(e1),T.MEM(e2))) =
	        emit(A.OPER{assem="MOVE M['s0] <- M['s1]\n",
		            src=[munchExp e1, munchExp e2],
			    dst=[], jump=NONE})
	      | munchStm (T.MOVE(T.MEM(T.CONST i), e2)) = 
	        emit(A.OPER{assem="STORE M[r0+" ^ Int.toString i ^ "] <- 's0\n",
		            src=[munchExp e2],
			    dst=[], jump=NONE})
	      | munchStm (T.MOVE(T.MEM(e1), e2)) =
	        emit(A.OPER{assem="STORE M['s0] <- s1\n",
		            src=[munchExp e1, munchExp e2],
			    dst=[], jump=NONE})
	      | munchStm (T.MOVE (T.TEMP i, e2)) =
	        emit(A.OPER{assem="ADD 'd0 <- 's0 + r0\n",
		            src=[munchExp e2],
			    dst=[i], jump=NONE})
	      | munchStm (T.LABEL lab) =
	        emit(A.LABEL{assem=Symbol.name(lab) ^ ":\n", lab=lab})
          | munchStm (T.EXP exp) =
            (munchExp exp; ())
	      | munchStm _ = raise TestStm "Out of statements\n"

	    and munchExp (T.MEM(T.BINOP(T.PLUS,e1,T.CONST i))) =
	        result(fn r => emit(A.OPER{assem="LOAD 'd0 <- M['s0+" ^ Int.toString i ^ "]\n",
		                         src=[munchExp e1],
					 dst=[r], jump=NONE}))
	      | munchExp (T.MEM(T.BINOP(T.PLUS,T.CONST i,e1))) =
	        result(fn r => emit(A.OPER{assem="LOAD 'd0 <- M['s0+" ^ Int.toString i ^ "]\n",
		                           src=[],
					   dst=[r], jump=NONE}))
	      | munchExp (T.MEM(T.CONST i)) =
	        result(fn r => emit(A.OPER{assem="LOAD 'd0 <- M[r0+" ^ Int.toString i ^ "]\n",
		                           src=[],
					   dst=[r], jump=NONE}))
	      | munchExp (T.MEM(e1)) =
	        result(fn r => emit(A.OPER{assem="LOAD 'd0 <- M['s0+0]\n",
		                           src=[munchExp e1],
					   dst=[r], jump=NONE}))
	      | munchExp (T.BINOP(T.PLUS,e1,T.CONST i)) =
	        result(fn r => emit(A.OPER{assem="ADDI 'd0 <- 's0+" ^ Int.toString i ^ "\n",
		                           src=[munchExp e1],
					   dst=[r], jump=NONE}))
	      | munchExp (T.CONST i) =
	        result(fn r => emit(A.OPER{assem="ADDI 'd0 <- r0+" ^ Int.toString i ^ "\n",
		                           src=[],
					   dst=[r],
					   jump=NONE}))
	      | munchExp (T.BINOP(T.PLUS,e1,e2)) =
	        result(fn r => emit(A.OPER{assem="ADD 'd0 <- 's0+'s1\n",
		                           src=[munchExp e1, munchExp e2],
					   dst=[r], jump=NONE}))
	      | munchExp (T.TEMP t) = t

	      | munchExp _ = raise TestExp "Out os expressions\n"
	     
	in
	    munchStm stm;
	    rev(!ilist)
	end
end
