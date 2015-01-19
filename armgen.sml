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

        fun cmp branch =
            case branch of
               T.EQ => "beq"
             | T.NE => "bne"
             | T.LT => "blt"
             | T.GT => "bgt"
             | T.LE => "ble"
             | T.GE => "bge"
             | _ => (print("armcodegen: impossible\n");"")

	    fun munchStm (T.SEQ(a,b)) =
	        (munchStm a; munchStm b)
          | munchStm (T.EXP exp) =
            (munchExp exp; ())
	      | munchStm (T.LABEL lab) =
	        emit(A.LABEL{assem=Symbol.name(lab) ^ ":\n", lab=lab})
          | munchStm (T.JUMP(exp,labelList)) =
            emit(A.OPER{assem="b `j0\n",src=[],dst=[],jump=SOME labelList})

            
			(* store a variable in a record field  e.g record.field := 7 *)
          | munchStm(T.MOVE(T.MEM(T.BINOP(T.PLUS,T.TEMP t,T.BINOP(T.MUL,T.CONST i1,
                     T.CONST i2))), e)) =
            emit(A.OPER{assem="str `d0, [`s0, #"^i2s(i1*i2)^"]"^"\n",
                        dst=[munchExp e], src=[t], jump=NONE})

            (* loading a record field, array index on a register *)
          | munchStm(T.MOVE(T.TEMP t1, T.MEM(T.BINOP(T.PLUS,T.TEMP t2,T.BINOP(T.MUL,
                     T.CONST i1, T.CONST i2))))) =
            emit(A.MOVE{assem="ldr `d0, [`s0, #"^i2s(i1*i2)^"]"^"\n", src=t2,
                        dst=t1})

            (* storing a variable in a record field, array index *)
          | munchStm(T.MOVE(T.MEM(T.BINOP(T.PLUS,T.TEMP t,T.CONST i)), e)) =
            emit(A.OPER{assem="str `s0, [`s1, #"^i2s(i)^"]"^"\n", src=[munchExp e, t],
                        dst=[], jump=NONE})

	    (* store a variable to memory 
          | munchStm(T.MOVE(T.TEMP t1, T.BINOP(T.PLUS, T.TEMP t2, T.CONST i))) =
            emit(A.OPER{assem="str `s0, [`s1, #" ^ Int.toString(i) ^ "]" ^ "\n", src=[t1,t2], 
			       dst=[], jump=NONE})
*)
(*
	  | munchStm(T.MOVE(T.TEMP t, T.BINOP(T.PLUS, T.TEMP t2, T.CONST i))) =
	    (case t2 of
	        (Frame.a1) => emit(A.OPER{assem="str `s0, [fp, #" ^ Int.toString(i) ^ "]" ^ "\n", src=[t], 
			dst=[], jump=NONE})
             | _ => ())
*)

            (* calling a function and storing the result on return value register *)
          | munchStm(T.MOVE(T.TEMP rv, T.CALL(T.NAME name,args))) =
            let 
            in
                (* force the usage of calling arguments with Frame.callargs *)
		 (emit(A.OPER{assem="str r0, [sp, #4]"^"\n", src=Frame.callargs, dst=[], jump=NONE});
		 emit(A.OPER{assem="str r1, [sp, #8]"^"\n", src=[Frame.a2], dst=[], jump=NONE});
		 emit(A.OPER{assem="str r2, [sp, #12]"^"\n", src=[Frame.a3], dst=[], jump=NONE});
		 emit(A.OPER{assem="str r3, [sp, #16]"^"\n", src=[Frame.a4], dst=[], jump=NONE});
                 emit(A.OPER{assem="bl "^Symbol.name(name)^"\n", src=munchArgs(0,args), dst=[], jump=NONE});
                 emit(A.MOVE{assem="mov `d0, `s0"^"\n", src=Frame.RV, dst=rv});
		 emit(A.OPER{assem="ldr r0, [sp, #4]"^"\n", src=[], dst=Frame.callargs, jump=NONE});
		 emit(A.OPER{assem="ldr r1, [sp, #8]"^"\n", src=[], dst=[Frame.a2], jump=NONE});
		 emit(A.OPER{assem="ldr r2, [sp, #12]"^"\n", src=[], dst=[Frame.a3], jump=NONE});
		 emit(A.OPER{assem="ldr r3, [sp, #16]"^"\n", src=[], dst=[Frame.a4], jump=NONE}))
            end
          | munchStm(T.MOVE(T.TEMP t1, T.TEMP t2)) =
            emit(A.MOVE{assem="mov `d0, `s0\n",src=t2, dst=t1})

          | munchStm(T.MOVE(T.TEMP t, e)) =
            emit(A.MOVE{assem="mov `d0, `s0\n", src=munchExp e, dst=t})

            (* storing expression to memory *)
          | munchStm(T.MOVE(T.MEM e1,e2)) =
            emit(A.OPER{assem="str `s0, [`s1]"^"\n", src=[munchExp e2, munchExp e1], 
                        dst=[], jump=NONE})

          | munchStm (T.MOVE(e1,e2)) =
            emit(A.MOVE{assem="ldr `d0, `s0\n", dst=munchExp e1, src=munchExp e2}) 

	    (*
          | munchStm(T.CJUMP(branch,e1,T.CONST i,lt,lf)) =
            emit(A.OPER{assem="cmp `s0, #" ^ Int.toString(i) ^ "\n" ^ (cmp branch) ^ " `j0\n",
            src=[munchExp e1], dst=[], jump=SOME [lt,lf]})
          | munchStm(T.CJUMP(branch,T.CONST i,e2,lt,lf)) =
            emit(A.OPER{assem="cmp `s0, #" ^ Int.toString(i) ^ "\n" ^ (cmp branch) ^ " `j0\n",
            src=[munchExp e2], dst=[], jump=SOME [lt,lf]})
*)
          | munchStm(T.CJUMP(branch,e1,e2,lt,lf)) =
            emit(A.OPER{assem="cmp `s0, `s1" ^ "\n" ^ (cmp branch) ^ " `j0\n",
            src=[munchExp e1,munchExp e2], dst=[], jump=SOME [lt,lf]})

	      | munchStm _ = raise TestStm "Out of statements\n"


	    and 
            munchExp (T.BINOP(T.MUL,T.CONST i1,T.CONST i2)) =
            result (fn r => emit(A.OPER{assem="ldr `d0, =" ^ i2s(i1*i2) ^ "\n",
                                src=[], dst=[r], jump=NONE}))
          | munchExp (T.BINOP(T.PLUS,T.CONST i1,T.CONST i2)) =
            result (fn r => emit(A.OPER{assem="ldr `d0, =" ^ i2s(i1+i2) ^ "\n",
                                src=[], dst=[r], jump=NONE}))
          | munchExp (T.BINOP(T.MINUS,T.CONST i1,T.CONST i2)) =
            result (fn r => emit(A.OPER{assem="ldr `d0, =" ^ i2s(i1-i2) ^ "\n",
                                src=[], dst=[r], jump=NONE}))
          | munchExp (T.BINOP(T.DIV,T.CONST i1,T.CONST i2)) =
            result (fn r => emit(A.OPER{assem="ldr `d0, =" ^ i2s(i1 div i2) ^ "\n",
                                src=[], dst=[r], jump=NONE}))
          | munchExp (T.BINOP(T.PLUS,e1,e2)) =
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
            let 
                val t = Temp.newtemp()
            in
                (* force the usage of calling arguments with Frame.callargs *)
             result (fn r =>
		 (emit(A.OPER{assem="str r0, [sp, #4]"^"\n", src=Frame.callargs, dst=[], jump=NONE});
		 emit(A.OPER{assem="str r1, [sp, #8]"^"\n", src=[Frame.a2], dst=[], jump=NONE});
		 emit(A.OPER{assem="str r2, [sp, #12]"^"\n", src=[Frame.a3], dst=[], jump=NONE});
		 emit(A.OPER{assem="str r3, [sp, #16]"^"\n", src=[Frame.a4], dst=[], jump=NONE});
                 emit(A.OPER{assem="bl "^Symbol.name(name)^"\n", src=munchArgs(0,args), dst=[], jump=NONE});
                 emit(A.MOVE{assem="mov `d0, `s0"^"\n", src=Frame.RV, dst=r});
		 emit(A.OPER{assem="ldr r0, [sp, #4]"^"\n", src=[], dst=Frame.callargs, jump=NONE});
		 emit(A.OPER{assem="ldr r1, [sp, #8]"^"\n", src=[], dst=[Frame.a2], jump=NONE});
		 emit(A.OPER{assem="ldr r2, [sp, #12]"^"\n", src=[], dst=[Frame.a3], jump=NONE});
		 emit(A.OPER{assem="ldr r3, [sp, #16]"^"\n", src=[], dst=[Frame.a4], jump=NONE}))
                 )
            end
          | munchExp(T.NAME n) =
            result (fn r => emit(A.OPER{assem="adr `d0, "^Symbol.name(n)^"\n", src=[], dst=[r], jump=NONE}))

             (* loading a record field in a register *)
          | munchExp(T.MEM(T.BINOP(T.PLUS,T.TEMP t,T.BINOP(T.MUL,T.CONST i1,T.CONST i2)))) =
            result (fn r => emit(A.MOVE{assem="ldr `d0, [`s0, #"^i2s(i1*i2)^"]"^"\n", 
                                        src=t, dst=r}))
          | munchExp(T.MEM(T.BINOP(T.PLUS,e,T.CONST i))) =
            result (fn r => emit(A.MOVE{assem="ldr `d0, [`s0, #"^i2s(i)^"]"^"\n", 
                                        src=munchExp e, dst=r}))
          | munchExp(T.MEM e) =
            result (fn r => emit(A.MOVE{assem="ldr `d0, [`s0]"^"\n", src=munchExp e, dst=r}))
	      | munchExp (T.TEMP t) = t

	      | munchExp _ = raise TestExp "Out os expressions\n"

       and munchArgs (i,args) =
           let val n = Frame.name frame
               val (escs,_) = Frame.getEsc n
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
               fun regs (esclst,i) =
                  case esclst of
                      e::nil => emit(A.OPER{assem="str r"^i2s(e)^", [sp, #"^Int.toString(i)^"]"^"\n", src=[], dst=[], jump=NONE})
                    | e::es => (emit(A.OPER{assem="str r"^i2s(e)^", [sp, #"^Int.toString(i)^"]"^"\n", src=[], dst=[], jump=NONE}); 
                                regs(es,i+4))
                    | nil => ()

               val nl = iter(0,args)
               val regstr = regs((!esc),4)
           in nl end
	in
	    munchStm stm;
	    rev(!ilist)
	end
end
