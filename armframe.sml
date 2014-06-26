structure ArmFrame : FRAME =
struct
    structure A = Assem
    type register = string
    datatype access = InFrame of int
                    | InReg of Temp.temp
    type frame = {name : Temp.label, formals : access list, locals : int ref}
    datatype frag = PROC of {body: Tree.stm, frame: frame}
                  | STRING of Temp.label * string
    val wordSize = 4 (* arm word is 4 bytes *)
    val k = 4 (* the first k formals go to registers *)

    val registers = ["a1","a2","a3","a4","v1","v2","v3","v4",
                     "v5","v6","v7","FP","IP","SP","LR","PC"]
                     
    val a1 = Temp.newtemp() (* argument/result scratch register  *)
    val a2 = Temp.newtemp() (* argument/result scratch register *)
    val a3 = Temp.newtemp() (* argument/scratch register *)
    val a4 = Temp.newtemp() (* argument/scratch register *)
    val v1 = Temp.newtemp() (* variable register *)
    val v2 = Temp.newtemp() (* variable register *)
    val v3 = Temp.newtemp() (* variable register *)
    val v4 = Temp.newtemp() (* variable register *)
    val v5 = Temp.newtemp() (* variable register *)
    val v6 = Temp.newtemp() (* variable register  *)
    val v7 = Temp.newtemp() (* variable register *)
    val FP = Temp.newtemp() (* frame pointer *)
    val IP = Temp.newtemp() (* Intra-Procedure-call scratch register *)
    val SP = Temp.newtemp() (* stack pointer *)
    val LR = Temp.newtemp() (* link register *)
    val PC = Temp.newtemp() (* program counter *)

    val RV = a1 (* return value *)

    val registerTemps = [a1,a2,a3,a4,v1,v2,v3,v4,
                         v5,v6,v7,FP,IP,SP,LR,PC]

    val tempMap = List.foldl (fn ((key, value), table) => Temp.Table.enter(table, key, value))
                             Temp.Table.empty (ListPair.zip(registerTemps, registers))

    (* register lists *)
    val argregs = [a1,a2,a3,a4]
    val callesaves = [v1,v2,v3,v4,v4,v5,v6,v7]
    val callersaves = [a1,a2,a3,a4]
    val specialregs = [SP,IP,SP,LR,PC]

    fun newFrame {name : Temp.label, formals : bool list} =
        let fun formalsiter (fs,offset,counter) =
	       case fs of
	           f::fs => (case f of
		                     true => InFrame (offset * wordSize) :: formalsiter(fs,offset+1,counter)
			               | false => if counter > k
			                          then InFrame (offset * wordSize) :: formalsiter(fs,offset+1,counter+1)
					                  else InReg (Temp.newtemp()) :: formalsiter(fs,offset,counter+1))
             | nil => nil
	 in
	     {name=name, formals=formalsiter(List.rev formals,0,0), locals=ref 0}
	 end
    
    fun name {name = n, formals = _, locals=_} = n
    
    fun formals {formals = f, name=_, locals=_} = f

    fun allocLocal frame var =
        let fun accessize (f,offset) =
	        case f of
		    true => InFrame (offset * wordSize)
		  | false => InReg (Temp.newtemp())
        in
            case frame of
	        {name =_, formals =_, locals = l} => let val l' =  ~(!l) (* in case of escaping locals goes to lower addresses *)
	                                                 val res = accessize(var, l'-1)
	                                                 val _ = l := !l+1
	        	                              in res end
	end

    fun exp facc fp =
        case facc of
            InFrame k => (print "INFRAME\n";Tree.MEM(Tree.BINOP(Tree.PLUS, fp, Tree.CONST k)))
          | InReg t => (print "INREG\n";Tree.TEMP t)

    fun externalCall (s,args) =
        Tree.CALL(Tree.NAME(Temp.namedlabel s), args)

    fun makeseq nil =
        Tree.EXP(Tree.CONST 0)
      | makeseq [exp] =
        exp
      | makeseq (exp::exps) = Tree.SEQ(exp,(makeseq exps))
    
    fun procEntryExit1 (frame,body) =  body (* TODO: dummy *) 

    fun string (lab,s) = "" (* TODO: dummy *)

    (*
    fun mapspecialregs temp =
        case Temp.Table.look(tempMap, temp) of
	    SOME v => ""
	  | NONE => "" *)

    (*
    fun procEntryExit2 (frame,body) =
        body @ [A.OPER{assem="",
	               src=[ZERO,RA,SP]@calleesaves,
		       dst=[], jump=SOME[]}]
               *)
end


structure Frame : FRAME = ArmFrame
