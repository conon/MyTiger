structure ArmFrame : FRAME =
struct
    structure A = Assem
    structure T = Tree
    type register = int
    type registerStr = string
    datatype access = InFrame of int
                    | InReg of Temp.temp
    type frame = {name : Temp.label, formals : access list, locals : int ref}
    (* escapes will be used to hold all escaping variables of a calling function *)
    datatype frag = PROC of {body: Tree.stm, frame: frame}
                  | STRING of Temp.label * string
    val wordSize = 4 (* arm word is 4 bytes *)
    val fpsub = 4 (* local are subtracted with an additional 4 bytes in order to not
                     overwrite the initial fp subtract *)

    val registersStr = ["r0","r1","r2","r3","r4","r5","r6","r7",
                     "r8","r9","r10","fp","r12","sp","lr","pc"]
                     
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
    val v8 = Temp.newtemp() (* frame pointer *)
    val IP = Temp.newtemp() (* Intra-Procedure-call scratch register *)
    val SP = Temp.newtemp() (* stack pointer *)
    val LR = Temp.newtemp() (* link register *)
    val PC = Temp.newtemp() (* program counter *)

    val RV = a1 (* return value *)
    val FP = v8 

    (* register lists *)
    val callesaves = [v1,v2,v3,v4,v5,v6,v7,v8,IP]
    val callersaves = [a1,a2,a3,a4]
    val specialregs = [SP,LR,PC]

    val K = 4 (* the first k formals go to registers *)
    val L = length(callesaves) - K (* the remaining registers are used for local variables *)


    val registerTemps = [a1,a2,a3,a4,v1,v2,v3,v4,
                         v5,v6,v7,v8,IP,SP,LR,PC]
    val preRegisterTemps = [a1,a2,a3,a4,SP,LR,PC]
    val registers = [0,1,2,3,4,5,6,7,8,9,10,12] 
    val registers' = [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15]

    val tempMap = List.foldl (fn ((key,value), table) => Temp.Table.enter(table,key,value)) Temp.Table.empty
                                                            (ListPair.zip(registerTemps,registers'))

    val tempMapStr = List.foldl (fn ((key,value), table) => Temp.Table.enter(table,key,value)) Temp.Table.empty
                                                            (ListPair.zip(registerTemps,registersStr))

    val callargs = [a1,a2,a3,a4]

    fun newFrame {name : Temp.label, formals : bool list} =
    (*app (fn f => print((Bool.toString f) ^ " ")) formals; print "\nfinish\n";*)
        let fun formalsiter (fs,offset,counter,args) =
	       case (fs,args) of
	           (f::fs,a::args) => (case f of
                         true => InFrame (offset * wordSize) :: formalsiter(fs,offset+1,counter,a::args)
                       | false => InReg (a) :: formalsiter(fs,offset,counter+1,args))
             | (nil,nil) => nil
             | (f::fs,nil) => InFrame (offset * wordSize) :: formalsiter(fs,offset+1,counter+1,nil)
             | (nil,args) => nil
        in
            (*app (fn a => case a of 
                               InFrame k => (print ("FRAME " ^ (Int.toString k) ^ "\n"))
                             | InReg r => (print ("REGISTER" ^ (Int.toString r) ^ "\n"))) ff; *)
            {name=name, formals=formalsiter(formals,0,0,callargs), locals=ref 0}
        end
    
    fun name {name = n, formals = _, locals=_} = n
    
    fun formals {formals = f, name=_, locals=_} = f

    fun allocLocal {name =_, formals =_, locals = l} var =
    (* in case of escaping locals goes to lower addresses *)
	        case var of
                false => InReg (Temp.newtemp())
              | true => (l := !l+1; InFrame (~(!l) * wordSize - fpsub))

    val esc : (Temp.label * int list * int) list ref = ref [(Temp.namedlabel("_start"),nil,0)]

    fun getEsc name = 
        let val res = List.find (fn (n,f,l) => if name = n then true else false) (!esc)
        in 
            case res of
               SOME (n,f,l) => (f,l)
             | NONE => (nil,0)
        end

    fun exp facc fp =
        case facc of
            InFrame k => Tree.MEM(Tree.BINOP(Tree.PLUS, fp, Tree.CONST k))
          | InReg t => Tree.TEMP t

    fun externalCall (s,args) =
        let val _ = if K > List.length(args) 
                    then ()
                    else print("Error: externalCall in armframe.sml: "^s^"\n")
        in Tree.CALL(Tree.NAME(Temp.namedlabel(s^"T")), args) end

    fun makeseq nil =
        Tree.EXP(Tree.CONST 0)
      | makeseq [exp] =
        exp
      | makeseq (exp::exps) = Tree.SEQ(exp,(makeseq exps))

    fun findEscArgs (callingframe,parentframe) =
        let val cfls = formals callingframe
            val n = name callingframe
            val locals = case parentframe of
                             {locals,...} : frame => !locals
            (*val _ = print("LENGTH ARGS :"^Int.toString(length(cfls))^"\n")*)
            (*val _ = print("LENGTH LOCALS :"^Int.toString(locals)^"\n")*)
            fun iter (fs,i) =
               case fs of
                   f::fs =>(case f of
                                 InReg r => iter(fs,i+1)
                               | InFrame m => if i = 0
                                              then iter(fs,i+1) (* ignore static link *)
                                              else (print("find: "^Int.toString(i)^"\n");
                                                    (i-1)::iter(fs,i+1)))
                 | nil => nil
            val e = iter(cfls,0)
        in esc := (n,e,locals)::(!esc) end
                        
        

    fun procEntryExit1(frame,body) =
        T.MOVE(T.TEMP RV, body)   (* Evaluate body and store the result to the Return Value register *)


    fun procEntryExit2(frame,body) =
        body @ [A.OPER{assem="", src=specialregs@callesaves, dst=[], jump=SOME []}]


    fun procEntryExit3(fr as {name=n,formals=f,locals=l},instrs) =
        let val (_,numlocals) = getEsc n
            val numlocals = numlocals * wordSize + fpsub
(*
            val _ = print("NAME: "^Symbol.name(n)^"\n")
            val _ = print("NUMLOCALS: "^Int.toString(numlocals)^"\n")
*)
            val n' = Symbol.name(n)
            val startProlog = ".global tigermain\n\n"^
                              ".text\n\n"^
                              ".type "^n'^", %function"^"\n"^
                               n'^":"^"\n"^
                              "stmfd sp!, {fp, lr}"^"\n"^
                              "add fp, sp, #4"^"\n"^
                              "sub sp, sp, #80\n"^ (* TODO: sub size, dummy *)
                              "str fp, [sp]"^"\n"
            val startEpilog = "sub sp, fp, #4"^"\n"^
                              "ldmfd sp!, {fp, pc}"^"\n"
            val funProlog =  n'^":\n"^
                            "sub fp, fp, #"^Int.toString(numlocals)^"\n"^
                            "stmdb fp, {r4-r10,r12,lr}\n"^
                            "mov fp, sp\n"^
                            "sub sp, sp, #80\n"^
                            "str fp, [sp]\n"
           val funEpilog = "mov sp, fp\n"^
                           "ldr fp, [sp]\n"^
                           "sub r3, fp, #"^Int.toString(numlocals)^"\n"^
                           "ldmdb r3, {r4-r10,r12,pc}\n"
           val sn = Symbol.symbol "tigermain"
        in
            if n = sn
            then {prolog=startProlog, body=instrs, epilog=startEpilog}
            else {prolog=funProlog, body=instrs, epilog=funEpilog}
        end
        


    fun string (lab,s) = 
        let val len = Int.toString(String.size s)
			(*
			val _ = print("Len of string: " ^ len ^ "\n")
			val _ = print("NAME:" ^ s ^ "\n")
			*)
        in
            Symbol.name(lab)^":"^"\n"^
            ".word "^len^"\n"^
            ".ascii "^"\""^s^"\""^"\n"^
            ".align 4"^"\n"
        end

end


structure Frame : FRAME = ArmFrame
