structure Translate : TRANSLATE =
struct
structure A = Absyn
structure T = Tree

exception ErrorAlloc 
exception CxToNx 
exception StartLevelExc
exception LabelCaseExc

datatype level = Level of Frame.frame * level * unit ref (* Actual frame, the parent frame, every new level is unique *)
       | StartLevel
type access = level * Frame.access
datatype exp = Ex of Tree.exp
             | Nx of Tree.stm
	     | Cx of Temp.label * Temp.label -> Tree.stm

val outermost = StartLevel

val frags : Frame.frag list ref = ref nil
		    
fun unEx (Ex e) = e
  | unEx (Cx genstm) =
    let val r = Temp.newtemp()
	val t = Temp.newlabel() and f = Temp.newlabel()
    in T.ESEQ(T.SEQ(T.MOVE(T.TEMP r, T.CONST 1),
	      T.SEQ(genstm(t,f),
	      T.SEQ(T.LABEL f,
	      T.SEQ(T.MOVE(T.TEMP r, T.CONST 0),
	            T.LABEL t)))),
	      T.TEMP r)
    end
  | unEx (Nx s) = T.ESEQ(s, T.CONST 0)

fun unNx (Nx s) = s
  | unNx (Ex e) = T.EXP(e)
  | unNx (Cx genstm) =
    let val r = Temp.newtemp()
	val t = Temp.newlabel() and f = Temp.newlabel()
    in T.SEQ(T.MOVE(T.TEMP r, T.CONST 1),
       T.SEQ(genstm(t,f),
       T.SEQ(T.LABEL f,
       T.SEQ(T.MOVE(T.TEMP r, T.CONST 0),
             T.LABEL t))))
    end

fun unCx (Cx c) = c
  | unCx (Ex (T.CONST 0)) = (fn (t,f) => T.JUMP(T.NAME f, [f]))
  | unCx (Ex (T.CONST 1)) = (fn (t,f) => T.JUMP(T.NAME t, [t]))
  | unCx (Ex e) = (fn (t,f) => T.CJUMP(T.NE, e, T.CONST 0, t, f))
  | unCx (Nx _) = raise CxToNx

fun printTree exp =
    Printtree.printtree (TextIO.stdOut,unNx(exp))

val dummy = Ex (T.CONST 0)

fun getResult() =
    !frags

fun newLevel {parent=lev, name=n, formals=f} =
    let val fr = Frame.newFrame({name=n, formals= (true::f)}) (* this extra true is the static link of the frame*)
    in
	    Level (fr, lev, ref ())
    end

fun formals lev =
    case lev of
	    Level(fr,_,_) => let val formals = Frame.formals fr
                             fun aux fs =
                                case fs of
                                    f::fs => (lev,f)::aux(fs)
                                  | nil => nil
                             val acc = aux formals
                         in tl(acc) end (* drop the static link *)
      | StartLevel => raise StartLevelExc

fun allocLocal lev esc =
    let val fracc = case lev of
	                Level(fr,_,_) => Frame.allocLocal fr esc
		      | StartLevel => raise ErrorAlloc
    in (lev,fracc) end

fun getStaticLink lev =
    case lev of
        Level (fr,_,_) => let val fracc = hd (Frame.formals fr)
	                  in fracc end
      | StartLevel => raise ErrorAlloc

fun makeExps (explst,exp) =
    let fun iter (exp'::exps') =
            T.SEQ(unNx exp', iter(exps'))
          | iter nil =
            unNx exp
    in Nx (iter(explst)) end

fun makeExp explst =
    let fun iter (exp::nil) =
            unNx exp
          | iter (exp::exps) =
            T.SEQ(unNx exp, iter(exps))
          | iter nil =
            T.EXP(T.CONST 0)
    in Nx (iter(explst)) end

fun concatExps (s,e) =
    Ex (T.ESEQ(unNx s,unEx e))
         
fun constIntVar i =
    Ex (T.CONST i)

(*
fun simpleVar (access, currentlevel) =
    let val (varlevel,varframeaccess) = access
        val fp = Tree.TEMP(Frame.FP)
        fun calcaddr curlev =
            let val (varframe,varparentlevel,varframeid) = case curlev of 
                                                       Level l => l
                                                     | StartLevel => raise StartLevelExc
            in if varparentlevel = curlev
               then fp
               else T.MEM(calcaddr varparentlevel)
            end
       val path = calcaddr currentlevel
    in
        Ex (Frame.exp varframeaccess fp)
    end
*)

fun simpleVar (access, currentlevel) =
    let val (varlevel,varframeaccess) = access
        val varunique = case varlevel of 
                            Level(_,_,unique) => unique
                          | StartLevel => raise StartLevelExc
        fun calcfraddr level =
            case level of
                Level(currentframe,currentparentlevel,currentunique) =>
                    if varunique = currentunique
                    then T.TEMP(Frame.FP)
                    else T.MEM(calcfraddr(currentparentlevel))
	          | StartLevel => raise StartLevelExc
        val afp = calcfraddr currentlevel
        in Ex (Frame.exp varframeaccess afp) end

fun nilVar () =
    Ex (T.MEM (T.CONST 0))

fun strVar str =
    let val lab = Temp.newlabel()
        val _ = frags := Frame.STRING(lab,str)::(!frags)
    in Ex (T.NAME(lab)) end

fun subScriptVar (a,i) =
    Ex (T.MEM(T.BINOP(T.PLUS, unEx(a),T.BINOP(T.MUL, (unEx i), T.CONST Frame.wordSize))))
(* mul by wordsize because all tiger values have the same size and minus 1 to get zero based offset *)

fun fieldVar (a,i) =
    Ex (T.MEM(T.BINOP(T.PLUS, unEx(a),T.BINOP(T.MUL, T.CONST i, T.CONST Frame.wordSize))))
       
fun opExp (oper,expl,expr) =
    let val expl' = unEx(expl)
	    val expr' = unEx(expr)
    in
        case oper of
	    A.PlusOp => Ex (T.BINOP(T.PLUS,expl',expr'))
	  | A.MinusOp => Ex (T.BINOP(T.MINUS,expl',expr'))
	  | A.TimesOp => Ex (T.BINOP(T.MUL,expl',expr'))
	  | A.DivideOp => Ex (T.BINOP(T.DIV,expl',expr'))
	  | A.LtOp => Cx (fn (t,f) => T.CJUMP(T.LT,expl',expr',t,f))
	  | A.LeOp => Cx (fn (t,f) => T.CJUMP(T.LE,expl',expr',t,f))
	  | A.GtOp => Cx (fn (t,f) => T.CJUMP(T.GT,expl',expr',t,f))
	  | A.GeOp => Cx (fn (t,f) => T.CJUMP(T.GE,expl',expr',t,f))
	  | A.EqOp => Cx (fn (t,f) => T.CJUMP(T.EQ,expl',expr',t,f))
	  | A.NeqOp => Cx (fn (t,f) => T.CJUMP(T.NE,expl',expr',t,f))
			  
    end 

fun cond1Exp (e1,e2) =
    let val r = Temp.newtemp()
        val t = Temp.newlabel() and f = Temp.newlabel()
    in
	Nx (
        T.SEQ(unCx e1(t,f),
	    T.SEQ(T.LABEL t,
	    T.SEQ(T.EXP(unEx e2),
	          T.LABEL f))))
    end

fun cond2Exp (e1,e2,e3) =
    let val r = Temp.newtemp()
        val t = Temp.newlabel() and f = Temp.newlabel() and fin = Temp.newlabel()
    in  
        Ex (
        T.ESEQ(
        T.SEQ(unCx e1(t,f),
	    T.SEQ(T.LABEL t,
	    T.SEQ(T.MOVE(T.TEMP r, unEx e2),
        T.SEQ(T.JUMP(T.NAME fin,[fin]), 
	    T.SEQ(T.LABEL f,
	    T.SEQ(T.MOVE(T.TEMP r, unEx e3),
        T.SEQ(T.JUMP(T.NAME fin,[fin]), 
              T.LABEL fin))))))), T.TEMP r))
    end
	
fun createRecord (fexps,n) =
    let fun placeFields (fexps,r) =
	    let fun seq(fexp,n) =
                    T.MOVE(T.MEM(T.BINOP(T.PLUS,r,T.CONST (n*Frame.wordSize))),unEx fexp)
		fun iter (exps,n) =
	            case exps of
		        e::nil => seq(e,n)
		      | e::es => T.SEQ(seq(e,n), iter(es,n+1))
		      | nil => raise ErrorAlloc
	    in
		    iter(fexps,0)
	    end
	val r = Temp.newtemp()
	val cr = Frame.externalCall("initRecord",[T.CONST n])
    in Ex (T.ESEQ(T.SEQ(T.MOVE(T.TEMP r,cr), placeFields(fexps,T.TEMP r)),T.TEMP r)) end

fun createArray (size,init) =
    Ex (Frame.externalCall("initArray",[unEx size,unEx init]))

fun callFunction (s,curlev,calledlev,args) =
    let 
        val curunique = case curlev of
                            Level (_,_,u) => u
                          | StartLevel => raise StartLevelExc
        fun calcfraddr level =
            case level of
                Level(calledfr,calledpl,calledunique) =>
                    if curunique = calledunique
                    then level
                    else calcfraddr(calledpl)
	          | StartLevel => raise StartLevelExc
    val calledfunlev = calcfraddr calledlev
    val calledfunframe = case calledfunlev of
                             Level (fr,_,_) => fr
                           | StartLevel => raise StartLevelExc
    val facc = getStaticLink(calledfunlev)
    val sl = Frame.exp facc (T.TEMP Frame.FP)
    (* TODO: 1. do(or not) something with sl *)
	fun uex arg =
	    unEx arg
	val args' = map uex args
    val _ = Frame.findEscArgs calledfunframe
    val _ = print("Call length: "^Int.toString(length(Frame.getEsc ()))^"\n")
    in Ex (T.CALL(T.NAME (Temp.namedlabel(s)), args')) end

fun assign (lvalue,rvalue) =
    Nx (T.MOVE(unEx lvalue,unEx rvalue))

val initLoop = Nx (T.LABEL (Temp.newlabel()))

fun breakLoop b =
    let val l = unNx b
    in 
        case l of T.LABEL n => Nx (T.JUMP (T.NAME n,[n]))
       | _ => raise LabelCaseExc
    end

fun whileExp (test,body,ldone) =
    let val ltest = Temp.newlabel()
        val lbody = Temp.newlabel()
	val ldone' = case (unNx ldone) of T.LABEL n => n
	                 | _ => raise LabelCaseExc
    in
        Nx (T.SEQ(T.LABEL ltest,
            T.SEQ(unCx test(lbody,ldone'),
            T.SEQ(T.LABEL lbody,
            T.SEQ(T.EXP (unEx body),
            T.SEQ(T.JUMP (T.NAME ltest,[ltest]),
                  T.LABEL ldone'))))))

    end

fun forExp (var, lo, hi, body, ldone) =
    let val lbody = Temp.newlabel()
	    val ldone' = case (unNx ldone) of T.LABEL n => n 
	                                    | _ => raise LabelCaseExc
        val limit = Temp.newtemp()
        val var' = unEx var
    in
        Nx (
        T.SEQ(unNx lo,
        T.SEQ(T.MOVE(T.TEMP limit,unEx hi),
        T.SEQ(T.CJUMP(T.LE,var',T.TEMP limit,lbody,ldone'),
        T.SEQ(T.LABEL lbody,
        T.SEQ(unNx body,
        T.SEQ(T.MOVE(var',T.BINOP(T.PLUS,var',T.CONST 1)),
        T.SEQ(T.CJUMP(T.LE,var',T.TEMP limit,lbody,ldone'),
              T.LABEL ldone'))))))))
    end

fun procEntryExit {level=lev, body=exp} =
    case lev of
        Level (fr,_,_) => let val body' = Frame.procEntryExit1(fr,unEx(exp))
                          in (frags := Frame.PROC{body=body', frame=fr} :: (!frags);
                              print("FRAGS length: "^Int.toString(length (!frags))^"\n"); 
                              print("Formals length: "^Int.toString(length(Frame.formals fr))^"\n"))
                          end
      | _ => raise StartLevelExc
end
