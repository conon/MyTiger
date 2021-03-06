structure Translate : TRANSLATE =
struct
structure A = Absyn
structure T = Tree

exception ErrorAlloc 
exception CxToNx 
exception StartLevelExc
exception LabelCaseExc
exception ArraySizeExc

(* Actual frame, the parent frame, every new level is unique *)
datatype level = Level of Frame.frame * level * unit ref 
               | StartLevel

type access = level * Frame.access

datatype exp = Ex of Tree.exp
             | Nx of Tree.stm
	         | Cx of Temp.label * Temp.label -> Tree.stm

val outermost = StartLevel

val frags : Frame.frag list ref = ref nil
val arraysize = ref nil (* stores a pair of array reference and size *)
		    
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

fun findArraySize unique =
    case (List.find(fn (u,_) => if u = unique then true else false) (!arraysize)) of
        SOME n => n
      | NONE => raise ArraySizeExc

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
                             (*val _ = print("FORMALS(translate): "^Int.toString(length(acc))^"\n")*)
                         in List.drop(acc,1) end (* drop the static link *)
      | StartLevel => (print "formals\n";raise StartLevelExc)

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

fun makeSeq explst =
    let fun iter (exp::nil) =
            unNx exp
          | iter (exp::exps) =
            T.SEQ(unNx exp, iter(exps))
          | iter nil =
            T.EXP(T.CONST 0)
    in Nx (iter(explst)) end

fun makeExp explst =
    let fun iter (exp::nil) =
            unEx exp
          | iter (exp::exps) = 
            T.ESEQ(unNx exp, iter(exps))
          | iter nil =
            T.CONST 0
    in Ex (iter(explst)) end

fun concatLet (s,e,l) =
    let val fr = case l of
                     Level(fr,_,_) => fr
                   | StartLevel => (print "concatLet\n";raise StartLevelExc)
        val ret = Frame.procEntryExit1(fr,unEx e)
    in Ex (T.ESEQ(T.SEQ(unNx s,ret), T.TEMP Frame.RV)) end
         
fun constIntVar i =
    Ex (T.CONST i)

fun simpleVar (access, currentlevel) =
    let val (varlevel,varframeaccess) = access
        val varunique = case varlevel of 
                            Level(_,_,unique) => unique
                          | StartLevel => (print "varunique\n";raise StartLevelExc)
        fun calcfraddr level =
            case level of
                Level(currentframe,currentparentlevel,currentunique) =>
                    if varunique = currentunique
                    then T.TEMP(Frame.FP)
                    else T.MEM(calcfraddr(currentparentlevel))
	          | StartLevel => (print "calcfraddr";raise StartLevelExc)
        val afp = calcfraddr currentlevel
        in Ex (Frame.exp varframeaccess afp) end

fun nilVar () =
    Ex (T.CONST 0)

fun strVar str =
    let val lab = Temp.newlabel()
        val _ = frags := Frame.STRING(lab,str)::(!frags)
    in Ex (T.NAME(lab)) end

fun subScriptVar (a,i,u) =
    let val (_,size) = findArraySize(u)
        val baseaddr = unEx a
        val i' = unEx i
        val offset = T.BINOP(T.MUL, i', T.CONST(Frame.wordSize))
        val f = Temp.newlabel()
        val t1 = Temp.newlabel()
        val t2 = Temp.newlabel()
    in
    Ex (
        T.ESEQ(T.SEQ(T.CJUMP(T.LE, size, i', f, t1),
               T.SEQ(T.LABEL t1,
               T.SEQ(T.CJUMP(T.LT, i', T.CONST 0, f, t2),
               T.SEQ(T.LABEL f,
               T.SEQ(T.EXP(T.MEM(T.CONST 0)),
                     T.LABEL t2))))),
               T.MEM(T.BINOP(T.PLUS, baseaddr, offset)))
       )
    end

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
	val cr = Frame.externalCall("allocRecord",[T.CONST n])
    in Ex (T.ESEQ(T.SEQ(T.MOVE(T.TEMP r,cr), placeFields(fexps,T.TEMP r)),T.TEMP r)) end

fun findArraySize unique =
    case (List.find(fn (u,_) => if u = unique then true else false) (!arraysize)) of
        SOME n => n
      | NONE => raise ArraySizeExc

fun createArray (size,init,unique) =
    let val size' = unEx size
        val t = Temp.newlabel()
        val f = Temp.newlabel()
    in
        (arraysize := (unique,size')::(!arraysize);
        Ex (T.ESEQ(
            T.SEQ(T.CJUMP(T.GT, size', T.CONST 0, t, f),
            T.SEQ(T.LABEL f,
            T.SEQ(T.EXP(T.MEM(T.CONST 0)), (* raise segmentation fault to prevent negative array size *)
                  T.LABEL t))),
            Frame.externalCall("initArray",[size',unEx init]))
          ))
    end

fun callFunction (s,curlev,callinglev,args) =
    let 
        (* calling level should never have a StartLevel parent EXCEPT library functions *)
        val (currentframe,curparentframe,_) = case curlev of
                            Level x => x
                          | StartLevel => (print "callFunction\n";raise StartLevelExc)
        fun uex arg =
            unEx arg
        val args' = map uex args
    in
        case callinglev of
            Level (callingframe,callingparentframe,_) => (Frame.findEscArgs (callingframe, currentframe);
                                                                      Ex (T.CALL(T.NAME (Temp.namedlabel(s)), args')))
          | StartLevel => Ex (Frame.externalCall(s,args'))
    end


                (* escaping formals are defined in callingframe and
                   escaping locals (which are needed for subtracting the fp in procExit3)
                   are defined in currentframe 
                   I use this findEscArgs function to record escaping formals and locals
                   instead of finding and passing the static link *)
    (*

        fun calcfraddr level =
            case level of
                Level(callingfr,callingpl,callingunique) =>
                    if curunique = callingunique
                    then level
                    else (print "YO\n";calcfraddr(callingpl))
	          | StartLevel => level
        val callingfunlev = calcfraddr callinglev
        (*
        val _ = if callingfunlev = curlev
                then (print "YESYYY\n")
                else (print "NOOOOO\n")
        *)
        fun uex arg =
            unEx arg
        val args' = map uex args
        (*val _ = print("Call length: "^Int.toString(length(Frame.getEsc ()))^"\n")*)
    in  
        case callingfunlev of
            Level (fr,plv,_) => let val facc = getStaticLink(callingfunlev)
                                  (* TODO: 1. do(or not) something with sl *)
                                  val sl = Frame.exp facc (T.TEMP Frame.FP)
                                  val pfr = case plv of
                                                Level(f,_,_) => f
                                              | StartLevel => fr
                                  val _ = Frame.findEscArgs (fr,pfr)
                              in Ex (T.CALL(T.NAME (Temp.namedlabel(s)), args')) end
          | StartLevel => if curlev = callinglev 
                          then (print "YES\n";Ex (T.CALL(T.NAME (Temp.namedlabel(s)), args')) )
                          else (print "NO\n";Ex (Frame.externalCall(s,args')))

    end
    *)


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
        Level (fr,_,_) => let val b = Frame.procEntryExit1(fr,unEx exp)
                          in frags := Frame.PROC{body=b, frame=fr} :: (!frags) end
                         (*print("FRAGS length: "^Int.toString(length (!frags))^"\n"); 
                         print("Formals length: "^Int.toString(length(Frame.formals fr))^"\n")*)
      | _ => (print "procEntryExit\n";raise StartLevelExc)
end
