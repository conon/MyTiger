structure Translate : TRANSLATE =
struct
structure A = Absyn
structure T = Tree

exception ErrorAlloc 

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
  | unCx (Nx _) = raise ErrorAlloc

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
	    Level(fr,_,_) => let val fracc = Frame.formals fr
                             fun aux fracc =
                                 (lev,fracc)
                             val acc = map aux fracc
                         in List.drop (acc,0) end (* drop the static link *)
      | StartLevel => raise ErrorAlloc

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

fun constIntVar i =
    Ex (T.CONST i)

fun simpleVar (access, currentlevel) =
(* TODO: check *)
    let val (varlevel,varframeaccess) = access
        val varunique = case varlevel of 
                            Level(_,_,unique) => unique
                          | StartLevel => raise ErrorAlloc
        fun calcfraddr level =
            case level of
                Level(currentframe,currentparentlevel,currentunique) =>
                    if varunique = currentunique
                    then T.TEMP(Frame.FP)
                    else T.MEM(calcfraddr(currentparentlevel))
	          | StartLevel => raise ErrorAlloc
        val afp = calcfraddr currentlevel
        in Ex (Frame.exp varframeaccess afp) end
		     (* FIXME: changing the const to 0 because sl should be on the first offset
		               The static link seems to work properly. What goes wrong is formal values'
			       starting offset, should be 4 bytes higher to due to static link*)

fun nilVar () =
    Ex (T.MEM (T.CONST 0))

fun strVar str =
    let val lab = Temp.newlabel()
        val _ = frags := Frame.STRING(lab,str)::(!frags)
    in Ex (T.NAME(lab)) end

fun subScriptVar (a,i) =
    Ex (T.MEM(T.BINOP(T.PLUS, T.MEM(unEx(a)),T.BINOP(T.MUL, (unEx i), T.CONST Frame.wordSize))))
(* mul by wordsize because all tiger values have the same size and minus 1 to get zero based offset *)

fun fieldVar (a,i) =
    Ex (T.MEM(T.BINOP(T.PLUS, T.MEM(unEx(a)),T.BINOP(T.MUL, T.CONST i, T.CONST Frame.wordSize))))
       
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
        Nx (
        T.SEQ(unCx e1(t,f),
	    T.SEQ(T.LABEL t,
	    T.SEQ(T.EXP(unEx e2),
        T.SEQ(T.JUMP (T.NAME fin, [fin]),
	    T.SEQ(T.LABEL f,
	    T.SEQ(T.EXP(unEx e3),
              T.LABEL fin)))))))
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
    in Nx (T.SEQ(T.MOVE(T.TEMP r,cr), placeFields(fexps,T.TEMP r))) end

fun createArray (size,init) =
    Ex (Frame.externalCall("initArray",[unEx size,unEx init]))

fun callExp (s,l,args) =
    let (*val facc = getStaticLink(l)
        val sl = Frame.exp facc (T.TEMP Frame.FP)
        val args = Ex(sl)::args*)
	fun uex arg =
	    unEx arg
	val args' = map uex args
    in Ex (T.CALL(T.NAME (Temp.namedlabel(s)), args')) end

fun assign (lvalue,rvalue) =
    Nx (T.MOVE(unEx lvalue,unEx rvalue))

val initLoop = Nx (T.LABEL (Temp.newlabel()))

fun breakLoop b =
    let val l = unNx b
    in 
        case l of T.LABEL n => Nx (T.JUMP (T.NAME n,[n]))
       | _ => raise ErrorAlloc
    end

fun whileExp (test,body,ldone) =
    let val ltest = Temp.newlabel()
        val lbody = Temp.newlabel()
	val ldone' = case (unNx ldone) of T.LABEL n => n
	                 | _ => raise ErrorAlloc
    in
        Nx (T.SEQ(unCx test(lbody,ldone'),
            T.SEQ(T.LABEL lbody,
            T.SEQ(T.EXP (unEx body),
            T.SEQ(T.JUMP (T.NAME lbody,[lbody]),
                  T.LABEL ldone')))))

    end

fun forExp (lo, hi, body, ldone) =
    let val lbody = Temp.newlabel()
	    val ldone' = case (unNx ldone) of T.LABEL n => n 
	                                    | _ => raise ErrorAlloc
    in
    (* TODO : check again *)
    Nx (
	T.SEQ(T.CJUMP(T.LE, unEx lo, unEx hi, lbody, ldone'),
	T.SEQ(T.LABEL lbody,
	T.SEQ(unNx body,
	T.SEQ(T.MOVE(unEx lo, T.BINOP(T.PLUS, unEx lo, T.CONST 1)),
	T.SEQ(T.CJUMP(T.LT, unEx lo, unEx hi, lbody, ldone'),
          T.LABEL ldone'))))))
    end

fun procEntryExit {level=lev, body=exp} =
    case lev of
        Level (fr,_,_) => frags := Frame.PROC{body=unNx(exp), frame=fr} :: (!frags)
      | _ => raise ErrorAlloc
    end
