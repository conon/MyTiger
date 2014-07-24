signature TRANSLATE =
sig
    type level
    type access (* not the same as Frame.access *)
    type exp
    exception ErrorAlloc

    val printTree : exp -> unit (* Print IR for debugging *)
    val outermost : level
    val newLevel : {parent : level, name : Temp.label,
                    formals : bool list} -> level
    val formals : level -> access list
    val allocLocal : level -> bool -> access

    val dummy : exp (* a dummy val to make compilation work *)
    val getResult : unit -> Frame.frag list

    val makeExps : exp list * exp -> exp
    val makeExp : exp list -> exp
    val concatExps : exp * exp -> exp

    val constIntVar : int -> exp
    val nilVar : unit -> exp
    val strVar : string -> exp 
    val simpleVar : access * level -> exp
    val subScriptVar : exp * exp -> exp (*exp1->address of value, exp2->element's position*)
    val fieldVar : exp * int -> exp (* exp1 -> address of value, int -> element's position *)
    val opExp : Absyn.oper * exp * exp -> exp
    val cond1Exp : exp * exp -> exp (*exp1:the condition,exp2:then expression*)
    val cond2Exp : exp * exp * exp -> exp (*exp1:the condition,exp2:then expression,exp3:else expression*)
    val createRecord : exp list * int -> exp 
    val createArray : exp * exp -> exp
    val callFunction : string * level * level * exp list -> exp
    val assign : exp * exp -> exp (*exp1:lvalue,exp2:rvalue*)
    val breakLoop : exp -> exp (* returns the done label exp *)
    val initLoop : exp (* the done label *)
    val whileExp : exp * exp * exp -> exp (*exp1:test cond, exp2:body exp, exp3: done label*)
    val forExp : exp * exp * exp * exp * exp -> exp (*exp0:var,exp1:lo,exp2:hi,exp3:body,exp4:done label*)

    val procEntryExit : {level: level, body: exp} -> unit

end

