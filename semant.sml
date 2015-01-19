structure Semant : sig 
	      val transProg : Absyn.exp -> Frame.frag list end =
struct

structure A = Absyn
structure Tr = Translate 
exception ForUndefined
val error = ErrorMsg.error
val anyErrors = ErrorMsg.anyErrors

type venv = Env.enventry Symbol.table
type tenv = Env.ty Symbol.table
type expty = {exp: Tr.exp, ty: Types.ty}

(* this is a helper function wich returns the actual type and not the one enclosed in NAME *)
fun actual_ty typ =
    case typ of
        Types.NAME(_,t) => (case !t of
	                        SOME ty => actual_ty ty
			      | NONE => typ)
      | _ => typ


fun checkUnit ty =
    case (actual_ty ty) of
        Types.UNIT => true
      | _ => false

fun checkInt ty =
    case (actual_ty ty) of
        Types.INT => true
      | _ => false

fun checkNil ty =
    case (actual_ty ty) of
        Types.NIL => true
      | _ => false

fun checkInts(lhs, rhs) =
    case (actual_ty lhs,actual_ty rhs) of
        (Types.INT,Types.INT) => true
      | _ => false

fun checkEqual(lhs, rhs) =
    case (actual_ty lhs, actual_ty rhs) of
        (Types.INT, Types.INT) => true
      | (Types.STRING, Types.STRING) => true
      | (Types.RECORD(_,u1), Types.RECORD(_,u2)) => if u1=u2
      						    then true
						    else false
      | (Types.ARRAY _, Types.ARRAY _) => true
      | (Types.RECORD _,Types.NIL) => true
      | (Types.NIL,Types.RECORD _) => true
      | _ => false

fun checkEqualIf(lhs,rhs) =
    case (actual_ty lhs,actual_ty rhs) of
        (Types.UNIT, Types.UNIT) => true
      | _ => checkEqual(lhs,rhs)

fun transProg ast =
    let
	val nested = ref 0
        (* the transTy functions translate Absyn.ty types to Types.ty types so they can be put on the
               symbol table(enviroment) and also ensures that the (rhs)types was already defined
               (the only predefined types are INT and STRING) *)	
	fun transTy(tenv, A.NameTy(name, pos)) =
            (case Symbol.look(tenv, name) of
		 SOME ty => ty
	       | NONE => (error pos ("unbound type: " ^ Symbol.name name);
	                  Types.UNIT))

	  | transTy(tenv,A.RecordTy(fields)) =
	    let val names = map #name fields
	        val types = map #typ fields
		val poses = map #pos fields
		val nt = ListPair.zip(names,types)
		val ntp = ListPair.zip(nt,poses)
	        fun insert(((fieldn,fieldt),pos),lst) = 
		    (case Symbol.look(tenv,fieldt) of
		         SOME ty => lst@[(fieldn,ty)]
		       | NONE => (error pos ("unbound field type: " ^ Symbol.name fieldn);
			          lst@nil))
	        val reclist = foldl insert nil ntp
	    in
	        Types.RECORD(reclist,ref())
            end

	  | transTy(tenv, A.ArrayTy(name, pos)) =
            (case Symbol.look(tenv, name) of
                 SOME ty => Types.ARRAY(ty,ref())
               | NONE => (error pos ("unbound array type: " ^ Symbol.name name);
	                  Types.UNIT))
		

        (* iterate declarations *)
        and transDecs (venv, tenv, dec::decs, lev, expslist, breaklabel) =
            let val ({tenv = tenv', venv = venv'},lev', expslist') = transDec(venv, tenv, dec, lev, expslist, breaklabel)
            in
                transDecs(venv', tenv', decs, lev', expslist', breaklabel)
            end
          | transDecs (venv, tenv, nil, lev, expslist, breaklabel) =
            ({tenv = tenv , venv = venv}, lev, expslist)


	(* Declares a variable with short form, the type of variable is determined from the type of the expression. *)
        and transDec (venv, tenv, A.VarDec{name, escape, typ=NONE, init, pos}, lev, expslist, breaklabel) =
            let val {exp = e, ty = expty} = transExp(venv, tenv, init, lev, breaklabel, expslist)
            in
                if checkNil expty
                then (error pos "expression returns nil type: use the long form";
                      ({tenv = tenv, venv = venv},lev,expslist) )
                else if checkUnit expty
		then (error pos "expression is no value";
		      ({tenv=tenv, venv=venv},lev,expslist) )
	        else let val acc = Tr.allocLocal lev (!escape)
		         val venv' = Symbol.enter(venv, name, Env.VarEntry{access = acc, ty = expty})
			 val var = Tr.assign(Tr.simpleVar(acc,lev),e)
	             in ({tenv = tenv, venv = venv'}, lev, expslist@[var]) end
            end
          (* Declares a variable with long form, the type given should match the type of the expression,
               and should be in the types enviroment *)
          | transDec (venv, tenv, A.VarDec{name, escape, typ=SOME(id,posId), init, pos}, lev, expslist, breaklabel) =
            let val {exp = e, ty = ty} = transExp(venv, tenv, init, lev, breaklabel, expslist)
                val ty_exp = ty (* the ty's field name, of the expression, conflicts with the VarEntry's field name *)
            in
                (case Symbol.look(tenv, id) of
                     SOME ty => 
		     let val aty = ty
		     in
			 if checkEqual(aty, ty_exp)
			 then let val acc = Tr.allocLocal lev (!escape)
			          val venv' = Symbol.enter(venv, name, Env.VarEntry{access = acc, ty = aty})
				  val var = Tr.assign(Tr.simpleVar(acc,lev),e)
			      in ({tenv = tenv, venv = venv'},lev,expslist@[var]) end
			 else (error posId "type specifier does not much expression type";
			       ({tenv = tenv, venv = venv},lev,expslist))
		     end
                   | NONE => (error posId ("unbound type: '" ^  Symbol.name id ^ "'");
                              ({tenv = tenv, venv = venv},lev,expslist) ))
            end


	  | transDec (venv, tenv, A.TypeDec(types), lev, expslist, breaklabel) =
	    let
		val names = map #name types
		val typs = map #ty types
		fun addt(n,envt) = Symbol.enter(envt,n,Types.NAME(n,ref(Symbol.look(tenv,n))))
		(* enter the known types in the env with SOME ... and the unknwown with NONE *)
		val tenv' = foldl addt tenv names
		(* then translate all types including the unknown(that will be there from the previous action *)
		val nts = map (fn t => transTy(tenv', t)) typs
		fun updt (n, nt) =
		    case Symbol.look(tenv', n) of
		        SOME (Types.NAME(id,r)) => r := SOME nt 
		      | _ => (error 0 "impossible: updt failed"; ())
		(* now that all(fortunetly) values are known update the NONE ones *)
		val _ = app updt (ListPair.zip(names,nts))
		(* if there still unknown values in declartion then we have 
		   a cycle like type a = b, type b = a *)
		fun checkcycle(n) = 
		    let fun aux(x, fid) =
		            (case x of
			         SOME (Types.NAME(id,r)) => 
				 if fid=id
				 then error 0 ("declaration cycle detected "^Symbol.name id)
				 else aux(!r, fid)
			       | SOME _ => ()
			       | NONE => error 0 "impossible: checkcycle failed 2")
		    in
		        (case Symbol.look(tenv', n) of
		             SOME (Types.NAME(id,r)) => aux(!r,id)
			   | SOME _ => ()
		           | NONE => error 0 "impossible: header of declaration type not found")
		    end
	        val _ = app checkcycle names 
	    in
	        if !anyErrors
		then ({tenv = tenv, venv = venv},lev,expslist)
		else ({tenv = tenv', venv = venv},lev,expslist)
	    end

          (* Declares a function with return type, the return type should math that of the body expression  *)
	  | transDec (venv, tenv, A.FunctionDec funs, lev, expslist, breaklabel) =
	    let val names = map #name funs
	        val resultsty = map #result funs
		val params = map #params funs (* a list of fields list *)
		val bodies = map #body funs
		val poses = map #pos funs
		fun transparam(fields : A.field list) = (* expects a field list *)
		    let fun nametype ((typ,name),(pos,escape)) =
                            (case Symbol.look(tenv, typ) of
			         SOME t => {name = name, ty = t, escape = escape}
                               | NONE => (error pos ("unbound type " ^ Symbol.name typ);
                                          {name=Symbol.symbol "", ty = Types.NIL, escape = ref false}))
			val typs = map #typ fields (* typs holds a list of types *)
			val names = map #name fields (* names holds a list of names *)
			val poses = map #pos fields
			val escapes = map #escape fields
			val tn = ListPair.zip(typs,names)
			val pe = ListPair.zip(poses,escapes)
			val tnp = ListPair.zip(tn,pe)
			val typlist = map nametype tnp (* typlist hold a list of name,type pair,escape *)
		    in
		        typlist
		    end
                (* 1. check all types given as parameters are bound *)
		val params' = map transparam params (* a list of field's name,type and escape *)
		fun resty t =
		    (case t of
		         SOME (typ,rpos) => (case Symbol.look(tenv,typ) of
			                         SOME ty => ty
					       | NONE => (error rpos ("unbound return type "^
								      Symbol.name typ); Types.NIL))
		       | NONE => Types.UNIT)
                (* 2. check that the result type is bound *)
		val resultsty' = map resty resultsty (* a list of results type *)
		val rp = ListPair.zip(resultsty',params')
		val nrp = ListPair.zip(names,rp)
		fun addf((n ,(r , p : {name : Symbol.symbol, ty : Types.ty, escape : bool ref} list)), (venv,levlist)) = 
		    let fun formalstobool f =
			    case f of
				f::fs => (!f) :: formalstobool(fs)
			      | nil => nil
		    in    
              if !anyErrors
			  then (venv,nil)
			  else let val formals = map #escape p
				   val formals' = formalstobool formals
				   val label' = Temp.namedlabel(Symbol.name(n))
				   val lev' = Tr.newLevel {parent=lev, name=label',
							   formals = formals'} 
                   (*val _ = print("SEMANT: "^Symbol.name(label')^"\n")*)
			       in 
                     (Symbol.enter(venv, n, Env.FunEntry{level=lev', label=label',
								     formals = map #ty p, result = r}), levlist @ [lev']) 
                   end
		    end
	        (* 3. update the current enviroment with the function name which contains: the result type
				and all the pairs(name,type) of the parameters as funentry *)
		val (venv',levs) = foldl addf (venv,nil) nrp
		val bp = ListPair.zip(bodies,poses)
		val bprp = ListPair.zip(bp,rp)
		val all = ListPair.zip(bprp, levs)
                (* 4. update the enviroment with the name of each
		   parameter and its type as varentry *)
		fun enterpar (({name = name, ty = ty, escape = escape},acc),venv) = 
		    Symbol.enter(venv, name, Env.VarEntry{access = acc, ty = ty})
		fun evalbody (((body,pos),(resulty,params')),lev) =
		    let val accesslst = Tr.formals lev
		        val pa = ListPair.zip(params',accesslst)
		        val venv'' = foldl enterpar venv' pa
		        val {exp = funbody, ty = expty} = transExp(venv'', tenv, body, lev, breaklabel, expslist)
		    in
			if checkEqualIf(resulty, expty)
		        then Tr.procEntryExit {level=lev, body=funbody}
		        else (error pos "result type does not match expression type"; ())
	            end
                (* 5. check the body expression type if matches the return type *)
		val _ = app evalbody all
	    in
		if !anyErrors
		then ({tenv = tenv, venv = venv},lev,expslist)
		else ({tenv = tenv, venv = venv'},lev,expslist)
	    end

	(* transExp : venv * tenv -> expty *)
	and transExp(venv, tenv, init, lev, breaklabel, expslist) =
            (* trexp : Absyn.exp -> expty *)
	    let fun trexp (A.IntExp i) =
		    {exp = Tr.constIntVar i, ty = Types.INT}
		  | trexp (A.StringExp (s,pos)) =
		    {exp = Tr.strVar(s), ty = Types.STRING}
          | trexp (A.NilExp) =
            {exp = Tr.nilVar (), ty = Types.NIL}
          | trexp (A.VarExp vr) =
             trvar(vr,lev)
          | trexp (A.BreakExp pos) =
		     if !nested > 0
		     then {exp=Tr.breakLoop breaklabel, ty = Types.UNIT}
		     else (error pos "break should be inside while or for";
                  {exp=Tr.dummy, ty = Types.UNIT})
		  | trexp (A.SeqExp seqs) =
			let fun makeTree ((exp,_),lst) =
					trexp(exp)::lst
				val tr = foldr makeTree nil seqs
				fun canonicalize ({exp=e,ty=t},lst) =
					e::lst
				val trcan = foldr canonicalize nil tr
				val tree = Tr.makeExp trcan
				val {exp=_,ty=lastType} = List.last tr handle Empty => {exp=Tr.dummy, ty=Types.UNIT}
			in {exp=tree,ty=lastType} end
				
		  | trexp (A.AssignExp{var, exp, pos}) =
			let val {exp=lval,ty= trty} = trvar(var,lev)
				val {exp=rval,ty= expty} = trexp(exp)
			in
				if checkEqual(trty,expty)
				then {exp = Tr.assign(lval,rval), ty = Types.UNIT}
				else (error pos "wrong type value assiged to variable";
					  {exp=Tr.dummy, ty = Types.UNIT})
			end
		  | trexp (A.CallExp{func, args, pos}) =
			let fun iter ((argtype,formaltype),exps) =
					let val {exp = e, ty = argtype'} = trexp(argtype)
					in
						if checkEqual(argtype', formaltype)
						then exps@[e]
						else (error pos "expression type does not match of declaration"; exps)
					end
                    in
                        (case Symbol.look(venv, func) of
                             SOME(Env.FunEntry{level, label, formals, result}) => 
                             let val res = if length(args) > length(formals)
                                           then (error pos "number of arguments do not match those of declaration";nil)
                                           else if length(args) < length(formals)
                                           then (error pos "number of declared parameters do not match arguments";nil)
                                           else let val pr = ListPair.zip(args,formals) in foldl iter nil pr end
                             in
                                 if !anyErrors
                                 then {exp=Tr.dummy, ty = Types.UNIT}
                                 else if result = Types.UNIT
                                 (* lev -> current level , level -> calling function level *)
                                 then {exp=Tr.callFunction(Symbol.name func,lev,level,res), ty = Types.UNIT}
                                 else {exp=Tr.callFunction(Symbol.name func,lev,level,res), ty = result}
                             end
                           | SOME (Env.VarEntry{access=_, ty=ty}) => (error pos ("identifier " ^ Symbol.name func ^
										 " is bound to a variable");
								      {exp=Tr.dummy, ty = Types.UNIT})
                           | NONE => (error pos ("unbound identifier " ^ Symbol.name func);
                                      {exp=Tr.dummy, ty = Types.UNIT}))
                    end
                  | trexp (A.RecordExp{fields, typ, pos}) =
		    let fun iterFields((s1,e,p1)::xs, (s2,decty)::ys, count, exps) =
		            (* TODO: consider change bool,count to SOME count and NONE *)
			    if s1=s2
                            then let val {exp=e, ty = varty} = trexp(e)
				 in if checkEqual(decty,varty)
                                    then iterFields(xs,ys,count+1,exps@[e])
                                    else (error pos ("The expression type of field " ^ "'" ^ Symbol.name s1 ^ "'" ^
						     " does not match the declared type"); (false,count,exps))
				 end
                            else (error pos ("field name " ^ "'" ^ Symbol.name s1 ^ "'"
					     ^ "does not match the type given at declaration"); (false,count,exps))
			  | iterFields((s1,e,p1)::xs,nil, count,exps) = (error pos ("field name " ^ "'" 
										    ^  Symbol.name s1 ^ "'" ^ " was defined but not declared"); (false,count,exps))
			  | iterFields(nil,(s2,t)::ys, count,exps) = (error pos ("field name " ^ "'" ^ Symbol.name s2 
										 ^ "'" ^ " was declared but not defined"); (false,count,exps))
			  | iterFields(nil,nil,count,exps) = (true,count,exps)
			val restyp = (case Symbol.look(tenv, typ) of
					  SOME ty => ty
					| NONE => (error pos ("unbound type: " ^ "'" ^ Symbol.name typ ^ "'");
                                                   Types.INT))

                    in
			(case actual_ty restyp of
                             Types.RECORD (reclst, uniq) => 
			     let val (res,count,exps) = iterFields(fields,reclst,0,nil)
			     in
			         if res
             		         then {exp=Tr.createRecord(exps,count), ty=Types.RECORD(reclst,uniq)}
			         else {exp=Tr.dummy, ty=Types.INT}
			     end
			   | _ => (error pos ("variable " ^ Symbol.name typ ^ " is bound to not record type");
				   {exp=Tr.dummy, ty = Types.INT}))
                    end
          | trexp (A.ArrayExp{typ, size, init, pos}) =
            let val typ' = (case Symbol.look(tenv, typ) of
					SOME ty => actual_ty ty
                  | NONE => (error pos ("unbound type: " ^ Symbol.name typ); Types.UNIT))
		    in
		        (case typ' of
                     Types.ARRAY (ty_arr,uniq) => 
			                    let val {exp=esize, ty = tysize} = trexp(size);
				                    val {exp=einit, ty = tyinit} = trexp(init)
			                    in
				                    if checkInt tysize
				                    then 
                                        if ty_arr = tyinit
                                        then {exp = Tr.createArray(esize,einit,uniq), 
                                              ty = Types.ARRAY(tyinit, uniq)}
      		              	            else (error pos "the initializing expression returns\
                                          			       \ a type which does not match the array type";
 					                          {exp=Tr.dummy, ty = Types.INT})
				                    else (error pos "the subscript should evaluate integer";
                                          {exp=Tr.dummy, ty = Types.INT})
                                end
			      |  _ => (error pos ("variable " ^ Symbol.name typ ^ " is bound to not array type");
                           {exp=Tr.dummy, ty = Types.UNIT}))
		    end
                  | trexp (A.IfExp{test, then',else' = SOME else', pos}) =
                    let val {exp=e1, ty = test'} = trexp(test)
		    in if checkInt(test')
		       then let val {exp = e2, ty = exp1} = trexp(then')
 			        val {exp = e3, ty = exp2} = trexp(else')
		            in
				if checkEqualIf(exp1, exp2)
				then {exp = Tr.cond2Exp(e1,e2,e3), ty = exp1}
				else (error pos "then and else should produce the same type"; {exp=Tr.dummy, ty = Types.INT})
			    end
		       else {exp=Tr.dummy, ty = Types.UNIT}
	            end
                  | trexp (A.IfExp{test, then', else' = NONE, pos}) =
		    let val {exp=e1, ty = test'} = trexp(test)
		        val {exp=e2, ty = then''} = trexp(then')
	            in
                        if checkInt(test')
		        then if checkUnit(then'')
                             then {exp = Tr.cond1Exp(e1,e2), ty = Types.UNIT}
                             else (error pos "if expression should produce no value"; {exp=Tr.dummy, ty = Types.UNIT})
                        else (error pos "if statement should produce an integer value"; {exp=Tr.dummy, ty = Types.UNIT})
	            end
		  | trexp (A.WhileExp{test, body, pos}) =
		    let val {exp=et, ty = test'} = trexp(test)
		        val breaklabel' = Tr.initLoop
		        val {exp=eb, ty = body'} = (nested := !nested + 1; 
			                            transExp(venv, tenv, body, lev, breaklabel',expslist))
	            in
		        (nested := !nested - 1;
                         if checkInt(test')
                         then if checkUnit(body')
                              then {exp=Tr.whileExp(et,eb,breaklabel'), ty = Types.UNIT}
                              else (error pos "while expression should produce no value"; {exp=Tr.dummy, ty = Types.UNIT})
                         else (error pos "while statement should produce an integer value"; {exp=Tr.dummy, ty = Types.UNIT}))
		    end
                  | trexp (A.ForExp{var, escape, lo, hi, body, pos}) =
		    let val {exp=ehi, ty = hi'} = trexp(hi) 
		        val ({venv = venv', tenv = tenv'},_,expslist) = transDec(venv, tenv,
										 A.VarDec{name = var, escape = ref false,
											  typ = NONE, init = lo, pos = pos},lev,nil,breaklabel)
			val (acc,lo') = case Symbol.look(venv', var) of
					    SOME (Env.VarEntry({access = acc, ty = lo'})) => (acc,lo')
					  | _ => raise ForUndefined
                                                       
			val var = Tr.simpleVar(acc, lev)
			(* elo contains an assign tree due to evaluation in varDec *)
			val elo = hd expslist
		    in
			if checkInts(lo',hi')
                        then let val breaklabel' = Tr.initLoop
			         val {exp=ebody, ty = body'} = (nested := !nested + 1;
								transExp(venv', tenv', body, lev, breaklabel',expslist))
			     in
				 (nested := !nested - 1;
				  if checkUnit body'
				  then {exp=Tr.forExp(var, elo, ehi, ebody, breaklabel'), ty = Types.UNIT}
				  else (error pos "for expression should produce no value"; {exp=Tr.dummy, ty = Types.UNIT}))
			     end
			else (error pos "lower and upper bounds should be integers"; {exp=Tr.dummy, ty = Types.UNIT})
		    end
	          | trexp (A.LetExp{decs, body, pos}) =
                    let val ({venv=venv', tenv=tenv'},lev',expslist) = transDecs(venv, tenv, decs, lev, nil, breaklabel)
                        val {exp=e,ty=ty} = transExp(venv', tenv', body, lev', breaklabel,expslist)
                        val exps = Tr.makeSeq(expslist)
                        val e' = Tr.concatLet(exps,e,lev')
                    in
                        {exp=e',ty=ty}
                    end
			
		  | trexp (A.OpExp{left, oper, right, pos}) =
                    let val {exp = expl, ty = tyleft} = trexp(left)
                        val {exp = expr, ty = tyright} = trexp(right)
                    in
		        if oper=A.PlusOp orelse oper=A.MinusOp orelse oper=A.LtOp orelse oper=A.LeOp orelse oper=A.GtOp
			   orelse oper=A.GeOp orelse oper=A.TimesOp orelse oper=A.DivideOp
			then if checkInts(tyleft,tyright)
			     then {exp = Tr.opExp(oper,expl,expr), ty = Types.INT}
			     else (error pos "Operators must be integers";
			           {exp=Tr.dummy, ty = Types.INT})
		        else if checkEqual(tyleft,tyright)
			then {exp = Tr.opExp(oper,expl,expr), ty = Types.INT}
			else (error pos "the two operants must have the same type, or must be record-nil comparison";
                              {exp=Tr.dummy, ty = Types.INT})
		    end


                (* these functions check that the lvalues as expressions are bound to a value,
                       records also need to check if the field requested is valid one and return its type*)
		and trvar (A.SimpleVar (id, pos),lev) =
                    (case Symbol.look(venv, id) of
                         SOME (Env.VarEntry{access,ty}) => {exp = Tr.simpleVar (access,lev), ty = actual_ty ty}
                       | SOME (Env.FunEntry{...}) => (error pos ("variable " ^ Symbol.name id ^
                                                                 " defined as funtion");
               					      {exp=Tr.dummy, ty = Types.INT})
                       | NONE => (error pos ("undefined variable " ^ Symbol.name id);
                                  {exp=Tr.dummy, ty = Types.INT}))

		  | trvar (A.FieldVar (var, id, pos),lev) =
	            let val {exp = varaddress, ty = ty} = trvar(var,lev) (* hopefully get back ty = Types.RECORD _ *)
                        fun findField(((sym,ty)::xs),id,offset) =
                            if sym=id
                            then (ty,offset)
                            else findField(xs, id, offset+1)
                          | findField(nil, id,offset) =
                            (error pos ("undefined field '" ^ Symbol.name id ^ "'"); (Types.INT,offset))
                    in
                        (case ty of
                             Types.RECORD(lst, uniq) => let val (fieldty,offset) = findField (lst,id,0)(*find field's type*)
			                                in {exp = Tr.fieldVar(varaddress,offset), ty = fieldty} end 
                           | _ => (error pos "variable not defined as record"; {exp=Tr.dummy, ty = Types.INT}))
                    end

		  | trvar (A.SubscriptVar (var, ex, pos),lev) =
		    let val {exp = varaddress, ty = ty} = trvar(var,lev)
	                val {exp = exint, ty = expty} = trexp(ex)
                    in
                        if checkInt expty
			            then (case ty of
                                  Types.ARRAY(ty, uniq) => {exp = Tr.subScriptVar(varaddress,exint,uniq), ty = ty}
                                | _ => (error pos "variable not defined as array";
				        {exp=Tr.dummy, ty = Types.INT}))
		        else (error pos "subscript expression should return an integer";
			      {exp=Tr.dummy, ty = Types.INT})
                    end
            in
		trexp(init)
            end
    in
        let val startLevel = Tr.newLevel {parent=Tr.outermost, name=Temp.namedlabel "tigermain", formals=nil}
	    val breaklabel = Tr.initLoop
	    val {exp=e,ty=_} = transExp(Env.base_venv, Env.base_tenv, ast, startLevel, breaklabel, nil)
            val _ = Tr.procEntryExit {level=startLevel, body=e}
	in 
	    (*Tr.printTree e;*)Tr.getResult()
	end
    end
end
