structure FindEscape :
sig
    val findEscape : Absyn.exp -> unit
end =
struct
    open Absyn
    type depth = int
    type escEnv = (depth * bool ref) Symbol.table

    fun traverseVar(env:escEnv, d:depth, s:Absyn.var) : unit =
        case s of
            SimpleVar (s,_) => 
                              (case Symbol.look(env,s) of
                                  SOME (declaredDepth,declaredEsc) => if declaredDepth < d
                                                                      then declaredEsc := true
                                                                      else ()
                                | NONE => ()) (* suppress any errors on findescape analysis,
                                                 such errors will be catch be the type-checker *)
          | FieldVar (var,s,_) => traverseVar(env, d, var)
          | SubscriptVar (var,e,_) => (traverseVar(env, d, var);
				       traverseExp(env, d, e))
    and traverseExp(env:escEnv, d:depth, s:Absyn.exp) : unit =
        case s of
            VarExp v => traverseVar(env,d,v)
          | NilExp => ()
          | IntExp _ => ()
          | StringExp _ => () 
          | CallExp {args, ...} => let fun argexp e =
                                       traverseExp(env,d,e)
                                   in app argexp args end
          | OpExp {left,right, ...} => (traverseExp(env,d,left);
                                        traverseExp(env,d,right))
          | RecordExp {fields, ...} => ()
          | SeqExp seqs => let val (exps,_) = ListPair.unzip seqs
                               fun seq e =
                                   traverseExp(env,d,e)
                               in app seq exps end
          | AssignExp {var,exp,...} => (traverseVar(env,d,var);
                                        traverseExp(env,d,exp))
          | IfExp {test,then',else',...} => (traverseExp(env,d,test);
                                             traverseExp(env,d,then');
                                             case else' of
                                                 SOME x => traverseExp(env,d,x)
                                               | NONE => ())
          | WhileExp {test,body,...} => (traverseExp(env,d,test);
                                        traverseExp(env,d,body))
          | ForExp {escape,lo,hi,body, ...} => ()
          | BreakExp _ => ()
          | LetExp {decs,body,...} => let val env' = traverseDecs(env,d,decs)
                                      in traverseExp(env',d,body) end
          | ArrayExp {size,init, ...} => (traverseExp(env,d,size);
                                          traverseExp(env,d,init))
    and traverseDecs(env, d, s:Absyn.dec list) : escEnv =
        let fun param ({name,escape,typ,pos},env) =
               (escape := false; Symbol.enter(env,name,(d+1,escape)))
            fun functions ({name,params,result,body,pos},env) =
                let val env' = foldl param env params
                in traverseExp(env',d+1,body) end
            fun traverseDec(dec,env) =
                case dec of
                FunctionDec fdlst => let fun iter (fd::fds) =
                                                (functions(fd,env); iter(fds))
                                           | iter nil =
                                                ()
                                     in (iter fdlst; env) end (* drop updated enviroment *)
              | VarDec {name,escape,init, ...} => (traverseExp(env,d,init);
                                                   escape := false;
                                                   Symbol.enter(env,name,(d,escape)))
              | TypeDec _ => env
       in foldl traverseDec env s end
        
    fun findEscape(prog: Absyn.exp) : unit =
        traverseExp (Symbol.empty,0,prog)

end
