structure Env :> ENV =
struct
  type ty = Types.ty 
  datatype enventry = VarEntry of {access : Translate.access, ty : ty}
                    | FunEntry of {level : Translate.level,
		                   label : Temp.label,
				   formals : ty list, result : ty}
  fun placePrimitives () =
    let open Symbol
        val env = enter(empty, symbol "int", Types.INT)
        val env = enter(env, symbol "string", Types.STRING)
    in env end 


  fun placePreFuns () =
    let open Symbol
        val env = enter(empty, symbol "print", FunEntry {formals = [Types.STRING], result = Types.UNIT,
	                                                 level = Translate.outermost, label = Temp.newlabel()})  
        val env = enter(env, symbol "flush", FunEntry {formals = [], result = Types.UNIT, 
	                                               level = Translate.outermost, label = Temp.newlabel()})  
        val env = enter(env, symbol "getchar", FunEntry {formals = [], result = Types.STRING, 
	                                                 level = Translate.outermost, label = Temp.newlabel()})  
        val env = enter(env, symbol "ord", FunEntry {formals = [Types.STRING], result = Types.INT, 
	                                             level = Translate.outermost, label = Temp.newlabel()})  
        val env = enter(env, symbol "chr", FunEntry {formals = [Types.INT], result = Types.STRING, 
	                                             level = Translate.outermost, label = Temp.newlabel()})  
        val env = enter(env, symbol "size", FunEntry {formals = [Types.STRING], result = Types.INT, 
	                                              level = Translate.outermost, label = Temp.newlabel()}) 
        val env = enter(env, symbol "substring", FunEntry {formals = [Types.STRING, Types.INT, Types.INT],
                                                           result = Types.STRING, level = Translate.outermost,
							   label = Temp.newlabel()})
        val env = enter(env, symbol "concat", FunEntry {formals = [Types.STRING, Types.STRING],
                                                        result = Types.STRING, level = Translate.outermost,
							label = Temp.newlabel()}) 
        val env = enter(env, symbol "not", FunEntry {formals = [Types.INT], result = Types.INT,
	                                             level = Translate.outermost, label = Temp.newlabel()})  
        val env = enter(env, symbol "exit", FunEntry {formals = [Types.INT], result = Types.UNIT,
	                                              level = Translate.outermost, label = Temp.newlabel()})  
    in env end

  val base_tenv = placePrimitives()
  val base_venv = placePreFuns()
end
