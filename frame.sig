signature FRAME =
sig 
    type register = int
    type registerStr = string
    val RV : Temp.temp (* return value *)
    val FP : Temp.temp (* frame pointer *)
    val K : int 
    val registers : register list
    val registerTemps : register list
    val tempMap : register Temp.Table.table
    val wordSize : int
    val externalCall : string * Tree.exp list -> Tree.exp

    type frame
    type access      
    val exp : access -> Tree.exp -> Tree.exp  

    val newFrame : {name: Temp.label, formals: bool list} -> frame
    val formals : frame -> access list
    val name : frame -> Temp.label
    val allocLocal : frame -> bool -> access
    val string : Temp.label * string -> string 
    
    val getEsc : unit -> int list list (* list of escaping arguments *)
    val removeEsc : unit -> unit (* removes evaluated list of escaping arguments *)
    (* The current organization of the frame makes it hard to evaluate
       escaping arguments, this function makes all the moves to "first K"
       or moves escaping and "more than K" values to outgoing
       registers in stack 
    val passingArguments : frame * Tree.exp list -> Tree.exp list
    *)
    val findEscArgs : frame -> unit
    val procEntryExit1 : frame * Tree.exp -> Tree.stm
    (*
    val procEntryExit2 : frame * Assem.instr list -> Assem.instr list
    val procEntryExit3 : frame * Assem.instr list ->
                  {prolog : string, body : Assem.instr list, epilog : string}
    *)

    datatype frag = PROC of {body: Tree.stm, frame: frame}
                  | STRING of Temp.label * string
end
