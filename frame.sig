signature FRAME =
sig 
    type register = int
    type registerStr = string
    val RV : Temp.temp (* return value *)
    val FP : Temp.temp (* frame pointer *)
    val a1 : register
    val a2 : register
    val a3 : register
    val a4 : register
    val K : int 
    val registers : register list
    val registersStr : registerStr list
    val registerTemps : register list
    val tempMap : register Temp.Table.table
    val tempMapStr : registerStr Temp.Table.table
    val callargs : register list
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

    (* 
       The current organization of the frame makes it hard to find
       escaping function arguments.findEscArgs finds all 
       escaping argumetns in called function frame.
       Any escaping variable can be get with getEsc 
       getEsc() function returns a list of 
       lists of calls  that contains all the escaping function argumnts 
       that will go to outgoing-registers in frame. removeEsc() function
       removes the first call list.
       A typical usage of the following functions is:
       1) findEscArgs
       2) getEsc
       3) removeEsc
    *)
    val findEscArgs : frame * frame -> unit (* calling level * parent of calling level *)
    val getEsc : Temp.label -> (int list * int) (* list of escaping arguments *)

    val procEntryExit1 : frame * Tree.exp -> Tree.stm
    val procEntryExit2 : frame * Assem.instr list -> Assem.instr list
    val procEntryExit3 : frame * Assem.instr list ->
                  {prolog : string, body : Assem.instr list, epilog : string}

    datatype frag = PROC of {body: Tree.stm, frame: frame}
                  | STRING of Temp.label * string
end
