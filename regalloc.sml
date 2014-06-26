signature REG_ALLOC =
sig
    structure Frame : FRAME
    exception Implementation
    type allocation = Frame.register Temp.Table.table
    val alloc : Assem.instr list * Frame.frame -> Assem.instr list * allocation
end

structure RegAlloc : REG_ALLOC =
struct
    structure Frame = MipsFrame
    structure IGraph = Liveness.IGraph
    structure Set = ListSetFn(type ord_key = int; 
                              val compare = Int.compare)
    type allocation = Frame.register Temp.Table.table
    exception Implementation

    fun mapping (table,node) =
        case IGraph.Table.look(table,node) of
	    SOME n => n
	  | NONE => raise Implementation

    fun alloc (instrs, fr) =
        ()
end
