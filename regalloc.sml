signature REG_ALLOC =
sig
    structure Frame : FRAME
    exception Implementation
    type allocation = Frame.registerStr Temp.Table.table
    val alloc : Assem.instr list * Frame.frame -> Assem.instr list * allocation

    (*val test : unit*)
end

structure RegAlloc : REG_ALLOC =
struct
    structure Frame = ArmFrame
    structure IGraph = Liveness.IGraph
    structure Set = ListSetFn(type ord_key = int; 
                              val compare = Int.compare)
    type allocation = Frame.registerStr Temp.Table.table
    exception Implementation

    fun mapping (table,node) =
        case IGraph.Table.look(table,node) of
	    SOME n => n
	  | NONE => raise Implementation
    fun alloc (instrs, frame) =
        let val initial' = Temp.Table.empty
            val spillCost' = (fn x => 1)
            val registers' = Frame.registers
            val instrs' = Frame.procEntryExit2(frame,instrs)
            val (alloc,templist) = Color.color{instrs=instrs,initial=initial',
                                      spillCost=spillCost',registers=registers'}
            in (instrs,alloc) end

    (*
    val t = case Temp.Table.look(Frame.tempMap,Frame.RV) of
                SOME x => x
              | NONE => (print "NONE!!\n";~1 )
    val test = print (Temp.makestring(t)^"\n")
    *)
end
