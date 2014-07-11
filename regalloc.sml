signature REG_ALLOC =
sig
    structure Frame : FRAME
    exception Implementation
    type allocation = Frame.register Temp.Table.table
    val alloc : Assem.instr list * Frame.frame -> Assem.instr list * allocation

    (*val test : unit*)
end

structure RegAlloc : REG_ALLOC =
struct
    structure Frame = ArmFrame
    structure IGraph = Liveness.IGraph
    structure Set = ListSetFn(type ord_key = int; 
                              val compare = Int.compare)
    type allocation = Frame.register Temp.Table.table
    exception Implementation

    fun mapping (table,node) =
        case IGraph.Table.look(table,node) of
	    SOME n => n
	  | NONE => raise Implementation
    fun alloc (instrs', frame) =
        let val initial' = Frame.initregisters
            val spillCost' = (fn x => 1)
            val registers' = Frame.registers
        in (Color.color{instrs=instrs',initial=initial',
                       spillCost=spillCost',registers=registers'};
            (nil,Frame.tempMap))
        end

    (*
    val t = case Temp.Table.look(Frame.tempMap,Frame.RV) of
                SOME x => x
              | NONE => (print "NONE!!\n";~1 )
    val test = print (Temp.makestring(t)^"\n")
    *)
end
