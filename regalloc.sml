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

    val newinstrs = ref nil

    fun rewriteprogram (instrs,spills) = 
       let
           fun transforminstrs (instrs) =
               case instrs of
                   i::is => (case i of
                       Assem.OPER {assem,dst,src,jump} => 
                            let fun findDefUse spill =
                                let val t1 = (List.exists (fn d => if d = spill then true else false) dst)
                                    val t2 = (List.exists (fn d => if d = spill then true else false) src)
                                in 
                                    case (t1,t2) of
                                       (true,false) => (newinstrs := (Assem.OPER{assem="str `d0, [fp, #-8]"^"\n", 
                                                                 dst=[spill], src=nil, jump=NONE})::i::(!newinstrs);
                                                        transforminstrs(is))
                                     | (false,true) => (newinstrs := i::(Assem.OPER{assem="ldr `d0, [fp, #-8]"^"\n", 
                                                           dst=[spill], src=[], jump=NONE})::(!newinstrs);
                                                        transforminstrs(is))
                                     | (true,true) => (newinstrs := (Assem.OPER{assem="ldr `d0, [fp, #-8]"^"\n", 
                                                                    dst=[spill], src=nil, jump=NONE})::i::
                                                                    (Assem.OPER{assem="str `d0, [fp, #-8]"^"\n", 
                                                                    dst=[spill], src=nil, jump=NONE})::(!newinstrs);
                                                       transforminstrs(is))
                                     | (false,false) => (print("OPER:false,false\n");newinstrs := i::(!newinstrs); transforminstrs(is))
                                end
                           in app findDefUse spills end
                     | Assem.LABEL {assem,lab} =>  (newinstrs := i::(!newinstrs); transforminstrs(is))
                     | Assem.MOVE {assem,dst,src} => 
                            let fun findDefUse spill =
                                let val t1 = dst = spill
                                    val t2 = src = spill
                                in
                                   case (t1,t2) of
                                       (true,false) => (newinstrs := (Assem.OPER{assem="str `d0, [fp, #-8]"^"\n", 
                                                                dst=[spill], src=nil, jump=NONE})::i::(!newinstrs);
                                                        transforminstrs(is))
                                     | (false,true) => (newinstrs := i::(Assem.OPER{assem="ldr `d0, [fp, #-8]"^"\n", 
                                                                    dst=[spill], src=nil, jump=NONE})::(!newinstrs);
                                                        transforminstrs(is))
                                     | (true,true) => (newinstrs := (Assem.OPER{assem="ldr `d0, [fp, #-8]"^"\n", 
                                                                    dst=[spill], src=nil, jump=NONE})::i::
                                                                    (Assem.OPER{assem="str `d0, [fp, #-8]"^"\n", 
                                                                    dst=[spill], src=nil, jump=NONE})::(!newinstrs);
                                                       transforminstrs(is))
                                     | (false,false) => (print("MOVE:false,false\n");newinstrs := i::(!newinstrs); transforminstrs(is))
                                end
                           in app findDefUse spills end)
                 | nil => ()
               
       in
           (transforminstrs(instrs);
           print ("TRANSFORM\n");
           app (fn i => TextIO.output(TextIO.stdOut,Assem.format(Temp.makestring)i)) (rev (!newinstrs));
           (*raise Implementation;*)
           List.rev(!newinstrs))
       end
           
    fun iter (instrs,initial,spillCost,registers) =
        let val (alloc,spilledNodes,coloredNodes) = Color.color{instrs=instrs,init=initial,
                                      spillCost=spillCost,registers=registers}
        in
           if List.null(spilledNodes)
           then (instrs,alloc)
           else let val instrs' = (newinstrs := nil; rewriteprogram(instrs,spilledNodes))
                in iter(instrs',spilledNodes@coloredNodes@initial,spillCost,registers) end
        end

    fun alloc (instrs, frame) =
        let val initial' = nil
            val spillCost' = (fn x => 1)
            val registers' = Frame.registers
            val instrs' = Frame.procEntryExit2(frame,instrs)
            val (instrs'',alloc) = iter (instrs,initial',spillCost',registers')
            in (instrs'',alloc) end

    (*
    val t = case Temp.Table.look(Frame.tempMap,Frame.RV) of
                SOME x => x
              | NONE => (print "NONE!!\n";~1 )
    val test = print (Temp.makestring(t)^"\n")
    *)
end
