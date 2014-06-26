structure MakeGraph :
	  sig
	      val instrs2graph : Assem.instr list ->
				 Flow.flowgraph * Flow.Graph.node list
	  end =
struct 
fun instrs2graph instrlist =
    (* Instructions are placed with reverse order *)
    let val graph = Graph.newGraph()
        val startblock = Graph.newNode graph
        fun iter(instr,(block,srclstlst,dstlstlst,mvlst,srctb,dsttb,ismovetb)) =
        case instr of
		    Assem.OPER {assem=a,src=slst,dst=dlst,jump=j} => 
                (case j of
                    SOME jumplist => let val srctb' = Graph.Table.enter(srctb,block,srclstlst)
                                         val dsttb' = Graph.Table.enter(dsttb,block,dstlstlst)
                                         val ismovetb' = Graph.Table.enter(ismovetb,block,mvlst)
                                         val newblock = Graph.newNode graph
                                         val _ = Graph.mk_edge{from=block,to=newblock}
                                     in (newblock,nil,nil,nil,srctb',dsttb',ismovetb') end
                  | NONE => (block,slst::srclstlst,dlst::dstlstlst,false::mvlst,srctb,dsttb,ismovetb))
          | Assem.MOVE {assem=a,src=s,dst=d} => (block,[s]::srclstlst,[d]::dstlstlst,
                                                 true::mvlst,srctb,dsttb,ismovetb)
	      | Assem.LABEL{assem=a,lab=l} => (block,srclstlst,dstlstlst,mvlst,srctb,dsttb,ismovetb)
          val (blocks,_,_,_,srctb,dsttb,ismovetb) = foldl iter (startblock,nil,nil,nil,Graph.Table.empty,
                                                        Graph.Table.empty,Graph.Table.empty) instrlist
          val nodelist = Graph.nodes graph
    in (Flow.FGRAPH{control=graph,def=dsttb,use=srctb,ismove=ismovetb},nodelist) end
end
