structure MakeGraph :
	  sig
	      val instrs2graph : Assem.instr list ->
			 Flow.flowgraph * Flow.Graph.node list
          val printNodes : TextIO.outstream * Flow.flowgraph -> unit
	  end =
struct 
exception TableNotFoundMakeGraph

fun instrs2graph instrlist =
    (* Instructions are placed with reverse order *)
    let val graph = Graph.newGraph()
        val startblock = Graph.newNode graph
        fun iter(instr,(blocklst,srclstlst,dstlstlst,mvlst,srctb,dsttb,ismovetb,labeltb,jumplisttb)) =
            case instr of
                Assem.OPER {assem=a,src=slst,dst=dlst,jump=j} => 
                    (case j of
                        SOME jumplist => let val block = hd blocklst
                                             val srctb' = Graph.Table.enter(srctb,block,slst::srclstlst) (* append possible srcs *)
                                             val dsttb' = Graph.Table.enter(dsttb,block,dlst::dstlstlst) (* append possible dsts *)
                                             val ismovetb' = Graph.Table.enter(ismovetb,block,false::mvlst)
                                             val jumplisttb' = Graph.Table.enter(jumplisttb,block,jumplist)
                                             val newblock = Graph.newNode graph
                                             val srctb'' = Graph.Table.enter(srctb',newblock,nil) 
                                             val dsttb'' = Graph.Table.enter(dsttb',newblock,nil) 
                                             val ismovetb'' = Graph.Table.enter(ismovetb',newblock,nil)
                                             val jumplisttb'' = Graph.Table.enter(jumplisttb',newblock,nil)
                                         in (newblock::blocklst,nil,nil,nil,srctb'',dsttb'',
                                             ismovetb'',labeltb,jumplisttb'') 
                                         end
                      | NONE => (blocklst,slst::srclstlst,dlst::dstlstlst,false::mvlst,srctb,
                                 dsttb,ismovetb,labeltb,jumplisttb))
              | Assem.MOVE {assem=a,src=s,dst=d} => (blocklst,[s]::srclstlst,[d]::dstlstlst,
                                                     true::mvlst,srctb,dsttb,ismovetb,labeltb,jumplisttb)
              | Assem.LABEL{assem=a,lab=l} => let val labeltb' = Symbol.enter(labeltb,l,hd blocklst)
                                              in (blocklst,srclstlst,dstlstlst,mvlst,
                                                  srctb,dsttb,ismovetb,labeltb',jumplisttb) 
                                              end
      (* Make the graph nodes(blocks) *)
      val (blocks,_,_,_,srctb,dsttb,ismovetb,labeltb,jumplisttb) = 
      foldl iter ([startblock],nil,nil,nil,Graph.Table.empty,
                  Graph.Table.empty,Graph.Table.empty,Symbol.empty,Graph.Table.empty) instrlist

      (*val _ = print ("Blocks length: " ^ Int.toString((length blocks)) ^ "\n")*)

      fun makeEdges block =
          let fun fetchBlock label =
          (*print("LABEL"^Symbol.name(label)^"\n");*)
                  case Symbol.look(labeltb,label) of
                      SOME b => b
                    | NONE => raise TableNotFoundMakeGraph
              fun fetchJumplist block =
                  case Graph.Table.look(jumplisttb,block) of
                      SOME jl => jl
                    | NONE => nil
              val jumplist = fetchJumplist(block)
              (*val _ = app (fn n => print("NONE: " ^ Graph.nodename(block) ^ " -> " ^ Graph.nodename(fetchBlock(n)) ^ "\n")) jumplist*)
         in app (fn jumplabel => Graph.mk_edge{from=block,to=fetchBlock jumplabel}) jumplist end

     (* adjust edges *)
     val _ = app makeEdges blocks
     val nodelist = Graph.nodes graph
    in (Flow.FGRAPH{control=graph,def=dsttb,use=srctb,ismove=ismovetb},nodelist) end


   fun printNodes(out,flowgraph) =
       let val Flow.FGRAPH{control,def,use,ismove} = flowgraph
           val nodelist = Graph.nodes control
           fun node n =
               let val deflistlist = case Graph.Table.look(def,n) of
                                         SOME d => d
                                       | NONE => (print("No defs in node" ^ (Graph.nodename(n)) ^ "\n"); nil)
                   val uselistlist = case Graph.Table.look(use,n) of
                                         SOME u => u
                                       | NONE =>  (print("No uses in node" ^ Graph.nodename(n) ^ "\n"); nil)
                   fun printuse use =
                       print (Temp.makestring(use) ^ "\n")
                   fun printdef def =
                       print (Temp.makestring(def) ^ "\n")
                   val defuselistlist = ListPair.zip(deflistlist,uselistlist)
               in  
                   (print ("node " ^ Graph.nodename(n) ^ "\n");
                    app (fn (deflist,uselist) => (print "defs:\n";
                                                  app printdef deflist; 
                                                  print "uses:\n";
                                                  app printuse uselist;
                                                  print "\n\n")) (rev defuselistlist))
               end
           in app node nodelist end
end
