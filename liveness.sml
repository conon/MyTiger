structure Liveness :
sig
    structure IGraph : GRAPH
    exception Impossible
    datatype igraph =
        IGRAPH of {graph : IGraph.graph,
	           tnode : Temp.temp -> IGraph.node,
		   gtemp : IGraph.node -> Temp.temp,
		   moves : (IGraph.node * IGraph.node) list}

    val liveness : Flow.flowgraph -> Flow.Graph.node -> Temp.temp list (* node(block) -> live-out temps *)

    val show : TextIO.outstream * igraph -> unit
end =
struct
    structure IGraph = Flow.Graph
    exception Impossible
    (* taken from sml/nj library *)
    structure Set = ListSetFn(type ord_key = int; (*Temp.temp is int*)
                              val compare = Int.compare)

    datatype igraph =
        IGRAPH of {graph : IGraph.graph,
	           tnode : Temp.temp -> IGraph.node,
		   gtemp : IGraph.node -> Temp.temp,
		   moves : (IGraph.node * IGraph.node) list}

    fun livenessalgo (nodelist,usetable,deftable) =
        let fun init (node,(intable,outtable,in'table,out'table)) =
	            (IGraph.Table.enter(intable,node,Set.empty), 
		     IGraph.Table.enter(outtable,node,Set.empty),
		     IGraph.Table.enter(in'table,node,Set.empty),
		     IGraph.Table.enter(out'table,node,Set.empty))
	    val (intablesets,outtablesets,in'tablesets,out'tablesets) = 
	        foldl init (IGraph.Table.empty,IGraph.Table.empty,IGraph.Table.empty,IGraph.Table.empty) nodelist

	    fun finish (intableset,outtableset,in'tableset,out'tableset,node::nodes) =
		let val inset = case IGraph.Table.look(intableset,node) of
				    SOME inset => inset
				  | NONE => raise Impossible
		    val outset = case IGraph.Table.look(intableset,node) of
				     SOME outset => outset
				   | NONE => raise Impossible
		    val in'set = case IGraph.Table.look(intableset,node) of
				     SOME in'set => in'set
				   | NONE => raise Impossible
		    val out'set = case IGraph.Table.look(intableset,node) of
				    SOME out'set => out'set
				  | NONE => raise Impossible
		in 
		    if Set.equal(inset,in'set) andalso Set.equal(outset,out'set) 
		    then finish(intableset,outtableset,in'tableset,out'tableset,nodes)
		    else false
		end
	      | finish (intableset,outtableset,in'tableset,out'tableset,nil) =
		true

	    fun iteration (node,(intablesets,outtablesets,in''tablesets,out''tablesets)) = 
	        let val use = (case Flow.Graph.Table.look(usetable,node) of
		                  SOME uselist => Set.fromList(List.concat(uselist))
				| NONE => raise Impossible)
		    val def = (case Flow.Graph.Table.look(deftable,node) of
		                  SOME deflist => Set.fromList(List.concat(deflist))
				| NONE => raise Impossible)

            (* union all successors sets of succ[n] *)
		    val succnodeslist = Flow.Graph.succ node
		    fun succins (succnode, set) = 
		        (case IGraph.Table.look(intablesets,succnode) of
			    SOME inset => Set.union(set,inset)
			  | NONE => raise Impossible)
	        val succinset = foldl succins Set.empty succnodeslist

		    val in'' = case IGraph.Table.look(intablesets,node) of
		                   SOME inset => Graph.Table.enter(in''tablesets,node,inset)
				 | NONE => raise Impossible
		    val out'' = case IGraph.Table.look(outtablesets,node) of
		                    SOME outset => Graph.Table.enter(out''tablesets,node,outset)
				  | NONE => raise Impossible

		    val out' = case IGraph.Table.look(outtablesets,node) of
		                   SOME outset => outset
				 | NONE => raise Impossible
		    val differ = Set.difference(out',def)

		    val in' = IGraph.Table.enter(intablesets,node,Set.union(use,differ))
		    val out' = IGraph.Table.enter(outtablesets,node,succinset)
	       
	        in
		    (in',out',in'',out'')
		end
	    fun evaliteration (intablesets,outtablesets,in''tablesets,out''tablesets) =
	        let val (intablesets',outtablesets',in'tablesets',out'tablesets) = foldl iteration 
		                                   (intablesets,outtablesets,in''tablesets,out''tablesets) nodelist
		in 
		   if finish (intablesets',outtablesets',in'tablesets',out'tablesets,nodelist)
		   then (intablesets',outtablesets')
		   else evaliteration (intablesets',outtablesets',in'tablesets',out'tablesets)
		end
	in
	    evaliteration (intablesets,outtablesets,in'tablesets,out'tablesets)
	end

    fun temptabletonode table temp =
        case Temp.Table.look(table,temp) of
	    SOME n => n
	  | NONE => raise Impossible

    fun nodetabletotemp table node =
        case IGraph.Table.look(table,node) of
	    SOME t => t
	  | NONE => raise Impossible

    fun outtablesetstotemp table node =
        case IGraph.Table.look(table,node) of
	    SOME tempset => Set.listItems tempset
	  | NONE => raise Impossible

    fun liveness flowgraph =
	let val Flow.FGRAPH{control=graph,def=deftable,use=usetable,ismove=movetable} = flowgraph
	    val nodelist = IGraph.nodes graph
	    val (_,outtablesets) = livenessalgo(nodelist,deftable,usetable)
    in outtablesetstotemp outtablesets end
        (*
	    val ig = IGraph.newGraph () 
	    fun nodes (node,(temptable,nodetable,moveslist)) =
	        let fun defs (def,(worklist,moveslist,adjSet,adjList,degree))=
                let val use = case Flow.Graph.Table.look(usetable,node) of (* for outlist[n] - use *)
                                  SOME uselist => hd uselist
                            | NONE => raise Impossible
                    val newnodedef = IGraph.newNode ig
                    val temptable' = Temp.Table.enter(temptable,def,newnodedef)
                    val nodetable' = IGraph.Table.enter(nodetable,newnodedef,def)
                    val ismove = case Flow.Graph.Table.look(movetable,node) of
                                     SOME m => m
                                   | NONE => raise Impossible
                    fun outs (out,(temptable,nodetable,moveslist)) =
                    let val newnodeout = IGraph.newNode ig
                        val temptable'' = Temp.Table.enter(temptable,out,newnodeout)
                        val nodetable'' = IGraph.Table.enter(nodetable,newnodeout,out)
                    in
                        case ismove of
                            true => if out=use (* a <- c add intererence for any bi not the same as c *)
                                then (temptable'',nodetable'',(newnodedef,newnodeout)::moveslist)
                            else (IGraph.mk_edge{from=newnodedef,to=newnodeout};
                                  (temptable'',nodetable'',moveslist))
                          | false => (IGraph.mk_edge{from=newnodedef,to=newnodeout};
                                      (temptable'',nodetable'',moveslist))
                    end
                   val outlist = case IGraph.Table.look(outablesets,node) of
                             SOME set => Set.listItems set
                           | NONE => raise Impossible
                in
                    foldl outs (temptable',nodetable',moveslist) outlist
                end
                   val deflist = case Flow.Graph.Table.look(deftable,node) of
                                 SOME defnodelist => defnodelist
                       | NONE => raise Impossible
	       in
               (* for every def (instruction) *)
	           foldl defs (worklist,moveslist,adjSet,adjList,degree) deflist
	       end
	    val (temptable,nodetable,moveslist) = foldl nodes (Temp.Table.empty,IGraph.Table.empty,nil) nodelist
	in
	    (IGRAPH{graph=ig,tnode=temptabletonode temptable,
	            gtemp=nodetabletotemp nodetable,moves=moveslist},outtablesetstotemp outablesets)
	end     
    *)

    fun show (out,igraph) =
        let val IGRAPH{graph=graph,tnode=t,gtemp=g,moves=mlst} = igraph
	    val nodelist = IGraph.nodes graph
	    fun printnode (node,count) =
	        let fun adjnodes node =
		        (TextIO.output(out, (Int.toString count) ^ " ");
		         count+1)
		in
	            (TextIO.output(out,"Node " ^ (Int.toString count) ^ "\n" ^
		                    "Adjacent to nodes: " ); 
		     foldl adjnodes 0 (IGraph.adj node);  
		     count+1)
	        end
	        
	    val _ = foldl printnode 0 nodelist 
	in () end
end
