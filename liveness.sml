structure Liveness :
sig
    structure IGraph : GRAPH
    exception Impossible
    exception TableNotFoundLiveness
    datatype igraph =
        IGRAPH of {graph : IGraph.graph,
	           tnode : Temp.temp -> IGraph.node,
		   gtemp : IGraph.node -> Temp.temp,
		   moves : (IGraph.node * IGraph.node) list}

    val liveness : Flow.flowgraph -> Flow.Graph.node -> Temp.temp list (* node(block) -> live-out temps *)

    val show : TextIO.outstream * Flow.Graph.node list -> unit

    val printliveouts : TextIO.outstream * Flow.Graph.node list * (Flow.Graph.node -> Temp.temp list)  -> unit
end =
struct
    structure IGraph = Flow.Graph
    exception Impossible
    exception TableNotFoundLiveness
    (* taken from sml/nj library *)
    structure Set = ListSetFn(type ord_key = int; (*Temp.temp is int*)
                              val compare = Int.compare)

    datatype igraph =
        IGRAPH of {graph : IGraph.graph,
	           tnode : Temp.temp -> IGraph.node,
		   gtemp : IGraph.node -> Temp.temp,
		   moves : (IGraph.node * IGraph.node) list}

    (* for debug *)
    fun printSet set =
        (Set.app (fn t => (print ((Int.toString t) ^ "\n"))) set;
        print "\n")

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
		    val outset = case IGraph.Table.look(outtableset,node) of
				     SOME outset => outset
				   | NONE => raise Impossible
		    val in'set = case IGraph.Table.look(in'tableset,node) of
				     SOME in'set => in'set
				   | NONE => raise Impossible
		    val out'set = case IGraph.Table.look(out'tableset,node) of
				    SOME out'set => out'set
				  | NONE => raise Impossible
		in 
            (*print "infinish:\n";printSet inset;TextIO.flushOut(TextIO.stdOut);
            print "outfinish:\n";printSet outset;TextIO.flushOut(TextIO.stdOut);
            print "in'finish:\n";printSet in'set;TextIO.flushOut(TextIO.stdOut);
            print "out'finish:\n";printSet out'set;TextIO.flushOut(TextIO.stdOut);*)
		    if Set.equal(inset,in'set) andalso Set.equal(outset,out'set) 
		    then finish(intableset,outtableset,in'tableset,out'tableset,nodes)
		    else false
		end
	      | finish (intableset,outtableset,in'tableset,out'tableset,nil) =
		true

	    fun iteration (intablesets,outtablesets,in''tablesets,out''tablesets,node::nodes) = 
	        let val use = (case Flow.Graph.Table.look(usetable,node) of
		                  SOME uselist => Set.fromList(List.concat(uselist))
				        | NONE => Set.empty)
		    val def = (case Flow.Graph.Table.look(deftable,node) of
		                  SOME deflist => Set.fromList(List.concat(deflist))
				        | NONE => Set.empty)

            (*
            val _ = print("node " ^ (Graph.nodename node) ^ "\n")
            val _ = (print "use:\n"; printSet use; TextIO.flushOut(TextIO.stdOut))
            val _ = (print "def:\n"; printSet def;TextIO.flushOut(TextIO.stdOut))
            *)

            (* union all successors sets of succ[n] *)
		    val succnodeslist = Flow.Graph.succ node
            (*val _ = print( (Int.toString (length succnodeslist)) ^ "\n")*)
		    fun succins (succnode, set) = 
		        (case IGraph.Table.look(intablesets,succnode) of
			    SOME inset => Set.union(set,inset)
			  | NONE => raise Impossible)
	        val succinset = foldl succins Set.empty succnodeslist
                
            (*val _ = (print "successnode:\n"; printSet succinset; TextIO.flushOut(TextIO.stdOut))*)

		    val in'' = case IGraph.Table.look(intablesets,node) of
		                   SOME inset => (*(print "in':\n";printSet inset;TextIO.flushOut(TextIO.stdOut);*)
                                        Graph.Table.enter(in''tablesets,node,inset)
				 | NONE => raise Impossible
		    val out'' = case IGraph.Table.look(outtablesets,node) of
		                    SOME outset => (*(print "out':\n";printSet outset;TextIO.flushOut(TextIO.stdOut);*)
                                            Graph.Table.enter(out''tablesets,node,outset)
				  | NONE => raise Impossible

		    val out' = case IGraph.Table.look(outtablesets,node) of
		                   SOME outset => outset
				 | NONE => raise Impossible
		    val differ = Set.difference(out',def)

            (*val _ = (print "differ:\n";printSet differ;TextIO.flushOut(TextIO.stdOut))*)

		    val in' = IGraph.Table.enter(intablesets,node,Set.union(use,differ))
		    val out' = IGraph.Table.enter(outtablesets,node,succinset)

            (*
            val _ = (print "in:\n";printSet (Set.union(use,differ));TextIO.flushOut(TextIO.stdOut))
            val _ = (print "out:\n";printSet succinset;TextIO.flushOut(TextIO.stdOut))
            *)
	        in
               if finish (in',out',in'',out'',nodelist)
               then (in',out')
               else iteration(in',out',in'',out'',nodes)
		    end
      | iteration(intablesets,outtablesets,in''tablesets,out''tablesets,nil) =
        iteration(intablesets,outtablesets,in''tablesets,out''tablesets,nodelist)
	in
        iteration(intablesets,outtablesets,in'tablesets,out'tablesets,nodelist)
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
	  | NONE => raise TableNotFoundLiveness

    fun liveness flowgraph =
	let val Flow.FGRAPH{control=graph,def=deftable,use=usetable,ismove=movetable} = flowgraph
	    val nodelist = IGraph.nodes graph
	    val (_,outtablesets) = livenessalgo(nodelist,usetable,deftable)
    in outtablesetstotemp outtablesets end

(*
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
*)

    fun show (out, nodelist) =
        let fun printnode (node,count) =
            let fun adjnodes node =
		    (TextIO.output(out, (Int.toString count) ^ " ");
		     count+1)
		in
	            (TextIO.output(out,"Node " ^ (Int.toString count) ^ "\n" ^
		                    "Adjacent to nodes: " ); 
		     foldl adjnodes 0 (IGraph.adj node);  
                     TextIO.output(out,"\n");
		     count+1)
	        end

           val _ = foldl printnode 0 nodelist
       in () end
          

    fun printliveouts(out,nodelist,nodetotemps) =
        let fun node n =
            let val nname = Graph.nodename(n)
                val temps = nodetotemps(n)
                val strtemps = foldl (fn (temp,str) => str ^ (Temp.makestring temp) ^ " ") "" temps 
                val strtemps' = Graph.nodename(n) ^ ": " ^ strtemps ^ "\n"
            in TextIO.output(out,strtemps') end
        in app node nodelist end
end
