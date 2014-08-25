signature COLOR =
sig
    structure Frame : FRAME
    type allocation = Frame.registerStr Temp.Table.table
    val color : {instrs: Assem.instr list,
                 init : Temp.temp list,
                 spillCost : Graph.node -> int,
                 registers : Frame.register list}
                 -> allocation * Temp.temp list * Temp.temp list
end

structure Color : COLOR =
struct
    structure Frame = ArmFrame
    exception TableNotFoundColor
    exception RegisterOverflow
    exception Testing
    (* In order to put nodes we use the node to temp(int) gtenp *)
    structure Set = ListSetFn(type ord_key = int; 
                              val compare = Int.compare)
    structure SetTuple = ListSetFn(type ord_key = {u:int,v:int};
                                   val compare = (fn ({u=ul,v=vl},{u=ur,v=vr}) => 
                                       if ul<ur orelse vr<vr
                                       then LESS
                                       else if ul>ur orelse vl>vr
                                       then LESS
                                       else EQUAL))
    type allocation = Frame.registerStr Temp.Table.table

    (* TODO: when finish coalesced all possible mappings *)
    fun mapping (table,node) =
        case Flow.Graph.Table.look(table,node) of
            SOME n => n
          | NONE => (print "mapping "; raise TableNotFoundColor)
    fun mapDegree (table,node) = 
        case Temp.Table.look(table,node) of
            SOME n => n
          | NONE => 0
    fun mapAdjList (table,node) = 
        case Temp.Table.look(table,node) of
            SOME n => n
          | NONE => Set.empty 
    fun mapMoveList (table,node) = 
        case Temp.Table.look(table,node) of
            SOME n => n
          | NONE => Set.empty 
    fun mapColor (table,node) = 
        case Temp.Table.look(table,node) of
            SOME n => n
          | NONE => (print "mapcolor "; ~1)
    fun mapAlias (table,node) = 
        case Temp.Table.look(table,node) of
            SOME n => n
          | NONE => (print "mapalias ";raise TableNotFoundColor)

    fun printSet (name,set) =
        (print name;Set.app (fn item => print(": " ^ Int.toString(item) ^ "\n")) set;print "\n")
    fun printTupleSet (name,set) =
        (print name;
         SetTuple.app (fn {u,v} => print(": "^ "("^ Int.toString(u)^","^Int.toString(v)^")"^"\n")) set;
         print "\n")

    fun printTemptoInt (name,map,nodes) = 
        (print (name ^ ": \n"); 
        Set.app (fn node => case Temp.Table.look(map,node) of 
                            SOME i => print(Temp.makestring(node)^"-> "^Int.toString(i)^" ")
                          | NONE => (print "NONE ";())) nodes; 
         print("\n");TextIO.flushOut(TextIO.stdOut))
    fun printTemptoSet (name,map,nodes) =
         (print (name ^ ": \n");
         Set.app (fn node => case Temp.Table.look(map,node) of 
                             SOME set => (print(Temp.makestring(node)^"-> ");printSet("",set);print(" "))
                           | NONE => (print "NONE ")) nodes;
         print("\n");TextIO.flushOut(TextIO.stdOut))


    fun color {instrs, init, spillCost, registers} =
        let 
        val precolored = Set.fromList(Frame.registerTemps) (* set of int *)
        val color = Frame.tempMap
        (*
        val _ =  (print "initColor\n";
        Set.app (fn x => let val k = case Temp.Table.look(color,x) of SOME x => x | NONE => ~1
         in print(Temp.makestring(x)^"->"^Int.toString(k)^"\n") end) 
           (Set.union(precolored,(Set.fromList(Frame.registerTemps)))))
        *)
        val initial = ref nil 
        val simplifyWorklist = Set.empty
        val freezeWorklist = Set.empty
        val spillWorklist = Set.empty
        val spilledNodes = Set.empty
        val coalescedNodes = Set.empty
        val coloredNodes = Set.empty
        val selectStack = nil (* list of temps *)
        val coalescedMoves = Set.empty
        val constrainedMoves = Set.empty
        val frozenMoves = Set.empty
	    val worklistMoves = Set.empty 
        val activeMoves = Set.empty
	    val adjSet = SetTuple.empty       (* set of {u:int,v:int}   *)
	    val adjList = Temp.Table.empty (* Temp to set *)
	    val degree = Temp.Table.empty   (* Temp to int *)
        val moveList = Temp.Table.empty (* map temp to set of ints(temps) *)
        val alias = Temp.Table.empty (* temp to temp *)
        val K = length(Frame.registers)

	    fun main (moveList,worklistMoves,adjList,degree,adjSet) =
	    let val (flowgraph,nodes(* blocks *)) = MakeGraph.instrs2graph instrs
            val _ = MakeGraph.printNodes (TextIO.stdOut,flowgraph)
            val Flow.FGRAPH{control=fgraph,def=deftable,use=usetable,ismove=ismovetable} = flowgraph
            val liveOut = Liveness.liveness flowgraph
            val _ = Liveness.printliveouts (TextIO.stdOut,nodes,liveOut)
            (*val _ = print(Int.toString(length nodes))
            val _ = app (fn x => print (Int.toString x)) (List.concat(mapping(usetable,hd nodes)))
            val _ = (print "HEU\n\n";print (Graph.printgraph(nodes)))*)

        fun addEdge (u,v,adjList,degree,adjSet) =
            let val adjListsetuset = mapAdjList(adjList,u)
                val adjListsetvset = mapAdjList(adjList,v)
                val degreeuint = mapDegree(degree,u)
                val degreevint = mapDegree(degree,v)
                fun utest (adjList,degree) =
                    if not (Set.exists (fn x => if x = u then true else false) precolored)
                    then (Temp.Table.enter(adjList,u,Set.add(adjListsetuset,v)),
                          Temp.Table.enter(degree,u,degreeuint+1))
                    else (adjList,degree)
                fun vtest (adjList,degree) =
                    if not (Set.exists (fn x => if x = v then true else false) precolored)
                    then (Temp.Table.enter(adjList,v,Set.add(adjListsetvset,u)),
                          Temp.Table.enter(degree,v,degreevint+1))
                    else (adjList,degree)
                val t1 = (SetTuple.exists (fn {u=us,v=vs} => if (u=us andalso v=vs) orelse (u=vs andalso v=us) then true else false) adjSet)
                val t2 = u <> v
                (*val _ = print("t1 "^Bool.toString(t1)^" t2 "^Bool.toString(t2)^"\n")*)
            in
                if not t1 andalso t2
                then let val adjSet' = SetTuple.add(adjSet,{u=u,v=v})(* (u,v),(v,u) *)
                         val (adjList',degree') = utest(adjList,degree)
                         val (adjList'',degree'') = vtest(adjList',degree')
                         (*val _ =(printTemptoInt("degree",degree'',adjListsetuset);
                                 print("\n\n\n\n\n"))*)
                         (*val _ = printTemptoSet("adjsetInside",adjList'',adjListsetuset)*)
                     in (adjList'',degree'',adjSet') end
                else (*print "ELESLE!!!\n"*)(adjList,degree,adjSet)
            end 

        fun build (block,(moveList,worklistMoves,adjList,degree,adjSet)) =
            let val liveset = Set.fromList(liveOut block) 
                val use = mapping(usetable,block) (* temp list list *)
                val def = mapping(deftable,block) (* temp list list *)
                val ismove = mapping(ismovetable,block) (* bool list *)
                fun instructions (use::uses,def::defs,ismove::ismoves,
                                  moveList,worklistMoves,adjList,degree,adjSet,live) = 
                    let val useset = Set.fromList use
                        val defset = Set.fromList def
                        val (moveList',worklistMoves',live') = 
                               (case ismove of
                                     true => let val live' = Set.difference(live,useset)
                                                 val defunionuse = Set.union(useset,defset)
         (* val _ =(print "\nTestPrint"; Set.app (fn item => print("\n"^Temp.makestring(item)))defunionuse) *)
                                                 fun addI (item,moveList) =
                                                     let val defunionuse' = case Temp.Table.look(moveList,item) of
                                                                         SOME i => Set.union(i,defunionuse)
                                                                       | NONE => defunionuse
                                               val defunionuse'' = Set.difference(defunionuse',Set.singleton(item))
                                                     in Temp.Table.enter(moveList,item,defunionuse'') end
                                                 val moveList' = Set.foldl addI moveList defunionuse
                                                 val worklistMoves' = Set.union(worklistMoves,defunionuse)
                                              in (moveList',worklistMoves',live') end
                                    | false => (moveList,worklistMoves,live))
                        (*val _ = printSet("Lives':",live')*)
                        val live'' = Set.union(live',defset)
                        (*val _ = printSet("Lives'':",live'')*)
                        fun defsI (d,(adjList,degree,adjSet)) =
                            let val _ = initial := d::(!initial)
                                fun lives (l,(adjList,degree,adjSet)) =
                                (initial := l::(!initial); addEdge(d,l,adjList,degree,adjSet))
                            in Set.foldl lives (adjList,degree,adjSet) live'' end
                        val (adjList',degree',adjSet') = Set.foldl defsI (adjList,degree,adjSet) defset
                        (*val _ = print "FINISH\n"*)
                        (*val _ = (printTemptoInt("degree",degree,live'');
                        printTemptoSet("moveList",moveList,live'');
                        printTemptoSet("adjList",adjList,live'');
                        print("\n\n\n\n\n"))*)
                        val live''' = Set.union(useset,Set.difference(live'',defset))
                        (*val _ = printSet("Lives''':",live''')*)
                  in instructions (uses,defs,ismoves,moveList',worklistMoves',adjList',degree',adjSet',live''') end

                | instructions (nil,nil,nil,moveList,worklistMoves,adjList,degree,adjSet,live) =
                        (*(printTemptoInt("degree",degree,live);
                        printTemptoSet("moveList",moveList,live);
                        printTemptoSet("adjList",adjList,live);
                        printTupleSet("adjSet",adjSet);
                        print("\n\n\n\n\n");*)
                        (moveList,worklistMoves,adjList,degree,adjSet)

               val (moveList,worklistMoves,adjList,degree,adjSet) = 
                              instructions (use,def,ismove,moveList,worklistMoves,adjList,degree,adjSet,liveset)
           in (moveList,worklistMoves,adjList,degree,adjSet) end

        (* temp * ... -> Set *)
        fun nodeMoves (n,ml,am,wlm) =
            let val m = mapMoveList(ml,n)
                val aw = Set.union(am,wlm)
                (*
                val _ = (print "insidenodeMoves\n";
                        printSet(Temp.makestring(n),m);
                        printSet("union",aw);
                        printSet("intersection",Set.intersection(m,aw)))
                *)
            in Set.intersection(m,aw) end

        (* temp * ... -> bool *)
        fun moveRelated (n,moveList,activeMoves,worklistmoves) =
            not (Set.isEmpty(nodeMoves(n,moveList,activeMoves,worklistmoves)))

        (* temp * ... -> Set *)
        fun adjacent (n,adjList,selectStack,coalescedNodes) =
            let val m = mapAdjList(adjList,n)
                val sc = Set.union(Set.fromList selectStack,coalescedNodes)
            in Set.difference(m,sc) end

        (* temp * (....) -> spillWorklist * freezeWorklist * simplifyWorklist *)
        fun makeWorklist (n,(spillWorklist,freezeWorklist,simplifyWorklist,moveList,activeMoves,worklistmoves)) =
            let val d = mapDegree(degree,n)
            in
                if d > K
                then (Set.add(spillWorklist,n),freezeWorklist,simplifyWorklist,moveList,activeMoves,worklistmoves)
                else if moveRelated(n,moveList,activeMoves,worklistmoves)
                then (spillWorklist,Set.add(freezeWorklist,n),simplifyWorklist,moveList,activeMoves,worklistmoves)
                else (spillWorklist,freezeWorklist,Set.add(simplifyWorklist,n),moveList,activeMoves,worklistmoves)
            end

        (* temp * (...) -> simplifyWorklist * freezeWorklist * degree * spillWorklist * activeMoves * worklistMoves * 
                           moveList * coalescedNodes * selectStack *)
        fun decrementDegree (m,(simplifyWorklist,freezeWorklist,degree,spillWorklist,activeMoves,worklistMoves,
                                moveList,coalescedNodes,selectStack)) =
            let val d = mapDegree(degree,m)
                val degree' = Temp.Table.enter(degree,m,d-1)
            in 
               (* TODO: augment to full implementation 
               if d = K
               then let val unionRes = Set.union(mset,adjacent(m,adjList,selectStack,coalescedNodes))
                        val (activeMoves',worklistMoves',moveList') = enableMoves (unionRes,activeMoves,
                                                                                   worklistMoves,moveList)
                        val spillWorklist' = Set.difference(spillWorklist,mset)
                    n if moveRelated(m,moveList,activeMoves,worklistMoves)
                       then (simplifyWorklist,Set.union(freezeWorklist,mset),degree',spillWorklist',
                             activeMoves',worklistMoves',moveList',coalescedNodes,selectStack)
                       else (Set.union(simplifyWorklist,mset),freezeWorklist,degree',spillWorklist',
                             activeMoves',worklistMoves',moveList',coalescedNodes,selectStack)
                    end
               *)
                (simplifyWorklist,freezeWorklist,degree',spillWorklist,activeMoves,worklistMoves,
                  moveList,coalescedNodes,selectStack)
            end 

        (* temp ... -> degree * simplifyWorklist * freezeWorklist * activeMoves * worklistMoves * selectStack * 
                       moveList * coalescedNodes * spillWorklist*)
        fun simplify(degree,simplifyWorklist,freezeWorklist,activeMoves,worklistMoves,selectStack,moveList,
                        coalescedNodes,spillWorklist,adjL) =
            let fun chooseNode nodes =
                    case nodes of
                        n::ns => if mapDegree(degree,n) >= K
                                 then (print "TRYING HARD!!!!\n";chooseNode(ns)) (* Try other node *)
                                 else n
                      | nil => raise RegisterOverflow (* if spilling is needed give up *)

                val n = hd (Set.listItems simplifyWorklist) handle List.Empty => (print("simplify: simplifyworklist empty\n"); ~1)
                (* TODO: augment to full implementation, handle spill nodes *)
                (* Note: decrementDegree is evaluated first, before substracting n from sipmlifyWorklist *)
                val adjacentRes = adjacent(n,adjL,selectStack,coalescedNodes)
                (*val _ = printSet("adjacentRes",adjacentRes)*)
                val (simplifyWorklist,freezeWorklist,degree',spillWorklist,activeMoves,worklistMoves,moveList,
                     coalescedNodes,selectStack) = 
                   Set.foldl decrementDegree (simplifyWorklist,freezeWorklist,degree,spillWorklist,activeMoves,
                                               worklistMoves,moveList,coalescedNodes,selectStack)  adjacentRes
                val simplifyWorklist' = Set.difference(simplifyWorklist,Set.singleton n)
                val selectStack' = n::selectStack
                (*val _ = (print("Node "^Temp.makestring(n)^"\n");printTemptoInt("degree",degree',adjacentRes))
                val _ = printSet("simplifyWorklist",simplifyWorklist')
                val _ = (print "selectStack: ";app (fn item => print(Int.toString(item)^" ")) selectStack';print "\n")
                *)
            in (degree',simplifyWorklist',freezeWorklist,activeMoves,worklistMoves,selectStack',moveList,
                coalescedNodes,spillWorklist,adjL) end

        fun getAlias (n,alias,coalescedNodes) =
            if Set.exists (fn x => if n = x then true else false) coalescedNodes
            then let val al = mapAlias(alias,n) in getAlias(al,alias,coalescedNodes) end
            else n

        fun assignColors(selectStack,adjList,coloredNodes,color,spilledNodes,coalescedNodes,alias) = 
                (* temp::temps ... -> spilledNodes * coloredNodes * color *)
            let fun iter (n::selectStack,spilledNodes,coloredNodes,color) =
                    let val adjListn = mapAdjList(adjList,n)
                        fun diffcolors(w,okColor) =
                            let val g = getAlias(w,alias,coalescedNodes)
                                val cp = Set.union(coloredNodes,precolored)
                            in if Set.exists (fn x => if g = x then true else false) cp
                               then let val mapc = mapColor(color,g)
                                    in Set.difference(okColor,Set.singleton mapc) end
                               else okColor
                            end
                        val okColors = Set.fromList(Frame.registers)
                        val okColors' = Set.foldl diffcolors okColors adjListn
                        in if Set.isEmpty okColors'
                           then iter(selectStack,Set.add(spilledNodes,n),coloredNodes,color)
                           else let val SOME(c,okColors'') = List.getItem(Set.listItems okColors')
                                    val color' = Temp.Table.enter(color,n,c)
                                    val coloredNodes' = Set.add(coloredNodes,n)
                                    val okColors''' = Set.fromList okColors''
                                in iter(selectStack,spilledNodes,coloredNodes',color') end
                        end
                  | iter(nil,spilledNodes,coloredNodes,color) =
                        (spilledNodes,coloredNodes,color)
                 val (spilledNodes,coloredNodes,color) = iter(selectStack,spilledNodes,coloredNodes,color)
                 fun updatecolor (n,color) =
                     let val g = getAlias(n,alias,coalescedNodes)
                     in Temp.Table.enter(color,n,g) end
                 val color' = Set.foldl updatecolor color coalescedNodes
             in (spilledNodes,coloredNodes,color') end

        (* 1. construct the control flow graph *)
        (* 2. construct the data flow graph *)
        
        (* 3. construct the interference graph *)
        val (moveList,worklistMoves,adjList,degree,adjSet) = foldl build (moveList,
                                                      worklistMoves,adjList,degree,adjSet) nodes

        (*
        val _ = (print "initials: \n";
                 Set.app (fn x => print (Int.toString(x)^" ")) (Set.fromList(!initial));
                 print "\n")
        *)
        (* remove any machine registers(registers that their colors do not change) from initials *)
        val initial' = Set.difference(Set.fromList(!initial),precolored)
        (*
        val _ = printSet("worklistMoves",initial')
        val _ = printTemptoSet("adjList",adjList,initial');
        *)
        val _ = printTemptoInt("degree",degree,initial');
        (*
        val _ = printTupleSet("adjSet",adjSet);
        val _ = printTemptoSet("moveList",moveList,initial');
        *)

        val initial' = if List.null(init) then initial' else (Set.fromList init)

        (* 4. make initial worklist *)
        val (spillWorklist,freezeWorklist,simplifyWorklist,moveList,activeMoves,worklistMoves) = Set.foldl makeWorklist 
                (spillWorklist,freezeWorklist,simplifyWorklist,moveList,activeMoves,worklistMoves) initial'

        (*
        val _ = printSet("spillWorklist",spillWorklist)
        val _ = printSet("freezeWorklist",freezeWorklist)
        val _ = printSet("simplifyWorklist",simplifyWorklist)
        *)

        fun repeat (d,swl,stack) =
            let val (d',swl',freezeWorklist,activeMoves,worklistMoves,stack',moveList,
                     coalescedNodes,spillWorklist,ajdL) = simplify(d,swl,freezeWorklist, 
                                                              activeMoves,worklistMoves,stack,
                                                              moveList,coalescedNodes,spillWorklist,adjList)
               (*val _ = printSet("simplifyWorklist",simplifyWorklist')*)
            in if (Set.isEmpty(swl'))
               then (*printTemptoInt("degree",d,initial'*)stack'
               else repeat(d',swl',stack')
            end

       (* In this case of register allocator only the simplify algorithm will be implemented *)
       (* Freeze worklist and spill worklist have no meaning so registers will union with simplify worklist *)
       val simplifyWorklist' = Set.union(simplifyWorklist,freezeWorklist)
       val simplifyWorklist' = Set.union(simplifyWorklist',spillWorklist)

       val freezeWorklist = Set.empty
       val spillWorklist = Set.empty

       (*val _ = printSet("simplifyWorklist",simplifyWorklist')*)


       (* 5. repeat simplification *)
       val selectStack' = repeat(degree,simplifyWorklist',selectStack)


      (*val _ = (print "selectStack: ";app (fn item => print(Int.toString(item)^" ")) selectStack';print "\n")*)
       (*val _ = if color = Temp.Table.empty then print "HOLA\n" else print "NO\n"*)

       (* 6. color the graph *)
       val (spilledNodes,coloredNodes,color') = 
                       assignColors(selectStack',adjList,coloredNodes,color,spilledNodes,coalescedNodes,alias)

       fun checkAssignColors s =
           let val t = Set.isEmpty s
           in if not t
              then (print "SpilledNodes not Empty!!!\n")
              else ()
           end
       (*val _ = checkAssignColors spilledNodes*)
       val _ = printSet("spilledNodes",spilledNodes)
       (*
       fun checkInitialColoredNodes (i,c) =
           case Set.compare(i,c) of
               EQUAL => ()
             | _ => (print "initial set NOT match with coloredNodes!!\n")
       val _ = checkInitialColoredNodes(initial',coloredNodes)

       val _ = printSet("coloredNodes",coloredNodes)
       val _ = printTemptoInt("color",color',initial')
       *)

       (*val color'' = Set.foldl (fn (c,l) => (mapColor(color',c))::l) nil initial'*)
       (*val _ = app (fn c => (print "colorList"; print(Int.toString(c)^" "); print "\n")) color''*)

       fun makeRegisterTable (n,t) =
           let val reg = mapColor(color',n)
           in Temp.Table.enter(t,n,Int.toString(reg)) end
       val regt = Set.foldl makeRegisterTable Frame.tempMapStr initial'

        (*
       val _ = (print "regt: "; Set.app (fn n => print(Int.toString(n)^": "^valOf(Temp.Table.look(regt,n))^" ")) (initial'); print "\n")
       *)

       in (regt,Set.listItems spilledNodes,Set.listItems coloredNodes) end (* main *)
    in (main(moveList,worklistMoves,adjList,degree,adjSet)) end (* color *)

end
