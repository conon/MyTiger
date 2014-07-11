signature COLOR =
sig
    structure Frame : FRAME
    exception TableNotFoundColor
    type allocation = Frame.register Temp.Table.table
    val color : {instrs: Assem.instr list,
                 initial : allocation,
                 spillCost : Graph.node -> int,
                 registers : Frame.register list}
                 -> allocation * Temp.temp list
end

structure Color : COLOR =
struct
    structure Frame = ArmFrame
    exception TableNotFoundColor
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
    type allocation = Frame.register Temp.Table.table

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
          | NONE => (print "mapcolor ";raise TableNotFoundColor)
    fun mapAlias (table,node) = 
        case Temp.Table.look(table,node) of
            SOME n => n
          | NONE => (print "mapalias ";raise TableNotFoundColor)

    val color = ref 0
    fun makeColorsPre (registers) =
        foldl (fn (str,colors) => let val c = !color in (color := !color + 1; c::colors) end) nil registers

    fun printSet (name,set) =
        (print name;Set.app (fn item => print(": " ^ Int.toString(item) ^ "\n")) set)
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


    fun color {instrs, initial, spillCost, registers} =
        let 
        val listprecolors = makeColorsPre(Frame.registers)
        val precolored = Set.fromList listprecolors (* TODO: assign colors, set of int *)
        (* val initial = ?? *)
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
        val color = Temp.Table.empty (* temp to int *)
        val K = length(Frame.registers)

	    fun main (moveList,worklistMoves,adjList,degree,adjSet) =
	    let val (flowgraph,nodes(* blocks *)) = MakeGraph.instrs2graph instrs
            val Flow.FGRAPH{control=fgraph,def=deftable,use=usetable,ismove=ismovetable} = flowgraph
            val liveOut = Liveness.liveness flowgraph
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
                val t1 = (SetTuple.exists (fn {u=us,v=vs} => if u=us andalso v=vs then true else false) adjSet)
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
                        val live'' = Set.union(live',defset)
                        fun defsI (d,(adjList,degree,adjSet)) =
                            let fun lives (l,(adjList,degree,adjSet)) =
                                addEdge(d,l,adjList,degree,adjSet)
                            in Set.foldl lives (adjList,degree,adjSet) live'' end
                        val (adjList',degree',adjSet') = Set.foldl defsI (adjList,degree,adjSet) defset
                        (*val _ = print "FINISH\n"*)
                        (*val _ = (printTemptoInt("degree",degree,live'');
                        printTemptoSet("moveList",moveList,live'');
                        printTemptoSet("adjList",adjList,live'');
                        print("\n\n\n\n\n"))*)
                        val live''' = Set.union(useset,Set.difference(live'',defset))
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
    fun nodeMoves (n,moveList,activeMoves,worklistmoves) =
        let val m = mapMoveList(moveList,n)
            val aw = Set.union(activeMoves,worklistMoves)
        in Set.intersection(m,aw) end

    (* temp * ... -> bool *)
    fun moveRelated (n,moveList,activeMoves,worklistmoves) =
        Set.isEmpty(nodeMoves(n,moveList,activeMoves,worklistmoves))

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
            then (Set.add(spillWorklist,n),freezeWorklist,simplifyWorklist)
            else if moveRelated(n,moveList,activeMoves,worklistmoves)
            then (spillWorklist,Set.add(freezeWorklist,n),simplifyWorklist)
            else (spillWorklist,freezeWorklist,Set.add(simplifyWorklist,n))
        end
    (*
    val (spillWorklist,freezeWorklist,simplifyWorklist) = foldl makeWorklist 
                (spillWorklist,freezeWorklist,simplifyWorklist,moveList,activeMoves,worklistmoves) initial ??*)

    (* Set.set * ... -> activeMoves * worklistmoves * moveList *)
    fun enableMoves (nodes,activeMoves,worklistMoves,moveList) =
        let fun iternodes (n,(activeMoves,worklistMoves,moveList)) =
            let fun iternodemoves (m,(activeMoves,worklistMoves,moveList)) =
                    if Set.exists (fn x => if x = m then true else false) activeMoves
                    then (Set.difference(activeMoves,Set.singleton m),worklistMoves,moveList)
                    else (activeMoves,Set.difference(worklistMoves,Set.singleton m),moveList)
                val res = nodeMoves(n,moveList,activeMoves,worklistMoves)
            in Set.foldl iternodemoves (activeMoves,worklistMoves,moveList) res end
        in Set.foldl iternodes (activeMoves,worklistMoves,moveList) nodes end 

    (* temp * (...) -> simplifyWorklist * freezeWorklist * degree * spillWorklist * activeMoves * worklistMoves * 
                       moveList * coalescedNodes * selectStack *)
    fun decrementDegree (m,(simplifyWorklist,freezeWorklist,degree,spillWorklist,activeMoves,worklistMoves,
                            moveList,coalescedNodes,selectStack)) =
        let val d = mapDegree(degree,m)
            val degree' = Temp.Table.enter(degree,m,d+1)
            val mset = Set.singleton m
        in if d = K
           then let val unionRes = Set.union(mset,adjacent(m,adjList,selectStack,coalescedNodes))
                    val (activeMoves',worklistMoves',moveList') = enableMoves (unionRes,activeMoves,
                                                                               worklistMoves,moveList)
                    val spillWorklist' = Set.difference(spillWorklist,mset)
                in if moveRelated(m,moveList,activeMoves,worklistMoves)
                   then (simplifyWorklist,Set.union(freezeWorklist,mset),degree',spillWorklist',
                         activeMoves',worklistMoves',moveList',coalescedNodes,selectStack)
                   else (Set.union(simplifyWorklist,mset),freezeWorklist,degree',spillWorklist',
                         activeMoves',worklistMoves',moveList',coalescedNodes,selectStack)
                end
           else (simplifyWorklist,freezeWorklist,degree',spillWorklist,activeMoves,worklistMoves,moveList,
                 coalescedNodes,selectStack)
       end 


    (* temp ... -> degree * simplifyWorklist * freezeWorklist * activeMoves * worklistMoves * selectStack * 
                   moveList * coalescedNodes * spillWorklist*)
    fun simplify(degree,simplifyWorklist,freezeWorklist,activeMoves,worklistMoves,selectStack,moveList,
                    coalescedNodes,spillWorklist) =
        let val n = hd(Set.listItems worklistMoves)
            (* Note: decrementDegree is evaluated first, before substructing n from sipmlifyWorklist *)
            val adjacentRes = adjacent(n,adjList,selectStack,coalescedNodes)
            val (simplifyWorklist,freezeWorklist,degree,spillWorklist,activeMoves,worklistMoves,moveList,
                 coalescedNodes,selectStack) = 
               Set.foldl decrementDegree (simplifyWorklist,freezeWorklist,degree,spillWorklist,activeMoves,
                                           worklistMoves,moveList,coalescedNodes,selectStack)  adjacentRes
            val simplifyWorklist' = Set.difference(simplifyWorklist,Set.singleton n)
            val selectStack' = n::selectStack
        in (degree,simplifyWorklist',freezeWorklist,activeMoves,worklistMoves,selectStack',moveList,
            coalescedNodes,spillWorklist) end

    (* temp ... -> temp *)
    fun getAlias (n,alias,coalescedNodes) =
        if Set.exists (fn x => if n = x then true else false) coalescedNodes
        then let val al = mapAlias(alias,n) in getAlias(al,alias,coalescedNodes) end
        else n

    (* temp ... -> freezeWorkList,simplifyWorkList *)
    fun addWorkList(u,moveList,activeMoves,worklistMoves,degree,freezeWorklist,simplifyWorklist) =
        let val t1 = Set.exists (fn x => if x = u then true else false) precolored
            val t2 = not(moveRelated(u,moveList,activeMoves,worklistMoves))
            val t3 = mapDegree(degree,u) < K
        in if t1 andalso t2 andalso t3
           then (Set.subtract(freezeWorklist,u),Set.subtract(simplifyWorklist,u))
           else (freezeWorklist,simplifyWorklist)
        end

    (* temp * temp ... -> bool *)
    fun ok(t,r,degree,adjSet) =
        let val t1 = mapDegree(degree,t) < K
            val t2 = Set.exists (fn x => if x = t then true else false) precolored
            val t3 = SetTuple.exists (fn {u,v} => if u=t andalso v=r then true else false) adjSet
        in t1 orelse t2 orelse t3 end

    (* temp list ... -> bool *)
    fun conservative(nodes,degree) =
        let val k = 0
            fun iter (n,k) =
                let val d = mapDegree(degree,n)
                in if d >= K
                   then k+1
                   else k
                end
            val k' = Set.foldl iter 0 nodes 
        in k < K end


    (* temp * temp ... -> ... *)
    fun combine(u,v,freezeWorklist,spillWorklist,coalescedNodes,alias,moveList,adjList,degree,adjSet,
                simplifyWorklist,activeMoves,worklistMoves,selectStack) =
        let fun eval(freezeWorklist,spillWorklist) =
                    if Set.exists (fn x => if x = v then true else false) freezeWorklist
                    then (Set.subtract(freezeWorklist,v),spillWorklist)
                    else (freezeWorklist,Set.subtract(spillWorklist,v))
             val (freezeWorklist,spillWorklist) = eval(freezeWorklist,spillWorklist)
             val coalescedNodes = Set.subtract(coalescedNodes,v)
             val alias = Temp.Table.enter(alias,v,u)
             val vmovelist = mapMoveList(moveList,v)
             val umovelist = mapMoveList(moveList,u)
             val unionmoves = Set.union(umovelist,vmovelist)
             val moveList = Temp.Table.enter(moveList,u,unionmoves)

             fun iterajds (t,(adjList,degree,adjSet,simplifyWorklist,freezeWorklist,
                           spillWorklist,activeMoves,worklistMoves,moveList,coalescedNodes,selectStack)) =

                 let val (adjList,degree,adjSet) = addEdge(t,u,adjList,degree,adjSet)
                     val (simplifyWorklist,freezeWorklist,degree,spillWorklist,activeMoves,worklistMoves,
                            moveList,coalescedNodes,selectStack) =
                                 decrementDegree(t,(simplifyWorklist,freezeWorklist,degree,spillWorklist,activeMoves,
                                                    worklistMoves,moveList,coalescedNodes,selectStack)) 
                 in
                     (adjList,degree,adjSet,simplifyWorklist,freezeWorklist,
                      spillWorklist,activeMoves,worklistMoves,moveList,coalescedNodes,selectStack)
                 end
             val vadjacent = adjacent(v,adjList,selectStack,coalescedNodes)
             val (adjList,degree,adjSet,simplifyWorklist,freezeWorklist,
                      spillWorklist,activeMoves,worklistMoves,moveList,coalescedNodes,selectStack) = 
                      Set.foldl iterajds (adjList,degree,adjSet,simplifyWorklist,freezeWorklist,
                           spillWorklist,activeMoves,worklistMoves,moveList,coalescedNodes,selectStack) vadjacent 
             val t1 = mapDegree(degree,u) >= K
             val t2 = Set.exists (fn x => if x = u then true else false) freezeWorklist
          in if t1 andalso t2
             then (adjList,degree,adjSet,simplifyWorklist,Set.subtract(freezeWorklist,u),
                   Set.subtract(spillWorklist,u),activeMoves,worklistMoves,moveList,coalescedNodes,selectStack)
             else (adjList,degree,adjSet,simplifyWorklist,freezeWorklist,
                   spillWorklist,activeMoves,worklistMoves,moveList,coalescedNodes,selectStack)
          end

    fun coalesce(worklistMoves,freezeWorklist,spillWorklist,coalescedMoves,alias,moveList,adjList,degree,adjSet,
                simplifyWorklist,activeMoves,selectStack,constrainedMoves,coalescedNodes) = 
        let val m = List.take(Set.listItems worklistMoves,1)
            val x = hd m
            val y = List.last m
            val x = getAlias(x,alias,coalescedNodes)
            val y = getAlias(y,alias,coalescedNodes)
            val (u,v) = if Set.exists (fn x => if x = y then true else false) precolored
                        then (y,x)
                        else (x,y)
            val worklistMoves = Set.subtractList(worklistMoves,m)
            fun t3 () =
                let fun eval t =
                        ok(t,u,degree,adjSet)
                    val t1 = Set.exists (fn x => if x = u then true else false) precolored
                    val vset = adjacent (v,adjList,selectStack,coalescedNodes)
                    val uset =  adjacent (u,adjList,selectStack,coalescedNodes)
                    val uvunion = Set.union(vset,uset)
                    val t2 = Set.all eval vset
                    val t3 = Set.exists (fn x => if x <> u then true else false) precolored
                    val t4 = conservative(uvunion,degree)
                in (t1 andalso t2) orelse (t3 andalso t4) end
        in if (u = v)
           then let val coalescedMoves = Set.subtractList(coalescedMoves,m)
                    val (freezeWorklist,simplifyWorklist) = addWorkList(u,moveList,activeMoves,worklistMoves,degree,
                                                                        freezeWorklist,simplifyWorklist)
                in (adjList,degree,adjSet,simplifyWorklist,freezeWorklist,
                    spillWorklist,worklistMoves,moveList,coalescedNodes,
                    selectStack,coalescedMoves,constrainedMoves,activeMoves)
                end
           else if (Set.exists (fn x => if x = v then true else false) precolored) orelse
                   (SetTuple.exists (fn {u=ui,v=vi} => if u=ui andalso v=vi then true else false) adjSet)
           then let val constrainedMoves = Set.subtractList(constrainedMoves,m)
                    val (freezeWorklist,simplifyWorklist) = addWorkList(u,moveList,activeMoves,worklistMoves,degree,
                                                                        freezeWorklist,simplifyWorklist)
                    val (freezeWorklist,simplifyWorklist) = addWorkList(v,moveList,activeMoves,worklistMoves,degree,
                                                                        freezeWorklist,simplifyWorklist)
                in (adjList,degree,adjSet,simplifyWorklist,freezeWorklist,
                    spillWorklist,worklistMoves,moveList,coalescedNodes,
                    selectStack,coalescedMoves,constrainedMoves,activeMoves) 
                end
           else if t3()
           then let val coalescedNodes = Set.addList(coalescedNodes,m)
                    val (adjList,degree,adjSet,simplifyWorklist,freezeWorklist,
                         spillWorklist,activeMoves,worklistMoves,moveList,coalescedNodes,selectStack) = 
                         combine(u,v,freezeWorklist,spillWorklist,coalescedNodes,alias,moveList,adjList,degree,adjSet,
                                 simplifyWorklist,activeMoves,worklistMoves,selectStack)
                    val (freezeWorklist,simplifyWorklist) = addWorkList(v,moveList,activeMoves,worklistMoves,degree,
                                                                        freezeWorklist,simplifyWorklist)
                in (adjList,degree,adjSet,simplifyWorklist,freezeWorklist,
                    spillWorklist,worklistMoves,moveList,coalescedNodes,
                    selectStack,coalescedMoves,constrainedMoves,activeMoves)
                end
            else let val activeMoves = Set.subtractList(activeMoves,m)
                 in (adjList,degree,adjSet,simplifyWorklist,freezeWorklist,
                    spillWorklist,worklistMoves,moveList,coalescedNodes,
                    selectStack,coalescedNodes,constrainedMoves,activeMoves) 
                 end
        end
                    
    fun assignColors(selectStack,adjList,coloredNodes,color,spilledNodes,coalescedNodes,alias) = 
            (* temp::temps ... -> spilledNodes * coloredNodes * color *)
        let fun iter (n::selectStack,spilledNodes,coloredNodes,color) =
            let fun makeColors k =
                    if k = ~1
                    then nil
                    else k :: makeColors(k-1)
                 val okColors = Set.fromList(makeColors(K-1))
                 val adjListn = mapAdjList(adjList,n)
                 fun diffcolors(w,okColor) =
                     let val g = getAlias(w,alias,coalescedNodes)
                         val cp = Set.union(coloredNodes,precolored)
                     in if Set.exists (fn x => if g = x then true else false) cp
                        then let val mapc = mapColor(color,w) 
                             in Set.difference(okColors,Set.singleton mapc) end
                        else okColor
                     end
                 val okColors' = Set.foldl diffcolors okColors adjListn
                in if Set.isEmpty okColors'
                   then iter(selectStack,Set.add(spilledNodes,n),coloredNodes,color)
                   else let val c = hd (Set.listItems okColors')
                            val color' = Temp.Table.enter(color,n,c)
                        in iter(selectStack,spilledNodes,Set.add(coloredNodes,n),color') end
                end
              | iter(nil,spilledNodes,coloredNodes,color) =
                    (spilledNodes,coloredNodes,color)
         val (spilledNodes,coloredNodes,color) = iter(selectStack,spilledNodes,coloredNodes,color)
         fun updatecolor (n,color) =
             let val g = getAlias(n,alias,coalescedNodes)
             in Temp.Table.enter(color,n,g) end
         val color' = Set.foldl updatecolor color coalescedNodes
         in (spilledNodes,coloredNodes,color') end

    fun freezeMoves(u,alias,coalescedNodes,activeMoves,frozenMoves,freezeWorklist,
                    simplifyWorklist,worklistMoves,moveList) =
        let val m = List.take(Set.listItems worklistMoves,1)
            val x = hd m
            val y = List.last m
            val xa = getAlias(x,alias,coalescedNodes)
            val ya = getAlias(y,alias,coalescedNodes)
            val ua = getAlias(u,alias,coalescedNodes)
            val e = if ya=ua then xa else ya
            val activeMoves = Set.subtractList(activeMoves,m)
            val frozenMoves = Set.subtractList(frozenMoves,m)
            val t1 = Set.isEmpty(nodeMoves (e,moveList,activeMoves,worklistMoves))
            val t2 = mapDegree(degree,e) < K
        in if t1 andalso t2
           then (activeMoves,frozenMoves,Set.subtract(freezeWorklist,e),simplifyWorklist)
           else (activeMoves,frozenMoves,freezeWorklist,Set.subtract(simplifyWorklist,e))
        end

    fun freeze (alias,coalescedNodes,activeMoves,frozenMoves,freezeWorklist,
                    simplifyWorklist,worklistMoves,moveList) = 
        let val u = hd(Set.listItems freezeWorklist)
            val freezeWorklist = Set.subtract(freezeWorklist,u)
            val simplifyWorklist = Set.add(simplifyWorklist,u)
        in freezeMoves(u,alias,coalescedNodes,activeMoves,frozenMoves,freezeWorklist,
                    simplifyWorklist,worklistMoves,moveList)
        end

    fun selectSpill(alias,coalescedNodes,activeMoves,frozenMoves,freezeWorklist,
                    simplifyWorklist,worklistMoves,moveList,spillWorklist) =
        (* TODO: use a better heuristic *)
        let val m = hd (Set.listItems spillWorklist)
            val spillWorklist = Set.subtract(spillWorklist,m)
            val simplifyWorklist = Set.subtract(simplifyWorklist,m)
        in freezeMoves (m,alias,coalescedNodes,activeMoves,frozenMoves,freezeWorklist,
                    simplifyWorklist,worklistMoves,moveList)
        end

    fun repeat(simplifyWorklist,freezeWorklist,spillWorklist,spilledNodes,coalescedNodes,coloredNodes,
                    selectStack,coalescedMoves,constrainedMoves,frozenMoves,worklistMoves,activeMoves,
                    adjSet,adjList,degree,moveList,alias,color) =
        if Set.isEmpty simplifyWorklist
        then let val (degree,simplifyWorklist,freezeWorklist,activeMoves,worklistMoves,selectStack,moveList,
                      coalescedNodes,spillWorklist) = simplify(degree,simplifyWorklist,freezeWorklist,activeMoves,
                                                  worklistMoves,selectStack,moveList,coalescedNodes,spillWorklist)
             in repeat (simplifyWorklist,freezeWorklist,spillWorklist,spilledNodes,coalescedNodes,coloredNodes,
                        selectStack,coalescedMoves,constrainedMoves,frozenMoves,worklistMoves,activeMoves,
                        adjSet,adjList,degree,moveList,alias,color)
             end
        else if Set.isEmpty worklistMoves
        then let val (adjList,degree,adjSet,simplifyWorklist,freezeWorklist,
                    spillWorklist,worklistMoves,moveList,coalescedNodes,
                    selectStack,coalescedMoves,constrainedMoves,activeMoves) = 
              coalesce(worklistMoves,freezeWorklist,spillWorklist,coalescedMoves,alias,moveList,adjList,degree,adjSet,
                simplifyWorklist,activeMoves,selectStack,constrainedMoves,coalescedNodes)
             in repeat (simplifyWorklist,freezeWorklist,spillWorklist,spilledNodes,coalescedNodes,coloredNodes,
                    selectStack,coalescedMoves,constrainedMoves,frozenMoves,worklistMoves,activeMoves,
                    adjSet,adjList,degree,moveList,alias,color)
             end
        else if Set.isEmpty freezeWorklist
        then let val (activeMoves,frozenMoves,freezeWorklist,simplifyWorklist) = freeze(alias,
                      coalescedNodes,activeMoves,frozenMoves,freezeWorklist,simplifyWorklist,worklistMoves,moveList)
             in repeat(simplifyWorklist,freezeWorklist,spillWorklist,spilledNodes,coalescedNodes,coloredNodes,
                    selectStack,coalescedMoves,constrainedMoves,frozenMoves,worklistMoves,activeMoves,
                    adjSet,adjList,degree,moveList,alias,color)
             end
        else if Set.isEmpty spillWorklist
        then let val (activeMoves,frozenMoves,freezeWorklist,simplifyWorklist) = 
                      selectSpill(alias,coalescedNodes,activeMoves,frozenMoves,freezeWorklist,
                                  simplifyWorklist,worklistMoves,moveList,spillWorklist)
             in repeat(simplifyWorklist,freezeWorklist,spillWorklist,spilledNodes,coalescedNodes,coloredNodes,
                    selectStack,coalescedMoves,constrainedMoves,frozenMoves,worklistMoves,activeMoves,
                    adjSet,adjList,degree,moveList,alias,color)
             end
        else let val (spilledNodes,coloredNodes,color)= assignColors(selectStack,
                                 adjList,coloredNodes,color,spilledNodes,coalescedNodes,alias)
             in 
             (simplifyWorklist,freezeWorklist,spillWorklist,spilledNodes,coalescedNodes,coloredNodes,
                selectStack,coalescedMoves,constrainedMoves,frozenMoves,worklistMoves,activeMoves,
                adjSet,adjList,degree,moveList,alias,color)
             end

        (* 1. construct the control flow graph *)
        (* 2. construct the data flow graph *)
        (* 3. construct the interference graph *)
        val (moveList,worklistMoves,adjList,degree,adjSet) = foldl build (moveList,
                                                      worklistMoves,adjList,degree,adjSet) nodes
        val _ = printSet("worklistMoves",worklistMoves)
        val _ = printTemptoSet("adjList",adjList,worklistMoves);
        val _ = printTemptoInt("degree",degree,worklistMoves);
        val _ = printTupleSet("adjSet",adjSet);
        val _ = printTemptoSet("moveList",moveList,worklistMoves);

        (* 4. make initial worklist TODO: check it 
        val (spillWorklist,freezeWorklist,simplifyWorklist) = foldl makeWorklist 
                (spillWorklist,freezeWorklist,simplifyWorklist,moveList,activeMoves,worklistmoves) initial ?? *)
        (* 5. repeat until simplifyWorklist = {} and worklistMoves = {} and freezeWorklist = {} and spillWorklist = {} TODO:check it 
        val (activeMoves,frozenMoves,freezeWorklist,simplifyWorklist) =
             repeat(simplifyWorklist,freezeWorklist,spillWorklist,spilledNodes,coalescedNodes,coloredNodes,
                    selectStack,coalescedMoves,constrainedMoves,frozenMoves,worklistMoves,activeMoves,
                    adjSet,adjList,degree,moveList,alias,color) *)
       (* 6. assign Colors TODO: check it
       val (spilledNodes,coloredNodes,color') = assignColors(selectStack,adjList,
                                                 coloredNodes,color,spilledNodes,coalescedNodes,alias) *)
       (* 7. if spillNodes != {} then rewrite the program 
       in if Set.empty spillNodes
          then let val (...) = rewriteProgram(...)
               in main(...) end
          else *)
       in () end (* main *)
    in (main(moveList,worklistMoves,adjList,degree,adjSet);(Frame.tempMap,nil)) end
end
