structure Main = struct

   structure Tr = Translate
   structure F = Frame
   structure R = RegAlloc

 fun getsome (SOME x) = x

   fun emitproc out (F.PROC{body,frame}) =
     let val _ = print ("emit " ^ Symbol.name(Frame.name frame) ^ "\n")
         (*val _ = Printtree.printtree(out,body)*)
	     val stms = Canon.linearize body
         (*val _ = app (fn s => Printtree.printtree(out,s)) stms*)
         (*val (stms',_) = Canon.basicBlocks stms*)
         val stms'' = Canon.traceSchedule(Canon.basicBlocks stms)
         (*val stms' = List.concat(stms')*)
         (* val _ = app (fn s => Printtree.printtree(out,s)) stms'' *)
	     val instrs = List.concat(map (Arm.codegen frame) stms'')
         (*val _ = TextIO.output(out,"\n\n")
         val (flowgraph,nodes) = MakeGraph.instrs2graph instrs
         val _ = Liveness.printNodes(TextIO.stdOut,flowgraph)
         val igraph = Liveness.liveness flowgraph*)
         (*val _ = print (Graph.printgraph nodes)*)
         (*val _ = Liveness.printliveouts(TextIO.stdOut,nodes,igraph)*)
         val (instrs',alloc) = RegAlloc.alloc(instrs,frame)
         (*
         fun iter(i) =
            if i = 150
            then ()
            else case Temp.Table.look(alloc,i) of
                     SOME s => (print("i "^Int.toString(i)^"s "^s^" ");iter(i+1))
                   | NONE => (print("NOt found i "^Int.toString(i)^"\n");iter(i+1))
         val _ = iter(0)
         *)
         val format0 = Assem.format((fn i => (case Temp.Table.look(alloc,i) of 
                                                  SOME s => let val t = (List.exists (fn x => if x = s then true else false) 
                                                                         Frame.registersStr)
                                                            in 
                                                                if t
                                                                then s
                                                                else "r"^s
                                                            end
                                                           (*print("i "^Int.toString(i)^
                                                            "s "^Int.toString(s)^" ")*)
                                                | NONE => (*print("NOt found i "^Int.toString(i)^"\n");*)"")))
         val {prolog, body, epilog } = Frame.procEntryExit3(frame,instrs')
         (*val format0 = Assem.format(Temp.makestring)*)
         (*val _ = RegAlloc.test*)
      in
          (TextIO.output(out,prolog);
           app (fn i => TextIO.output(out,format0 i)) body;
           TextIO.output(out,epilog))
      end
    | emitproc out (F.STRING(lab,s)) = TextIO.output(out,F.string(lab,s))

   fun withOpenFile fname f = 
       let val out = TextIO.openOut fname
        in (f out before TextIO.closeOut out) 
	    handle e => (TextIO.closeOut out; raise e)
       end 

   fun compile filename = 
       let val absyn = Parse.parse filename
           (*val _ = PrintAbsyn.print(TextIO.stdOut,absyn)*)
        in 
            if not (!ErrorMsg.anyErrors)
            then let val frags = (FindEscape.findEscape absyn; Semant.transProg absyn)  
                 in 
				 (
                    withOpenFile (filename ^ ".s") (fn out => (app (emitproc out) frags));
					OS.Process.system ("as -g " ^ filename ^ ".s" ^ " -o " ^ filename ^ ".o");
					OS.Process.system ("gcc -g runtime.o " ^ filename ^ ".o -o " ^ filename ^ ".exe")
				 )
                 end
            else OS.Process.failure
       end

end

val filename = hd (CommandLine.arguments())
val _ = Main.compile filename



