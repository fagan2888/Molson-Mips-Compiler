structure Color =
struct
  type allocation = Registers.register Temp.Map.map
  fun color {interference as Liveness.IGRAPH{graph, tnode, gtemp, moves}, initial, registers} =
    let
      fun precolored node = List.exists (fn listitem => listitem=Flow.Graph.nodeInfo(node)) (#1 (ListPair.unzip(Temp.Map.listItemsi(initial))))

      fun spill graph =
        let
          fun removeNode [] = (graph,NONE)
            | removeNode (node::nodelist) = if not(precolored node) then (Flow.Graph.remove(graph,node), SOME(node)) else removeNode(nodelist)
        in
          removeNode (Flow.Graph.nodes graph)
        end

      fun simplify graph =
        let
          fun removeNode [] = (graph,NONE)
            | removeNode (node::nodelist) = if (not(precolored node) andalso Flow.Graph.degree(node) < List.length(registers)) then (Flow.Graph.remove(graph,node), SOME(node)) else removeNode(nodelist)
        in
          removeNode (Flow.Graph.nodes graph)
        end

      (* Add all nodes non-precolored nodes to stack (even if spilling) *)
      fun createStack (graph, stack) = case simplify graph of
        (graphsi, SOME(nodesi)) => createStack (graphsi, nodesi::stack)
        | (_, NONE) => (case spill graph of (graphsp, SOME(nodesp)) => createStack (graphsp, nodesp::stack) | (_, NONE) => stack)

      (* pops stack and chooses color for node (different color than neighbors) *)
      fun createColors (map, [], spills) = (map,spills)
        | createColors (map, node::stack, spills) =
        let
          val nodeTemp = Flow.Graph.nodeInfo(node)
          fun colorNode node =
            let
              val adjNodes = Flow.Graph.adj' graph node
              fun usedColors (adjnode, list) = case Temp.Map.find(map, Flow.Graph.nodeInfo(adjnode)) of SOME(adjreg) => adjreg::list | NONE => list
              val adjColors = foldl usedColors [] adjNodes
              fun regAlreadyUsed (reg:Registers.register) = List.exists (fn (usedreg:Registers.register) => usedreg=reg) adjColors
              fun selectReg [] = NONE
                | selectReg (reg::reglist) = if regAlreadyUsed reg then selectReg reglist else SOME(reg)
            in
              selectReg (List.rev registers)
            end
        in
          case colorNode node of SOME(nodeReg) => createColors(Temp.Map.insert(map,nodeTemp,nodeReg),stack,spills) | NONE => createColors(map, stack, nodeTemp::spills)
        end

      val stack = createStack (graph, [])
      val (allocation, spills) = createColors(initial, stack, [])
    in
      (allocation, spills)
    end
end
