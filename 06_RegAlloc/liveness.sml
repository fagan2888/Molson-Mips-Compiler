structure Liveness =
struct

datatype igraph = IGRAPH of {graph: Temp.temp Flow.Graph.graph, tnode: Temp.temp Flow.Graph.node Temp.Map.map, gtemp: Temp.temp M.map, moves: (Temp.temp Flow.Graph.node * Temp.temp Flow.Graph.node) list}

fun interferenceGraph (fg as Flow.FLOWGRAPH{control,def,use,ismove}) =
	let
		(* Create Live-In and Live-Out maps *)
		fun compLiveness (inMap,outMap) =
			let
				fun eqMap (map1 : Temp.Set.set M.map, map2 : Temp.Set.set M.map) =
					let
						fun checkNode(nodeid, set1, ans) = if ans then case M.find(map1, nodeid) of SOME (set2) => Temp.Set.equal(set1,set2) | NONE => false else false
					in
						M.foldli checkNode true map2
					end

				fun getOut (node,somemap) =
					let
						val succList = Flow.Graph.succs node
						fun getLive (id,set) = case M.find(inMap,id) of SOME(tempset) => Temp.Set.union(tempset,set) | NONE => Temp.Set.empty
						val nodeID = Flow.Graph.getNodeID node
					in
						M.insert(somemap, nodeID, foldl getLive Temp.Set.empty succList)
					end

				val nodeList = Flow.Graph.nodes control
				val outMap' = foldl getOut M.empty nodeList
				val outDefMap = M.unionWith Temp.Set.difference (outMap', def)
				val inMap' = M.unionWith Temp.Set.union (use, outDefMap)

			in
				if (eqMap(inMap,inMap') andalso eqMap(outMap,outMap')) then (inMap,outMap) else compLiveness(inMap',outMap')
			end

		val liveOutMap = #2 (compLiveness(M.empty, M.empty))

		(* Create Nodes of Igraph *)
    val nodeID = ref 0
		val tnode = Temp.Map.empty
		val usesets = M.foldl Temp.Set.union Temp.Set.empty use
		val defsets = M.foldl Temp.Set.union Temp.Set.empty def
		val templist = Temp.Set.union(usesets,defsets)
    fun add_node (t, graph) = (nodeID:=(!nodeID)+1; Flow.Graph.addNode(graph,!nodeID,t))
    val igraph = Temp.Set.foldl add_node Flow.Graph.empty templist

		(* Make tnode *)
		val _ = nodeID:=0
		fun make_tnode (temp, map) = (nodeID:=(!nodeID)+1; Temp.Map.insert(map,temp,Flow.Graph.getNode(igraph,!nodeID)))
		val tnode = Temp.Set.foldl make_tnode Temp.Map.empty templist

		(* Make gtemp *)
		val _ = nodeID:=0
		fun make_gtemp (t, map) = (nodeID:=(!nodeID)+1; M.insert(map,!nodeID,t))
		val gtemp = Temp.Set.foldl make_gtemp M.empty templist

		(* Create Edges of IGraph *)
		val _ = nodeID:=1
		fun temptoNodeID t = Flow.Graph.getNodeID(case Temp.Map.find(tnode,t) of SOME(node) => node | NONE => Flow.Graph.getNode(Flow.Graph.empty,~1))
		fun temptoNode t = case Temp.Map.find(tnode,t) of SOME(node) => node | NONE => Flow.Graph.getNode(Flow.Graph.empty, ~1)
		fun makeEdges (defs,graph) =
			let
				val liveouts = case M.find(liveOutMap,!nodeID) of SOME(templist) => templist | NONE => Temp.Set.empty
				val liveNodes = map temptoNodeID (Temp.Set.listItems liveouts)
				fun addEdges (graph, x::xs, y::ys) =
					let
						fun addEdge (graph, t1, t2::t2s) = addEdge(Flow.Graph.doubleEdge(graph,t1,t2),t1,t2s)
              | addEdge (graph, t1, []) = graph
					in
						addEdges(addEdge(graph,x,y::ys),xs,y::ys)
					end
				| addEdges (graph,[],y::ys) = graph
				| addEdges (graph,_,_) = graph
			in
				(nodeID:=(!nodeID)+1; addEdges(graph,defs,liveNodes))
			end

		val defNodes = M.map (map temptoNodeID) (M.map Temp.Set.listItems def)
		val igraph' = M.foldl makeEdges igraph defNodes

		fun makeMove (node,list) =
			let
				val nodeID = Flow.Graph.getNodeID node
				val isMove = case M.find(ismove,nodeID) of SOME(bool) => bool | NONE => false
			in
				if isMove then
					let
						val temp1 = temptoNode(List.hd(case M.find(def,nodeID) of SOME(tlist) => Temp.Set.listItems(tlist) | NONE => []))
						val temp2 = temptoNode(List.hd(case M.find(use,nodeID) of SOME(tlist) => Temp.Set.listItems(tlist) | NONE => []))
					in
						(temp1,temp2)::list
					end
				else
					list
			end

		val moves' = foldl makeMove [] (Flow.Graph.nodes(control))
	in
		IGRAPH{graph=igraph',tnode=tnode,gtemp=gtemp,moves=moves'}
	end
end
