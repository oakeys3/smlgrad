structure Scalar = struct

  (* GLOBAL COUNTER: Needed for unique node IDs (crucial for visited sets) *)
  val idCounter = ref 0
  fun nextId () = (idCounter := !idCounter + 1; !idCounter)

  datatype value = Val of {
    id: int,              (* Unique ID for DAG traversal *)
    data: real,
    grad: real ref,
    backward: unit -> unit,
    prev: value list,
    opLabel: string
  }

  fun getData (Val v) = #data v
  fun getGrad (Val v) = !(#grad v)
  fun getId (Val v) = #id v

  (* Zero out gradients *)
  fun zeroGrad (Val v) = (#grad v) := 0.0

  (* leaf node constructor *)
  fun make (x: real) =
    Val {
      id = nextId(),
      data = x,
      grad = ref 0.0,
      backward = (fn () => ()),
      prev = [],
      opLabel = "Leaf"
    }

  (* addition *)
  fun add (v1 as Val r1, v2 as Val r2) =
    let
      val outData = #data r1 + #data r2
      val outGrad = ref 0.0
      fun backwardStep () = (
         #grad r1 := !(#grad r1) + 1.0 * (!outGrad);
         #grad r2 := !(#grad r2) + 1.0 * (!outGrad)
      )
    in
      Val {
        id = nextId(),
        data = outData,
        grad = outGrad,
        backward = backwardStep,
        prev = [v1, v2],
        opLabel = "+"
      }
    end

  (* multiplication *)
  fun mul (v1 as Val r1, v2 as Val r2) =
    let
      val outData = #data r1 * #data r2
      val outGrad = ref 0.0
      fun backwardStep () = (
         #grad r1 := !(#grad r1) + (#data r2 * (!outGrad));
         #grad r2 := !(#grad r2) + (#data r1 * (!outGrad))
      )
    in
      Val {
        id = nextId(),
        data = outData,
        grad = outGrad,
        backward = backwardStep,
        prev = [v1, v2],
        opLabel = "*"
      }
    end

  (* ReLU *)
  fun relu (v as Val r) =
    let
      val isPos = #data r > 0.0
      val outData = if isPos then #data r else 0.0
      val outGrad = ref 0.0
      
      fun backwardStep () = (
         #grad r := !(#grad r) + ((if isPos then 1.0 else 0.0) * (!outGrad))
      )
    in
      Val {
        id = nextId(),
        data = outData,
        grad = outGrad,
        backward = backwardStep,
        prev = [v],
        opLabel = "ReLU"
      }
    end

  (* topological sort *)
  
  fun contains (targetId, []) = false
    | contains (targetId, x::xs) = if x = targetId then true else contains(targetId, xs)

  (* DFS to build topo order *)
  fun buildTopo (root: value) : value list = 
    let 
      fun build (v as Val node, (visitedIds, topoList)) = 
        if contains(#id node, visitedIds) then 
          (visitedIds, topoList) 
        else
          let
            val newVisited = (#id node) :: visitedIds
            val (finalVisited, finalTopo) = 
              foldl (fn (child, (vis, top)) => build(child, (vis, top))) 
                    (newVisited, topoList) 
                    (#prev node)
          in
            (finalVisited, v :: finalTopo)
          end
    in
      #2 (build (root, ([], []))) 
    end

  fun backward (root as Val r) = 
    let
      val topo = buildTopo root
      val _ = (#grad r) := 1.0
    in
      (* process in reverse topo order (output -> input) *)
      app (fn (Val v) => (#backward v) ()) (List.rev topo)
    end
end