structure Scalar = struct

  val idCounter = ref 0
  fun nextId () = (idCounter := !idCounter + 1; !idCounter)

  (* data is a ref so SGD can update weights in-place between forward passes *)
  datatype value = Val of {
    id       : int,
    data     : real ref,
    grad     : real ref,
    backward : unit -> unit,
    prev     : value list,
    opLabel  : string
  }

  fun getData (Val v) = !(#data v)
  fun getGrad (Val v) = !(#grad v)
  fun getId   (Val v) = #id v

  fun zeroGrad (Val v) = (#grad v) := 0.0

  fun make (x : real) =
    Val {
      id       = nextId (),
      data     = ref x,
      grad     = ref 0.0,
      backward = fn () => (),
      prev     = [],
      opLabel  = "leaf"
    }

  fun add (v1 as Val r1, v2 as Val r2) =
    let
      val outData = !(#data r1) + !(#data r2)
      val outGrad = ref 0.0
      fun back () = (
        #grad r1 := !(#grad r1) + !outGrad;
        #grad r2 := !(#grad r2) + !outGrad
      )
    in
      Val { id = nextId (), data = ref outData, grad = outGrad,
            backward = back, prev = [v1, v2], opLabel = "+" }
    end

  fun neg (v as Val r) =
    let
      val outData = ~(!(#data r))
      val outGrad = ref 0.0
      fun back () = #grad r := !(#grad r) + (~1.0 * !outGrad)
    in
      Val { id = nextId (), data = ref outData, grad = outGrad,
            backward = back, prev = [v], opLabel = "neg" }
    end

  fun sub (v1, v2) = add (v1, neg v2)

  fun mul (v1 as Val r1, v2 as Val r2) =
    let
      (* snapshot data at forward time — backward must use these, not post-SGD values *)
      val d1      = !(#data r1)
      val d2      = !(#data r2)
      val outData = d1 * d2
      val outGrad = ref 0.0
      fun back () = (
        #grad r1 := !(#grad r1) + (d2 * !outGrad);
        #grad r2 := !(#grad r2) + (d1 * !outGrad)
      )
    in
      Val { id = nextId (), data = ref outData, grad = outGrad,
            backward = back, prev = [v1, v2], opLabel = "*" }
    end

  (* v ^ n  where n is a real-valued constant, not a node *)
  fun pow (v as Val r, n : real) =
    let
      val d       = !(#data r)
      val outData = Math.pow (d, n)
      val outGrad = ref 0.0
      fun back () =
        #grad r := !(#grad r) + (n * Math.pow (d, n - 1.0) * !outGrad)
    in
      Val { id = nextId (), data = ref outData, grad = outGrad,
            backward = back, prev = [v], opLabel = "pow" }
    end

  fun exp (v as Val r) =
    let
      val outData = Math.exp (!(#data r))
      val outGrad = ref 0.0
      (* d/dx e^x = e^x, captured as outData *)
      fun back () = #grad r := !(#grad r) + (outData * !outGrad)
    in
      Val { id = nextId (), data = ref outData, grad = outGrad,
            backward = back, prev = [v], opLabel = "exp" }
    end

  fun tanh (v as Val r) =
    let
      val t       = Math.tanh (!(#data r))
      val outGrad = ref 0.0
      (* d/dx tanh(x) = 1 - tanh(x)^2, snapshot t at forward time *)
      fun back () = #grad r := !(#grad r) + ((1.0 - t * t) * !outGrad)
    in
      Val { id = nextId (), data = ref t, grad = outGrad,
            backward = back, prev = [v], opLabel = "tanh" }
    end

  fun relu (v as Val r) =
    let
      val d       = !(#data r)
      val isPos   = d > 0.0
      val outData = if isPos then d else 0.0
      val outGrad = ref 0.0
      fun back () =
        #grad r := !(#grad r) + ((if isPos then 1.0 else 0.0) * !outGrad)
    in
      Val { id = nextId (), data = ref outData, grad = outGrad,
            backward = back, prev = [v], opLabel = "relu" }
    end

  (* ── Topological sort ────────────────────────────────────────────── *)

  fun contains (id, [])      = false
    | contains (id, x :: xs) = id = x orelse contains (id, xs)

  (* Post-order DFS with prepend: root ends up at the FRONT of the list.
     Process topo directly for backprop (root first → leaves last). *)
  fun buildTopo (root : value) : value list =
    let
      fun build (v as Val node, (vis, topo)) =
        if contains (#id node, vis) then (vis, topo)
        else
          let
            val vis' = (#id node) :: vis
            val (vis'', topo') =
              foldl (fn (child, acc) => build (child, acc)) (vis', topo) (#prev node)
          in
            (vis'', v :: topo')
          end
    in
      #2 (build (root, ([], [])))
    end

  (* topo is already root-first (see buildTopo above). Do NOT reverse —
     reversing would give leaves-first and break gradient flow in deep nets. *)
  fun backward (root as Val r) =
    let
      val topo = buildTopo root
      val _    = (#grad r) := 1.0
    in
      app (fn (Val v) => (#backward v) ()) topo
    end

end
