structure NN = struct
  open Scalar

  (* Linear congruential generator, range [-1, 1] *)
  val seed = ref 1337.0
  fun rand () =
    let
      val a = 1103515245.0
      val c = 12345.0
      val m = 2147483648.0
      val _ = seed := Real.rem (!seed * a + c, m)
    in
      (!seed / m) * 2.0 - 1.0
    end

  (* ── Neuron ─────────────────────────────────────────────────────── *)

  datatype neuron = Neuron of {
    weights : value list,
    bias    : value,
    nonlin  : bool          (* true = tanh activation; false = linear *)
  }

  fun initNeuron (n_in : int) (nonlin : bool) : neuron =
    Neuron {
      weights = List.tabulate (n_in, fn _ => Scalar.make (rand ())),
      bias    = Scalar.make 0.0,
      nonlin  = nonlin
    }

  fun neuronForward (Neuron n) (x : value list) : value =
    let
      val dot = ListPair.foldl
                  (fn (w, xi, acc) => Scalar.add (acc, Scalar.mul (w, xi)))
                  (#bias n)
                  (#weights n, x)
    in
      if #nonlin n then Scalar.tanh dot else dot
    end

  fun neuronParams (Neuron n) : value list = (#weights n) @ [#bias n]

  (* ── Layer ──────────────────────────────────────────────────────── *)

  datatype layer = Layer of neuron list

  fun initLayer (n_in : int) (n_out : int) (nonlin : bool) : layer =
    Layer (List.tabulate (n_out, fn _ => initNeuron n_in nonlin))

  fun layerForward (Layer neurons) (x : value list) : value list =
    map (fn n => neuronForward n x) neurons

  fun layerParams (Layer neurons) : value list =
    List.concat (map neuronParams neurons)

  (* ── MLP ────────────────────────────────────────────────────────── *)

  datatype mlp = MLP of layer list

  (* initMLP n_in [h1, h2, ..., n_out]
     All hidden layers use tanh; the output layer is linear. *)
  fun initMLP (n_in : int) (hidden_and_out : int list) : mlp =
    let
      val sizes = n_in :: hidden_and_out
      val n     = List.length sizes
      fun makeLayer i =
        initLayer (List.nth (sizes, i))
                  (List.nth (sizes, i + 1))
                  (i < n - 2)   (* last layer: i = n-2, nonlin = false *)
    in
      MLP (List.tabulate (n - 1, makeLayer))
    end

  fun mlpForward (MLP layers) (x : value list) : value list =
    foldl (fn (layer, acc) => layerForward layer acc) x layers

  fun mlpParams (MLP layers) : value list =
    List.concat (map layerParams layers)

  (* Zero all parameter gradients before each backward pass *)
  fun zeroGrad (params : value list) : unit =
    app Scalar.zeroGrad params

end
