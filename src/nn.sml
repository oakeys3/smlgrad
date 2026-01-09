structure NN = struct
  open Scalar

  (* Simple Linear Congruential Generator for random numbers between -1.0 and 1.0 *)
  val seed = ref 1337.0
  fun rand () = 
    let 
      val a = 1103515245.0
      val c = 12345.0
      val m = 2147483648.0
      val _ = seed := Real.rem (!seed * a + c, m)
    in 
      (!seed / m) * 2.0 - 1.0 (* Scale to range [-1, 1] *)
    end

  (* THE NEURON TYPE *)
  (* Holds a list of weights (Scalars) and one bias (Scalar) *)
  datatype neuron = N of { 
    weights: value list, 
    bias: value 
  }

  (* Initialization 
     - n_in: The number of inputs this neuron expects.
     - Return: A new neuron with 'n_in' random weights and a random bias.
  *)
  fun initNeuron (n_in: int) : neuron = 
    let
      val w = List.tabulate(n_in, fn _ => Scalar.make(rand()))
      val b = Scalar.make(rand())
    in
      N { weights = w, bias = b }
    end

  (* Forward Pass
     - n: The neuron
     - x: A list of input Scalars
     - Return: A single Scalar (The result of ReLU(dot_product + bias))
  *)
  fun forward (N n) (x: value list) : value = 
    let
      val zero_val = Scalar.make 0.0
      val pair_mul = ListPair.map Scalar.mul (#weights n, x)
      val pairs_sum = foldl Scalar.add zero_val pair_mul
    in
      Scalar.relu (Scalar.add (pairs_sum, #bias n))
    end
end