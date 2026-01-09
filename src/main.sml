structure Main = struct
  open Scalar
  open NN

  fun run () =
    let
      val _ = print "\n--- Initializing Neuron ---\n"
      (* 1. Create a Neuron expecting 2 inputs *)
      val n = NN.initNeuron(2)
      
      (* 2. Create inputs *)
      val x = [Scalar.make 1.0, Scalar.make ~2.0]
      
      (* 3. Forward Pass *)
      val out = NN.forward n x
    in
      (* 4. Backward Pass (The moment of truth) *)
      backward out;
      
      print ("Output Value: " ^ Real.toString(getData out) ^ "\n");
      print "Backprop passed. Graph is alive.\n"
    end
end