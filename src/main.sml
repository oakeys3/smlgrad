structure Main = struct
  open Scalar

  fun run () =
    let
      val a = make 2.0
      val b = relu a
      (* Case 1: Active Neuron (Input 2.0 -> Output 2.0) *)
      
      val c = make ~3.0
      val d = relu c
      (* Case 2: Dead Neuron (Input -3.0 -> Output 0.0) *)
    in
      print "--- ReLU Test ---\n";
      
      (* Test Active: Gradient should pass through *)
      backward b;
      print ("Input: 2.0 -> Grad: " ^ Real.toString(getGrad a) ^ " (Expected: 1.0)\n");
      
      (* Test Dead: Gradient should be killed (0.0) *)
      backward d;
      print ("Input: ~3.0 -> Grad: " ^ Real.toString(getGrad c) ^ " (Expected: 0.0)\n")
    end
end