structure Main = struct
  open Scalar
  fun run () =
    let
      val a = make 2.0
      val b = make 3.0
      val c = mul(a, b)
      val (Val v_c) = c
    in
      (#grad v_c) := 1.0;
      (#backward v_c) ();
      print ("dc/da (should be 3.0): " ^ Real.toString(getGrad a) ^ "\n");
      print ("dc/db (should be 2.0): " ^ Real.toString(getGrad b) ^ "\n")
    end
end
