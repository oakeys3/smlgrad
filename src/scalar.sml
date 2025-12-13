structure Scalar = struct
  datatype value = Val of {
    data: real,
    grad: real ref,
    backward: unit -> unit,
    prev: value list,
    opLabel: string
  }

  fun getData (Val v) = #data v
  fun getGrad (Val v) = !(#grad v)
  fun make (x: real) =
    Val { data = x, grad = ref 0.0, backward = (fn () => ()), prev = [], opLabel = "Leaf" }

  fun add (v1 as Val r1, v2 as Val r2) =
    let
      val outData = r1.data + r2.data
      val outGrad = ref 0.0
      fun backwardStep () = (
         r1.grad := !(r1.grad) + 1.0 * (!outGrad);
         r2.grad := !(r2.grad) + 1.0 * (!outGrad)
      )
    in
      Val { data = outData, grad = outGrad, backward = backwardStep, prev = [v1, v2], opLabel = "+" }
    end

  fun mul (v1 as Val r1, v2 as Val r2) =
    let
      val outData = r1.data * r2.data
      val outGrad = ref 0.0
      fun backwardStep () = (
         r1.grad := !(r1.grad) + (r2.data * (!outGrad));
         r2.grad := !(r2.grad) + (r1.data * (!outGrad))
      )
    in
      Val { data = outData, grad = outGrad, backward = backwardStep, prev = [v1, v2], opLabel = "*" }
    end
end
