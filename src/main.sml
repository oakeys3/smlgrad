structure Main = struct
  open Scalar
  open NN

  (* Mean-squared-error loss over parallel lists of predictions and targets *)
  fun mseLoss (preds : value list) (targets : value list) : value =
    let
      val n      = real (List.length preds)
      val sqErrs = ListPair.map
                     (fn (p, t) => Scalar.pow (Scalar.sub (p, t), 2.0))
                     (preds, targets)
      val total  = foldl Scalar.add (Scalar.make 0.0) sqErrs
    in
      Scalar.mul (total, Scalar.make (1.0 / n))
    end

  (* Vanilla SGD: nudge each parameter opposite its gradient *)
  fun sgdStep (params : value list) (lr : real) : unit =
    app (fn (Val v) => (#data v) := !(#data v) - lr * !(#grad v)) params

  (* ── Dataset ────────────────────────────────────────────────────── *)
  (* Four 3-dimensional inputs with binary ±1 targets (Karpathy demo) *)
  val dataset : (real list * real) list = [
    ([2.0,  3.0, ~1.0],  1.0),
    ([3.0, ~1.0,  0.5], ~1.0),
    ([0.5,  1.0,  1.0], ~1.0),
    ([1.0,  1.0, ~1.0],  1.0)
  ]

  fun run () =
    let
      val _ = print "\n=== smlgrad: MLP training demo ===\n"

      (* MLP: 3 inputs -> [4, 4] hidden (tanh) -> 1 output (linear) = 41 params *)
      val model  = NN.initMLP 3 [4, 4, 1]
      val params = NN.mlpParams model
      val _      = print ("Parameters: " ^ Int.toString (List.length params) ^ "\n\n")

      val xs = map (fn (x, _) => map Scalar.make x) dataset
      val ys = map (fn (_, y) => Scalar.make y) dataset

      fun epoch i =
        let
          val preds = map (fn x => hd (NN.mlpForward model x)) xs
          val loss  = mseLoss preds ys

          val _ = NN.zeroGrad params
          val _ = Scalar.backward loss
          val _ = sgdStep params 0.05
        in
          print ("epoch " ^ Int.toString i ^
                 "  loss = " ^ Real.toString (Scalar.getData loss) ^ "\n")
        end

      fun loop 0 = ()
        | loop n = (epoch (21 - n); loop (n - 1))

      val _ = loop 20

      val _ = print "\nFinal predictions vs targets:\n"
      val _ = ListPair.app
                (fn (x, (_, y)) =>
                   let val pred = hd (NN.mlpForward model x)
                   in print ("  pred = " ^ Real.toString (Scalar.getData pred) ^
                              "  target = " ^ Real.toString y ^ "\n")
                   end)
                (xs, dataset)
    in
      print "\n=== done ===\n"
    end

end

val _ = Main.run ()
