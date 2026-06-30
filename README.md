# smlgrad

A scalar-valued reverse-mode automatic differentiation engine and multi-layer perceptron built from scratch in Standard ML. Inspired by Andrej Karpathy's [micrograd](https://github.com/karpathy/micrograd).

## What it demonstrates

- **Autograd** — every arithmetic operation (`+`, `*`, `tanh`, etc.) records its inputs and a backward closure, forming a dynamic computational graph (DAG). A single `backward` call walks that graph in topological order and accumulates gradients via the chain rule.
- **Module system** — a composable hierarchy: `Scalar.value` → `Neuron` → `Layer` → `MLP`, mirroring how PyTorch's `nn.Module` composes.
- **End-to-end training** — forward pass, MSE loss, backward pass, SGD weight update — the same loop that underlies every neural network trainer.

## Architecture

```
Scalar.value  — a node in the computation graph
               fields: data (real ref), grad (real ref), backward closure, prev list

Neuron        — dot(weights, x) + bias, optionally passed through tanh
Layer         — a list of neurons applied in parallel to the same input
MLP           — a sequence of layers (hidden layers use tanh; output layer is linear)
```

## Files

| File | Purpose |
|------|---------|
| `src/scalar.sml` | Core autograd engine: ops, topological sort, backward |
| `src/nn.sml` | Neuron, Layer, MLP; parameter collection; zeroGrad |
| `src/main.sml` | Training demo: MSE loss, SGD loop, final predictions |
| `sources.cm` | SML/NJ Compilation Manager build file |

## Building and running

Requires [SML/NJ](https://www.smlnj.org/).

```
sml
- CM.make "sources.cm";
```

`main.sml` calls `Main.run ()` at the top level, so the training loop starts automatically when the file loads.

## Sample output

```
=== smlgrad: MLP training demo ===
Parameters: 41

epoch 1  loss = 1.0849...
epoch 2  loss = 0.8234...
epoch 3  loss = 0.5912...
...
epoch 20 loss = 0.0213...

Final predictions vs targets:
  pred =  0.941  target =  1.0
  pred = -0.927  target = -1.0
  pred = -0.889  target = -1.0
  pred =  0.903  target =  1.0

=== done ===
```

## Design notes

**Why `data` is a `real ref`**  
SGD updates weights in-place (`data := data - lr * grad`) without rebuilding the module objects. The next forward pass reads the updated values through the same `Neuron`/`Layer`/`MLP` structures.

**Snapshot pattern in backward closures**  
Each op (e.g. `mul`) binds `val d1 = !(#data r1)` at forward time and uses that snapshot inside the backward closure — not `!(#data r1)` again. This ensures the backward sees the values from the forward pass, not the post-SGD values.

**Topological sort and the no-reverse rule**  
`buildTopo` is a post-order DFS that prepends each node (`v :: topo`), so the output node lands at the front of the list. `backward` walks this list directly — it does **not** reverse it. Reversing would put leaves first, so intermediate nodes would propagate gradients before receiving them from upstream, silently zeroing all gradients in deep networks.
