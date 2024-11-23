let mux2_connective s0 a b = (s0 && a) || ((not s0) && b);;
let mux2_if s0 a b = if s0 then a else b;;
let mux2_match s0 a b = match s0 with true -> a | * -> b;;

let mux2 s0 a b = mux2_if s0 a b;; (* brevity *)
let mux4 s1 s0 a0 a1 a2 a3 = mux2 s1 (mux2 s0 a3 a2) (mux2 s0 a1 a0);;