-- Named explicit and implicit binders in function types, with and without
-- modalities.

f : {n : Nat} -> Index n -> Index n
f x = x

g : (n : Nat) -> Index n -> Index n
g n x = x

h : {A : Type} -> A -> A
h x = x

k : {@0 n : Nat} -> (@0 m : Nat) -> Index n -> Index n
k m x = x

@property
p : Bool
p = h (f (g 3 1) == k 5 (1 : Index 3))
