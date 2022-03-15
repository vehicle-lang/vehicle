-- This is an embedding of a DSL for imperative programming
-- we use 'network' to basically mean postulate

-- datatypes (things that can be stored in variables)
network datatype : Type 0
network int : datatype
network bool : datatype

-- phrase types: expressions, variables, and commands
network exp : datatype -> Type 0
network var : datatype -> Type 0
network comm : Type 0

-- some expression builders
network plus : exp int -> exp int -> exp int
network isZero : exp int -> exp bool

-- reading and assigning to variables
network rd : forall {d : datatype}. var d -> exp d
network assign : forall {d : datatype}. var d -> exp d -> comm

-- commands: skip, sequence, if-then, and while loops
network skip : comm
network seq : comm -> comm -> comm
network while : exp bool -> comm -> comm
network if_ : exp bool -> comm -> comm

-- create a new stack allocated variable
network new : forall {d : datatype}. (var d -> comm) -> comm


-- a little program, assuming two variables 'x' and 'b'
--  new y.
--    y := x
--    b := isZero (rd y)
program : var int -> var bool -> comm
program x b = new {int} (\y -> seq (assign y (rd x)) (assign b (isZero (rd y))))

-- TODO: the type annotation on 'new' wouldn't be needed if we solved
-- flex-flex problems.
