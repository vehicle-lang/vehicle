#lang racket
(require redex)

;; Syntax
(define-language
  vehicle

  ;; Tensors
  (T ::=
     e
     #[T ...]
     )

  ;; Terms
  (e ::=
     ;; Core calculus
     x
     (λ [x τ] e)
     (e_fun e_arg)
     (Λ [α κ] e)
     (e [τ])
     ;; Numbers and tensors
     number
     (tensor τ_type T)
     (at e_tensor [e_index ...])
     (map e_fun e_tensor)
     (reduce e_1 e_2 e_3)
     ;; Arithmetic (numbers and tensors)
     (- e)
     (+ e_1 e_2)
     (- e_1 e_2)
     (* e_1 e_2)
     (/ e_1 e_2) ;; total, (/ e 0) is equal to 0
     ;; Conditionals
     (if e_cond e_true e_false)
     (= e_1 e_2)
     (< e_1 e_2)
     )

  ; Types
  (τ ::=
     ;; Core calculus
     (τ_arg → τ_ret)
     α
     (∀ [α κ] τ)
     ;; Primitive numeric types
     bool
     int8
     int16
     int32
     int64
     uint8
     uint16
     uint32
     uint64
     float32
     float64
     ;; Tensor types and dimensions
     (tensor τ_type [τ_dim ...])
     number
     ;; Arithmetic (numbers only)
     (- τ)
     (+ τ_1 τ_2)
     (- τ_1 τ_2)
     (* τ_1 τ_2)
     (/ τ_1 τ_2) ;; total, (/ τ 0) is equal to 0
     ;; Conditionals
     (if τ_1 τ_2 τ_3)
     (= τ_1 τ_2)
     (< τ_1 τ_2)
     )

  ;; Kinds
  (κ ::=
     ⋆
     Nat
     Bool
     )

  ;; Variables
  (x ::= variable-not-otherwise-mentioned)
  (α ::= variable-not-otherwise-mentioned)
  )
