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
     (reduce e_cons e_nil e_tensor)

     ;; Arithmetic over numbers and tensors
     (+ e_1 e_2) ;; scalar, pointwise
     (- e_1 e_2) ;; scalar, pointwise
     (* e_1 e_2) ;; scalar, pointwise, tensor multiplication
     (/ e_1 e_2) ;; scalar, pointwise ;; total, (/ e 0) is equal to 0

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
     ;; Faux real number type
     real
     ;; Tensor types and dimensions
     (tensor τ_type [τ_dim ...])
     number
     ;; Arithmetic (numbers only)
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
     nat
     bool
     )

  ;; Variables
  (x ::= variable-not-otherwise-mentioned)
  (α ::= variable-not-otherwise-mentioned)

  ;; Binding forms
  (λ [x τ] e #:refers-to x)
  (Λ [α κ] e #:refers-to α)
  (∀ [α κ] τ #:refers-to α)
  )
