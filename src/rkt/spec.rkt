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
     (∀ [α κ] τ_ret)
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
     natural
     ;; Arithmetic (numbers only)
     (+ τ_1 τ_2)
     (- τ_1 τ_2)
     (* τ_1 τ_2)
     (/ τ_1 τ_2) ;; total, (/ τ 0) is equal to 0
     ;; Conditionals
     (if τ_cond τ_true τ_false)
     (= τ_1 τ_2)
     (< τ_1 τ_2)
     )

  ;; Type environments
  (Γ ::=
     (Γ x τ)
     ∙
     )

  ;; Kinds
  (κ ::=
     ⋆
     nat
     bool
     )

  ;; Kind environments
  (Δ ::=
     (Δ α κ)
     ∙
     )

  ;; Variables
  (x ::= variable-not-otherwise-mentioned)
  (α ::= variable-not-otherwise-mentioned)

  ;; Binding forms
  (λ [x τ] e #:refers-to x)
  (Λ [α κ] e #:refers-to α)
  (∀ [α κ] τ #:refers-to α)
  )

;; Kind lookup
(define-metafunction
  vehicle
  kind-lookup : Δ α -> κ or #f
  [(kind-lookup (Δ α_1 κ) α_1) κ]
  [(kind-lookup (Δ α_1 κ) α_2) (kind-lookup Δ α_2)]
  [(kind-lookup ∙ α) #f]
  )

;; Kind checking
(define-judgment-form
  vehicle
  #:mode (kindof I I O)
  #:contract (kindof Δ τ κ)

  [(kindof Δ τ_arg ⋆)
   (kindof Δ τ_ret ⋆)
   ---------------------------- "arrow"
   (kindof Δ (τ_arg → τ_ret) ⋆)
   ]

  [(where κ (kind-lookup Δ α))
   --------------------------- "var"
   (kindof Δ α κ)
   ]

  [(kindof (Δ α κ) τ_ret ⋆)
   ---------------------------- "forall"
   (kindof Δ (∀ [α κ] τ_ret) ⋆)
   ]

  [----------------- "bool"
   (kindof Δ bool ⋆)
   ]

  [----------------- "int8"
   (kindof Δ int8 ⋆)
   ]

  [------------------ "int16"
   (kindof Δ int16 ⋆)
   ]

  [------------------ "int32"
   (kindof Δ int32 ⋆)
   ]

  [------------------ "int64"
   (kindof Δ int64 ⋆)
   ]

  [------------------ "uint8"
   (kindof Δ uint8 ⋆)
   ]

  [------------------- "uint16"
   (kindof Δ uint16 ⋆)
   ]

  [------------------- "uint32"
   (kindof Δ uint32 ⋆)
   ]

  [------------------- "uint64"
   (kindof Δ uint64 ⋆)
   ]

  [-------------------- "float32"
   (kindof Δ float32 ⋆)
   ]

  [-------------------- "float64"
   (kindof Δ float64 ⋆)
   ]

  [----------------- "real"
   (kindof Δ real ⋆)
   ]

  [---------------------- "natural"
   (kindof Δ natural nat)
   ]

  [(kindof Δ τ_type ⋆)
   (kindof Δ τ_dim nat) ...
   ---------------------------------------- "tensor"
   (kindof Δ (tensor τ_type [τ_dim ...]) ⋆)
   ]

  [(kindof Δ τ_1 nat)
   (kindof Δ τ_2 nat)
   -------------------------- "+"
   (kindof Δ (+ τ_1 τ_2) nat)
   ]

  [(kindof Δ τ_1 nat)
   (kindof Δ τ_2 nat)
   -------------------------- "-"
   (kindof Δ (- τ_1 τ_2) nat)
   ]

  [(kindof Δ τ_1 nat)
   (kindof Δ τ_2 nat)
   -------------------------- "*"
   (kindof Δ (* τ_1 τ_2) nat)
   ]

  [(kindof Δ τ_1 nat)
   (kindof Δ τ_2 nat)
   -------------------------- "/"
   (kindof Δ (/ τ_1 τ_2) nat)
   ]

  [(kindof Δ τ_cond bool)
   (kindof Δ τ_true κ)
   (kindof Δ τ_false κ)
   --------------------------------------- "if"
   (kindof Δ (if τ_cond τ_true τ_false) κ)
   ]

  [(kindof Δ τ_1 nat)
   (kindof Δ τ_2 nat)
   --------------------------- "="
   (kindof Δ (= τ_1 τ_2) bool)
   ]

  [(kindof Δ τ_1 nat)
   (kindof Δ τ_2 nat)
   --------------------------- "<"
   (kindof Δ (< τ_1 τ_2) bool)
   ]
  )

;; Type lookup
(define-metafunction
  vehicle
  type-lookup : Δ α -> κ or #f
  [(type-lookup (Δ α_1 κ) α_1) κ]
  [(type-lookup (Δ α_1 κ) α_2) (type-lookup Δ α_2)]
  [(type-lookup ∙ α) #f]
  )

;; Type checking
(define-judgment-form
  vehicle
  #:mode (typeof I I I O)
  #:contract (typeof Γ Δ e τ)

  [(where τ (type-lookup Γ x))
   --------------------------- "var"
   (typeof Γ Δ x τ)
   ]

  [(typeof (Γ x τ_arg) Δ e_body τ_ret)
   ------------------------------------------------- "abs"
   (typeof Γ Δ (λ [x τ_arg] e_body) (τ_arg → τ_ret))
   ]

  [(typeof Γ Δ e_fun (τ_arg → τ_ret))
   (typeof Γ Δ e_arg τ_arg)
   -------------------------------- "app"
   (typeof Γ Δ (e_fun e_arg) τ_ret)
   ]

  [(typeof Γ (Δ α κ_arg) e_body τ_ret)
   --------------------------------------- "type-abs"
   (typeof Γ Δ (Λ [α κ_arg] e_body) τ_ret)
   ]

  [(typeof Γ Δ e_fun (∀ [α κ_arg] τ_body))
   (kindof Δ τ_arg κ_arg)
   (where τ_ret (substitute τ_body α τ_arg))
   ----------------------------------------- "type-app"
   (typeof Γ Δ (e_fun [τ_arg]) τ_ret)
   ]

  ;; Typing rules for:
  ;;
  ;; number
  ;; (tensor τ_type T)
  ;; (at e_tensor [e_index ...])
  ;; (map e_fun e_tensor)
  ;; (reduce e_cons e_nil e_tensor)
  ;; (+ e_1 e_2) ;; scalar, pointwise
  ;; (- e_1 e_2) ;; scalar, pointwise
  ;; (* e_1 e_2) ;; scalar, pointwise, tensor multiplication
  ;; (/ e_1 e_2) ;; scalar, pointwise ;; total, (/ e 0) is equal to 0
  ;; (if e_cond e_true e_false)
  ;; (= e_1 e_2)
  ;; (< e_1 e_2)
  )
