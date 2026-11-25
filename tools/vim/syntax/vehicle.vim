" vehicle.vim -- Syntax highlighting for the Vehicle language
"
" To install, copy this syntax file and the corresponding `ftdetect` file to
" your ~/.vim/ directory:
"
"  $ cp -r ./tools/vim/* ~/.vim/
"

if exists("b:current_syntax")
  finish
endif

" Keywords
" Declaration keywords
syn match vehicleDeclaration "\(^\(type\|record\)\|\(\<where\|supports\)\)\>"

" Annotation keywords
syn match vehicleAnnotation "^@\(network\|dataset\|parameter\|property\|builtin\|noinline\|tensor\|typeclass\|instance\)\>"

" Control keywords
syn keyword vehicleControl forall exists foreach let in if then else

" Builtins
" Value builtins
syn match vehicleValueBuiltin "\w\@<!()\w\@!"
syn keyword vehicleValueBuiltin True False nil not map dfold fold indices fromNat fromInt

" Type builtins
syn keyword vehicleTypeBuiltin Unit Bool Nat Int Real Vector Tensor List Index
syn match vehicleTypeBuiltin "\<Type\(\s*\([0-9]\+\)\?\)\>"

" Operator builtins
syn match vehicleOperator "->\|\.\|:\|\\\|=>\|==\|!=\|<=\|<\|>=\|>\|\*\|\/\|+\|-\|::\|!\|="
syn keyword vehicleOperator and or

" Comments
" Line comments: -- ... until end of line
syn region vehicleLineComment start="--" end="$" contains=@Spell
" Block comments: {- ... -} with nesting
syn region vehicleBlockComment start="{-" end="-\}" contains=vehicleBlockComment,@Spell

" Numeric literals
" Integer literals
syn match vehicleInteger "\<\d\+\>"
" Float literals
syn match vehicleFloat "\<\d\+\.\d\+\>"

" Holes
syn match vehicleHole "\w\@<!?[A-Za-z0-9_]*\>"


" Define default highlighting

" Declaration keywords
hi def link vehicleDeclaration Keyword

" Annotation keywords
hi def link vehicleAnnotation PreProc

" Control keywords
hi def link vehicleControl Conditional

" Value builtins
hi def link vehicleValueBuiltin Constant

" Type builtins
hi def link vehicleTypeBuiltin Type

" Operator builtins
hi def link vehicleOperator Operator

" Comments
hi def link vehicleLineComment Comment
hi def link vehicleBlockComment Comment

" Numeric literals
hi def link vehicleInteger Number
hi def link vehicleFloat Float

" Holes
hi def link vehicleHole Identifier

let b:current_syntax = "vehicle"
