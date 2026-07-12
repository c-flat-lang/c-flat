" Vim syntax file
" Language:     c-flat
" Maintainer:   cowboy8625
" Last Change:  2026 Jul 12
if exists("b:current_syntax")
  finish
endif

" Keywords
syn keyword aKeyword if else defmacro
syn keyword aKeyword true false use
syn keyword aKeyword or and let in const pub
syn keyword aKeyword fn struct enum union type
syn keyword aKeyword return extern for

hi link aKeyword Keyword

" Types: identifiers following 'type' keyword
syn match aType "\(type\_s\+\)\@<=\<[A-Za-z0-9_]\+\>"

" Functions: identifiers following 'fn' keyword
syn match aFn "\(fn\_s\+\)\@<=\<[A-Za-z0-9_]\+\>"

" Generics cradle: <( ... )>
syn region aCradle start="<(" end=")>" contains=aType,aNumber,aIdentifier

" Module path separator (::) used in use statements
syn match aPathSep "::"

" Method/field access dot
syn match aMethodCall "\.\<[A-Za-z_][A-Za-z0-9_]*\>\ze\s*("

" Struct field initializer (.field = expr)
syn match aStructField "\.\<[A-Za-z_][A-Za-z0-9_]*\>\ze\s*="

" Comments
syn keyword aTodo contained TODO FIXME XXX NOTE
syn match aComment "//.*$" contains=aTodo
syn region aCommentBlock start="{-\%(!\|\*[*/]\@!\)\@!" end="-}" contains=aTodo

" Numbers
" Regular int
syn match aNumber '\d\+' display
syn match aNumber '[-+]\d\+' display
" Float without exponent
syn match aNumber '\d\+\.\d*' display
syn match aNumber '[-+]\d\+\.\d*' display
" Float with exponent, no decimal
syn match aNumber '[-+]\=\d[[:digit:]]*[eE][\-+]\=\d\+' display
syn match aNumber '\d[[:digit:]]*[eE][\-+]\=\d\+' display
" Float with exponent and decimal
syn match aNumber '[-+]\=\d[[:digit:]]*\.\d*[eE][\-+]\=\d\+' display
syn match aNumber '\d[[:digit:]]*\.\d*[eE][\-+]\=\d\+' display

" Strings
syn region aString start='"' end='"'

" Characters
syn match aCharacter /b'\([^\\]\|\\\(.\|x\x\{2}\)\)'/
syn match aCharacter /'\([^\\]\|\\\(.\|x\x\{2}\|u{\%(\x_*\)\{1,6}}\)\)'/

" Identifiers
syn match aIdentifier "\%([^[:cntrl:][:space:][:punct:][:digit:]]\|_\)\%([^[:cntrl:][:punct:][:space:]]\|_\)*" display

" Highlight links
hi def link aIdentifier        Identifier
hi def link aFn                Function
hi def link aTodo              Todo
hi def link aComment           Comment
hi def link aCommentBlock      Comment
hi def link aType              Type
hi def link aCradle            Special
hi def link aPathSep           Operator
hi def link aMethodCall        Function
hi def link aStructField       Identifier
hi def link aString            String
hi def link aNumber            Number
hi def link aCharacter         Character

let b:current_syntax = "cb"
