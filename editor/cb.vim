" Vim syntax file
" Language:	c flat
" Maintainer:	cowboy8625
" Last Change:	2022 Dec 12
if exists("b:current_syntax")
  finish
endif

syn keyword aKeyword if else
syn keyword aKeyword true false
syn keyword aKeyword or and let in const
syn keyword aKeyword fn struct enum union type
syn keyword aKeyword return

" syn keyword aFunction println print

hi link aKeyword Keyword

syn match aFn "\(fn\_s\+\)\@<=\<[A-z0-9]\+\>"

syn keyword aTodo contained TODO FIXME XXX NOTE
syn match aComment "--.*$" contains=aTodo
syn region aCommentBlock start="{-\%(!\|\*[*/]\@!\)\@!" end="-}" contains=aTODO


" Regular int like number with - + or nothing in front
syn match aNumber '\d\+' contained display
syn match aNumber '[-+]\d\+' contained display

" Floating point number with decimal no E or e (+,-)
syn match aNumber '\d\+\.\d*' contained display
syn match aNumber '[-+]\d\+\.\d*' contained display

" Floating point like number with E and no decimal point (+,-)
syn match aNumber '[-+]\=\d[[:digit:]]*[eE][\-+]\=\d\+' contained display
syn match aNumber '\d[[:digit:]]*[eE][\-+]\=\d\+' contained display

" Floating point like number with E and decimal point (+,-)
syn match aNumber '[-+]\=\d[[:digit:]]*\.\d*[eE][\-+]\=\d\+' contained display
syn match aNumber '\d[[:digit:]]*\.\d*[eE][\-+]\=\d\+' contained display

syn region aString start='"' end='"' contained
syn region aDesc start='"' end='"'

syn match aHip '\d\{1,6}' nextgroup=aString
syn region aDescBlock start="{" end="}" fold transparent contains=ALLBUT,aHip,crashString

syn keyword aBlockCmd RA Dec Distance AbsMag nextgroup=aNumber
syn keyword aBlockCmd SpectralType nextgroup=aDesc

syn match aCharacter /b'\([^\\]\|\\\(.\|x\x\{2}\)\)'/
syn match aCharacter /'\([^\\]\|\\\(.\|x\x\{2}\|u{\%(\x_*\)\{1,6}}\)\)'/

syn match aIdentifier contains=aIdentifierPrime "\%([^[:cntrl:][:space:][:punct:][:digit:]]\|_\)\%([^[:cntrl:][:punct:][:space:]]\|_\)*" display contained


hi def link aIdentifierPrime   aIdentifier
hi def link aIdentifier        Identifier
hi def link aFn                Function
hi def link aTodo              Todo
hi def link aComment           Comment
hi def link aCommentBlock      Comment
hi def link aBlockCmd          Statement
hi def link aHip               Type
hi def link aString            Constant
hi def link aDesc              PreProc
hi def link aNumber            Constant
hi def link aCharacter         Character

let b:current_syntax = "cb"
