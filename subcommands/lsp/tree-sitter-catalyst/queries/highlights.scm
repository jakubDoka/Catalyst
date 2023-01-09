(
  (path (path_seg (name) @type))
  (#match? @type "[A-Z][a-z0-9]*")
)

(
  (path start: (path_seg (name) @type.builtin) !tail)
  (#match? @type.builtin "[a-z_][a-z_0-9]*")
)

(
  (path (path_seg (name) @namespace))
  (#match? @namespace "[a-z_][a-z_0-9]*")
)


(enum_pat tag: (name) @tag)

(sig_arg (pat [
  (name) 
]))


[
  "("
  ")"
  "["
  "]"
  "{"
  "}"
] @punctuation.bracket



":" @punctuation.delimiter
; "." @punctuation.delimiter
"," @punctuation.delimiter
";" @error ; to discorage the use of them


; "break" @keyword
; "const" @keyword
; "continue" @keyword
; "else" @keyword
; "enum" @keyword
"extern" @keyword
"fn" @keyword
; "if" @keyword
; "impl" @keyword
; "let" @keyword
; "loop" @keyword
; "match" @keyword
; "mod" @keyword
"pub" @keyword
"priv" @keyword
"return" @keyword
; "struct" @keyword
; "spec" @keyword
; "unsafe" @keyword
"use" @keyword

"*" @operator
"&" @operator
