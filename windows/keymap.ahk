#Requires AutoHotkey v2.0

;; Suppress Alt menu activation when pressed alone
;; ~ lets Alt pass through as a modifier; {vkE8} tricks Windows
;; into thinking Alt was used in a combo, preventing menu activation
~LAlt::Send "{Blind}{vkE8}"

;; Set capslock tap to escape & hold to be control
; $Capslock:: {
;   KeyWait "Capslock", "T.2"
;   If !ErrorLevel
;     Send "{Escape}"
;   Else {
;     Send "{LControl Down}"
;     KeyWait "Capslock", "T5"
;     If ErrorLevel
;       MsgBox("5 secs!")
;   }
;   KeyWait "Capslock"
;   Send "{LControl Up}"
; }

; $Capslock:: {
;   If (A_PriorKey = "CapsLock") {
;     Send "{Escape}"
;   }
; }

*CapsLock:: {
  Send "{Blind}{Ctrl Down}"
  global cDown := A_TickCount
}

*CapsLock up:: {
  global cDown
  If ((A_TickCount-cDown)<150)  ; Modify press time as needed
    Send "{Blind}{Ctrl Up}{Esc}"
  Else
    Send "{Blind}{Ctrl Up}"
}

q::w
w::g
e::d
r::f
t::b

a::r
;s::s
d::t
f::h
g::k

; Standard
; z::x
; x::c
; c::m
; v::p
; b::v
; LAlt & z::Send("{^}")
; LAlt & x::Send("{@}")
; LAlt & c::Send("{#}")
; LAlt & v::Send("``")
; LAlt & b::Send("{}")

; Angle
z::c
x::m
c::p
v::v
b::x
LAlt & z::Send("{@}")
LAlt & x::Send("{#}")
LAlt & c::Send("``")
LAlt & v::Send("{}")
LAlt & b::Send("{^}")

; Angle - fat pinky
; $LShift:: {
;   KeyWait "LShift", "T.15"
;   If !ErrorLevel
;     Send "{x}"
;   Else {
;     Send "{LShift Down}"
;   }
;   KeyWait "LShift"
;   Send "{LShift Up}"
; }

y::q
u::l
i::u
;o::o
p::y

h::j
j::n
k::e
l::a

`;::i
n::z
m::;

LAlt & q::Send("{}")
LAlt & w::Send("{PgUp}")
LAlt & e::Send("{PgDn}")
LAlt & r::Send("{~}")
LAlt & t::Send("{}")

LAlt & a::Send("{Left}")
LAlt & s::Send("{Up}")
LAlt & d::Send("{Down}")
LAlt & f::Send("{Right}")
LAlt & g::Send("{}")

LAlt & y::Send("{%}")
LAlt & u::Send("{&}")
LAlt & i::Send("{-}")
LAlt & o::Send("{*}")
LAlt & p::Send("{+}")

LAlt & h::Send("{!}")
LAlt & j::Send("{Tab}")
LAlt & k::Send("{Backspace}")
LAlt & l::Send("{(}")
LAlt & `;::Send("{)}")

LAlt & n::Send("{$}")
LAlt & m::Send("{_}")
LAlt & ,::Send("{=}")
LAlt & .::Send("{{}")
LAlt & /::Send("{}}")
LAlt & Space::Send("{Space}")
