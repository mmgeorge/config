#NoEnv  
SendMode Input  
SetWorkingDir %A_ScriptDir%  

;; Set capslock tap to escape & hold to be control
; $Capslock::                
;   KeyWait Capslock,T.2
;   If !ErrorLevel         
;     Send {Escape}       
;   Else{ 
;     Send {LControl Down}   
;     KeyWait Capslock,T5   
;     If ErrorLevel        
;       MsgBox % "5 secs!" 
;   }                     
;   KeyWait Capslock     
;   Send {LControl Up}  
; Return               

; $Capslock::                
;   If (A_PriorKey = "CapsLock") {
;     Send { Escape }
;   }

*CapsLock::
  Send {Blind}{Ctrl Down}
  cDown := A_TickCount
Return

*CapsLock up::
  If ((A_TickCount-cDown)<150)  ; Modify press time as needed
    Send {Blind}{Ctrl Up}{Esc}
  Else
    Send {Blind}{Ctrl Up}
Return
 Return

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
; !z::Send {^}
; !x::Send {@}
; !c::Send {#}
; !v::Send {``}
; !b::Send {}

; Angle
z::c
x::m
c::p
v::v
b::x
!z::Send {@}
!x::Send {#}
!c::Send {``}
!v::Send {}
!b::Send {^}

; Angle - fat pinky 
; $LShift::                
;   KeyWait LShift,T.15
;   If !ErrorLevel         
;     Send {x}       
;   Else{ 
;     Send {LShift Down}   
;   }                     
;     KeyWait LShift     
;     Send {LShift Up}  
; Return               
 
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

!q::Send {}
!w::Send {PgUp}
!e::Send {PgDn}
!r::Send {~}
!t::Send {}

!a::Send {Left}
!s::Send {Up}
!d::Send {Down}
!f::Send {Right}
!g::Send {}

!y::Send {`%}
!u::Send {&}
!i::Send {-}
!o::Send {*}
!p::Send {+}

!h::Send {!}
!j::Send {Tab}
!k::Send {Backspace}
!l::Send {(}
!;::Send {)}

!n::Send {$}
!m::Send {_}
!,::Send {=}
!.::Send {`{}
!/::Send {`}}
!Space::Send {Space}
