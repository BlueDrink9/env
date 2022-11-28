#SingleInstance force
#NoEnv ; Recommended for performance and compatibility with future AutoHotkey releases.
; SendMode Input ; Recommended for new scripts due to its superior speed and reliability.
SendMode Event ; Allows input command to catch sent shortcuts (used by hotkey_sequence)
SetKeyDelay , 5,, ; sendevent is delayed by 10 by default, which may be inconvenvient
Process, Priority,, High

; We want other hotkey scripts to be usable with this one. Sendlevel means if
; this script sends ^t, the user's ahk ^t script should also be triggered.
sendlevel 99
#inputlevel 99

e::f
r::p
t::g
y::j
u::l
i::u
o::y
p::`;

a::a
s::r
d::s
f::t
g::d
j::n
k::e
l::i
`;::o

n::k

Capslock::Backspace

; Block #l from locking windows. Alternatives include pressing win + physical L
; key, or the linux-inspired version below.
#u::return
^!u::#l

; Overrides the change layout key... but that's exactly what this script is
; doing.
#Space::Suspend
