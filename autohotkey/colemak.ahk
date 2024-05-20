; Colemak layout for AutoHotkey (MS Windows)
; 2006-01-01 Shai Coleman, http://colemak.com/ . Public domain.
; See http://www.autohotkey.com/ for more information
; Need the requires because `; isn't recognised as non-v2
#Requires AutoHotkey v2.0

; For this to work you have to make sure that the US (QWERTY) layout is installed,
; that is set as the default layout, and that it is set as the current layout.
; Otherwise some of the key mappings will be wrong.
;
; This is mainly useful for those who don't have privileges to install a new layout
; This doesn't support the international features of the Colemak layout.
#SingleInstance force
; SendMode Input ; Recommended for new scripts due to its superior speed and reliability.
SetKeyDelay(5) ; sendevent is delayed by 10 by default, which may be inconvenvient
ProcessSetPriority("High")
KeyHistory 0
A_HotkeyModifierTimeout := 100
A_MenuMaskKey := "vkFF"

; We want other hotkey scripts to be usable with this one. Sendlevel means if
; this script sends ^t, the user's ahk ^t script should also be triggered.
; Generally works best if this ahk script is run AFTER any other ahk script, so
; that colemak keys will trigger other scripts as expected.
SendLevel(99)
#InputLevel 99

; Results in lower-level hotkeys? Forces using system hotkey register api
; #UseHook false

; Remap things to their original to ensure any delay is even across keys, to
; reduce transposition typos.

`::`
1::1
2::2
3::3
4::4
5::5
6::6
7::7
8::8
9::9
0::0
-::-
=::=

q::q
w::w
e::f
r::p
t::g
y::j
u::l
i::u
o::y
p::;
[::[
]::]
\::\

a::a
s::r
d::s
f::t
g::d
h::h
j::n
k::e
l::i
`;::o
'::'

z::z
x::x
c::c
v::v
b::b
n::k
m::m
,::,
.::.
/::/
Space::Space

Capslock::Backspace

; Block #l from locking windows. Alternatives include pressing win + physical L
; key, or the linux-inspired version below.
#u::return
^!u::#l
; Commonly want to add this to local overrides when using komorebic. Need the #if to allow override.
; #hotif true
; #u::RunWait, komorebic.exe focus right, , Hide
; #hotif

; Overrides the change layout key... but that's exactly what this script is
; doing.
#Space::Suspend()

#Include *i %A_MyDocuments%\local colemak overrides.ahk
