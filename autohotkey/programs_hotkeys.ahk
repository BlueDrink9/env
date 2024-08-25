#HotIf (!IsPrefix(""))
Escape::ResetPrefix
#HotIf

#HotIf (IsPrefix(""))
#e::SetPrefix("exe")
#m::SetPrefix("media")
#HotIf

#HotIf IsPrefix("office")
w::Run("winword.exe")
p::Run("powerpnt.exe")
s::Run("excel.exe")
#HotIf

#HotIf IsPrefix("exe")
o::SetPrefix("office")
e::Run("explorer")
t::terminal
+t::terminal(true)
b::Run(GetDefaultBrowser())
v::editor
!v::liteEditor
+v::ide
i::ide
; notes
n::notes
; Games
g::Run(A_AppData . "..\Playnite\Playnite.DesktopApp.exe")
; Kalendar
k::calendar
; Signal
s::chat
; Passwords
p::passwords
w::WorkraveToggle
#HotIf

#HotIf IsPrefix("media")
s::Run("spotify.exe")
; main playlist
m::{
  Run("spotify:playlist:3DDy0dVcH0v8hwTsow3rCN:play")
  ; WinWaitActive ahk_exe spotify.exe,,10
  ; send ^s  ; shuffle
}
; Beats playlist
b::{
  Run("spotify:playlist:07DNxdotyBhZf3R77tPheP:play")
  ; WinWaitActive ahk_exe spotify.exe,,10
  ; send ^s  ; shuffle
  ; send ^{right}  ; skip to next to ensure shuffling takse place
}
#HotIf
