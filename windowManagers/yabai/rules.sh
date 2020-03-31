#!/usr/bin/env bash
# https://github.com/koekeishiya/yabai/blob/master/doc/yabai.asciidoc#66-rule

yabai -m rule --add app="workrave" manage=off
yabai -m rule --add app="System Preferences" manage=off
yabai -m rule --add label="Finder" app="^Finder$" title="(Co(py|nnect)|Move|Info|Pref)" manage=off
yabai -m rule --add label="Calculator" app="^Calculator$" manage=off
yabai -m rule --add label="Dictionary" app="^Dictionary$" manage=off
yabai -m rule --add app="Archive Utility" manage=off
yabai -m rule --add app="App Store" manage=off
yabai -m rule --add app="Word*" manage=on
yabai -m rule --add app="OneNote*" manage=on
yabai -m rule --add app="PowerPoint*" manage=on
yabai -m rule --add app="Excel*" manage=on
yabai -m rule --add app="PowerPoint" manage=on
# yabai -m rule --add app="PowerPoint" --subrole AXUnknown manage=off
# yabai -m rule --add app="PowerPoint" --subrole AXDialog manage=off
# yabai -m rule --add --subrole AXUnknown manage=off
# yabai -m rule --add --subrole AXDialog manage=off
yabai -m rule --add app="Excel" manage=on
yabai -m rule --add app="Word" manage=on
yabai -m rule --add app="OneNote" manage=on
yabai -m rule --add app="kitty" opacity=0.9
# XQuartz basically cannot be accessed by accessability API.
# yabai -m rule --add app=XQuartz manage=on
# yabai -m rule --add app=Inkscape manage=on

