# vim: set ft=bash:
# # If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

# Normal Colors
Black='\e[0;30m'        # Black
Red='\e[0;31m'          # Red
Green='\e[0;32m'        # Green
Yellow='\e[0;33m'       # Yellow
Blue='\e[0;34m'         # Blue
Purple='\e[0;35m'       # Purple
Cyan='\e[0;36m'         # Cyan
White='\e[0;37m'        # White

# Bold
BBlack='\e[1;30m'       # Black
BRed='\e[1;31m'         # Red
BGreen='\e[1;32m'       # Green
BYellow='\e[1;33m'      # Yellow
BBlue='\e[1;34m'        # Blue
BPurple='\e[1;35m'      # Purple
BCyan='\e[1;36m'        # Cyan
BWhite='\e[1;37m'       # White

# Background
bg_Black='\e[40m'       # Black
bg_Red='\e[41m'         # Red
bg_Green='\e[42m'       # Green
bg_Yellow='\e[43m'      # Yellow
bg_Blue='\e[44m'        # Blue
bg_Purple='\e[45m'      # Purple
bg_Cyan='\e[46m'        # Cyan
bg_White='\e[47m'       # White

NC='\e[m'               # Color Reset

# Prompt-escaped versions
# Normal Colors
pblack='\[\e[0;30m\]'        # Black
pred='\[\e[0;31m\]'          # Red
pgreen='\[\e[0;32m\]'        # Green
pyellow='\[\e[0;33m\]'       # Yellow
pblue='\[\e[0;34m\]'         # Blue
ppurple='\[\e[0;35m\]'       # Purple
pcyan='\[\e[0;36m\]'         # Cyan
pwhite='\[\e[0;37m\]'        # White

# Bold
pBBlack='\[\e[1;30m\]'       # Black
pBRed='\[\e[1;31m\]'         # Red
pBGreen='\[\e[1;32m\]'       # Green
pBYellow='\[\e[1;33m\]'      # Yellow
pBBlue='\[\e[1;34m\]'        # Blue
pBPurple='\[\e[1;35m\]'      # Purple
pBCyan='\[\e[1;36m\]'        # Cyan
pBWhite='\[\e[1;37m\]'       # White

# Background
pbg_Black='\[\e[40m\]'       # Black
pbg_Red='\[\e[41m\]'         # Red
pbg_Green='\[\e[42m\]'       # Green
pbg_Yellow='\[\e[43m\]'      # Yellow
pbg_Blue='\[\e[44m\]'        # Blue
pbg_Purple='\[\e[45m\]'      # Purple
pbg_Cyan='\[\e[46m\]'        # Cyan
pbg_White='\[\e[47m\]'       # White

pNC='\[\e[m\]'               # Color Reset
