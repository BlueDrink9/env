SCRIPT_DIR=${0:a:h}
source "${SCRIPT_DIR}/../functions.sh"

# allows cd ..../dir
rationalise-dot() {
  if [[ $LBUFFER = *.. ]]; then
    LBUFFER+=/..
  else
    LBUFFER+=.
  fi
}
zle -N rationalise-dot
bindkey . rationalise-dot
