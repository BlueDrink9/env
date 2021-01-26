#!/usr/bin/env bash
# Requires template_install.sh sourced first, to define functions and define:
# installID installText baseRC

eval "$(cat <<END
do${installID}() {
    printErr "Enabling custom ${installID} setup..."
    addTextIfAbsent "${installText}" "${baseRC}"
  }
END
)"

eval "$(cat <<END
undo${installID}(){
    sed -in "s|.*${installText}.*||g" "${baseRC}"
  }
END
)"

# If directly run instead of sourced, do all
if [ ! "${BASH_SOURCE[0]}" != "${0}" ]; then
  do${installID}
fi
