#!/usr/bin/env bash
source "$DOTFILES_DIR/shell/script_functions.sh"

if [[ "$OSTYPE" == 'linux-gnu' ]]; then
    FONTDIR="$HOME/.fonts"
elif [[ "$OSTYPE" =~ 'darwin' ]]; then
    FONTDIR="$HOME/Library/Fonts"
fi

doFonts() {
    mkdir -p "$FONTDIR"
    if [[ ! -d "${FONTDIR}/truetype/custom" ]]; then
        mkdir -p "${FONTDIR}/truetype/custom"
    fi

    printErr "Downloading fonts..."
    # # Get latest Iosevka font release.
    # fontUrl=$(getLatestReleaseFileURL "be5invis/Iosevka" "iosevka-pack-[^z]*zip")

    downloadFont "mesloNF" "ryanoasis/nerd-fonts" "Meslo\.zip"
    downloadFont "SauceCodeProNF" "ryanoasis/nerd-fonts" "SourceCodePro\.zip"

    # if [[ $OSTYPE == 'linux-gnu' ]]; then
    #     # Unused mac-required SCP fonts.
    #     rm -f "${SCPdir}/*Windows Compatible.ttf"
    # elif [[ $OSTYPE =~ 'darwin' ]]; then
    #     rm -f "${SCPdir}/*Complete.ttf"
    #     rm -f "${SCPdir}/*Mono.ttf"
    # fi

    fc-cache && printErr "${OK} Fontcache updated" || \
        printErr "${Error} ${Red}Failed to update fontcache${NC}"
}

downloadFont(){
    name="$1"
    repo="$2"
    file="$3"
    printErr "Downloading ${name}..."
    fontUrl=$(getLatestReleaseFileURL "${repo}" "${file}")
    fontdir="${FONTDIR}/${name}"
    downloadURLAndExtractZipTo "${fontUrl}" "${fontdir}" && \
        printErr "${OK} ${name} installed to ${Yellow}${fontdir}${NC}" || \
        printErr "${Error} ${Red} ${name} failed to install to ${Yellow}${fontdir}${NC}"

}

undoFonts(){
    rm -rf "${FONTDIR}/SauceCodeProNF"
}

# If directly run instead of sourced, do all
if [ ! "${BASH_SOURCE[0]}" != "${0}" ]; then
    source "$DOTFILES_DIR/shell/script_functions.sh"
    source "$DOTFILES_DIR/shell/bash/colour_variables.sh"
    doFonts
fi
