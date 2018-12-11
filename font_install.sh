#!/usr/bin/env bash
source "$DOTFILES_DIR/bash/script_functions.sh"

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
    # fontdir="${FONTDIR}/Iosevka"
    # downloadURLAndExtractZipTo $fontUrl $fontdir && \
    #     printErr "${OK} Fonts installed to ${Yellow}${fontdir}${NC}" || \
    #     printErr "${Error} ${Red}Fonts failed to install to ${Yellow}${fontdir}${NC}"

    printErr "Downloading Sauce Code Pro..."
    SCPUrl=$(getLatestReleaseFileURL "ryanoasis/nerd-fonts" "SourceCodePro\.zip")
    SCPdir="${FONTDIR}/SauceCodeProNF"
    downloadURLAndExtractZipTo "$SCPUrl" "$SCPdir" && \
        printErr "${OK} Fonts installed to ${Yellow}${SCPdir}${NC}" || \
        printErr "${Error} ${Red}Fonts failed to install to ${Yellow}${SCPdir}${NC}"

    if [[ $OSTYPE == 'linux-gnu' ]]; then
        # Unused mac-required SCP fonts.
        rm -f "${SCPdir}/*Windows Compatible.ttf"
    elif [[ $OSTYPE =~ 'darwin' ]]; then
        rm -f "${SCPdir}/*Complete.ttf"
        rm -f "${SCPdir}/*Mono.ttf"
    fi

    fc-cache && printErr "${OK} Fontcache updated" || \
        printErr "${Error} ${Red}Failed to update fontcache${NC}"
}

undoFonts(){
    rm -rf "${FONTDIR}/SauceCodeProNF"
}

# If directly run instead of sourced, do all
if [ ! "${BASH_SOURCE[0]}" != "${0}" ]; then
    source "$DOTFILES_DIR/bash/script_functions.sh"
    source "$DOTFILES_DIR/bash/colour_variables.sh"
    installFonts
fi
