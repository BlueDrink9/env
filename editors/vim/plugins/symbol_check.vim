" Automatically check for powerline compatible font installed locally
" (unix) or to system (windows)
" If we are (probably) using a powerline compatible font, set it so.
" If a nerd font is found, assume powerline-compat, as well as devicons.

" Env variables, can be set by ssh client on login if it supports PL.
if $USENF==1
    let g:usePLFont = 1
    let g:useNerdFont = 1
elseif $USEPF==1
    let g:usePLFont = 1
elseif $USEPF==0
    let g:usePLFont = 0
    let g:useNerdFont = 0
elseif $USENF==0
    let g:useNerdFont = 0
elseif g:hasGUI
    let s:guiUsesNerdFont = 
                \ &guifont =~ "Nerd" ||
                \ &guifont =~ "Meslo" ||
                \ &guifont =~ "Sauce"

    let s:guiUsesPLFont = s:guiUsesNerdFont || 
                \ &guifont =~ "Powerline" ||
                \ &guifont =~ "Source\\ Code\\ Pro"

    let g:usePLFont = s:guiUsesPLFont
    let g:useNerdFont = s:guiUsesNerdFont
else

    if has("unix")
        let s:uname = system("uname")
        if s:uname =~ "Darwin"
            " OSX
            exec "let s:fontdir = expand('" . $HOME . "/Library/Fonts')"
        else
            " Linux
            exec "let s:fontdir = expand('" . $HOME . "/.fonts')"
        endif
    else
        " Windows
        exec "let s:fontdir = expand('" . $windir . "/Fonts')"
    endif

    let s:nerdFontNames = [
                \ "Sauce Code Pro Nerd Font Complete Mono Windows Compatible.ttf",
                \ "Sauce Code Pro Nerd Font Complete Windows Compatible.ttf",
                \ "Sauce Code Pro Nerd Font Complete Mono.ttf",
                \ "Sauce Code Pro Medium Nerd Font Complete.ttf",
                \ "Sauce Code Pro Nerd Font Complete.ttf" ]
    let s:nerdFontIsInstalled = []
    let s:PLFontNames = [
                \ "SourceCodePro-Regular.ttf",
                \ "SourceCodePro-Regular.otf"]
    let s:PLFontIsInstalled = []

    let s:nerdFontExists = 0
    let s:PLFontExists = 0
    let i = 0
    while i < len(s:nerdFontNames)
        exec "call add(s:nerdFontIsInstalled, 
                    \ filereadable( expand('" . s:fontdir . "/" . s:nerdFontNames[i] . "')))"
        exec "let s:nerdFontExists = " . s:nerdFontExists . " || " . s:nerdFontIsInstalled[i]
        exec "let s:PLFontExists = " . s:nerdFontExists . " || " . s:nerdFontIsInstalled[i]
        let i += 1
    endwhile

    let i = 0
    if !s:nerdFontExists
        while i < len(s:PLFontNames)
            exec "call add(s:PLFontIsInstalled, 
                        \ filereadable( expand('" . s:fontdir . "/" . s:PLFontNames[i] . "')))"
            exec "let s:PLFontExists = " . s:PLFontExists . " || " . s:PLFontIsInstalled[i]
            let i += 1
        endwhile
    endif

    let g:usePLFont = s:PLFontExists
    let g:useNerdFont = s:nerdFontExists
endif

let g:usePLFont = g:useNerdFont || g:usePLFont

