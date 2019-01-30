# Environment - My Dotfiles repo

Unix environment setup. Custom vim, bash, tmux, and a few other misc option files.
Basically yet another dotfiles repo, but with some twists/unconventional aspects.

This document is probably out of date.

## Configuration

Some of these are set automatically, but if set manually, should override the default.

|Set in file|Option|Possible values|Description|Set after or before sourcing|
|-----------|------|---------------|-----------|----------------------------|
| `.bashrc` | `USENF` | 1 or unset | Affects whether vim uses powerline and nerd symbols. Change this depending on your current font.| Before |
| `.bashrc` | `USEPF` | 1 or 0 | Affects whether vim uses powerline symbols. Change this depending on your current font.| Before |
| `.bashrc` | `COLORTERM` | truecolor, 24bit, 16, 256 | Mostly used for vim. Advertises your term capabilities. 16 is useful if the term's colours are customised, eg for solarized, and you want to use them instead of a 256 colourscheme | Before |
| `.bashrc` | `TERM_PROGRAM` | whatever you want | By default only a few terms set this. Just gets passed to SSH. Used for identifying term capabilities. | Before |
| `.bashrc` | `LPUSERNAME` | email | Lastpass login email | Either |
| `.bashrc` | `NOTMUX` | set or unset | Affects whether TMUX will start up automatically if there is no display | Before |
| `.bashrc` | `TMUX_ALLOW_DETACH` | set or unset | Whether tmux replaces the shell via exec, or starts on top of it. If unset, detaching exits the shell session. Good for debugging. | Before |
| `.bashrc` | `HOMEBREW_PREFIX` | path to brew folder | . | Before |
| `.bashrc` | `COLOURSCHEME` | Name of current preferred colourscheme. | UK spelling to avoid possible clobber. | Before |
| `.bashrc` | `CLIP_PROGRAM_COPY` | a command | Accepts input on stdin and sends it to clipboard. Used for ctrl+c in vim insert if clipboard not enabled. | Before |
| `.bashrc` | `CLIP_PROGRAM_PASTE` | a command | Outputs clipboard to stdout. Used for ctrl+v in vim insert if clipboard not enabled. | Before |
| `.vimrc` | `colorSch` | Name of preferred vim colourscheme | Allows overriding from term scheme/default of solarized | Before |
| `.vimrc` | `g:backgroundColour` | "light" or "dark" | Sets vim theme to light or dark | Before |
| `.vimrc` | `ideMode` | 1 or 0 | Enables heavy vim plugins, linting, snippets, etc. | Before |
| `.vimrc` | `liteMode` | 1 or 0 | Only loads lighter plugins. Good for quick editing. | Before |
| `.vimrc` | `noPlugins` | 1 or 0 | Don't load any vim plugins. | Before |
| `.vimrc` | `g:termColors` | same as `COLORTERM` | Allows vim-specific override for COLORTERM | Before |

Edit `TERMOPTIONS` and append any env variables you want to be passed through SSH.

## Possible points of interest

The install system, starting with `Install.sh`, and the nature of the files in the directories, which are kept separate to stop them getting too big, and to an extent keep them modular.
Main way `install.sh` works is that it adds a line to the relevant dotfile in your home dir, and that line sources the primary dotfile in the repo. That then sources all further files. This obviously requires the system to support includign other dotfiles. In particular, window managers never seem to like that, so orthogonal setup scripts are included for these.

The ssh system, that sets several options relating to your current term capabilities, and passes them to your ssh system (including tmux!)

A system for adding ssh keys and unlocking them from the lastpass password manager

`pack` a generic package manager wrapper. Supports brew, apt, yum and pacman for basic functions.

A system that keeps airline's theme up to date with vim's.

## Philosopy:

* Try to adapt to a large variety of versions, screen sizes and setups automatically, so things "just work" without having to add too much to local .vimrcs
* All of the font and colour specifications can be overwritten in the local dotfile, either before or after the line sourcing the repo dotfile.
* Allow vim and unix to be used as a high-powered IDE
* Don't sacrifice the ability to use vim as a quick, light editor (achieved by modular plugin install levels - `idevim` vs `vim` vs `qvim` vs `vi` commands.

argument `-u` uninstalls, but that may not always guarantee the system will be left the exact same as it was before. Probably just might not delete all files it creates (e.g. fonts)
Asks for which user of the script you are. `WW` is the only answer that installs the files in `editors/vim`.
Vim assumes that, if you have one of the listed nerd-fonts installed, you're using them in your console.

Note that aliases and functions are kept separate. An 'alias' you might miss, for example, is ;q in bash (or any readline app). This will (assuming vi-mode is still active) replace the current line with `exit`, allowing you to then press enter and exit the app. This is more reliable than :q, because it replaces the line contents. It also doesn't require pressing shift.
