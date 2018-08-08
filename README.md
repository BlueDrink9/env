# Environment
Unix environment setup. Custom vim, bash, tmux, and a few other misc option files.
Basically yet another dotfiles repo, but with some twists.

Key points of interest may be `install.sh`, and the nature of the files in the directories, which are kept separate to stop them getting too big, and to an extent keep them modular.
Main way `install.sh` works is that it adds a line to the relevant dotfile in your home dir, and that line sources the primary dotfile in the repo. That then sources all further files.

## Philosopy:

* Try to adapt to a large variety of versions, screen sizes and setups automatically, so things "just work" without having to add too much to local .vimrcs
* All of the font and colour specifications can be overwritten in the local dotfile, either before or after the line sourcing the repo dotfile.
* Allow vim and unix to be used as a high-powered IDE
* Don't sacrifice the ability to use vim as a quick, light editor (achieved by modular plugin install levels - `idevim` vs `vim` vs `qvim` vs `vi` commands.

argument `-u` uninstalls, but that may not always guarantee the system will be left the exact same as it was before. Probably just might not delete all files it creates (e.g. fonts)
Asks for which user of the script you are. `WW` is the only answer that installs the files in `editors/vim`.
Vim assumes that, if you have one of the listed nerd-fonts installed, you're using them in your console.

Note that aliases and functions are kept separate. An 'alias' you might miss, for example, is ;q in bash (or any readline app). This will (assuming vi-mode is still active) replace the current line with `exit`, allowing you to then press enter and exit the app. This is more reliable than :q, because it replaces the line contents. It also doesn't require pressing shift.
