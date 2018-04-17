# Environment
Unix environment setup. Custom vim, bash, tmux, and a few other misc option files.
Basically yet another dotfiles repo, but with some twists.

Key points of interest may be `install.sh`, and the nature of the files in the directories, which are kept separate to stop them getting too big, and to an extent keep them modular.
Main way `install.sh` works is that it adds a line to the relevant dotfile in your home dir, and that line sources the primary dotfile in the repo. That then sources all further files.

All of the font and colour specifications can be overwritten in the local dotfile, either before or after the line sourcing the repo dotfile.

argument `-u` uninstalls, but that may not always guarantee the system will be left the exact same as it was before. Probably just might not delete all files it creates (e.g. fonts)
Asks for which user of the script you are. `WW` is the only answer that installs the files in `editors/vim`.
Vim assumes that, if you have one of the listed nerd-fonts installed, you're using them in your console.

Note that aliases and functions are kept separate. An 'alias' you might miss, for example, is ;q in bash (or any readline app). This will (assuming vi-mode is still active) replace the current line with `exit`, allowing you to then press enter and exit the app. This is more reliable than :q, because it replaces the line contents. It also doesn't require pressing shift.


**Oh man, this is soooo out of date now...**

## Fonts

### Iosevka Term
https://github.com/be5invis/Iosevka/releases/latest

### Adobe Source Code Pro

## Visual Studio Code

### Extensions

A few, read the files.

## Config

### .bashrc

Custom coloured prompt, aliases. Vi-mode enabled. Solarised options where posible.
**`time user@hostname: currentDir [git branch & indicators] $ `**

### .bash_aliases: 

| Command | Action |
| :------ | :----- |
| `rmcr <file>` | Function to remove Carriage Return from a file. |
| `mkbak <file>` | Creates a backup of the given file with form: `<file.ext>.bak` |
| `cl` | Shorthand for `clear` |
| `..` | Shorthand for `cd ..` |
| `cd..` | Alt for `cd ..` incase the space is missed, obviously we want the same thing. |
| `lsa` | Shorthand for `ls -al` |
| `gc` | Shorthand for `git clone` |
| `fopen <path>` | Runs through list of file managers, opens with that. |
| `ws`\* | Shorthand for navigation to '$HOME/workspace' |
| And tonnes more | see `bash/aliases.sh` for more |

\* If the directory exists on the system.

### Vim

For WW: maaaasssses of changes. Main from a usability perspective: `;` and `:` are swapped.


For WS:
Clones and installs configuration.  
Updates the configuration if install is run again.  
Uses Amir Salihefendic's Ultimate Vim Configuration https://github.com/amix/vimrc

Clones and installs color schemes from: flazz https://github.com/flazz/vim-colorschemes  
Allows selection of a color scheme, and a shorcut to change it: `./install.sh -vc`

## Tools
