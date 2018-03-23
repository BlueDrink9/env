# Environment
Coding environment setup.

**Oh man, this is soooo out of date now...**

## Fonts

### Iosevka Term
https://github.com/be5invis/Iosevka/releases/latest

## Visual Studio Code

### Extensions

## Config

### .bashrc

Custom PS1 **`user@hostname:currentDir (git branch & indicators)$ `**

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
| `open <path>` | Shorthand for `nautilus <path>` |
| `ws`\* | Shorthand for navigation to '$HOME/workspace' |

\* If the directory exists on the system.

### Vim
Clones and installs configuration.  
Updates the configuration if install is run again.  
Uses Amir Salihefendic's Ultimate Vim Configuration https://github.com/amix/vimrc

Clones and installs color schemes from: flazz https://github.com/flazz/vim-colorschemes  
Allows selection of a color scheme, and a shorcut to change it: `./install.sh -vc`

### Nano

Simple nano config:
+ set tabsize 4
+ set tabstospaces

## Tools

