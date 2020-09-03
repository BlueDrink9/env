# Environment - My Dotfiles repo

Unix environment setup. Custom vim, bash, tmux, and a few other misc option files.
Basically yet another dotfiles repo, but with some twists/unconventional aspects.

This document is probably out of date.

## Configuration

Some of these are set automatically, but if set manually, should override the default.

|Set in file|Option|Possible values|Description|Set after or before sourcing|
|-----------|------|---------------|-----------|----------------------------|
| Shell or rc | `WINDOW_CUSTOM_NAME` | string | Prepended to title of window, useful for naming windows for a session. Include a training space to give a gap before the short path |
| `.bashrc` | `USENF` | 1 or unset | Affects whether vim uses powerline and nerd symbols. Change this depending on your current font.| Before |
| `.bashrc` | `USEPF` | 1 or 0 | Affects whether vim uses powerline symbols. Change this depending on your current font.| Before |
| `.bashrc` | `COLORTERM` | truecolor, 24bit, 16, 256 | Mostly used for vim. Advertises your term capabilities. 16 is useful if the term's colours are customised, eg for solarized, and you want to use them instead of a 256 colourscheme | Before |
| `.bashrc` | `TERM_PROGRAM` | whatever you want | By default only a few terms set this. Just gets passed to SSH. Used for identifying term capabilities. | Before |
| `.bashrc` | `LPUSERNAME` | email | Lastpass login email | Either |
| `.bashrc` | `NOTMUX` | set or unset | Affects whether TMUX will start up automatically if there is no display | Before |
| `.bashrc` | `TMUX_ALLOW_DETACH` | set or unset | Whether tmux replaces the shell via exec, or starts on top of it. If unset, detaching exits the shell session. Good for debugging. | Before |
| `.bashrc` | `HOMEBREW_PREFIX` | path to brew folder | . | Before |
| `.bashrc` | `COLOURSCHEME` | Name of current preferred colourscheme. Append `_light` or `_dark` to specify vim background. | UK spelling to avoid possible clobber. | Before |
| `.bashrc` | `CLIP_PROGRAM_COPY` | a command | Accepts input on stdin and sends it to clipboard. Used for ctrl+c in vim insert if clipboard not enabled. | Before |
| `.bashrc` | `CLIP_PROGRAM_PASTE` | a command | Outputs clipboard to stdout. Used for ctrl+v in vim insert if clipboard not enabled. | Before |
| `.bashrc` | `PACKCMD` | a package manager command | Forces `pack` aliases to use the specified cmd as the package manager. | Any |
| `.vimrc` | `colorSch` | Name of preferred vim colourscheme | Allows overriding from term scheme/default of solarized | Before |
| `.vimrc` | `g:backgroundColour` | "light" or "dark" | Sets vim theme to light or dark | Before |
| `.vimrc` | `ideMode` | 1 or 0 | Enables heavy vim plugins, linting, snippets, etc. | Before |
| `.vimrc` | `liteMode` | 1 or 0 | Only loads lighter plugins. Good for quick editing. | Before |
| `.vimrc` | `noPlugins` | 1 or 0 | Don't load any vim plugins. | Before |
| `.vimrc` | `g:termColors` | same as `COLORTERM` | Allows vim-specific override for COLORTERM | Before |

Edit `TERMOPTIONS` and append any env variables you want to be passed through SSH.

## Bindings

Editor bindings are mostly in `mappings.vim`, with some in `functions.vim`. Mappings for plugins are kept with the plugin settings, in the various files in `plugins/`.

There is a dictionary in `mappings.vim` with all IDE-related mappings, which are used for relevant plugins.

## Viewports

All systems for arranging views should behave in as similar a fashion as possible, with similar bindings. Tmux is the base analogy, and the associated view metaphors are summarized in this table:

| Tmux term   | Window manager/desktop metaphor term   | Vim term        |
| ----------- | -------------------------------------- | ----------      |
| Pane        | Window                                 | Split           |
| Window*     | Tab within tabbed window               | unlisted buffer |
| Session     | Desktop/workspace                      | Tab             |

`*` Windows and sessions behave very similarly in tmux, so this doesn't quite work perfectly.

Mappings for manipulating/moving between these 3 should be as similar as possible, just with a different prefix/modifier key, with some exceptions.
Operations that are often repeated several times, like resizing or rearranging views, should use a different mode if possible.
Operations that occur rarely can be on prefixes, or use modes if need be. Modes should create a visual change when active, and be exitable with `esc` or pressing the mode key again.

Operations that change state are usually modified with `shift`

#### Bindings

Table shows bindings for the different modes, with `#` representing the modifier, `%` representing the prefix/mode change.

`*` indicates it is repeatable/becomes a mode. `!` indicates it is only available within a mode/after prefix.

Window manager prefix is `winkey/super/option` prefix is modifier + `w`, vim mode change/prefix is `<C-w>`, tmux prefix is `<C-s>` (`C-b` is awkward, and `C-a` overrides increment in vim).

Actions with arrow keys don't use the prefix in vim (Resizing, moving to next).

Not everything works in every context.

| Action |Binding |
|--------|--------|
| Focus [left, right, above, below] pane<sup>1</sup> | `<#-[hlkj]>` |
| Focus [next, previous] window | `*<#-[right,left]>` |
| Focus [next, previous] session | `*<#-[up,down]>` |
| `!`Expand current pane (Behaviour differs between programmes here) | `*%<S-[down,right]>` |
| `!`Contract current pane | `*%<S-[up,left]>` |
| `!`Contract/expand pane in other direction (if permitted) | `*%<S-C-[up,down,left,right]>` |
| `!`Swap pane with next container | `%<S-[hjkl]>` |
| `!`Push pane into next container | `%<S-#-[hjkl]>` |
| `!`Zoom/fullscreen pane | `%f` |
| `!`Minimize/pause/hide | `%z` |
| `!`Rotate tree | `%r` |
| `!`Rotate tree backwards | `%<S-r>` |
| Close pane (not vim) | `!x`/`#x` |

| Windowm manager only Action |Binding |
|--------|--------|
| Float pane | `%u` |
| Open browser | `#b` |
| Open file explorer | `#e` |
| Open terminal | `<C-A-t>` and `<#-return>`|
| Float next pane created | `%<C-u>` |
| Pin pane | `%p` |
| Use tiling layout | `%<S-t>` |
| Use floating layout | `%<S-u>` |
| Use monocle/tabbed layout | `%<S-m>` |
| Float next window | `%<c-u>` |
| Focus [next,prev] desktop | `<#-[[,]]>` |
| Focus desktop [number] | `<#-[number]>` |
| Focus previously focussed desktop (`^`) | `<#-S-6>` |
| Focus [prev,next] monitor | `<#-A-[[,]]>` |
| Move pane to [next,prev] desktop | `<S-#-[[,]]>` |
| Move pane to [next,prev] desktop and focus | `%<[[,]]>` |
| Move pane to [next,prev] desktop and focus | `%<C-#-[[,]]>` |
| Move pane to desktop [num] | `%[num]` |
| jump to desktop [num] | `<#-[num]>` |
| Move desktop to [next,prev] monitor | `%<S-A-[[,]]>` |
| Move desktop to [next,prev] monitor and focus | `%<C-A-[[,]]>` |
| Move desktop [left,right] on monitor | `%[<,>]` |
| Create desktop | `%n` |
| Delete desktop | `%<S-n>` |

<sup>1</sup> Vim and tmux share `ctrl` as the modifier here. Tmux also allows this after prefix, just in case the vim plugin isn't working.

#### Window manager terms/abbreviations used for keys:

* One window fullscreen (ie i3's 'tabbed' mode): Monacle (m)
* Tiling/binary layout: Tiling (t)
* Floating: Untiled (u)
* Pin/make sticky/keep on top: Pin (p)

## Vim IDE function mappings

Sourced from `ide_plugins.vim`. Check there for the latest.

```
let g:IDE_mappings = {
            \ "allActions" : "<leader>ia",
            \ "rename" : "<leader>in",
            \ "references" : "<leader>ir",
            \ "references2" : "gr",
            \ "refactor" : "<leader>if",
            \ "definition" : "<leader>id",
            \ "definition2" : "gd",
            \ "type-definition" : "gy",
            \ "implementation" : "<leader>ii",
            \ "implementation2" : "gi",
            \ "documentation" : "K",
            \ "documentation2" : "gh",
            \ "documentation3" : "<leader>ih",
            \ "codeAction" : "<leader>eca",
            \ "codeActionSelected" : "<leader>eca",
            \ "codelensAction" : "<leader>ecl",
            \ "fix" : "<leader>ef",
            \ "listErrs" : "<leader>el",
            \ "complete" : "<plug>Non-existant",
            \ "reformat" : "<plug>Non-existant",
            \}
```


## Possible points of interest to others

The install system, starting with `Install.sh`, and the nature of the files in the directories, which are kept separate to stop them getting too big, and to an extent keep them modular.
Main way `install.sh` works is that it adds a line to the relevant dotfile in your home dir, and that line sources the primary dotfile in the repo. That then sources all further files. This obviously requires the system to support includign other dotfiles. In particular, window managers never seem to like that, so orthogonal setup scripts are included for these.

The ssh system, that sets several options relating to your current term capabilities, and passes them to your ssh system (including tmux!). Tell the server that you have a `truecolor` terminal!

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
