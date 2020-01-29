# Aliases to programs with arguments must be created as functions. Consider just using functions instead.
Set-Alias which where.exe
Set-Alias g git
function gs { git status }
function gco { git commit }
function gca { git commit --all }
function gup { git commit --amend --no-edit --all }
function gupa { git commit --amend --no-edit --all }
function gupe { git commit --amend }
Set-Alias e nvim
function liteEdit {nvim --cmd "let g:liteMode=1"}
Set-Alias le liteEdit
function ide {nvim --cmd "let g:ideMode=1"}
Set-Alias time measure-command
Set-Alias fopen explorer.exe
function envupd { git -C "$DOTFILES_DIR" pull }
# function sudo { elevate.exe -k }  # k = persistent

# ln -h linkname source
function ln($linkname, $source, $s, $j, $h){
    if ($j){
        New-Item -ItemType junction -Path $source -Target $linkname
    } elseif ($h){
        New-Item -ItemType hardlink -Path $source -Target $linkname
    } else { #if ($s){
        New-Item -ItemType SymbolicLink -Path $source -Target $linkname
    }
}
