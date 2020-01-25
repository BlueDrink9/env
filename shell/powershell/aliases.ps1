# Aliases to programs with arguments must be created as functions. Consider just using functions instead.
# Set-Alias ;q exit
Set-Alias ';q' exit
# Set-Alias where where.exe
Set-Alias which where.exe
Set-Alias g git
Set-Alias e nvim
function liteEdit {nvim --cmd "let g:liteMode=1"}
Set-Alias le liteEdit
function ide {nvim --cmd "let g:ideMode=1"}
Set-Alias ide ide
