" if executable('jupytext')
"     " call system('jupytext --to py %:h:p')
"     pass
" endif

" augroup my.ipynb
"     au! <buffer>
"     au bufwritepost .py !jupytext <a:file>
" augroup end
