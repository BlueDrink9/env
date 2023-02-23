" {[} ------ IDE Mappings ------
" gh - get hint on whatever's under the cursor
" Use g[] for get [something].
" Use <leader>i for ide bits.
" Use <leader>e for errors/linting/fixing.
let g:IDE_mappings = {
            \ 'FuzzyFuzzy' : '<leader><cr>',
            \ 'FuzzyBuffers' : '<leader>,',
            \ 'FuzzyCommands' : '<leader>;',
            \ 'FuzzyOpenFile' : '<leader><leader>',
            \ 'FuzzySymbols' : '<leader>.',
            \ 'FuzzyOldFiles' : '<leader>fr',
            \ 'FuzzySearchBuffer' : '<leader>/',
            \ 'FuzzySearchBuffers' : '<leader>f',
            \ 'FuzzySearchFiles' : '<leader>F',
            \ 'FuzzyTags' : '<leader>it',
            \ 'FuzzyLspTags' : '<leader>iT',
            \ 'VCSNextHunk' : '<leader>g]',
            \ 'VCSPreviousHunk' : '<leader>g[',
            \ 'REPLSend' : '<leader>s',
            \ 'REPLSendLine' : '<leader>ss',
            \ 'REPLSendFile' : '<leader><s-S>',
            \ 'REPLSendAndInsert' : '<leader>s+',
            \ 'REPLClear' : '<leader>sc',
            \ 'REPLCancel' : '<leader>s<c-c>',
            \ 'REPLClose' : '<leader>sx',
            \ 'lintBuffer' : '<leader>il',
            \ 'make' : 'm<cr>',
            \ 'make2' : '<leader>im',
            \ 'make3' : 'm<space>',
            \ 'allActions' : '<leader>ia',
            \ 'allCommands' : '<leader>ic',
            \ 'hover' : '<leader>ih',
            \ 'codeAction' : '<leader>ia',
            \ 'codeActionSelected' : '<leader>iaa',
            \ 'codelensAction' : '<leader>ial',
            \ 'complete' : '<c-space>',
            \ 'signature' : '<leader>is',
            \ 'definition' : 'gd',
            \ 'definition2' : '<leader>id',
            \ 'typeDefinition' : 'gD',
            \ 'typeDefinition2' : '<leader>iD',
            \ 'documentation' : 'K',
            \ 'documentation2' : 'gh',
            \ 'fix' : '<leader>ef',
            \ 'implementation' : '<leader>ii',
            \ 'implementation2' : 'gi',
            \ 'listErrs' : '<leader>el',
            \ 'diagnostic' : '<leader>i?',
            \ 'diagnosticNext' : ']e',
            \ 'diagnosticPrev' : '[e',
            \ 'refactor' : '<leader>irr',
            \ 'references' : 'gr',
            \ 'references2' : '<leader>if',
            \ 'reformat' : '<leader>irf',
            \ 'rename' : '<leader>irn',
            \ 'renameModule' : '<leader>irm',
            \ 'snippetExpand' : '<c-e>',
            \ 'snippetPrev' : '<c-b>',
            \ 'snippetNext' : '<c-f>',
            \ 'debugFile' : '<leader>dd',
            \ 'debugStart' : '<leader>dD',
            \ 'debugRestart' : '<leader>dr',
            \ 'debugReset' : '<leader>dC',
            \ 'debugContinue' : '<leader>dc',
            \ 'debugStepOver' : '<leader>.',
            \ 'debugStepInto' : '<leader>>',
            \ 'debugStepOut' : '<leader>^',
            \ 'debugStepBack' : '<leader><',
            \ 'debugRunToHere' : '<leader>dh',
            \ 'debugShowOutput' : '<leader>do',
            \ 'debugHover' : '<leader>d?',
            \ 'debugInspect' : '<leader>di',
            \ 'debugFrameUp' : '<leader>dk',
            \ 'debugFrameDown' : '<leader>dj',
            \ 'debugShowStoppedLine' : '<leader>dH',
            \ 'setBreakpoint' : '<leader>b',
            \ 'setBreakpointConditional' : '<leader>dbc',
            \ 'addBreakpointFunctional' : '<leader>dbf',
            \ 'clearBreakpoints' : '<leader>dbC',
            \ 'breakpointList' : '<leader>dbl',
            \ 'breakpointNext' : ']b',
            \ 'breakpointPrev' : '[b',
            \ 'GitCommit' : '<leader>gcc',
            \ 'GitAmend' : '<leader>gce',
            \ 'GitStage' : '<leader>gs',
            \ 'GitStageFile' : '<leader>gS',
            \ 'GitUnstage' : '<leader>gu',
            \ 'GitUnstageFile' : '<leader>gU',
            \}
            " \ 'declaration' : 'gD',

" {]} ------ IDE Mappings ------

" To consider:
" workspace open file?
