" Command-T
let g:CommandTMaxHeight = 25
map <Leader>p :CommandT<CR>
map <Leader>b :CommandTBuffer<CR>

" NERDTree
map <Leader>n :silent :NERDTreeToggle<CR>
let NERDTreeIgnore=['^cache$', '^logs$', '\~$']

" VimClojure
let vimclojure#SetupKeyMapCloseResultBuffer = 0
nmap <buffer> <unique> <silent> <LocalLeader>o <Plug>ClojureCloseResultBuffer.
let g:vimclojure#HighlightBuiltins = 1
let g:vimclojure#ParenRainbow = 1
let vimclojure#SplitPos = "right"
let vimclojure#WantNailgun = 1
"let vimclojure#NailgunClient = "/usr/local/bin/ng"

