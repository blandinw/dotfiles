"Command-T
let g:CommandTMaxHeight = 20
set wildignore+=*.o,*.obj,.git,*.pyc
set wildignore+=app/cache/*,app/logs/* "symfony2
map <Leader>p :CommandT<CR>

"NERDTree
map <Leader>n :silent :NERDTreeToggle<CR>
let NERDTreeIgnore=['cache', 'logs', 'vendor']

"Yankring
map <Leader>y :silent :YRShow<CR>

"Haskellmode
let g:haddock_browser = "open"
let g:haddock_browser_callformat = "%s %s"
