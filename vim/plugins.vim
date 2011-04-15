" Command-T
let g:CommandTMaxHeight = 20
map <Leader>p :CommandT<CR>
map <Leader>b :CommandTBuffer<CR>

"NERDTree
map <Leader>n :silent :NERDTreeToggle<CR>
let NERDTreeIgnore=['cache', 'logs', 'vendor']

"Yankring
map <Leader>y :silent :YRShow<CR>

" fugitive
map <Leader>gs :silent :Gstatus<CR>
map <Leader>gc :silent :Gcommit<CR>

"Haskellmode
let g:haddock_browser = "open"
let g:haddock_browser_callformat = "%s %s"
