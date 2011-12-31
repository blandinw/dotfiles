" Command-T
let g:CommandTMaxHeight = 20
map <Leader>p :CommandT<CR>
map <Leader>b :CommandTBuffer<CR>

" NERDTree
map <Leader>n :silent :NERDTreeToggle<CR>
let NERDTreeIgnore=['^cache$', '^logs$', '\~$']

" custom
let curr=$PWD
if (curr =~ 'chouquette')
    let NERDTreeIgnore+=['\.js$']
    set wildignore+=*.js
endif
if (curr =~ 'twitter_oliv')
    let NERDTreeIgnore+=['\.js$']
    set wildignore+=*.js
endif

