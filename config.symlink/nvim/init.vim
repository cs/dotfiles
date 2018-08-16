call plug#begin()

Plug 'ElmCast/elm-vim', { 'for': 'elm' }
Plug 'ctrlpvim/ctrlp.vim'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-eunuch'
Plug 'tpope/vim-fugitive'
Plug 'vim-airline/vim-airline'

call plug#end()

let mapleader = "\<Space>"
let maplocalleader = ","
cnoremap %% <C-R>=expand('%:h').'/'<CR>

set number relativenumber
set colorcolumn=80
set hidden
set cursorline cursorcolumn

set autoindent
set expandtab
set shiftwidth=0
set tabstop=2

set foldmethod=indent foldlevel=10 foldnestmax=10

set showmatch matchtime=2

set incsearch hlsearch ignorecase smartcase
nnoremap <silent> <CR> :nohlsearch<CR><CR>

set showtabline=2
set scrolloff=5
set sidescrolloff=5

set switchbuf=useopen

set wildmode=full
set wildmenu

set mouse=a " X Integration
set clipboard+=unnamedplus

let g:netrw_banner = 0
let g:netrw_liststyle = 3
let g:netrw_altv = 1

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Color Schemes
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set background=dark

" colorscheme solarized
" colorscheme Tomorrow-Night-Blue
colorscheme Tomorrow-Night-Bright
" colorscheme Tomorrow-Night-Eighties
" colorscheme Tomorrow-Night
" colorscheme Tomorrow

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Key Mappings
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
nmap     <Left>  <Nop>
imap     <Left>  <Nop>
nmap     <Right> <Nop>
imap     <Right> <Nop>
nmap     <Up>    <Nop>
imap     <Up>    <Nop>
nmap     <Down>  <Nop>
imap     <Down>  <Nop>
nmap     <F1>    <Nop>
imap     <F1>    <Nop>

" Move around between splits with <C-hjkl>
nnoremap <C-h>   <C-w>h
nnoremap <C-j>   <C-w>j
nnoremap <C-k>   <C-w>k
nnoremap <C-l>   <C-w>l
tnoremap <Esc>   <C-\><C-n>
tnoremap <C-h>   <C-\><C-N><C-w>h
tnoremap <C-j>   <C-\><C-N><C-w>j
tnoremap <C-k>   <C-\><C-N><C-w>k
tnoremap <C-l>   <C-\><C-N><C-w>l

" Treat long lines as break lines (useful when moving around in them)
map      j       gj
map      k       gk

" Text formatting to get with with up textwidth characters
vmap     Q       gq
nmap     Q       gqap

" Edit alternate file with <leader><leader>. See `:help CTRL-^`.
" Note: `:bprevious` is different because it "wraps around".
nnoremap <leader><leader> <c-^>

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Rename current file
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
function! RenameFile()
    let old_name = expand('%')
    let new_name = input('New file name: ', old_name, 'file')
    exec ':Move! ' . new_name
endfunction
map <leader>n :call RenameFile()<CR>

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Multi-purpose tab key
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
function! InsertTabWrapper()
    let col = col('.') - 1
    if !col || getline('.')[col - 1] !~ '\k'
        return "\<tab>"
    else
        return "\<c-p>"
    endif
endfunction
inoremap <tab> <c-r>=InsertTabWrapper()<CR>
inoremap <s-tab> <c-n>

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Config for ElmCast/elm-vim
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:elm_format_autosave = 1
let g:elm_format_fail_silently = 1
let g:elm_setup_keybindings = 1
let g:elm_make_output_file = "/tmp/elm.js"

augroup elmSettings
  autocmd!
  autocmd FileType elm set tabstop=4
augroup END

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Config for ctrlpvim/ctrlp.vim
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:ctrlp_match_window = 'bottom,max:15'
let g:ctrlp_use_caching = 0
let g:ctrlp_user_command = 'fd --type f . %s'

map <leader>f :CtrlPCurWD<CR>
map <leader>F :CtrlPCurFile<CR>
map <leader>b :CtrlPBuffer<CR>

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Config for vim-airline/vim-airline
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:airline_powerline_fonts = 1
let g:airline#extensions#whitespace#enabled = 1
let g:airline#extensions#whitespace#max_lines = 10000
let g:airline#extensions#whitespace#checks = [ 'indent', 'trailing', 'mixed-indent-file' ]
