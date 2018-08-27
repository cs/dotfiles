call plug#begin()

Plug 'ElmCast/elm-vim', { 'for': 'elm' }
Plug 'junegunn/fzf'
Plug 'junegunn/fzf.vim'
Plug 'thoughtbot/vim-rspec', { 'for': 'ruby' }
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

" Compatible with tpope/vim-sensible:
set showtabline=2 ruler wildmenu
set scrolloff=1 sidescrolloff=5 display+=lastline

set switchbuf=useopen

set guicursor=n-v-c-sm:block,i-ci-ve:block,r-cr-o:hor20

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

augroup termSettings
  autocmd!
  autocmd TermOpen * tnoremap <buffer> <Esc> <C-\><C-n>
  autocmd TermOpen * tnoremap <buffer> <C-h> <C-\><C-n><C-w>h
  autocmd TermOpen * tnoremap <buffer> <C-j> <C-\><C-n><C-w>j
  autocmd TermOpen * tnoremap <buffer> <C-k> <C-\><C-n><C-w>k
  autocmd TermOpen * tnoremap <buffer> <C-l> <C-\><C-n><C-w>l
augroup END

" Treat long lines as break lines (useful when moving around in them)
map      j       gj
map      k       gk

" Text formatting to get with with up textwidth characters
vmap     Q       gq
nmap     Q       gqap

" Edit alternate file with <leader><leader>. See `:help CTRL-^`.
" Note: `:bprevious` is different because it "wraps around".
nnoremap <leader><leader> <c-^>

augroup vimSettings
  autocmd!
  autocmd FileType vim command! -buffer W w|so %
  autocmd FileType vim if exists('b:undo_ftplugin')
                    \|   let b:undo_ftplugin .= " | delcommand W"
                    \| else
                    \|   let b:undo_ftplugin = "delcommand W"
                    \| endif
augroup END

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Move current file
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
function! MoveFile()
  let old_name = expand('%')
  let new_name = input('New file name: ', old_name, 'file')
  if new_name != ''
    exec ':Move! ' . new_name
  endif
endfunction
map <leader>n :call MoveFile()<CR>

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
  autocmd FileType elm setlocal tabstop=4
  autocmd FileType elm if exists('b:undo_ftplugin')
                    \|   let b:undo_ftplugin .= " | set tabstop<"
                    \| else
                    \|   let b:undo_ftplugin = "set tabstop<"
                    \| endif
augroup END

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Config for junegunn/fzf.vim
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:fzf_layout = { 'down': '20' }

augroup fzfSettings
  autocmd!
  autocmd FileType fzf set laststatus=0 noshowmode noruler nonumber
    \| autocmd BufLeave <buffer> set laststatus=2 showmode ruler number
  autocmd FileType fzf tunmap <buffer> <Esc>
  autocmd FileType fzf tunmap <buffer> <C-h>
  autocmd FileType fzf tunmap <buffer> <C-j>
  autocmd FileType fzf tunmap <buffer> <C-k>
  autocmd FileType fzf tunmap <buffer> <C-l>
augroup END

map <leader>f :Files<CR>
map <leader>F :Files %%<CR>
map <leader>b :Buffers<CR>

" Override the built-in Ag command to include hidden files:
command! -bang -nargs=* Ag call fzf#vim#ag(<q-args>, '--hidden', <bang>0)

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Config for thoughtbot/vim-rspec
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:rspec_command = "split | terminal bundle exec rspec {spec}"
augroup rubySettings
  autocmd!
  autocmd FileType ruby nnoremap <localleader>a :call RunAllSpecs()<CR>
  autocmd FileType ruby nnoremap <localleader>t :call RunCurrentSpecFile()<CR>
  autocmd FileType ruby nnoremap <localleader>s :call RunNearestSpec()<CR>
  autocmd FileType ruby nnoremap <localleader>l :call RunLastSpec()<CR>
augroup END

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Config for tpope/vim-fugitive
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
augroup fugitiveSettings
  autocmd!
  autocmd FileType gitcommit setlocal textwidth=0
  autocmd FileType gitcommit if exists('b:undo_ftplugin')
                          \|   let b:undo_ftplugin .= " | set textwidth<"
                          \| else
                          \|   let b:undo_ftplugin = "set textwidth<"
                          \| endif
augroup END

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Config for vim-airline/vim-airline
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:airline_powerline_fonts = 1
let g:airline#extensions#whitespace#enabled = 1
let g:airline#extensions#whitespace#max_lines = 10000
let g:airline#extensions#whitespace#checks = [ 'indent', 'trailing', 'mixed-indent-file' ]
