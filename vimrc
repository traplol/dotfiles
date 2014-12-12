execute pathogen#infect()
set nocompatible
" turn syntax highlighting on
filetype plugin indent on
set t_Co=256
syntax on
" set UTF-8 encoding
set enc=utf-8
set fenc=utf-8
set termencoding=utf-8
" disable swap files
set noswapfile
" use indentation of previous line
set autoindent
" use intelligent indentation for C
set smartindent
" configure tabwidth and insert spaces instead of tabs
" tab width is 4 spaces
set tabstop=4
" indent also with 4 spaces
set shiftwidth=4
" expand tabs to spaces
set expandtab
set nohlsearch
set incsearch
set ignorecase
set ruler
set nowrap
set laststatus=2
" intelligent comments
set comments=sl:/*,mb:\ *,elx:\ */

autocmd StdinReadPre * let s:std_in=1
autocmd VimEnter * if argc() == 0 && !exists("s:std_in") | NERDTree | endif
" turn line numbers on
set relativenumber
set number

set scrolloff=10
" backspace over everything in insert mode
set backspace=indent,eol,start 
set list
set listchars=tab:>-,trail:.,extends:>,precedes:<
highlight SpecialKey guifg=gray ctermfg=66

" disable search highlight in insert mode
autocmd InsertEnter * :set nohlsearch
" enable search highlight in normal mode
autocmd InsertLeave * :set hlsearch

" prevents vim from using spaces instead of tabs in make files
autocmd FileType make setlocal noexpandtab

map <C-n> :NERDTreeToggle<CR>

nnoremap ; :

map <up> :resize -5<CR>
map <down> :resize +5<CR>
map <left> :vertical resize +5<CR>
map <right> :vertical resize -5<CR>

map <S-W> <Plug>CamelCaseMotion_w
map <S-B> <Plug>CamelCaseMotion_b
map <S-E> <Plug>CamelCaseMotion_e

" Disable arrow keys in insert.
imap <up> <nop>
imap <down> <nop>
imap <left> <nop>
imap <right> <nop>

" Colors
" Highlight columns 81+
match ErrorMsg '\%>80v.\+'
colorscheme slate
highlight Search cterm=NONE ctermfg=black ctermbg=yellow
highlight SyntasticError cterm=NONE ctermfg=yellow ctermbg=red
highlight SyntasticWarning cterm=NONE ctermfg=black ctermbg=magenta
