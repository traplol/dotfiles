execute pathogen#infect()
set nocompatible
set showcmd
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
set softtabstop=4
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

" disable search highlight in insert mode
autocmd InsertEnter * :set nohlsearch
" enable search highlight in normal mode
autocmd InsertLeave * :set hlsearch

" prevents vim from using spaces instead of tabs in make files
autocmd FileType make setlocal noexpandtab

nnoremap <Leader>trim :%s/\s\+$//e<CR>


map <C-n> :NERDTreeToggle<CR>

nnoremap ; :

nnoremap Q @q

nnoremap <up> :resize -5<CR>
nnoremap <down> :resize +5<CR>
nnoremap <left> :vertical resize +5<CR>
nnoremap <right> :vertical resize -5<CR>

map <S-W> <Plug>CamelCaseMotion_w
map <S-B> <Plug>CamelCaseMotion_b
map <S-E> <Plug>CamelCaseMotion_e

if exists('$TMUX')
    function! TmuxOrSplitSwitch(wincmd, tmuxdir)
        let previous_winnr = winnr()
        silent! execute "wincmd " . a:wincmd
        if previous_winnr == winnr()
            call system("tmux select-pane -" . a:tmuxdir)
            redraw!
        endif
    endfunction

    let previous_title = substitute(system("tmux display-message -p '#{pane_title}'"), '\n', '', '')
    let &t_ti = "\<Esc>]2;vim\<Esc>\\" . &t_ti
    let &t_te = "\<Esc>]2;". previous_title . "\<Esc>\\" . &t_te

    nnoremap <silent> <C-h> :call TmuxOrSplitSwitch('h', 'L')<cr>
    nnoremap <silent> <C-j> :call TmuxOrSplitSwitch('j', 'D')<cr>
    nnoremap <silent> <C-k> :call TmuxOrSplitSwitch('k', 'U')<cr>
    nnoremap <silent> <C-l> :call TmuxOrSplitSwitch('l', 'R')<cr>
else
    map <C-h> <C-w>h
    map <C-j> <C-w>j
    map <C-k> <C-w>k
    map <C-l> <C-w>l
endif

nnoremap Cw cw<C-r>0<ESC>b


" Disable arrow keys in insert.
imap <up> <nop>
imap <down> <nop>
imap <left> <nop>
imap <right> <nop>

" Colors
colorscheme slate
highlight Search cterm=NONE ctermfg=black ctermbg=yellow
highlight SyntasticError cterm=NONE ctermfg=yellow ctermbg=red
highlight SyntasticWarning cterm=NONE ctermfg=black ctermbg=magenta
highlight SpecialKey guifg=gray ctermfg=66
highlight Pmenu ctermfg=yellow ctermbg=59
