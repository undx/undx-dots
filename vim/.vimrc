" vim: set foldmethod=marker foldlevel=0:
set nocompatible               " be iMproved
set showcmd
set shell=bash
filetype off                   " required!
let usr= substitute(system('whoami'), '\n', '', '')
let $TMP='/home/'. usr . '/tmp'
"
" Plugins system {{{
call plug#begin('~/.vim/plugged')
"
Plug 'tpope/vim-surround'
Plug 'tpope/vim-repeat'
Plug 'godlygeek/tabular'
" visual stuff
Plug 'undx/undx-cobalt'
Plug 'ap/vim-css-color'
Plug 'hzchirs/vim-material'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'airblade/vim-gitgutter'
Plug 'nathanaelkane/vim-indent-guides'
Plug 'endel/vim-github-colorscheme'
Plug 'altercation/vim-colors-solarized'
" utils
Plug 'junegunn/fzf.vim'
Plug 'scrooloose/syntastic'
Plug 'jremmen/vim-ripgrep'
Plug 't9md/vim-textmanip'
" languages
Plug 'sheerun/vim-polyglot'
Plug 'PotatoesMaster/i3-vim-syntax'
call plug#end()
" }}}
"
filetype plugin indent on     " required!
let mapleader = ','
syntax on
if has('gui_running')
    set background=light
else
    set background=dark
    let g:solarized_termcolors=256
endif
"colorscheme undx-cobalt
colorscheme solarized
call togglebg#map("<F5>")
"colorscheme vim-material

set undodir=$TMP/vim
set backupdir=$TMP/vim/backup
set directory=$TMP/vim
set hidden                     " buffer switching w/o saving
set backup
set undofile
set noswapfile                 " really really annoying behaviour
set showcmd                    " display commands ran
set showmatch                  " display matching pair
set noshowmode                 " don't show mode.
set autoread                   " refresh when a file is changed from the outside
set autochdir
set browsedir=buffer           " browse directory of the current buffer
set encoding=utf-8             " default encoding
set fileencoding=utf-8         " default encoding for new files
set nowrap                     " long lines
set backspace=indent,eol,start
set autoindent                 " Preserve current indent on new lines
set textwidth=120              " Wrap at this column
set backspace=indent,eol,start " Make backspaces delete sensibly
set tabstop=2                  " Indentation levels every two columns
set expandtab                  " Convert all tabs typed to spaces
set shiftwidth=2               " Indent/outdent by two columns
set shiftround                 " Indent/outdent to nearest tabstop
set history=1000
set number
"set nojoinspaces
"set statusline=%<%f\ %r%1*%m%*%h%y%k\ %{\"[\".&ff.\":\".(&fenc==\"\"?&enc:&fenc).((exists(\"+bomb\")\ &&\ &bomb)?\",B\":\"\").\"]\ \"}%=\ B#%n\ %-10.(L#%l/%L\ C#%c%V%)\ \[\%03.3b:\%02.2B]\ %P
" highlight statusline term=none ctermfg=White ctermbg=darkRed gui=none guifg=White guibg=darkRed
" highlight User1      term=bold ctermfg=Black ctermbg=darkRed gui=bold guifg=Black guibg=darkRed
set ruler                      " position displayed
set laststatus=2 " 0=never; 1=only if there are multiple windows; 2=always
"set cursorcolumn
set cc=+1
set shortmess=atI
set list
set listchars=tab:▸⋅,trail:⋅,nbsp:⋅,eol:¬,extends:»,precedes:« "⋅u22c5 ▸u25b8 ¬ u00ac
set showbreak=↪

"set incsearch       " incremental search
set hls             " hilight search results
set ignorecase      " search ignore case
set smartcase       " intelligent case searching

set grepprg=rg
set grepformat=%f:%l:%m

set wildmenu
set wildignore=.svn,CVS,.git,*.o,*.a,*.class,*.mo,*.la,*.so,*.obj,*.swp,*.jpg,*.png,*.xpm,*.gif
set completeopt=menu,longest

set keymodel=startsel

set dictionary =/usr/share/dict/words
"set dictionary += ...

filetype plugin indent on



"
autocmd BufNewFile *.mdwn 0r ~/.vim/templates/skeleton.md | set ft=ikiwiki
autocmd BufNewFile *.md   0r ~/.vim/templates/skeleton.md | set ft=markdown
"autocmd BufEnter   *      execute "lcd ".escape(expand("%:p:h"), ' ')

augroup configgroup
  autocmd!
  autocmd VimEnter * highlight clear SignColumn
  "autocmd BufWritePre *.php,*.py,*.js,*.txt,*.hs,*.java,*.md :call <SID>StripTrailingWhitespaces()
  autocmd FileType vim         setlocal foldmethod=marker
  autocmd FileType java        setlocal noexpandtab
  autocmd FileType java        setlocal list
  autocmd FileType java        setlocal listchars=tab:+\ ,eol:-
  autocmd FileType java        setlocal formatprg=par\ -w80\ -T4
  autocmd FileType php         setlocal expandtab
  autocmd FileType php         setlocal list
  autocmd FileType php         setlocal listchars=tab:+\ ,eol:-
  autocmd FileType php         setlocal formatprg=par\ -w80\ -T4
  autocmd FileType ruby        setlocal tabstop=2
  autocmd FileType ruby        setlocal shiftwidth=2
  autocmd FileType ruby        setlocal softtabstop=2
  autocmd FileType ruby        setlocal commentstring=#\ %s
  autocmd FileType python      setlocal commentstring=#\ %s
  autocmd FileType python      setlocal noexpandtab
  autocmd BufEnter *.cls       setlocal filetype=java
  autocmd BufEnter *.zsh-theme setlocal filetype=zsh
  autocmd BufEnter Makefile    setlocal noexpandtab
  autocmd BufEnter make        setlocal noexpandtab
  autocmd BufEnter *.sh        setlocal tabstop=2
  autocmd BufEnter *.sh        setlocal shiftwidth=2
  autocmd BufEnter *.sh        setlocal softtabstop=2
augroup END

" Run xrdb whenever Xdefaults or Xresources are updated.
autocmd BufWritePost *Xresources,*Xdefaults !xrdb %

""set ispell="aspell -e -c"
autocmd FileType mail :nmap <F8> :w<CR>:!aspell -e -c %<CR>:e<CR>
" ##################
" # extra hiligjht #
" ##################
hi TrailingWhitespace guifg=#FFFFFF guibg=DarkRed ctermfg=white ctermbg=red
autocmd BufWinEnter * match TrailingWhitespace /\s\+$/
autocmd InsertLeave * match TrailingWhitespace /\s\+$/
autocmd InsertEnter * match TrailingWhitespace /\s\+\%#\@<!$/
" #################
" # Abbreviations #
" #################
" source ~/.vim/abbr.vim
" GUI {{{
" Mac OS X {{{
if has("gui_macvim")
  set guioptions=aAce
  " Menu remapping
  macmenu &File.New\ Tab key=<D-S-t>
  set guifont=Ubuntu\ Mono\ derivative\ Powerline:h18
endif
" }}}
" Windows platform {{{
if has('win32') || has('win64')
  set guifont=Consolas:h12
  "set guifont=DejaVu\ Sans\ Mono:h12
  set guifont=Meslo_LG_M_for_Powerline:h12:cANSI
endif
" }}}
" Linux {{{
if has("x11")
  set guioptions=a
  set guifont=Ubuntu\ Mono\ 18
"  set guifont=Ubuntu\ Mono\ derivative\ Powerline\ 14
endif
"}}}
" }}}

" #############
" # functions #
" #############
" Show syntax highlighting groups for word under cursor
nmap <C-S-P> :call <SID>SynStack()<CR>
function! <SID>SynStack()
  if !exists("*synstack")
    return
  endif
  echo map(synstack(line('.'), col('.')), 'synIDattr(v:val, "name")')
endfunc
"nmap <Leader>hex  <Plug>HexHighlightToggle
"nmap <Leader>hexr <Plug>HexHighlightRefresh
"nmap <Leader>hexc <Plug>RefreshColorScheme

"set statusline=%<%f\ %h%m%r%=%-14.(%l,%c%V%)\ %P
"set statusline+=%#warningmsg#
"set statusline+=%{SyntasticStatuslineFlag()}
"set statusline+=%*

" Airline {{{
  let g:airline_powerline_fonts = 1
  let g:airline#extensions#tabline#enabled = 1
  "let g:airline_theme='material'
" }}}

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0
"
let g:indent_guides_enable_on_vim_startup = 1
"
" fzf and ripgrep
map  <Leader>p :Files<CR>
nmap <Leader>; :Buffers<CR>
" use ripgrep for finding text
map <Leader>f :Find<space>
command! -bang -nargs=* Find call fzf#vim#grep( 'rg --column --line-number --no-heading --fixed-strings --ignore-case --hidden --follow --color "always" '.shellescape(<q-args>), 1, <bang>0)

" saving search results into a buffer
command! -nargs=? Filter let @a='' | execute 'g/<args>/y A' | new | setlocal bt=nofile | put! a
nnoremap <silent> <F3> :redir @a<CR>:g//<CR>:redir END<CR>:new<CR>:put! a<CR>
nnoremap <silent> <F4> :redir >>matches.tmp<CR>:g//<CR>:redir END<CR>:new matches.tmp<CR>

" toggle gundo
nnoremap <leader>u :GundoToggle<CR>

" some useful notes
"
" write each line in a separated file  g/./execute '.w '.line('.').'.txt'
"
" v is equal to g!
"

" dvb
" Vim makes a distinction between inclusive and exclusive motion. v toggles
" the "inclusiveness" or "exclusiveness" of a motion. For an example of
" toggling the opposite direction (inclusive => exclusive), try it with e:

"dve
" See :help inclusive for an explication.
" If you're already on the last character of a word (Maybe you got there by
" using e or E), dve will leave a nasty extra space. In that case you can
" (ab)use forced exclusivity to delete the word by using dvge.


"
" scratch
"let g:scratch_no_mappings = 1
let g:scratch_filetype = "markdown"
let g:scratch_persistence_file = $HOME . '/OneDrive/Data/profiles/vim_scratch.md'
nmap gs <plug>(scratch-insert-reuse)

" ############
" # Mappings #
" ############
set ttimeout
"set timeoutlen=500 " be quick

" Allow saving of files as sudo when I forgot to start vim using sudo.
cnoremap sudow w !sudo tee % >/dev/null
" space open/closes folds
nnoremap <space> za

" search / replace
nnoremap  / /\v
vnoremap  / /\v
" Replace all is aliased to S. (was delete line and go insert mode)
nnoremap S :%s//g<Left><Left>

" Some emacs inspirated binding
nnoremap <M-q> gqap
inoremap <M-q> <C-O>gqap
" just-one-space 【Alt+Space] :s/\s\+/ /g
" delete-horizontal-space 【Alt+\] (re-search-backward "[^ \t\r\n]" nil t) (re-search-forward "[ \t\r\n]+" nil t) (replace-match "" nil nil))))))
" delete-blank-lines 【Ctrl+x Ctrl+o]
nmap <M-\> dt<space>

function! HungryBackspaceWrapper()
  let column = col('.')
  if column == 1
    return "\<Esc>kJxi"
  elseif column >= 2 && getline('.')[column - 2] =~ '\S'
    return "\<BS>"
  else
    return "\<Esc>d?\\S?e1\<CR>i"
  endif
endfunction
"inoremap <silent> <BS> <c-r>=HungryBackSpaceWrapper()<CR>

" Remap for destroying trailing whitespace cleanly but save cursor position
:nnoremap <Leader>w :let _save_pos=getpos(".") <Bar>
    \ :let _s=@/ <Bar>
    \ :%s/\s\+$//e <Bar>
    \ :let @/=_s <Bar>
    \ :nohl <Bar>
    \ :unlet _s<Bar>
    \ :call setpos('.', _save_pos)<Bar>
    \ :unlet _save_pos<CR><CR>
" sdlds;lk;dsf;lkdf;lkfd
" see janus\vim\core\before\plugin\mappings.vim for cool mappings ideas
inoremap jj <ESC>
inoremap <M-BS> <C-W>
inoremap <C-S-BS> <C-o>dd
inoremap <C-K> <Esc>lDa
inoremap <C-U> <Esc>d0xi
inoremap <C-X><C-S> <Esc>:w<CR>a
inoremap <C-Y> <Esc>Pa
inoremap <C-a> <C-o>^
inoremap <C-b> <Left>
inoremap <C-d> <Del>
inoremap <C-e> <End>
inoremap <C-f> <Right>
inoremap <C-h> <BS>
inoremap <C-k> <C-o>D
inoremap <silent> <C-Y> <C-R>"
inoremap <M-S-b> <Esc>Bi
inoremap <M-S-f> <Esc>lWi
inoremap <M-b> <S-Left>
"inoremap <M-b> <Esc>bi
inoremap <M-d> <C-O>de
inoremap <M-f> <S-Right>
"inoremap <M-f> <ESC>lwi
inoremap <M-x> <Esc>:

if has("gui_running")
  map  <silent> <S-Insert> "+p
  imap <silent> <S-Insert> <Esc>"+pa
  " SHIFT-Del are Cut
  vnoremap <S-Del>    "+x
  map      <S-Insert> "+gP
  cmap     <S-Insert> <C-R>+
  vnoremap <C-Insert> "+y
endif
"
" plugin - nerdtree
map <F1> :NERDTreeToggle<CR>
" plugin - textmanip
xmap <M-d> <Plug>(textmanip-duplicate-down)
nmap <M-D> <Plug>(textmanip-duplicate-up)
nmap <M-Up>   <Plug>(textmanip-move-up)
nmap <M-Down> <Plug>(textmanip-move-down)
xmap <C-j> <Plug>(textmanip-move-down)
xmap <C-k> <Plug>(textmanip-move-up)
xmap <C-h> <Plug>(textmanip-move-left)
xmap <C-l> <Plug>(textmanip-move-right)
" tabs - I like tabs!
"
nnoremap <Leader>te :tabedit<space>
nnoremap <Leader>tn :tabnew<space>
" edit vimrc/zshrc and load vimrc bindings
nnoremap <leader>ev :vsp $MYVIMRC<CR>
nnoremap <leader>ez :vsp ~/.zshrc<CR>
nnoremap <leader>sv :source $MYVIMRC<CR>

"imap <D-r>       : w !ruby %<cr>
"nmap <D-R>       : w<esc>                  : r!ruby %<cr>
"vmap <D-r> <C-u> : ! ruby -e "<cword>"<cr>
"vmap <D-R> <C-u> : r!ruby -e "<cword>"<cr>

"
" Tabular
"
nmap <leader>a= :Tabularize /=<CR>
vmap <leader>a= :Tabularize /=<CR>
nmap <leader>a: :Tabularize /:\zs<CR>
vmap <leader>a: :Tabularize /:\zs<CR>
