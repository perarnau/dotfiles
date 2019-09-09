"" pathogen
execute pathogen#infect()
syntax on
filetype plugin indent on


" Uncomment the following to have Vim jump to the last position when
" reopening a file
if has("autocmd")
	au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif
endif

set showcmd		" Show (partial) command in status line.
set showmatch		" Show matching brackets.
set ignorecase		" Do case insensitive matching
set smartcase		" Do smart case matching
set incsearch		" Incremental search
set nobackup		" do not keep a backup file, use versions instead
set history=50		" keep 50 lines of command line history
set ruler		" show the cursor position all the time
set autoindent		" automatic indenting
set smartindent		" smart box

" absolute numbers in insert, relative in command
set number
autocmd BufEnter,FocusGained,InsertLeave * set relativenumber
autocmd BufLeave,FocusLost,InsertEnter   * set norelativenumber


if has('spell')
	set spell

	if has('eval')
		" Rotate through languages of spelling checker.
		let g:myLangIdx = 0
		let g:myLangCodes = [ "en_us", "fr" ]
		let g:myLangs = [ "language:", "langue :" ]
		function! MySpellLang()
			let g:myLangIdx = g:myLangIdx + 1
			if g:myLangIdx >= len(g:myLangs) | let g:myLangIdx = 0 | endif
			exe "setlocal spell spelllang=" . g:myLangCodes[g:myLangIdx]
			echo g:myLangs[g:myLangIdx] g:myLangCodes[g:myLangIdx]
		endf

		map <F7> :call MySpellLang()<CR>
		imap <F7> <C-o> :call MySpellLang()<CR>
	endif
	map <F8> :set spell!<CR>
endif

set background=dark
colorscheme solarized
"highlight the column after tw
if exists('+colorcolumn')
	set colorcolumn=+1
endif
"" vim using the wrong terminfo for some of its colors
set t_Cs=

"" editing options
set backspace=indent,eol,start
set tw=79

"" remove smartindent for tex files
au BufEnter *.tex set nosmartindent

"" mutt specific
autocmd Filetype mail setlocal tw=72 fo=wtqc noundofile

if has("multi_byte")
	if &termencoding == ""
		let &termencoding = &encoding
	endif
	set encoding=utf-8
	setglobal fileencoding=utf-8
	set fileencodings=utf-8,latin1
endif


" global white space errors
highlight ExtraWhitespace ctermbg=red
map <C-G> :match ExtraWhitespace /^\s \s*\<Bar>\s\+$/<CR>
imap <C-G> :match ExtraWhitespace /^\s \s*\<Bar>\s\+$/<CR>

" paste mode
nnoremap <F2> :set invpaste paste?<CR>
set pastetoggle=<F2>
set showmode

"save undo across runs
set undofile
set undodir=~/.vim/undo
set undolevels=1000
set undoreload=10000
au BufEnter /tmp/* setlocal noundofile


" airline always active
set laststatus=2
let g:airline_theme = 'solarized'

" some syntastic config
let g:syntastic_python_checkers=['flake8']
let g:syntastic_c_checkers=['make']
let g:syntastic_rust_checkers=['cargo']

" activate rust autocompletion
let g:racer_cmd = "/usr/bin/racer"

" toggles
nmap <F9> :TagbarToggle<CR>
nmap <F10> :NERDTreeToggle<CR>
