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
set number		" line number
set autoindent		" automatic indenting
set smartindent		" smart box

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

"" editing options
set backspace=indent,eol,start
set tw=79

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

" airline always active
set laststatus=2

" toggles
nmap <F9> :TagbarToggle<CR>
nmap <F10> :NERDTreeToggle<CR>
