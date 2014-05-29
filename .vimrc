" All system-wide defaults are set in $VIMRUNTIME/debian.vim (usually just
" /usr/share/vim/vimcurrent/debian.vim) and sourced by the call to :runtime
" you can find below.  If you wish to change any of those settings, you should
" do it in this file (/etc/vim/vimrc), since debian.vim will be overwritten
" everytime an upgrade of the vim packages is performed.  It is recommended to
" make changes after sourcing debian.vim since it alters the value of the
" 'compatible' option.

" This line should not be removed as it ensures that various options are
" properly set to work with the Vim-related packages available in Debian.
runtime! debian.vim

" Uncomment the next line to make Vim more Vi-compatible
" NOTE: debian.vim sets 'nocompatible'.  Setting 'compatible' changes numerous
" options, so any other options should be set AFTER setting 'compatible'.
"set compatible

"" pathogen
call pathogen#infect()

" Vim5 and later versions support syntax highlighting. Uncommenting the next
" line enables syntax highlighting by default.
if has("syntax")
  syntax on
endif

" If using a dark background within the editing area and syntax highlighting
" turn on this option as well
set background=dark

" Uncomment the following to have Vim jump to the last position when
" reopening a file
if has("autocmd")
  au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif
endif

" Uncomment the following to have Vim load indentation rules and plugins
" according to the detected filetype.
if has("autocmd")
  filetype plugin indent on
endif

" The following are commented out as they cause vim to behave a lot
" differently from regular Vi. They are highly recommended though.
set showcmd		" Show (partial) command in status line.
set showmatch		" Show matching brackets.
set ignorecase		" Do case insensitive matching
set smartcase		" Do smart case matching
set incsearch		" Incremental search
"set autowrite		" Automatically save before commands like :next and :make
"set hidden             " Hide buffers when they are abandoned
"set mouse=a		" Enable mouse usage (all modes)

" Source a global configuration file if available
if filereadable("/etc/vim/vimrc.local")
  source /etc/vim/vimrc.local
endif


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

set backspace=indent,eol,start

set nobackup		" do not keep a backup file, use versions instead
set history=50		" keep 50 lines of command line history
set ruler		" show the cursor position all the time
set showcmd		" display incomplete commands
set incsearch		" do incremental searching
set number		" line number
"colorscheme jammy	" theme
"let g:solarized_termcolors=256
colorscheme solarized
set autoindent		" automatic indenting
set smartindent		" smart box
" default to 79 column of text
set tw=79

if has("multi_byte")
	if &termencoding == ""
		let &termencoding = &encoding
	endif
	set encoding=utf-8
	setglobal fileencoding=utf-8 
	set fileencodings=utf-8,latin1
endif

"highlight the column after tw
if exists('+colorcolumn')
	set colorcolumn=+1
endif

" global white space errors
highlight ExtraWhitespace ctermbg=red
map <C-G> :match ExtraWhitespace /^\s \s*\<Bar>\s\+$/<CR>
imap <C-G> :match ExtraWhitespace /^\s \s*\<Bar>\s\+$/<CR>

nnoremap <F2> :set invpaste paste?<CR>
set pastetoggle=<F2>
set showmode

"save undo across runs
set undofile
set undodir=~/.vim/undo
set undolevels=1000
set undoreload=10000
