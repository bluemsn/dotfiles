" If filetypes were already loaded, don't continue
if exists("did_load_filetypes")
	finish
endif

" Detect some Markdown fileytypes
augroup filetypedetect
	au! BufRead,BufNewFile *.md setfiletype markdown
augroup END

augroup filetypedetect
	au! BufRead,BufNewFile *.mdown setfiletype markdown
augroup END

augroup filetypedetect
	au! BufRead,BufNewFile *.markdown setfiletype markdown
augroup END
