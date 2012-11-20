" Add Markdown syntax
if exists("did_load_filetypes")
	finish
endif
augroup filetypedetect
	au! BufRead,BufNewFile *.md setfiletype markdown
"   au! BufRead,BufNewFile *.mdown setfiletype markdown
"   au! BufRead,BufNewFile *.markdown setfiletype markdown
augroup END
