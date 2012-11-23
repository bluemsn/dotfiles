" If filetypes were already loaded, don't continue
if exists("did_load_filetypes")
	finish
endif

" Detect Markdown files
au! BufRead,BufNewFile *.md setfiletype markdown
au! BufRead,BufNewFile *.mdown setfiletype markdown
au! BufRead,BufNewFile *.markdown setfiletype markdown
