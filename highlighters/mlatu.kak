# mlatu

provide-module -override mlatu %{
	add-highlighter shared/mlatu regions
	add-highlighter shared/mlatu/other default-region group

	# comment
	add-highlighter shared/mlatu/comment region '#' '$' group
	add-highlighter shared/mlatu/comment/ fill comment

	# primitives and statements
	add-highlighter shared/mlatu/other/ regex "[\?=.()]" 0:keyword
	add-highlighter shared/mlatu/other/ regex "[\*]"     0:operator
}

hook global BufCreate .*\.(mlt) %{ set-option buffer filetype mlatu }
hook global WinSetOption filetype=mlatu %{ require-module mlatu }

hook -group mlatu-highlight global WinSetOption filetype=mlatu %{
	add-highlighter window/mlatu ref mlatu
	hook -once -always window WinSetOption filetype=.* %{ remove-highlighter window/mlatu }
}

# comment token
hook global BufSetOption filetype=mlatu %{
	set-option buffer comment_line '#'
	set-option buffer comment_block_begin '#'
	set-option buffer comment_block_end ''
}

