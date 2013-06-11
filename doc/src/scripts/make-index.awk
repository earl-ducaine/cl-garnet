# Eliminate all the dummy pages in the middle of the index.ps file, just keep
# the global index pages.

/^%!PS-Adobe/,/^%%EndProlog/ {print}
/last-page-before-index\)/	{pre_on = 1}
/^%%Page: /	{if (pre_on) on = 1; if (on) print $1, $2, ++page; next}
/^%%Pages: /	{if (on) print $1, page; next}
		{if (on) print}
