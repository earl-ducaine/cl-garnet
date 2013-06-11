/%%Page: i/			{++toc}
/^\(List of Figures\)SH$/	{if (toc) --toc}
/^%%Pages: [0-9]/		{print substr(FILENAME,1, index(FILENAME,"/")-1), toc, $2}
