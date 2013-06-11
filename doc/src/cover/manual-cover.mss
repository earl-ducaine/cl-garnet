@device(postscript)
@make(report)
@define(high,Hyphenation Off,HyphenBreak Off,Script +0.8in,
                        TabExport)
@style(spacing 1.8)
@style(font = TimesRoman, size = 42)
@PageHeading[]

@begin(format, rotate=left, size=36)
@graphic(Postscript="color-logo.PS", boundingbox=file,
           magnify=0.65)@high[Reference Manual V3.0]
@blankspace(2 in)
@begin(center, size 14)
(This page and the next one are intended as covers for binders that
that have slots for inserts.  These two pages are @i(color)
postscript, in case you have have a color printer.)

(This page can be cut and used on the @i(edge) of the binder.
The next page is for the cover.)
@end(center)
@end(format)


@newpage()
@center[@graphic(Postscript="color-logo.PS", boundingbox=file)]
@begin(Center)
@blue[Garnet Reference Manual

Version 3.0]
@end(Center)


@blankspace(1 inch)

@begin(format, size 24, Columns = 2, boxed)
@tabset(0.125 in)
@\@red[G]enerating an
@\@red[A]malgam of
@\@red[R]ealtime,
@\@red[N]ovel
@\@red[E]ditors and
@\@red[T]oolkits 
@newcolumn()
@graphic(Postscript="color-cmulogo.ps",
         boundingbox=file)
@begin(format, size 20)
School of Computer Science
@end(format)
@end(format)

