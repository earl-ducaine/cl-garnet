@device(postscript)
@Make(gmanual)
@disable(figurecontents)
@style(TopMargin .75 inch, BottomMargin .75 inch, PageNumber <@ii>)
@LibraryFile(Garnet)
@String(TitleString = "Garnet Toolkit")
@Use(Bibliography = "garnet.bib")

@begin(TitlePage)
@begin(TitleBox, Fixed .25 inches)
@Comment{** Logo in the upper left corner of front page **}
@graphic(Postscript="refman/logo.ps", boundingbox=file, magnify=0.75)
@blankspace(0.4 inch)
@Bg(The Garnet Reference Manuals
Revised for Version 3.0)
@blankspace(0.2 line)
@b(Brad A. Myers, Dario Giuse, Brad Vander Zanden,
Andrew Mickish, David Kosbie, James A. Landay,
Richard McDaniel, Rajan Parthasarathy, Matthew Goldberg,
Roger B. Dannenberg, Philippe Marchal, Ed Pervin)
@BlankSpace(0.2 line)
@value[date]
CMU-CS-90-117-R5
@end(TitleBox)

@Center(School of Computer Science
Carnegie Mellon University
Pittsburgh, PA 15213)
@Blankspace(0.4 inches)

@begin(Text, spacing=1.1)
@Center[This is a revision which supercedes all previous versions:
CMU-CS-90-117-R4 (October 1993), CMU-CS-90-117-R3 (November 1992),
CMU-CS-90-117-R2 (May 1992), CMU-CS-90-117-R (June 1991),
CMU-CS-90-117 (March, 1990), CMU-CS-89-196 (November, 1989),
and all change documents.]
@Blankspace(0.4 inches)
@include(creditetc.mss)
@end(Text)
@end(TitlePage)

@newpage
@PageHeading(odd, immediate, right = "@value(Page)")
@PageHeading(even, immediate, left = "@value(Page)")
@blankspace(3 inches)
@center[@bg(Abstract)]
@begin(Text, spacing=1.1)
The Garnet User Interface Development Environment contains a comprehensive
set of tools that make it significantly easier to design and implement
highly-interactive, graphical, direct manipulation user interfaces.  
Garnet provides a high level of support, while still being
Look-and-Feel independent and providing the applications with tremendous
flexibility.  The Garnet tools are organized into two layers.
The toolkit layer provides an object-oriented, constraint-based
graphical system that allows properties of graphical objects to be
specified in a simple, declarative manner, and then maintained automatically
by the system.  The dynamic, interactive behavior of the objects can be
specified separately by attaching high-level ``interactor'' objects to the
graphics.  This layer includes two complete widget sets, one with the
Garnet look and feel, and the other with a Motif look and feel.
The higher layer of Garnet includes three tools at this time.  The first
is an interface builder tool, called Gilt, which allows dialog boxes and other
windows to be created.  The second is C32, which is a spreadsheet interface
for editing constraints among objects.
Last is Lapidary, which allows the user interface designer to
draw pictures of @i(all) graphical aspects of the user interface.  Other tools
are currently in production, such as Jade (which creates
dialog boxes from a specification).

The Garnet toolkit layer software is available for unlimited
distribution by anonymous FTP.  Garnet uses 
Common Lisp and the X window manager, and is therefore portable across a
wide variety of platforms.  This document contains an overview, tutorial
and a full set of reference manuals for the Garnet System.
@End(Text)

@Blankspace(1 inch)
@b(Keywords:)  User Interface Development Environments, User
Interface Management Systems, Constraints, Interface Builders,
Object-Oriented Programming, Direct Manipulation, Input/Output, Garnet.


@newpage
@Heading(Overall Table of Contents)
@blankspace(0.25 inch)
@Begin(Comment)
**********************************************************************
Strings for all the sections of this manual, used in a number of places in
this part of the manual.  These should be filled in with the correct page
numbers where the sections start.
**********************************************************************
@End(Comment)

@include(pagenumbers.mss)

@Begin(Comment)
**********************************************************************
@End(Comment)
@begin(Format)
@tabclear()
@tabset(0.25inch)
@b{Overview of the Garnet System@>@Value(Overview)}
@\@i{Introduction to the toolkit and overview of this technical report.}

@b{Pictures of Applications Using Garnet@>@Value(Apps)}
@\@i{Some flashy color pictures sent in by Garnet users.}

@b{An On-line Tour through Garnet@>@Value(Tour)}
@\@i{A guided tour of some of Garnet's features.}

@b{The Garnet Tutorial@>@Value(Tutorial)}
@\@i{A tutorial to introduce the basic Garnet features.}

@b{KR Reference Manual; Constraint-Based Knowledge Representation@>@Value(KR)}
@\@i{The object and constraint system in Garnet.}

@b{Opal Reference Manual; The Garnet Graphical Object System@>@Value(Opal)}
@\@i{Support for graphical output.}

@b{Interactors Reference Manual; Encapsulating Mouse and Keyboard Behaviors@>@Value(Inter)}
@\@i{Support for input from the user.}

@b{Aggregadgets, Aggrelists, & Aggregraphs Reference Manual@>@Value(aggregadgets)}
@\@i{Convenient way to create composite objects.  AggreLists and
@\Aggregraphs lay out elements automatically.}

@b{Garnet Gadgets Reference Manual@>@Value(gadgets)}
@\@i{A set of pre-defined interaction techniques.}

@b{Debugging Tools for Garnet; Reference Manual@>@Value(Debug)}
@\@i{Tools to help debug Garnet code.}

@b{Garnet Demos@>@Value(demos)}
@\@i{Descriptions of the demonstration programs provided with Garnet.}

@b{A Sample Garnet Program@>@Value(sampleprog)}
@\@i{Actual code for a simple graphical editor.}

@b{Gilt Reference Manual: A Simple Interface Builder for Garnet@>@Value(gilt)}
@\@i{A high-level tool for laying out gadgets using the mouse.}

@b{C32 Reference Manual: A Spreadsheet Interface to Constraints@>@Value(c32)}
@\@i{C32 displays the slots of an object in a scrolling menu, allowing the
@\user to type in values directly or define constraints among objects.}

@b{Lapidary Reference Manual@>@Value(Lapidary)}
@\@i{Lapidary allows application-specific graphical objects to be created interactively.}

@b{How to Make Garnet Programs Run Faster@>@Value(hints)}
@\@i{Some hints on making Garnet applications run faster.}

@b{Global Index@>@Value(globalindex)}
@\@i{An index to all the names and procedures in the entire Garnet Toolkit.}
@end(Format)

@begin(comment)
@include(overview/overview.mss)
@include(tour/tour.mss)
@include(tutorial/tutorial.mss)
@include(kr/kr-manual.mss)
@include(opal/opal-manual.mss)
@include(inter/inter-manual.mss)
@include(aggregadgets/aggregadgets-manual.mss)
@include(gadgets/gadgets-manual.mss)
@include(debug/debug-manual.mss)
@include(demos/demoguide.mss)
@include(sampleprog/sampleprog.mss)
@include(gilt/gilt-manual.mss)
@include(c32/c32-manual.mss)
@include(lapidary/lapidary-manual.mss)
@include(hints/hints.mss)
@end(comment)

@comment(set up for global index)
@set(page = GlobalIndex)
