@Make(manual)
@disable(figurecontents)
@LibraryFile(Garnet)
@String(TitleString = "Garnet Toolkit")
@Use(Bibliography = "garnet.bib")

@begin(TitlePage)
@begin(TitleBox)
@comment(This is to make sure it does not affect the previous page,
	e.g. putting it at the top of the file won't cut it:)
@blankspace(0.6 inch)
@Bg(Overview of the Garnet System)

@b(Brad A. Myers
Andrew Mickish)
@BlankSpace(0.3 line)
@value(date)
@end(TitleBox)
@blankspace(0.5 inch)
@b(Abstract)

@begin(Text, spacing=1.1)
This article provides an overview of the Garnet system, and a guide to
this set of manuals.
@blankspace(0.7 inch)
@include(creditetc.mss)
@end(Text)
@end(TitlePage)


@comment[ The @PageHeading command is needed because this manual does not use
          @Chapter, which normally performs this command. ]
@PageHeading(Even,Immediate, left "@ux[Page @value(page)@hsp(0.15 in)@Value(TitleString) @>]")

@include(pagenumbers.mss)
@set(page=overview-first-page)
@Section(Introduction)
The Garnet research project in the School of Computer Science at Carnegie
Mellon University is creating a comprehensive set of tools which make it
significantly easier to create graphical, highly-interactive user
interfaces.  The lower levels of Garnet are called the ``Garnet Toolkit,''
and these provide mechanisms that allow programmers to code user interfaces
much more easily.  The higher level tools allow both programmers and
non-programmers to create user interfaces by just drawing pictures of what
the interface should look like.  
Garnet stands for @u(G)enerating an
@u(A)malgam of @u(R)eal-time, @u(N)ovel @u(E)ditors and @u(T)oolkits.

At the time of this writing, the Garnet toolkit is in use by about
80 projects throughout the world.
This document contains an overview, tutorial, and a full set of reference
manuals for the Garnet System.  

@b[This manual describes version 3.0 of Garnet.  It replaces all
previous versions and the change documents for versions 1.3, 1.4, 2.0, 2.1,
and 2.2.]

Garnet is written in Common Lisp and can be used with either Unix systems
running X windows or on the Mac.  Therefore,
Garnet is quite portable to various environments.
It works in virtually any Common Lisp environment, including Allegro (Franz),
Lucid, CMU, Harlequin, CLISP, AKCL, and Macintosh Common Lisps.
The computers we
know about it running on include Sun, DEC, HP, Apollo, IBM RT, IBM
6000, TI, SGI, NeXTs running X/11, PC's running Linux, Macs,
and there may be others.  Currently,
Garnet supports X/11 R4 through R6 using the standard CLX interface.
Garnet does @i(not) use the standard Common Lisp Object System (CLOS) or
any X toolkit (such as Xtk or CLIM).

Garnet has also been implemented using the native Macintosh QuickDraw and
operating system.
To run the Macintosh version of Garnet, you need to have System 7.0 or
later, Macintosh Common Lisp (MCL) version 2.0.1 or later, and at
least 8Mb of RAM.  The system takes about 10 megabytes of disk space
on a Mac, not including the documentation (which takes an additional 8
megabytes).  We find that performance of Garnet on MCL is acceptable
on Quadra's, and fine on a Quadra 840 A/V.  It is really too slow on a
Mac II.  To do anything useful, you probably need 12mb of memory.  A
PowerPC Mac does not work well for Lisp (see discussion on
@pr[comp.lang.lisp.mcl]).

More details about Garnet are available in the Garnet FAQ:
@programexample[ftp://a.gp.cs.cmu.edu/usr/garnet/garnet/FAQ]
which is posted periodically.

This document is a technical reference manual for the entire Garnet system.
There have been many conference and journal papers about Garnet
(see section @ref(articles) for a complete bibliography).
The best overview of Garnet is @Cite(GarnetIEEE).  Section @ref(articles)
includes instructions for retrieving some Garnet papers via FTP.

@index[amulet]
A new project named Amulet is actively developing a system with features
similar to those in Garnet, but implemented in C++.  Section @ref[future]
discusses how to get more information about the Amulet project.


@Section(Garnet Bulletin Board)
@index[bugs (reporting)]
@index(garnet-users)
@index(bboard)

There is an international bboard for Garnet users named
@pr(comp.windows.garnet).  Topics discussed on this bboard include user
questions and software releases.  There is also a mailing list called
@pr(garnet-users@@cs.cmu.edu) which carries exactly the same messages as the
bboard.  If you cannot read the bboard in your area, please send mail
to @pr(garnet-users-request@@cs.cmu.edu) to get on the mailing list.

You can report bugs to @pr(garnet-bugs@@cs.cmu.edu) which is read only by
the Garnet developers.


 
@Section(Important Features of Garnet)
@Index(Features)
Garnet has been designed as part of a research project, so it contains a
number of novel and unique features.  Some of these are:
@Begin(Itemize)
The Lapidary tool is the only interactive tool that allows
application-specific graphics and new widgets to be created without
programming.

The Garnet Toolkit is designed to support the @i(entire) user interface of an
application; both the contents of the application window and its menus and
dialog boxes.  For example, Garnet directly supports selecting graphical
objects with the mouse, moving them around, and changing their size.

It is @i(look-and-feel independent).  Garnet allows the programmer to
define a new graphical style, and use that throughout a system.  Alternatively,
a pre-defined or standard style can be used, if desired.

It uses a @i(prototype-instance) object model instead of the more conventional
class-instance model, so that the programmer can create a @i(prototype) of
a part of the interface, and then create instances of it.  If the prototype
is changed, then the instances are updated automatically.
Garnet's custom object system is called KR.  Garnet does not use CLOS.

@i(Constraints) are integrated with the object system, so any slot
(also called an ``instance variable'') of any object can contain a
formula rather than a value.  When a value that the formula references
changes, the formula is re-evaluated automatically.  Constraints can be
used to keep lines attached to boxes, labels centered within
rectangles, etc. (see Figure @Ref(toolkit-SampleFig)).  Constraints can also be
used to keep application-specific values connected with the values of
graphical objects, menus, scroll bars or gauges in the user interface.

Objects are @i(automatically refreshed) when they change.  Pictures are
displayed by creating graphical objects which are retained.  If a slot of
an object is changed, the system automatically redraws the object and any
other objects that overlap it.  Also, the system handles window refresh
requests from X and the Mac.

The programmer specifies the handling of
input from the user at a high level using abstract
@i(interactor) objects.  Typical user interface behaviors
are encapsulated into a few different types of interactors, and the
programmer need only supply a few parameters to get objects to respond to
the mouse and keyboard in sophisticated ways.

There is built-in support for laying out objects in @i(rows and columns),
for example, for menus, or in @i(graphs or trees), for example, to
show a directory structure or a dependency graph.

Two complete sets of @i(gadgets) (also called widgets or interaction
techniques) are provided to help the programmer get started.
These include menus, buttons, scroll bars, sliders, circular gauges, graphic
selection, scrollable windows, and arrows.
One set has the Garnet look and feel, and the other has
the Motif look and feel.  The Motif set is implemented entirely in
Lisp on top of Garnet, to provide maximum flexibility.  Note: There are no
Macintosh look-and-feel gadgets.  When you use Garnet on the Macintosh,
the gadgets will @u(not) look like standard Macintosh widgets.

Garnet is designed to be @i(efficient).  Even though Garnet handles many
aspects of the interface automatically, an important goal is that it
execute quickly and not take too much memory.  We are always
working to improve the efficiency, but Garnet can currently handle dozens
of constraints attached to objects that are being dragged with the mouse.

Garnet will automatically produce PostScript for any picture on the
screen, so the programmer does not have to worry about printing.

Gesture recognition (such as drawing an "X" over an object to delete
it) is supported, so designers can explore innovative 
user interface concepts.
@End(Itemize)

@begin(group)
@Section(Coverage)
@Index(Coverage)
Garnet is designed to handle interfaces containing a number
of graphical objects which the user can manipulate with the mouse and
keyboard.

Garnet is suitable for applications of the following kinds:
@end(group)
@begin(Itemize)
User interfaces for expert systems and other AI applications.

Box and arrow diagram editors like Apple Macintosh MacProject (which helps
with project management).

Graphical Programming Languages where computer
programs can be constructed using icons and other pictures (a common
example is a flowchart).

Tree and graph editing programs, including editors for semantic
networks, neural networks, and state transition diagrams.

Conventional drawing programs such as Apple Macintosh MacDraw.

Simulation and process monitoring programs where the user interface shows the
status of the simulation or process being monitored, and allows the user to
manipulate it.

User interface construction tools (Garnet was implemented using itself).

Some forms of CAD/CAM programs.

Icon manipulation programs like the Macintosh Finder (which allows users to
manipulate files).

Board game user interfaces, such as Chess.
@end(Itemize)

Figure @Ref(toolkit-SampleFig) shows a simple Garnet application that
was created from start to finish (including debugging) in three hours.
The code for 
this application is shown in the sample program manual, which begins
on page @Value(sampleprog-first-page).

@Begin(Figure)
@bar()
@Center{@graphic(Postscript="overview/toolkitpic.PS", boundingbox=file)}
@Caption{A sample Garnet application.  The code for this application is
listed the "Sample Garnet Program" section of this manual, starting on
page @value(sampleprog-first-page).}
@Tag(toolkit-SampleFig)
@bar()
@End(Figure)

Other examples of applications created using Garnet appear in the picture
section of this manual, starting on page @value(apps).


@begin(group)
@Section(Running Garnet From /afs)

If you are running Garnet in X windows from CMU, or if you have access
to AFS, you can access Garnet directly on the @pr(/afs) servers.  We maintain
binaries of the official release version in machine- and
lisp-specific subdirectories of @pr(/afs/cs/project/garnet/).  If you are
at CMU, you can skip section @ref(retrieving) altogether, and just start lisp
and load Garnet with:

@programexample[(load "/afs/cs/project/garnet/garnet-loader.lisp")]

The CMU version of @pr(garnet-loader.lisp) will attempt to determine what
kind of machine and lisp you're using, and load the appropriate binaries
for you.  You will not have to supply or customize any pathnames.
@end(group)

@Section(How to Retrieve and Install Garnet)
@label(retrieving)
@Index(Getting Garnet)
@index(retrieving Garnet)
@index(installing Garnet)
@index(compiling Garnet)
@Index(Address)
@Index(License)
@Index(FTP Instructions)

Garnet is available for free by anonymous FTP.  There are different
instructions for obtaining the software depending on whether it will
be installed on a Mac or a Unix system (the code is the same, but the
packaging is different).

@Subsection(Installation on a Mac)
@blankspace(1 line)

@b[Retrieving the Stuffit Files:]

Garnet 3.0 is available in @pr(Stuffit) files that include the
sources, the library files, the binary files compiled for
Macintosh Common Lisp 2.0.1, and documentation.  To download the Garnet
collection
that includes MCL binaries, use FTP or Fetch (a Mac file transfer utility)
to connect to @pr[a.gp.cs.cmu.edu] (@pr[128.2.242.7]) and login as
@pr[``anonymous''] with your e-mail address as the password.  Change
to "binary" mode for FTP, or stay in "automatic" mode for Fetch, and
download the @pr(Stuffit) archive
@programexample[/usr/garnet/garnet/mac.sit]
Alternatively, you can get the BinHex version in text mode by
retrieving
@programexample[/usr/garnet/garnet/mac.sit.hqx]
If you are using Fetch, it will automatically convert the BinHex file into
a binary @pr(.sit) file after it is installed on your Mac.  If you used FTP to
get the @pr(.hqx) file, you will need to BinHex4 Decode the file.
You should also retrieve one version of the documentation file:
@begin(programexample)
/usr/garnet/garnet/doc.sit
/usr/garnet/garnet/doc.sit.hqx
@end(programexample)

If you do not have a version of @pr(Stuffit), you can also download the copy of
@pr(Stuffit_Expander) from the same directory to uncompress the Garnet archive.
The @pr(Stuffit) utility is a self-extracting archive that you only need to
double-click on to install on your Mac.  Be sure to use binary
transfer mode in FTP if you are retrieving @pr(StuffIt_Expander_.sea).

@begin(group)
@b[Unpacking the Stuffit Files:]

Once you have downloaded the @pr(.sit) or @pr(.sit.hqx) archives
(and installed the @pr[Stuffit_Expander], if necessary), launch the
@pr(Stuffit) utility.  Next, "Expand..." or "Open" the @pr(mac.sit) archive,
and choose a folder into which the uncompressed Garnet folder will be
expanded.  The instructions below assume you have installed the
uncompressed folder at the top-level of your hard drive, and that your
hard drive is named "Macintosh HD" (i.e., the uncompressed folder will
become "Macintosh HD:Garnet:").  It is a good idea to expand the @pr(doc.sit)
archive in the Garnet folder that was created by the first archive.
For further instructions about printing the documentation, consult the
README file in the doc folder.
@end(group)
@blankspace(1 line)

@b[Preparing MCL Before Loading Garnet:]

When using Garnet, you may need to increase the amount of memory that is
claimed by the Lisp application.  You can change the memory claimed by MCL by
selecting the MCL application in the Macintosh Finder and choosing "Get Info"
from the Finder's "File" menu.  Most Garnet applications will require that
MCL use at least 6Mb of RAM, and using at least 12Mb is recommended.
The default "Preferred size" for MCL is 3072K, so you will need to edit that
value to be upwards of 6000K.  You are only allowed to change this
information when the application is NOT running, and it should be done before
proceeding with the rest of these instructions.  Note: All Lisp images saved
from the MCL application will retain the new "Preferred size" value.

Before loading Garnet, you will need to compile several MCL library files
that are used by Garnet.  A compiler script for this procedure is provided in
the Garnet collection.  In the fresh MCL listener, load the file "Macintosh
HD:Garnet:compile-mcl-libraries.lisp" (replacing the hard drive prefix with
whatever is appropriate for your machine).  After the script is finished,
quit MCL and then launch MCL again.


@b[Loading Garnet:]

Using the MCL text editor, edit the file @pr(garnet-loader.lisp) from the new
Garnet folder (choose "File...", "Open..." from the MCL menubar to edit
a file).  Find the definition of the variable @pr(Your-Garnet-Pathname)
and set its value to the path of the new Garnet folder you created with
@pr(Stuffit).  All other subfolders of Garnet will be computed relative to this
pathname.  Save the new version of the @pr(garnet-loader.lisp) file.

In the fresh lisp listener, load "Macintosh HD:Garnet:garnet-loader.lisp"
(using whatever prefix is appropriate instead of "Macintosh HD:Garnet:").
Garnet will inform you as it loads each module, and will finally return with
a prompt.  At this point, Garnet is fully loaded and you are ready to try
the Tour or some demos as discussed later in this manual.



@Subsection(Installation on a Unix System)

When running on X windows, Garnet uses the CLX interface from Lisp to
X/11.  CLX should be supplied with every Lisp, and the following instructions
assume that CLX has been installed correctly on your system.  If you need help
with CLX, you need to contact your Lisp vendor.  We cannot help you
acquire, compile, or install CLX, sorry.


@b(Retrieving the TAR Files:)

The Garnet software is about 9 megabytes.  In order to make it easy to
copy the files over, we have created TAR files, so to use the
mechanism below requires double the storage area.  Therefore, you
first need to find a machine with enough room, and then create a
directory called @pr[garnet] wherever you want the system to be:
@begin(programexample)
	% mkdir garnet
@end(programexample)

@newpage
Then, cd to the @pr(garnet) directory.
@begin(programexample)
	% cd garnet
@end(programexample)
Now, ftp to @pr[a.gp.cs.cmu.edu (128.2.242.7)].  When asked to log in, 
use @pr[anonymous], and your name as the password.
@begin(programexample)
   % ftp a.gp.cs.cmu.edu
   Connected to A.GP.CS.CMU.EDU.
   220 A.GP.CS.CMU.EDU FTP server (Version 4.105 of 10-Jul-90 12:07) ready.
   Name (a.gp.cs.cmu.edu:bam): anonymous
   331 Guest login ok, send username@@node as password.
   Password:
   230 Filenames can not have '/..' in them.
@end(programexample)

Then change to the @pr[garnet] directory (note the double
@pr[garnet]'s) and use binary transfer mode:
@begin(programexample)
	ftp> cd /usr/garnet/garnet/
	ftp> bin
@end(programexample)

The files have all been combined into TAR format files for your convenience.
These will create the appropriate sub-directories automatically.  We
have both compressed and uncompressed versions.  For the regular
versions, do the following:
@begin(programexample)
	ftp> get src.tar
	ftp> get lib.tar
	ftp> get doc.tar
@end(programexample)

To get the compressed version, do the following:
@begin(programexample)
	ftp> get src.tar.Z
	ftp> get lib.tar.Z
	ftp> get doc.tar.Z
@end(programexample)

Now you can quit FTP:
@begin(programexample)
	ftp> quit
@end(programexample)


@b(Installing the Source Files:)

If you got the compressed versions, you will need to uncompress them:
@begin(programexample)
	% uncompress src.tar.Z
	% uncompress lib.tar.Z
	% uncompress doc.tar.Z
@end(programexample)

Now, for each tar file, you will need to "untar" it, to get all the original
files:
@begin(programexample)
	% tar -xvf src.tar
	% tar -xvf lib.tar
	% tar -xvf doc.tar
@end(programexample)

This will create subdirectories will all the sources in them.  At this point
you can delete the original tar files, which will free up a lot of space:
@begin(programexample)
	% rm *.tar
@end(programexample)

Now, copy the files @pr[garnet-loader.lisp, garnet-compiler.lisp, 
garnet-@|prepare-@|compile.lisp], and @pr[garnet-after-compile] from the
src directory into the @pr[garnet] directory:
@begin(programexample)
	% cp src/garnet-*  .
@end(programexample)


@b(Customizing the PathNames:)

The file @pr[garnet-loader.lisp] contains variables that should be set with
the pathnames of your Garnet directory and the location of CLX for your lisp.
You will now need to edit @pr[garnet-loader.lisp] in an editor, and set
these variables.  Comments in the file will direct you how
to do this.  At the top of the file are the two variables you will need to set:
@pr[Your-Garnet-Pathname] and @pr[Your-CLX-Pathname].  NOTE: If CLX is already
loaded in your lisp image, you do not need to set the CLX variable.


@newpage
@b(Compiling Garnet to Make Binary Files:)

Lisp requires very large address spaces.  We have found on many Unix
systems, that you need to expand the area that it is willing to give
to a process.  The following commands work in many systems.  Type
these commands to the C shell (csh).  You might want to also put these
commands into your @pr[.login] file.
@begin(programexample)
	% unlimit datasize
	% unlimit stacksize
@end(programexample)

Now, you will need to compile the Garnet source to make your own binaries.
This is achieved by loading the compiler scripts.  There is more information
on compiling in section @ref(compilinggarnet) below, and special
instructions for compiling Garnet in CLISP are in section @ref(clisp).
@begin(programexample)
        @i[;; Only LispWorks users need to do the next two commands.  See section @ref(lispworks).]
        lisp> #+lispworks (load "src/utils/lispworks-processes.lisp")
        lisp> #+lispworks (guarantee-processes)

	lisp> (load "garnet-prepare-compile")
	lisp> (load "garnet-loader")
	lisp> (load "garnet-compiler")
@end(programexample)

Now Garnet is all compiled and loaded, but a shell script still needs to be
executed to separate the binary files from the source files.  To set up
for the next time, it is best to quit lisp now, and
run the @pr(garnet-after-compile) shell script.
If your sources are not in a directory named @pr[garnet/src] or your binaries
should not be in a directory named @pr[garnet/bin], then you will need to
edit @pr[garnet-after-compile] to set the directories.  Also, if your
compiler produces binary files that do not have one of the following
extensions, then you need to edit the variable @pr[CompilerExtension] in
@pr[garnet-after-compile]: ".fasl", ".lbin", ".sbin", ".hbin", ".sparcf",
".afasl", or ".fas".  Otherwise, you
can just execute the file as it is supplied (NOTE: this is run from
the shell, not from Lisp).  You should be in the @pr(garnet) directory.
@begin(programexample)
	% csh garnet-after-compile
@end(programexample)

Now you can start lisp again, and load Garnet:
@begin(programexample)
	lisp> (load "garnet-loader")
@end(programexample)

Details about how to customize the loading of Garnet are provided in
section @ref(loading-garnet).

@Section(Directory Organization)
@Index(Directories)
@Index(src)
@Index(doc)
@Index(garnet-loader)
@Index(garnet-version-number)

All of the information about where various files of Garnet are stored is
in the file @Pr(garnet-loader.lisp).  This file also defines the Garnet
version number:
@begin(programexample)
* user::Garnet-Version-Number
"3.0"
@end(programexample)

You may need to edit the @pr[garnet-loader] file to
tell Garnet where all the files are.  Normally, there
will be a directory
called @pr(garnet) with sub-directories called @pr(src), @pr(lib) and
@pr(bin).  In the @pr(src) and @pr(bin) directions will be sub-directories
for all the parts of the Garnet system:
@begin(itemize, spread 0)
@pr(utils) - Utility files and functions.

@pr(kr) - KR object system.

@pr(gworld) - Mac routines for off-screen drawing (only used on the Mac)

@pr(gem) - Garnet's interface to machine-specific graphics routines (X and Mac)

@pr(opal) - Opal Graphics management system.

@pr(inter) - Interactors input handling.

@pr(aggregadgets) - Files to handle aggregates and lists.

@pr(gadgets) - Pre-defined gadgets, such as menus and scroll bars.

@pr(gesture) - Tools for handling gestures as input.

@pr(ps) - Functions for printing Garnet windows with PostScript.

@pr(debug) - Debugging tools.

@pr(demos) - Demonstration programs written using Garnet.

@pr(gilt) - The Gilt interface builder.

@pr(c32) - A spreadsheet for editing constraints among objects.

@pr(lapidary) - The Lapidary interactive tool.

@pr(contrib) - Files contributed by Garnet users that are not
supported by the Garnet group, but just provided for your use.
@end(Itemize)

@Section(Site-Specific Changes)
@Index(Site specific changes)
@Index(Machine-specific features)
If you are transferring Garnet to your site, you will need to make a
number of edits to files in order for Garnet to load, compile and
operate correctly.  All users will need to edit the Garnet pathnames as
discussed in section @ref(pathnames), but relatively few users should need
the other sections @ref(optimization-settings) - @ref(clisp).  Garnet has
been adjusted to load on the widest possible variety of lisps and operating
systems with minimum modification.

Of course, if you change any @pr(.lisp) files in the Garnet subdirectories
(not including @pr(garnet-loader.lisp)), you will need to
recompile them (section @ref(garnet-load)), even if you do not need to
recompile other parts of Garnet.

@SubSection(Pathnames)
@label(pathnames)
@index(pathnames)
@index(file names)
@index(Garnet-loader)
@Index(garnet-version)

After you have copied Garnet to your machine and untar'ed the source files,
the top level Garnet directory will contain the file
@pr(garnet-loader.lisp).  This one file contains the file names for all the
parts of Garnet.  You should edit this file to put in your own file names.
The best way to do this is to set the @pr(Garnet-Version) to be
@pr(:external) and edit the string
at the top of the file called @pr(Your-Garnet-Pathname) to say where the
files are.  This change is normally done during the compile procedure,
already described in section @ref(retrieving).

@SubSection(Compiler Optimization Settings)
@label(optimization-settings)

The variable @pr(user::*default-garnet-proclaim*), defined in
@pr(garnet-loader.lisp), holds a list of
compiler optimization flags and default values.  These flags determine things
like the size and speed of your resulting Garnet binaries.
For example, the default value of this variable in Allegro is:

@begin(programexample)
'(optimize (speed 3) (safety 1) (space 0) (debug 3))
@end(programexample)
@blankspace(-1.5 lines)

This optimization causes Allegro to generate compiled binaries that
are as fast and small as possible.  The @i(safety) setting of 1 means
that the compiled code will allow keyboard interrupts if you somehow
go into an infinite loop, and the @i(debug) setting of 3 means you
will get the most helpful error messages that Allegro can give you
when you are thrown into the debugger.

Different implementations of lisp require different values for the
optimization flags, and @pr(garnet-loader.lisp) provides values for
Allegro, Lucid, CMUCL, LispWorks, and MCL that we have found work particularly
well.  You can override the default optimizations by defining the
@pr(*default-garnet-proclaim*) variable before loading
@pr(garnet-loader.lisp).  A value of NIL for this variable means that
you want to maintain the declarations that are already in effect
for your lisp.


@SubSection(Fonts in X/11)
@index(Fonts)
@index(text-fonts.lisp)

In X/11 R4 through R6, there are almost always a full set of fonts available
with standard names.  Garnet relies on these fonts being available on the
standard font paths set up by X/11.  You can try loading Garnet and see
if it finds the standard fonts.  

If not, look in the file @pr(garnet/src/opal/text-fonts.lisp).  This file
constructs font names according to the standard X/11 format (with lots of
"-*-*-*"'s).  You will have to substitute
the names of fonts that are available at your installation.

@SubSection(Keyboard Keys)
@index(Keyboard Keys)
@index(Key Caps)

If your keyboard has some specially-labeled keys on it, Garnet will allow
you to use these as part of the user interface.  The file
@begin(programexample)
define-keys.lisp
@end(programexample)
which is in the @pr(garnet/src/inter) sub-directory,
defines the mappings from the codes that come back from X/11 and the Mac
to the special Lisp characters or atoms that define the keys in Garnet.  

For many machines, such as Suns, HP's, DECStations, and Macs,
we have built in mappings for all of the keyboard keys.  Since there
are no Lisp characters for the
special keys, they are named with keywords such as @pr(:uparrow) and
@pr(:F1).  If some keys on your keyboard are not mapped to keywords, you
can use the following mechanism to set this up. 

@index(find-key-symbols)
@index(define-keys)
To find the correct codes to use for each undefined key, load the
Find-Key-Symbols utility with
@begin(programexample)
(garnet-load "inter-src:find-key-symbols.lisp")
@end(programexample)
After loading this file, simply type the keys you need to find mappings for
while input is focused on the Find-Key-Symbols window (you may have to click
on the window's title-bar to change the input focus).
Garnet will print out the code number of the keys you type.

Then, you can go into the file @pr(define-keys.lisp) and edit it so the
codes you found map to appropriate keywords.

Next, you might want to bind these keys to keyboard editing operations.  If
you want these to be global to all Garnet applications, then you can edit
the files
@index(textkeyhandling.lisp)
@programexample[textkeyhandling.lisp]
and
@programexample[multifont-textinter.lisp]
which contain the default mappings of keyboard keys to text editing
operations.  The Interactors Manual contains full more information on how
this works.

@b[If you surround your changes to all these files with
@pr(#+<your-switch>) and mail them back to us
(@pr(garnet@@cs.cmu.edu)), then we will incorporate them into future
versions so you won't need to continually edit the files.]

@SubSection(Multiple Screens)
@index(Garnet-Screen-Number)
@index(Multiple Screens)
@index(Screens)
If you are working on a machine with only one screen, you need
not pay attention to this section.
However, certain machines, such as the color Sun 3/60, have more
than one screen.  The color Sun 3/60 has
both a black-and-white screen (whose display name is "unix:0.0")
and a color screen (whose display name is "unix:0.1").  If you type
"echo $DISPLAY" in a Unix shell, you will get 
the display name of the screen you are working on; that name should
look like "unix:0.*" where * is some integer.

Garnet assumes that the DISPLAY environment variable has this form of
"displayname:displaynumber.screennumber", and extracts the display and
screen numbers from that.  If any fields are missing, then the missing display
or screen number defaults to zero.


@SubSection(OpenWindows Window Manager)
@index(OpenWindows)
@index(FocusLenience)

If you are running OpenWindows from Sun, you will need to add
the following line to your @pr[.Xdefaults] file to make text input work
correctly:
@begin(programexample)
	OpenWindows.FocusLenience:		True
@end(programexample)


@SubSection(LispWorks)
@label(lispworks)
@index(Harlequin) @index(lispworks)  @index(guarantee-processes)

LispWorks is the common lisp sold by Harlequin Ltd.  There is one peculiarity
about LispWorks that requires an additional step
before executing the main-@|event-@|loop background process of Garnet
(Garnet uses multiprocessing by default in LispWorks -- see the Interactors
Manual, section "The Main Event Loop" for details).  You need to perform this
step both when @u(compiling) and @u(loading) Garnet (the appropriate steps
are mentioned during the standard compile procedure in section
@ref(retrieving)).

LispWorks has an unconventional "initialization phaze" to multiprocessing,
which requires that a special function be called before launching a background
process.  There are two ways to initialize multiprocessing in LispWorks.
One way is to start the big window-oriented LispWorks interface by
executing @pr[(tools:start-@|lispworks)].  This will cause a menu to
appear, and you can open a lisp listener as a selection from the menu.
From this listener, you can load @pr(garnet-loader.lisp), and Garnet's
main-event-loop process will be launched by default.

If you do not need all the functionality of the LispWorks interface, you can
initialize multiprocessing with much less overhead.  Before loading Garnet,
load the file @pr("src/utils/lispworks-process.lisp") and execute the function
@pr(guarantee-processes) to start multiprocessing.  For example, at the
LispWorks prompt you could type:

@begin(programexample)
@b[>] (garnet-load "utils-src:lispworks-process.lisp")
@b[>] (guarantee-processes)

@i[;; At this point, a new lisp listener has been spawned]
@b[>] (load "garnet/garnet-loader")
@end(programexample)

It is @u(important) to realize that when you call @pr(guarantee-processes),
a @u(new) lisp listener is spawned, and all subsequent commands will be typed
into the second listener.  Putting the @pr(guarantee-processes) call at the top
of the @pr(garnet-loader.lisp) file will not work, because the first listener
will remain hung at the @pr(guarantee-processes) call, while the second
process is waiting for user input.

On the other hand, it has been reported that putting the special steps for
LispWorks in a @pr(.lispworks) file may serve to automate the process a bit.
To automatically initialize multiprocessing whenever LispWorks is started,
put the following lines in your @pr(.lispworks) file:
@begin(programexample)
(progn
  (load "<your-garnet-pathname>/src/utils/lispworks-process.lisp")
  (guarantee-processes))
@end(programexample)
You will not be able to call @pr(garnet-load) from your @pr(.lispworks) file
because the function will not have been defined when the file is read.

Whenever you enter the debugger of the new listener spawned by
@pr[guarantee-processes], you will get restart options that include:

@begin[programexample]
...
5 (abort) return to level 0.
6 Return to top level
7 Return from multiprocessing
@end[programexample]

When you want to exit the debugger, you should choose either
"(abort) return to level 0," or "Return to top level", since both of these
options will return you to the top-level LispWorks prompt.  If you ever
choose "Return from multiprocessing", then you will kill both the
second listener and the main-event-loop-process, and you will have to call
@pr(guarantee-processes) and @pr(opal:launch-@|main-@|event-@|loop-@|process)
to restart Garnet's main-event-loop process.

It is not necessary to load @pr("lispworks-process.lisp") or execute
@pr(guarantee-processes) if you instead choose to execute
@pr(tools:start-lispworks).



@SubSection(CLISP)
@index(clisp)
@label(clisp)

CLISP is a Common Lisp (CLtL1) implementation by Bruno Haible of
Karlsruhe University and Michael Stoll of Munich University, both in
Germany.  There are a couple of additional steps you must take to run
Garnet in CLISP that are not required in other lisps.


@b(Renaming .lisp files to .lsp)

If you have an older version of CLISP, you will have to rename
all of the source files from ".lisp" to ".lsp"
before starting the procedure to compile Garnet.  A @pr(/bin/sh) shell script
has been provided to automate this process in the file
@pr(src/utils/rename-for-clisp).  This script requires that you @pr(cd) into
the @pr(src) directory and execute

@programexample[% sh utils/rename-for-clisp]

The script will rename all of the @pr("src/*/*.lisp") files to @pr(".lsp"),
so that they can be read by CLISP.


@b(Obtaining CLX)

If you are already using CLISP, you may need to additionally retrieve the
CLX module.  CLX for CLISP can be retrieved via @pr(ftp) from
@pr(ma2s2.mathematik.uni-karlsruhe.de), in the file
@pr(/pub/lisp/clisp/packages/pcl+clx.clisp.tar.z).


@b(Making a Garnet image)

Once you have installed the CLX module, you can make a restartable image of
Garnet with the following procedure (NOTE: this is different from other lisps).
This is the standard procedure for compiling Garnet, followed by a dump of
the lisp image.

@begin(programexample)
clisp -M somewhere/clx.mem
> (load "garnet-prepare-compile.lsp")
> (load "garnet-loader.lsp")
> (load "garnet-compiler.lsp")
> (opal:make-image "garnet.mem" :quit t)
@end(programexample)

The saved image can then be restarted with the command:

@programexample[clisp -M garnet.mem]



@Subsection(AKCL)

Some of the default parameters for the AKCL lisp image are insufficient
for running Garnet.  You may be able to change some of these parameters
in the active lisp listener, but it is probably better to rebuild your
AKCL image from scratch with the following parameter values:

@begin(programexample)
MAXPAGES for AKCL should be at least 10240, and

(SYSTEM:ALLOCATE-RELOCATABLE-PAGES 800)
(SYSTEM:ALLOCATE-CONTIGUOUS-PAGES 45 T)
(SYSTEM:ALLOCATE 'CONS   3500 t)
(SYSTEM:ALLOCATE 'SYMBOL  450 t)
(SYSTEM:ALLOCATE 'VECTOR  150 t)
(SYSTEM:ALLOCATE 'SPICE   300 t)
(SYSTEM:ALLOCATE 'STRING  200 t)
@end(programexample)

Garnet runs about half as fast in AKCL as on other Common Lisps.  Increasing
the RAM in your machine may help.  Users have reported that 16MB on a
Linux-Box 486 yields unacceptable performance.



@begin(group)
@Section(Mac-Specific Issues)

@Subsection[Compensating for 31-Character Filenames:]
There are several gadgets files that
normally have names that are longer than 31 characters.  Mac users may
continue to specify the full-length names of these files by using
@pr[user::garnet-load], described in section @ref(garnet-load),
which translates the regular names of the gadgets into their truncated
31-character names so they can be loaded.  It is recommended that
@pr[garnet-load] be used
whenever any Garnet file is loaded, so that typically long and cumbersome
pathnames can be abbreviated by a short prefix.
@end(group)

@Subsection[Directories:]
Unlike the Unix version, the Macintosh version stores all the
binary and source files together in the various subdirectories under
"src".  This difference will not matter when a Garnet application is
moved between Unix and Mac platforms as long as @pr[garnet-load] is being
used to load Garnet files.  @pr[Garnet-load] will always knows where to
find the files.

@Subsection[Binding Keys:]
We have assigned Lisp keywords for most of the keys on the Macintosh keyboard.
Thus, to start an interactor when the "F1" key is hit, use @pr(:F1) as the
interactor's @pr(:start-event).  If you want to know what a key generates, you
can use the small utility @pr(Find-Key-Symbols) which has been ported to the
Mac.  Execute
@programexample[(garnet-load "inter-src:find-key-symbols")]
to bring up a window which can perceive keyboard events and prints out the
resulting characters.  The data you collect from this utility can be used in
the @pr[:start-where] slot of interactors to describe events that will start
the interactor, and can be used to modify the characters generated by the
keyboard key by editing the file @pr[src:inter:mac-define-keys.lisp].

@Subsection[Simulating Multiple Mouse Buttons With the Keyboard:]
Most of the Garnet demos assume a three button mouse.  To simulate this
on the Macintosh, we use keyboard keys to replace a three-button mouse.
By default, the keys are @pr[F13], @pr[F14], and @pr[F15] for the left,
middle, and right mouse buttons, respectively.  The real mouse button
is also mapped to @pr[:leftdown].

You can redefine the keys to be any three keys you want by setting
@pr[inter::*leftdown-key*], @pr[inter::*middledown-key*], and
@pr[inter::*rightdown-key*] after loading Garnet or by editing the
file @pr[src:inter:mac-define-keys.lisp] directly.  These variables
should contain numerical key-codes corresponding to your desired keys.
Some key-codes are shown on p. I-251 of @u[Inside Macintosh Volume I],
but you can also do @pr[(garnet-load "inter:find-key-symbols")] to run
a utility program that tells you the key-code for any keyboard key.
The utility will generate numbers that can be used directly in
@pr[src:inter:mac-define-keys.lisp].

@index[mouse-keys.lisp]
To facilitate Garnet's use with keyboards not equipped with function keys,
Garnet supplies another utility program called @pr[mouse-keys.lisp],
which is in the top-level Garnet directory in the Mac version (and is
in @pr[src/utils/mouse-keys.lisp] if you acquired the Unix-packaged
version of Garnet).  When loaded, this utility creates a window that
allows you to toggle between using
the function keys and arrow keys for the simulated mouse buttons.  If
you are frequently switching between using Garnet on an Extended
Keyboard and a smaller laptop keyboard, you may use this utility a lot
to tell Garnet which keys should be used for middle-down and right-down.

@begin(group)
@Subsection[Modifier Keys:]
Like MCL itself, Garnet treats the @pr[Option] key as the "Meta" key.  Also,
you currently cannot get access to the @pr[Command] (Open-Apple) key
from Garnet.
@end(group)

@Subsection[Things to Keep in Mind When You Want Your Garnet Programs
to Run on Both X Windows and the Mac:]
@begin(itemize)
Use @pr[user::garnet-load] instead of @pr[load] when loading gadget files

Only supply @pr[:face] values for fonts that run on both systems -- this
typically restricts you to using only the standard faces available
in Garnet 2.2 and earlier versions.

The @pr[#+apple] and @pr[#-apple] reader macros can be used to
indicate code that should be used only for Macs and only for non-Macs,
respectively.  When defining fonts, for example, you may want to
provide the slot description @pr[(:face #+apple :underline #-apple :bold)]
to indicate that the font will be underlined on the Mac but bold in X.

The default place for windows is at (0,0) which unfortunately puts their
title bars under the Macintosh menubar, so you cannot even move them
using the mouse!  (You can still @pr[s-value] the position from the Lisp 
Listener.)  Therefore, never create a window on the Mac with a @pr[:top] less
than 45 or it will not be movable.

Remember that many Mac screens are much smaller than most workstations'
screens.  Positioning windows perfectly may not be possible, and a better
goal may be to simply keep the window title-bars within reach of the
mouse so that the windows can be moved.
@end(itemize)


@Section(Compiling Garnet)
@label(compilinggarnet)
@Index(Compiling Garnet)
@Index(garnet-compiler)

After executing the compile procedure in section @ref(retrieving),
the result should be that all the files are compiled and loaded.
(If there was a problem and you need to restart the compile procedure,
please see below.)  The compiler scripts do @i(not) check for compile errors.
We have attempted to make Garnet compile without errors
on all common lisps, but some lisps generate more warnings than others.

The compiler scripts compile the binaries into the same
directories as the source files.  For example, all the interactor binaries
will be in @pr(garnet/src/inter/) along with the source (@pr(.lisp)) files.
Therefore, after the compilation is completed, you will need to move the
binaries into their own directory (e.g., @pr(garnet/bin/inter)).
To do this, use the c-shell script
@begin(programexample)
csh garnet-after-compile
@end(programexample)
The @pr(garnet-after-compile) file will normally be in the top level garnet
directory.  Note that this is typed to the shell, not to Lisp.  Even if you
normally run the "regular" (Bourne) shell (sh), the above command should work.

To prevent certain parts of Garnet from being compiled, set
@pr(user::compile-)@i(xxx)@pr(-p) to NIL, where @i(xxx) is replaced with the
part you do not want to compile.  See the comments at the top of the file
@pr(garnet-prepare-compile) for more information.

If you ever have to restart the compile process, you do not have to start
from scratch.  If you have not yet moved the binary files out of the @pr(src/)
directory (i.e., you have not yet run @pr(garnet-after-compile)), then you
can use the files that have been compiled already instead of compiling them
again.  Restart lisp, and for each Garnet module that has been compiled,
set the variable @pr(user::compile-)@i(xxx)@pr(-p) to NIL to indicate that it
should not be compiled
again.  Then load the three script files again in the usual order.  Note:
if a module has been only partially compiled, then you must recompile the
whole module.


@Section(Loading Garnet)
@label(loading-garnet)
@Index(Loading Garnet)
@Index(garnet-loader)
@Index(load-@i(xxx)-p)
@Index(Garnet-@i(xxx)-Pathname)
@Index(Garnet-@i(xxx)-Src)

To load Garnet, it is only necessary to load the file:
@begin(programexample)
(load "garnet-loader")
@end(programexample)

(Of course, you may have to preface the file name with the directory
path of where it is located.  It is usually in the top level
@pr(garnet) directory.)

To prevent any of the Garnet sub-systems from being loaded, simply set the
variable @pr(user::load-)@i(xxx)@pr(-p) to NIL, where @i(xxx) is replaced by
whatever part you do not want to load. Normally, some parts of the system are
not loaded, such as the gadgets and demos.  This is because you normally do not
want to load or use all of these in a session.  Files that use gadgets will
load the appropriate ones automatically, and the @pr(demos-controller)
program loads the demos as requested.

It is possible to save an image of lisp after loading Garnet, so that when
you restart lisp, Garnet will already be loaded and you will not have to
load @pr(garnet-loader.lisp).  For details about making lisp images, see
the function @pr(opal:make-image) in the Opal manual.

@begin(group)
@Section(Loader and Compiler Functions)
@label(garnet-load)
@index(loading) @index(compiling) @index(garnet-load) @index(garnet-compile)

@SubSection(Garnet-Load and Garnet-Compile)

There are two functions that allow you to save a lot of typing when
you load and compile files.  When you supply @pr(garnet-load) and
@pr(garnet-compile) with the Garnet subdirectory that you want to get a
file from (e.g., "gadgets"), the functions will automatically
append your Garnet pathname to the front of the specified file.

@begin(programexample)
user::Garnet-Load "@i(prefix):@i(filename)"@value(function)

user::Garnet-Compile "@i(prefix):@i(filename)"@value(function)
@end(programexample)

These functions are defined in @pr(garnet-loader.lisp) and are internal
to the @pr(user) package.
@end(group)

@blankspace(1 line)
@begin(group)
The @i(prefix) parameter corresponds to one of the Garnet
subdirectories, and the @i(filename) is a file in that directory.
A list of the most useful prefixes appear in section @ref(garnet-load-alist),
and a full list can be seen by evaluating the variable
@pr(user::Garnet-@|Load-@|Alist) in your lisp (after loading Garnet).
Examples:

@begin(programexample)
* (garnet-load "gadgets:v-scroll-loader")
Loading #p"/afs/cs/project/garnet/bin/gadgets/v-scroll-loader"
Loading V-Scroll-Bar...
...Done V-Scroll-Bar.

T
* (garnet-compile "opal:aggregates")
Compiling #p"/afs/cs/project/garnet/src/opal/aggregates.lisp"
for output to #p"/afs/cs/project/garnet/bin/opal/aggregates.fasl"
...
; Writing fasl file "/afs/cs/project/garnet/bin/opal/aggregates.fasl"
; Fasl write complete

NIL 
* 
@end(programexample)
@end(group)
@blankspace(1 line)

There are two groups of prefixes that @pr(garnet-load) accepts -- the
"bin" prefixes and the "src" prefixes.  @pr(Garnet-load) assumes that
when you load files, you will want to load the compiled binaries.
Therefore, when you use prefixes like "gadgets", @pr(garnet-load) uses
the Garnet-Gadgets-Pathname variable to find the file you want.  If
you really want to load a file from your source directory, you should
use the subdirectory name with "-src" tacked on.  For example,

@begin(programexample)
* (garnet-load "gadgets-src:motif-parts")
Loading #p"/afs/cs/project/garnet/src/gadgets/motif-parts"
...

T
*
@end(programexample)

@pr(Garnet-compile) does not accept "-src" prefixes, because it always
assumes that you want to take a lisp file from your source directory,
compile it, and output it to your bin directory.  Note: do not specify
".lisp" or ".fasl" with your filename -- @pr(garnet-compile) will
supply suffixes for you.  @pr(Garnet-compile) attempts to determine
your correct binary extension (".fasl", ".lbin", etc.) from the kind
of Lisp that you are using.  If @pr(garnet-compile) ever gets the
extension wrong, you can change it by setting the variable
@pr(*compiler-extension*), which is defined in the @pr(user) package.

@begin(group)
@SubSection(Adding Your Own Pathnames)
@label(garnet-load-alist)

The functions @pr(user::garnet-load) and @pr(user::garnet-compile)
look up their prefix parameters in an association list called
@pr(user::Garnet-Load-Alist).  Its structure looks like:

@begin(programexample)
(defparameter Garnet-Load-Alist
  `(("opal" . Garnet-Opal-Pathname)          @i[; For loading the ] multifont-loader
    ("gg" . Garnet-Gadgets-PathName)         @i[; For loading gadgets]
    ("gestures" . Garnet-Gestures-PathName)  @i[; For loading ] agate
    ("debug" . Garnet-Debug-PathName)        @i[; For loading the ] Inspector
    ("demos" . Garnet-Demos-PathName)        @i[; For loading demos]
    ("gilt" . Garnet-Gilt-PathName)          @i[; For loading high-level tools...]
    ("c32" . Garnet-C32-PathName)
    ("lapidary" . Garnet-Lapidary-PathName)
    ...))
@end(programexample)

This alist is expandable so that you can include your own prefixes and
pathnames.  Prefixes can be added with the following function:

@index(add-garnet-load-prefix)
@begin(programexample)
user::Add-Garnet-Load-Prefix @i(prefix pathname) @value(function)
@end(programexample)

For example, after executing
@pr[(add-garnet-load-prefix "home" "/usr/amickish/")],
you would be able to do @pr[(garnet-load "home:my-file")].
@end(group)



@Section(Overview of the Parts of Garnet)
@Index(Parts of Garnet)
@Index(Packages in Garnet)

Garnet is composed of a number of sub-systems, some of which can be loaded
and used separately from the others.  Most of the subsystems also have
their own separate packages.  The following list shows the components of
Garnet, the package used by that component, and the page number of the
corresponding section in this manual.

@begin(Itemize)
@pr(KR) - Package @Pr(kr). @Index[kr (package)]  The object and constraint
system.  Page @Value(kr).

@pr(Gem) - Package @Pr(gem).  @Index[gem (package)]  Low-level graphics
routines that allow the system to run on the Mac or on X/11.  We do not
support user code directly calling Gem, so it is not described further
in this manual.

@pr(Opal) - Package @Pr(opal). @Index[opal (package)]
The graphical object system.  Page @Value(Opal).

@pr(Interactors) - Package @Pr(inter). @Index[inter (package)] Handling of
mouse and keyboard
input.  Page @Value(Inter).

@pr(Gestures) - Package @pr(inter).  Code to handle gesture recognition
and training.  Described in the interactors manual, page @Value(Inter).

@pr(Aggregadgets) - Package @Pr(opal).  Support for creating instances of
collections of objects, and for rows or columns of objects.  Page
@Value(aggregadgets).

@pr(AggreGraphs) - Package @Pr(opal).  Support for
creating graphs and trees of objects.  Also described in the
aggregadgets manual, page @Value(aggregadgets).

@pr(Gadgets) - Package @Pr(garnet-gadgets), nicknamed @pr(gg).
@Index[garnet-gadgets (package)] A collection of pre-defined
gadgets, including menus, buttons, scroll bars, circular gauges, graphics
selection, etc.  Page @Value(gadgets).

@pr(Debugging tools) - Package @Pr(garnet-debug), nicknamed @pr(gd).
@Index[garnet-debug (package)] Useful functions to help debug Garnet programs,
including the Inspector.  Page @Value(Debug).

@pr(Demonstration programs) - Each demonstration program is in its own
package.  Page @value(demos).

@pr(Gilt)  - Package @Pr(gilt). @Index[gilt (package)] The Garnet
interface builder.  Page @value(gilt).

@pr(C32) - Package @pr(c32).  A spreadsheet interface for editing
constraints.  Page @value(c32).

@pr(Lapidary)  - Package @Pr(Lapidary). @Index[lapidary (package)] A
sophisticated interactive design tool.  Page @value(lapidary).

@pr(Contrib) - A set of file contributed by Garnet users.  These have
not been tested by the Garnet group, and are not supported.  Each file
should have a comment at the top describing how it works and who to
contact for help and more information.
@end(Itemize)

@Section(Overview of this Technical Report)

In addition to the reference manuals for all the parts of the Garnet
toolkit listed above, this technical report also contains:
@begin(itemize)
A guided on-line tour of the Garnet system that will help you become
familiar with a few of the features of the Garnet toolkit.  Page @Value(tour).

A tutorial to teach you the basic things you need to know to use
Garnet.  Page @Value(tutorial).

The code for a simple graphical editor, as a sample of code written for
Garnet.  Page @Value(sampleprog).

The Hints manual starting on page @value(hints) includes some suggestions
that have been collected from the experience of Garnet users for making
Garnet programs run faster.
If you have ideas for things to add to this section, let us know.
@end(itemize)

@Section(What You Need To Know)

Although this is a large technical report, you certainly do not need to
know everything in it to use Garnet.  Garnet is designed to support many
different styles of interface.
Therefore, there are many options and functions that you will probably not
need to use.

In fact, to run the @u(Tour) (page @value(tour)), it is not
necessary to read any of the reference manuals.  The tour is
self-explanatory.  

Next, you should probably read the @u(Tutorial) (page @value(tutorial)), since
it tries to provide enough information about most of Garnet so that you don't
need the other manuals right away.

To run the Gilt Interface Builder, you do not need to know about the
rest of the system either.  The Gilt manual should be sufficient.
When you are ready to set some properties of the gadgets, you will
need to look up the particular gadget in the Gadgets manual to see
what the properties do.

Even when you are ready to start programming, you will still not need most
of the information described here.  To start, you should probably do the
following:
@begin(enumerate)
Read this overview.

Run the tour, to get a feel for Garnet programming.

Read the tutorial.

You might try creating a few dialog boxes using Gilt.  This will
familiarize you with the Gadgets.  See the Gilt manual (Page @value(gilt)).

After that, you can look at the sample program at the end of
this technical report, to see what you need more information about.

You could now try to start writing your own programs, and just use the rest
of the manuals as reference when you need information.

Next, look at the introduction and the following functions in the KR
document: @pr(gv, gvl, s-value, formula, o-formula,) and
@pr(create-instance).  The KR manual documents the entire KR module, but
Garnet does not use every feature that KR provides.  Some concepts (like
demons), will never be used by the typical Garnet user.  Once you have
gained some familiarity with the system, you may want to return to the KR
Manual and read about object-oriented programming, type-checking, and
constants.

Next, skim the first five chapters of the Opal manual, and look at the
various graphical objects, so you know what kinds are provided.  The primary
functions you will use from Opal are: @pr(add-component, update,) and
@pr(destroy), as well as the various types of graphical objects
(@pr(rectangle, line, circle), etc.), drawing styles (@pr(thin-line,
dotted-line, light-gray-fill), etc.) and fonts.

Next, in the Interactors manual, you will need to skim the first four
chapters to see how interactors work, and then see which interactors there
are in the next chapter.  You will probably not need to take advantage of
the full power provided by the interactors system.

Aggregadgets and Aggrelists are very useful for handling collections of
objects, so you should read their manual.  They support creating instances
of groups of objects.

You should then look at the gadget manual to see all the built-in
components, so you do not have to re-invent what is already supplied.

User interface code is often difficult to debug, so we have provided a
number of helpful tools.  The Inspector is mentioned briefly in the Tutorial,
and it is discussed thoroughly in the debugging manual.  You will probably
find many debugging features very useful.

The demo programs can be a good source of ideas and coding style, so the
document describing them might be useful.
@end(enumerate)

If all you want Garnet for is to display menus and gauges that are supplied
in the gadget set, you can probably just read the KR, Gadgets and Gilt
manuals, and skip the rest.


@Section(Planned Future Extensions)
@Index(Future work)
@label[future]

We expect 3.0 to be the last release of the lisp version of Garnet.
No enhancements of the lisp version are planned.  However, if you need
something and would like to sponsor its development, write to
@pr(garnet@@cs.cmu.edu).

@index[amulet]
The group is now working on a C++ system called Amulet, which will have
many features similar to those found in Garnet.  Watch for announcements
about the Amulet project on @pr(comp.windows.garnet) and @pr(comp.lang.c++).
To sign up for the new Amulet mailing list, please send mail to
@pr(amulet-users-request@@cs.cmu.edu).


@newpage
@Section(Garnet Articles)
@label(articles)
@index(articles) @index(papers)

A number of articles about Garnet have been made available for FTP from the
directory @pr(/usr/garnet/garnet/doc/papers/) on @pr(a.gp.cs.cmu.edu).
There is a README file in that directory, indicating which @pr(.ps) files
correspond to the Garnet bibliography citations.

@include(overview/garnetpapers.mss)

