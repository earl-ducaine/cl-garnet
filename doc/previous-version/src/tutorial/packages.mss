@Chapter(A Note on Packages)

@i(This section should replace the "Packages" section in the Tour.)

The Garnet software is divided into a number of Lisp packages.  A
@i(package) may be thought of as a module containing procedures and
variables that are all associated in some way.  Usually, the
programmer works in the @pr(user) package, and is not aware of
other packages in Lisp.  In Garnet, however, function calls are
usually accompanied by the name of the package in which the function
was defined.

For example, one of the packages in Garnet is @pr(opal), which
contains all the objects and procedures dealing with graphics.  To
reference the @pr(rectangle) object, which is defined in @pr(opal),
the user has to explicitly mention the package name, as in
@pr(opal:rectangle).

On the other hand, the package name may be omitted if the user
calls @pr(use-package) on the package that is to be referenced.  That
is, if the command @pr[(use-package 'opal)] or @pr[(use-package
"OPAL")] is issued, then the @pr(rectangle) object may be referenced
without naming the @pr(opal) package.

The recommended "Garnet Style" is to @pr(use-package) only one
Garnet package -- @pr(KR) -- and explicitly reference objects in other
packages.  This convention is followed in the code examples below.
The file @pr(tour.lisp) that you loaded contains the line
@pr[(use-package "KR")], which implements the convention.  You will
probably want to put this line at the top of all your future Garnet
programs as well.

The packages in Garnet include:
@begin(itemize)
@pr(KR) - contains the procedures for creating and accessing objects.  This
contains the functions @pr(create-instance), 
@pr(gv), @pr(s-value), and @pr(o-formula). 

@pr(Opal) - contains the graphical objects and some functions for them.

@pr(Inter) - contains the interactor objects for handling the mouse.

@pr(Garnet-Gadgets) - contains a collection of predefined "gadgets" like
menus and scroll bars.

@pr(Garnet-Debug) - contains a number of debugging functions.  These are
not discussed in this tour, however.

@End(itemize)

