@device(postscript)
@Make(manual)
@disable(figurecontents)
@LibraryFile(Garnet)
@String(TitleString = "Debugging")
@use(bibliography "garnet.bib")
@begin(TitlePage)
@begin(TitleBox)
@blankspace(0.6 inch)
@heading(Debugging Tools for Garnet
Reference Manual)

@b{Roger B. Dannenberg
Andrew Mickish
Dario Giuse}
@BlankSpace(0.3 line)
@value(date)
@end(TitleBox)

@BlankSpace(0.5 inch)
@center(@b(Abstract))
@begin(Text, spacing=1.1)
Debugging a constraint-based graphical system can be difficult
because critical interdependencies can be hard to visualize
or even discover.  The debugging tools for Garnet 
provide many convenient ways to inspect objects and constraints
in Garnet-based systems.

@blankspace(0.5 inch)
@include(creditetc.mss)
@End(Text)
@end(TitlePage)


@include(pagenumbers)
@set(page = debug-first-page)

@chapter(Introduction)
This manual is intended for users of the Garnet system and assumes that
the reader is familiar with Garnet.  Other reference manuals cover
the object and constraint system KR @cite(KRTR2), 
the graphics system Opal @cite(OpalMANUAL),
Interactors @cite(InterMANUAL)
for handling keyboard and mouse input, Aggregadgets @cite(AggregadgetsMANUAL)
for making instances of aggregates of Opal objects.

@Section(Notation in this Manual)

In the examples that follow, user type-in follows the asterisk (@pr(*)),
which is the prompt character in CMU Common Lisp on the RT.  
Function results are printed following the characters ``@pr(-->)''.
This is not what CMU Common Lisp prints, but is added to avoid
confusion, since most debugging functions print text in addition
to returning values:
@begin(programexample)
* (some-function an-arg or-two)
some-function prints out this information,
    which may take several lines
--> function-result-printed-here
@end(programexample)

@Section(Loading and Using Debugging Tools)
@index(loading)
Normally, debugging tools will be loaded automatically when you load
the file @pr(garnet-loader.lisp).  Presently, the debugging tools are
located in the files @pr(debug-fns.lisp) and @pr(objsize.lisp).  A
few additional functions are defined in the packages they support.

Most of the debugging tools are in the @pr(GARNET-DEBUG) package,
@index[garnet-debug (package)]
and you should ordinarily type
@begin(programexample)
(use-package "GARNET-DEBUG")
@end(programexample)
to avoid typing the package name when using these tools.  Functions
and symbols mentioned in this document that are @i(not) in the
@pr(GARNET-DEBUG) package will be shown with their full package name.

@chapter(Inspecting Objects)

@Section(Inspector)
@label(inspectorsec)
@index(Inspector)
The @pr(Inspector) is a powerful tool that can be of significan help in
debugging.  It pops up a window showing an object, and also
shows the aggregate and is-a hierarchy for objects, and the
dependencies for formulas.  Various operations can be performed on
objects and slots.  In general, the @pr(Inspector) is quite useful for
debugging programs, and provides interfaces to many of the other
debugging functions in Garnet.  A view of an object being inspected in
shown in Figure @ref(inspectorfig).

@Begin(Figure)
@bar()
@center{@graphic(Postscript="debug/inspectorfig.PS",boundingbox=file,magnify=.75)}
@Caption{The Inspector showing a text object.}
@Tag(inspectorfig)
@bar()
@End(Figure)

The @pr(Inspector) is loaded automatically when you load the debugging
tools which is enabled by default in garnet-loader, but it can 
also be loaded explicitly using @pr[(garnet-load "debug:inspector")] 
The @pr(Inspector) is in the @pr(garnet-debug) package.

An example of using the Inspector is included in the Tutorial at the beginning
of this Reference Manual.


@Subsection(Invoking the Inspector)
There are a number of ways to inspect objects.  The easiest is to put
the mouse over an object and hit the @pr(HELP) keyboard key.  This
will print a message in the Lisp listener window and pop up a window
like Figure @ref(inspectorfig).  If you want to use the @pr(HELP)
keyboard key for something else, you can set the variable
@index(HELP key)
@index(inspector-key)
@pr(garnet-debug:*inspector-key*) to a different key (or NIL for none)
@i(before) loading the @pr(Inspector).

@blankspace(1 line)
@begin(group)
Alternatively, you can explicitly invoke the @pr(Inspector) on an object
using either
@begin(programexample)
garnet-debug:Inspector @i<obj> @value(function)
gd:Inspector @i<obj>
@end(programexample)
(@pr(gd) is an abbreviation for @pr(garnet-debug)).
@end(group)

@index(inspect-next-inter)
To inspect the @i(next) interactor that runs, you can hit
@pr(CONTROL-HELP) on the keyboard (the mouse position is irrelevant),
or call the function
@begin(programexample)
gd:Inspect-Next-Inter @value(function)
@end(programexample)
Hitting @pr(CONTROL-HELP) a second time before an interactor runs will
cancel the invocation of the @pr(Inspector).  To change the binding of this
function, set the variable
@index(inspector-next-inter-key)
@pr(gd:*inspector-@|next-@|inter-@|key*) @i(before) loading the
@pr(Inspector).

By default, @pr(SHIFT-HELP) is bound to a little function that simply
prints the object under the mouse to the Lisp Listener, and does not
invoke the @pr(Inspector).  Example output from it is:
@begin(programexample)
--> (24,96) = #k<MULTIFONT-LINE-1447> in window #k<INTERACTOR-WINDOW-1371>

--> No object at (79,71) in window #k<INTERACTOR-WINDOW-1371>
@end(programexample)
To change the binding of this function, use the variable
@index(show-object-key) @pr(gd:*show-object-key*).

@Subsection(Schema View)
The schema view shown in Figure @ref(inspectorfig) tells all the local
slots of an object.  To see the inherited slots also, click on the
@pr(Inherited Slots) button.  For each slot, the display is:
@begin(itemize, spread = 0)
The slot name.

A @p[(C)] if the slot is constant.

An @p[(I)] if the slot is inherited.

The formula for the slot, if any.

If there is a formula, then a @p[(V)] if the slot value is valid,
otherwise a @p[(IV)] for invalid.

The current value of the slot, which may wrap to multiple lines if the
value is long.

The entire line is red if the slot is a @i(parameter) to the object
(if it is in the @pr[:parameters] list), otherwise the line is black.
@end(itemize)

If the object's values change while it is being inspected, the view is
@i(not) updated automatically.  To see the current value of slots, hit
the "@pr(Re-Fetch)" button.

To change the value of a slot of an object, click in the value part of
the slot (after the =), and edit the value to the desired value and
hit return.  The object will immediately be updated and the @pr(Inspector)
display will be re-fetched.  If you change your mind about editing the value
before hitting return, simply hit @pr(control-g).  If you try to set a
slot which is marked constant, the @pr(Inspector) will go ahead and set the
slot, but it gives you a warning because often dependencies based on
the slot will no longer be there, so the effect of setting the slot
may not work. 

If a slot's value is an object and you want to inspect that
object, or if you want to inspect a formula, you can double-click the
left button over the object name, and hit the "@pr(Show Object)" button.
Also, you can use the "@pr(Show in New)" button if you want the object
to be inspected in a new window.

@Subsection(Object View)
@label(inspectobjects)
@index[Object View (in Inspector)]
Hitting the "@pr(Objects)" button brings up the view in Figure
@ref(inspectorobjects).  This view shows the name of the object being
inspected at the top, then the @pr(is-a) hierarchy.  In Figure
@ref(inspectorobjects), @pr(TEXT-6509) is the immediate prototype of
the inspected object (@pr(TEXT-6508)), and @pr(TEXT-7180) is the
prototype of @pr(TEXT-6509), and so on.  The next set of objects shows
the aggregate hierarchy.  Here, @pr(TEXT-6508) is in the aggregate
@pr(MOTIF-BAR-ITEM-3024-5430), etc.  The last item in this list is
always the window that the object is in (even though that is
technically not the @pr(:parent) of the top-level aggregate).  The
final list is simply the list of objects that have been viewed in this
window, which forms a simple history of views.

@Begin(Figure)
@bar()
@center{@graphic(Postscript="debug/inspectorobjsfig.PS",boundingbox=file,magnify=.75)}
@Caption{The Inspector showing the objects related to the text object
of Figure @ref(inspectorfig).}
@Tag(inspectorobjects)
@bar()
@End(Figure)

To return to the schema view of the current object, use the
"@pr(Re-Fetch)" button.  You can double-click on any object and use
"@pr(Show Object)" or "@Pr(Show in New)" to see its fields, or you can hit
"@pr(Objects)" to go to the object view of the selected object.

@Subsection(Formula Dependencies View)
@label(dependencysec)
@index[Dependency View (in Inspector)]
If you select a formula or a slot name (by double-clicking on it) and
then hit the "@pr(Dependencies)" button, you get the view of Figure
@ref(inspectordepfig).  This slows the slots used in calculating the
value in the formula.

@Begin(Figure)
@bar()
@center{@graphic(Postscript="debug/inspectordepfig.PS",boundingbox=file,magnify=.75)}
@Caption{The Inspector showing the dependencies of the @pr(:visible)
slot of the object shown in Figure @ref(inspectorfig).}
@Tag(inspectordepfig)
@bar()
@End(Figure)

The first lines show the object, the slot, the formula name, and the
expression of the formula.  Then the dependencies are shown.  The
outer-most level of indenting are those slots that are immediately
used by the formula.  In this case, the @pr(:visible) slot of
@pr(#k<MOTIF-BAR-ITEM-3024-5430>).  Note that only non-constant slots
are shown, 
which is why the @Pr(:parent) slot of @pr(TEXT-6508) is not listed (it
is constant).  Indented underneath each slot are the slots it depends
on in turn, so @pr(:visible) of @pr(#k<MOTIF-BAR-ITEM-3024-5430>)
depends on its @pr(:parent) and its parent's @pr(:visible).  The
@pr("...") means that there are more levels of dependencies.  To see
these, you can double click on the @pr("...") or on any slot name and
hit the "@pr(Dependencies)" button.

@Subsection(Summary of Commands)

@begin(description)
@i(double-clicking) the left button on an object or slot will select it, and it will
then be the parameter for further commands.

@i(single-clicking) the left button after the @Pr(=) will let you edit
the value.  Hit return to set the value or control-g to abort.

@pr(Show Object) - displays the selected object in the same window.

@pr(Show in New) - displays the selected object in a new window.

@pr(Re-Fetch) - redisplay the current object, and re-fetch the values
of all slots, in case any have changed.  This command is also used to
get back to the schema view from the object or dependency views.

@pr(Dependencies) - when a formula or a slot containing a formula is
selected, then shows the slots that are used to calculate it (see
section @ref(dependencysec)).

@pr(Done) - get rid of this @pr(Inspector) window.

@pr(Done All) - get rid of all @pr(Inspector) windows.

@pr(Flash) - if an object is selected, flash it, otherwise flash the
current object being inspected.  The object is flashed by bringing its
window to the top and putting an XOR rectangle over it (using the
function @pr(gd:flash)).

@pr(Search) - Find a slot of the object and display it at the top of
the list.  This helps you find slots in a long list, and it will find
inherited slots, so you don't have to hit @pr(Inherited Slots) and get
the whole list when you are only interested in one slot.  After
hitting the @pr(Search) button, you will be prompted for the slot
name, and you can type in a few letters, hit RETURN, and the @pr(Inspector)
will try to fill out the name based on all current slots of the object.

@pr(Notify) - If a slot is selected, then will print a message in the
@pr(Inspector) @i(and) in the Lisp Listener window
whenever the selected slot of the object is set.  If no slot is
selected, then will print a message whenever @i(any) slot of
the object is set.  You can be waiting for a Notify or Break on
multiple slots of multiple objects at the same time.  Note that
execution is much slower when there are @i(any) Breaks or Notifies
in effect. 

@pr(Break) - If a slot is selected, then will break into the debugger
whenever the selected slot of the object is set.  If no slot is
selected, then will break into the debugger whenever @i(any) slot of
the object is set.  You should go to your Lisp Listener window to
handle the break, and then @i(continue) from the break (rather than
aborting or popping from the break).  The @pr(Inspector) will not operate
while you are in the debugger unless you type
@pr[(inter:main-event-loop)].

@pr(Clear Breaks) - Clear all the breaks and notifies.  All Breaks and
Notifies are also cleared when you hit the @pr(Done) or @pr(Done All)
buttons.  There is no interface for clearing a single break or notify.

@pr(Inherited Slots) - Toggle the display of inherited slots in the
schema view.

@pr(Objects) - Switch to the object view that shows the is-a and
aggregate hierarchy (see section @ref(inspectobjects)).
@end(description)


@section(PS -- Print Schema)
The same information that is shown in the Inspector for an object's slots and
values can be printed with the simpler @pr(kr:ps).  This function does not
create a new window to show the information, but instead prints right into
the lisp listener.
@begin(programexample)
kr:PS @i(object) &key @i{types-p  all-p} (@i{control} T) (@i{inherit} NIL) @value(function)
                (@i{indent} 0) (@i{stream} *standard-output*)
@end(programexample)
@index(ps)
(All the nuances of this function are described in the KR manual.)

@section(Look, What, and Kids)
For quick inspection of objects, the @pr(look), @pr(what), and @pr(kids) 
functions may be used:
@begin(programexample)
gd:Look @i(object) &optional (@i(detail) 2)@value(function)
@index(look)
gd:What @i(object)@value(function)
@index(what)
gd:Kids @i(object)@value(function)
@index(kids)
@end(programexample)
The @pr(look) function prints out varying amounts of information
about an object, depending upon the optional argument @i(detail):
@index(detail)
@begin(itemize, justification=off)
@pr[(look obj 0)] prints a one-line description of @pr(obj).  This
is equivalent to calling @pr[(what obj)].

@pr[(look obj 1)] prints a one-line description of @pr(obj) and also
shows the immediate components of @pr(obj) if it is an aggregate.
This form is equivalent to calling @pr[(kids obj)].

@pr[(look obj 2)] recursively prints all components of @pr(obj).
This is the default, equivalent to typing @pr[(look obj)].  Use
it to look at the structure of an aggregate.

@pr[(look obj 3)] prints slots of @pr[obj], using @pr(ps), and
then prints the tree of components.

@pr[(look obj 4)] prints slots of @pr[obj] and its immediate
components.  Any (trees of) sub-components are also printed.

@pr[(look obj 5)] prints what is essentially complete information
about a tree of objects, including all slots of all components.
@end(itemize)
For example, 
@begin(programexample)
* (what mywindow)
#k<MYWINDOW> is-a #k<INTERACTOR-WINDOW> (WINDOW)
--> NIL
* (look mywindow)
#k<MYWINDOW> is-a #k<INTERACTOR-WINDOW> (WINDOW)
   #k<MYAGG> is-a #k<AGGREGATE> (VIEW-OBJECT)
      #k<MYRECT> is-a #k<MOVING-RECTANGLE> (RECTANGLE)
      #k<MYTEXT> is-a #k<CURSOR-MULTI-TEXT> (MULTI-TEXT)
--> NIL
@end(programexample)

@section(Is-A-Tree)
Look prints the parent of the object and then the ``standard
parent'' @index(standard parent) of the object's parent in parentheses.
The ``standard parent'' is 
the first named object encountered traveling up the @pr(:is-a)
tree.  If @pr(look) does not print enough information about an object,
the @pr(is-a-tree) function might be useful:
@begin(programexample)
gd:Is-A-Tree@value(function)
@index(is-a-tree)
@end(programexample)
This function traces up @pr(:is-a) links and prints the resulting
tree:
@begin(programexample)
* (is-a-tree mytext)
#k<MYTEXT> is-a
   #k<CURSOR-MULTI-TEXT> is-a
      #k<MULTI-TEXT> is-a
         #k<TEXT> is-a
            #k<GRAPHICAL-OBJECT> is-a
               #k<VIEW-OBJECT>
--> NIL
@end(programexample)

@section(Finding Graphical Objects)
It is often necessary to locate @index(locate) 
a graphical object or figure out
why a graphical object is not visible. @index(visibility)  The function
@begin(programexample)
gd:Where @i(object)@value(function)
@index(where)
@end(programexample)
prints out the @i(object)'s @pr(:left), @pr(:top), @pr(:width), 
@pr(:height), and @pr(:window) in a one-line format.

@begin(programexample)
* (where mywindow)
#k<MYWINDOW> :TOP 43 :LEFT 160 :WIDTH 355 :HEIGHT 277
--> NIL
* (where myagg)
#k<MYAGG> :TOP 20 :LEFT 80 :WIDTH 219 :HEIGHT 150 :WINDOW #k<MYWINDOW>
--> NIL
@end(programexample)

If you are not sure which screen image corresponds with a 
particular Opal object, use the following function:
@begin(programexample)
gd:Flash @i(object)@value(function)
@index(flash)
@end(programexample)
The @pr(flash) function will invert the bounding box of @i(object)
making the object flash on and off.  @pr(flash) has two interesting
features:
@begin(enumerate)
You can @pr(flash) aggregates, which are otherwise invisible.

If the object is not visible, @pr(flash) will try hard to
tell you why not.  Possible reasons include:
@begin(itemize)
The object does not have a window,

The window does not have an aggregate,

The object is missing a critical slot (e.g. @pr(:left)),

The object is outside of its window,

The object's @pr(:visible) slot is @pr(nil),

The aggregate containing the object is not visible, or

The object is outside of its aggregate (a problem with the
aggregate).
@end(itemize)
@end(enumerate)
@pr(Flash) does not test to see if the object is obscured by
another window.  If @pr(flash) does not complain and you
do not see any blinking, use @pr(where) to find the object's
window.  Then use @pr(where) (or @pr(flash)) applied to the window 
to locate the window on your screen.  Bring the window to the front
and try again.

The @pr(invert) function is similar to @pr(flash), but it leaves
the object inverted.  The @pr(uninvert) function will undo the 
effect of @pr(invert):
@begin(programexample)
gd:Invert @i(object)@value(function)
@index(invert)
gd:Uninvert @i(object)@value(function)
@index(uninvert)
@end(programexample)
@pr(invert) uses a single Opal rectangle to
invert an area of the screen.  If the rectangle is in use, it
is first removed, so at most one region will be inverted at any
given time.  Unlike, @pr(flash), @pr(invert) depends upon Opal,
so if Opal encounters problems with redisplay, @pr(invert) will 
not work (see @pr(fix-up-window) in Section @ref(fix-up-windows)).

The previous functions are only useful if you know the name
of a graphical object.  To obtain the name of an object that
is visible on the screen, use:
@index(locate)
@begin(programexample)
gd:Ident@value(function)
@index(ident)
@end(programexample)
@pr(Ident) waits for the next input event and reports the object
under the mouse at the time of the event.
In addition to printing the leaf @index(leaf) 
object under the mouse,
@pr(ident) runs up the @pr(:parent) links and prints the chain
of aggregates up to the window.
Some interesting features to note are:
@begin(itemize)
@pr(ident) will report a window if you do not select an object.

@pr(ident) returns a list@foot[A list is returned rather than
a multiple value because multiple values print out on multiple lines
in CMU Common Lisp, taking too much screen space when @pr(ident) is
used interactively.] in the form (@i(object) @i(window) @i(x) @i(y) @i(code))
so you can then use the selection in another expression, e.g.
@pr[(kr:ps (car (ident)))].  @i(Object) will be @pr(nil) if none was
selected.

@pr(ident) also prints the input event and mouse location.  For
instance, use @pr(ident)
if you want to know the Lisp name @index(character code)
for the character 
transmitted when you type the key labeled ``Home'' on your keyboard
or to tell you the window coordinates @index(coordinates)
of the mouse.
@end(itemize)

Another way to locate @index(locate) a window is to use the function:
@begin(programexample)
gd:Windows@value(function)
@index[Windows (debugging function)]
@end(programexample)
which prints a list of Opal windows and their locations.  The
list of windows is returned.  Only mapped windows are listed,
so @pr(windows) will only report a window that has been
@pr(opal:update)'d.  @index(update)  For example:
@begin(programexample)
* (windows)
#k<MYWINDOW> :TOP 43 :LEFT 160 :WIDTH 355 :HEIGHT 277
#k<DEMO-GROW::VP> :TOP 23 :LEFT 528 :WIDTH 500 :HEIGHT 300
--> (#k<DEMO-GROW::VP> #k<MYWINDOW>)
@end(programexample)

@chapter(Inspecting Constraints)
@index(constraint)
Formulas@index(formula) often have unexpected values, and program listings do
not always help when formulas and objects are inherited and/or
created at run time.  To make dependencies @index(dependencies) 
explicit, the
@pr(explain-slot)@index(explain-slot)@index(slot) function can be used:
@begin(programexample)
gd:Explain-Slot @i(object slot)@value(function)
@end(programexample)
@pr(explain-slot) will track down all dependencies of @i(object)'s
@i(slot) and prints them.  Indirect dependencies that occur when
a formula depends upon the value of another formula are also
printed.  The complete set of dependencies is a directed graph,
but the printout is tree-structured, representing a depth-first
traversal of the graph.  The search is cut off whenever a 
previously visited node is encountered.  This can represent
either a cycle or two formulas with a common dependency.

In the following example, the @pr(:top) of @pr(mytext) depends
upon the @pr(:top) of @pr(myrect) which in turn depends upon its
own @pr(:box) slot:
@begin(programexample)
* (explain-slot mytext :top)
#k<MYTEXT>'s :TOP is #k<F2449> (20 . T),
which depends upon:
   #k<MYRECT>'s :TOP is #k<F2439> (20 . T),
   which depends upon:
      #k<MYRECT>'s :BOX is (80 20 100 150)
--> NIL
@end(programexample)

When @pr(explain-slot) is too verbose, a non-recursive version
can be used:@index(explain-short)
@begin(programexample)
gd:Explain-Short @i(object slot)@value(function)
@end(programexample)
For example:
@begin(programexample)
* (explain-short mytext :top)
#k<MYTEXT>'s :TOP is #k<F2449> (20 . T),
which depends upon:
   #k<MYRECT>'s :TOP is #k<|1803-2439|> (20 . T),
      ...
--> NIL
@end(programexample)

@p(Warning:) @pr(explain-slot) and @pr(explain-short)
may produce incorrect results in the following ways:
@begin(itemize)
Both @pr(explain-slot) and @pr(explain-short) rely on dependency
pointers maintained for internal use by KR.
In the present version,
KR sometimes leaves dependencies around that are no longer current.
This is not a bug because, at worst, extra dependencies only cause
formulas to be reevaluated unnecessarily.  However, this may cause
@pr(explain-slot) or @pr(explain-short) to print extra dependencies.

Formulas may access slots but not use the values.  This will create
the appearance of a dependency when none actually exists.

Formulas that that try to follow a null link,@index(null link) e.g. 
@pr[(gv :self :feedback-obj :top)] where @pr[:feedback-obj] is
@pr(nil), may be marked as invalid and have their dependency lists cleared.
@pr(explain-slot) and @pr(explain-short) will detect this case and
warn you if it happens.
@end(itemize)


@Chapter(Choosing Constant Slots)

Since the use of constants can significantly reduce the storage requirements
and execution time of an application, we have provided several new functions
that help you to choose which slots should be declared constant.  The following
functions are used in conjunction to identify slots that are candidates for
constant declarations.


@Section(Suggest-Constants)
@label(suggest-constants)

@index(record-from-now) @index(suggest-constants)
@begin(programexample)
gd:Record-From-Now@value(function)

gd:Suggest-Constants @i(object) &key @i(max) (@i[recompute-p] T) (@i[level] 1)@value(function)
@end(programexample)

To use these functions, bring up the application you want to analyze, and
execute @pr(record-from-now).  Exercise all the parts and gadgets of the
interface that are expected to be operated during normal use, and then
call @pr(suggest-constants).  Information will be printed out that identifies
slots which, if declared constant, would cause dependent formulas to be
replaced by their actual values.

Keep in mind that it is usually not necessary to declare every reported slot
constant.  Many formulas will @u(become) constant if they depend on constant
slots.  For example, declaring many of the parameters of a
@pr(gg:text-@|button-@|panel) constant in the top-level gadget is sufficient
to eliminate the internal formulas that depend on them.

Also, it is important to exercise @u(all) parts of the application in order to
get an accurate list of constant slot candidates.  If you forget to operate
a certain button while recording, slots may be suggested that would cause the
button to become inoperable, since @pr(suggest-constants) would assume it
was a static object.

@pr(Suggest-constants) will tell you if a potential slot is in the object's
@pr(:maybe-constant) list.  When the slot is in this list, then it can be
declared constant by supplying the value of T in the @pr(:constant) list.
As you add constants, though, you may want to carefully name each slot
individually in the @pr(:constant) list to avoid erroneous constant
declarations.

The parameters to @pr(suggest-constants) are used as follows:

@begin(description, indent=-2)
@i(object) - This can be any Garnet object, but it is usually a window or its
top-level aggregate.  The function examines formulas in @i(object) and
all its children.

@i(max) - This parameter controls how many constant slot candidates are printed
out.  The default is to print all potential constant slots that are found
in @i(object) and all its children.

@i(recompute-p) - Set this parameter to NIL if you do not need to reexamine
all the objects and you trust what was computed earlier (the same information
that was printed out before will be printed out again, without checking that
it is still valid).

@begin(multiple)
@i(level) - The default value of @i(level), which is 1, causes the
function to print only slots which would, by themselves, eliminate
some formula.  If @i(level) is made higher, slots will be printed that may
not eliminate formulas by themselves, but will at least eliminate some
dependencies from the formulas that remain.

For example, consider a formula that depends on slots A and B.  Declaring
constant either A or B alone would not eliminate the formula, so with @i(level)
set to 1, slots A and B would not be suggested by @pr(suggest-constants).
Setting @i(level) to 2, however, will printe both A and B, since the
combination of the two slots would indeed eliminate a formula.  Higher
values of @i(level) make @pr(suggest-constants) print out formulas that are
less and less likely to eliminate formulas.
@end(multiple)
@end(description)


@section(Explain-Formulas and Find-Formulas)

@begin(programexample)
@index(explain-formulas)
gd:Explain-Formulas @i(aggregate) &optional (@i[limit] 50) @i[eliminate-useless-p]@value(function)
@end(programexample)

@pr(Explain-formulas) is used to analyze all the formulas that were @u(not)
evaluated since the last call to @pr(record-from-now).  These formulas might
have been evaluated when the application was first created, to position the
objects appropriately, but are not a dynamic part of the interface, and are
thus candidates for constant declarations.
If the @i(eliminate-useless-p) option is non-NIL, then formulas that are 
in fact unnecessary (i.e., would go away if they were recomputed) are actually
eliminated immediately.


@begin(programexample)
@index(find-formulas)
gd:Find-Formulas @i(aggregate) &optional (@i[only-totals-p] T) (@i[limit] 50) @i[from]@value(function)
@end(programexample)

If the function @pr(find-formulas) is called with a non-NIL @i(only-totals-p)
option, it will print out the total number of formulas that have not
been reevaluated since the last call to @pr(record-from-now).
If @i(only-totals-p) is NIL and @i(limit) is specified,
it will print out at most @i(limit) formula names.  If @i(limit) is NIL,
all formula names will be printed out.

You will seldom need to specify the @i(from) parameter.  This allows you
to print out formulas that have been unevaluated since @i(from).  The
default value is the number returned by the last call to
@pr(record-from-now); specifying a smaller number reduces the number of
formulas that are printed out, since formulas that were evaluated
earlier are discarded.


@begin(group)
@section(Count-Formulas and Why-Not-Constant)
@label(count-formulas)

@index(count-formulas)
@begin(programexample)
gd:Count-Formulas @i(object)@value[function]
@end(programexample)

@pr(Count-formulas) will print a list of all existing formulas in @i(object)
and all its children.  It is important to note that formulas are not copied
down into an object until they are specifically requested by a @pr(g-value)
or @pr(gv) call.  Thus, you may not get an accurate count of the real number
of formulas in an object until you exercise the object in its intended way.
For example, if a prototype @pr(A) has a formula in its
@pr(:left) slot and you count the formulas in @pr(B), an instance of @pr(A),
before asking for @pr(B)'s @pr(:left) slot, then @pr(B)'s @pr(:left) formula
will not be counted, because it has not been copied down yet.
@end(group)
@blankspace(1 line)


@begin(group)
@index(why-not-constant)
@begin(programexample)
gd:Why-Not-Constant @i(object  slot)@value[function]
@end(programexample)

This function is extremely useful when you are trying to get rid of formulas
by declaring constant slots.  If @pr(count-formulas) tells you that formulas
still exist in your application that you think should go away due to
propagation of constants, then you can call @pr(why-not-constant) on a
particular slot to find out what its formula depends on.  The function will
print out a list of dependencies for the formula in the @i(slot), which will
give you a hint about what other slot could be declared constant to make
this formula go away.
@end(group) 



@Chapter(Noticing when Slots are Set)
@label(notify-on-slot-set-sec)

It is often useful to be notified when a slot of an object is set, so now
we provide a set of debugging functions that do this.   There is also an
interface to these functions through the @pr(Inspector) (section
@ref(inspectorsec)) which makes them more convenient to use.

Note that the
implementation of this is @i(very) inefficient and is intended only
for debugging.  Don't use this as general-purpose demon technique
since a search is performed for @i(every) formula evaluation and every
slot setting when any notifies or breaks are set.

@index(Notify-On-Slot-Set)
@begin(programexample)
gd:Notify-On-Slot-Set &key @i(object  slot  value) @value(function)
@end(programexample)

This will print out a message in the Lisp Listener window whenever the
appropriate slot is set.  If a value is supplied, then only notifies
when the slot is set to that particular value.  
If an object is provided, then only notifies
when a slot of that object is set.  If no object is supplied, then
notifies whenever @i(any) object is set.  If a slot is provided, then
only notifies when that slot is set.  If no slot is supplied, then
notifies whenever @i(any) slot is set.  If object is NIL, then clears
all breaks and notifies.  If all parameters are missing, then shows
current status.  For example,
@begin(programexample)
(gd:Notify-On-Slot-Set :object obj :slot :left) @i[;notify when :left of obj set]
(gd:Notify-On-Slot-Set :object obj :slot :left :value 0) @i[;notify when :left of obj set to 0]
(gd:Notify-On-Slot-Set :value NIL) @i[;notify when any slot of any obj set to NIL]
(gd:Notify-On-Slot-Set :object obj) @i[;notify when any slot of obj set]
@end(programexample)

Each call to @pr(Notify-On-Slot-Set) adds to the previous list of
breaks and notifies, unless the object is NIL.  You can use
@pr(clear-slot-set) to remove a break or notify (see below).

@index(Break-On-Slot-Set)
@begin(programexample)
gd:Break-On-Slot-Set &key @i(object  slot  value) @value(function)
@end(programexample)

Same as @pr(Notify-On-Slot-Set), but breaks into the debugger when the
appropriate slot is set.

@index(Call-Func-On-Slot-Set)
@begin(programexample)
gd:Call-Func-On-Slot-Set @i(object  slot  value  fnc  extra-val) @value(function)
@end(programexample)
This gives you more control, since you get to supply the function
that is called when the appropriate slot is set.   The parameters here are not
optional, so if you don't want to specify the object, slot or value,
use the special keyword @pr(:*any*).  The function @pr(fnc) is called as:
@begin(programexample)
(lambda (obj slot val reason extra-val))
@end(programexample)
where the @i(slot) of @i(obj) is being set with @i(val).  The
@i(reason) explains why the slot is being set and will be one
of @c(:s-value), @c(:formula-evaluation), @c(:inheritance-propagation) or
@c(:destroy-slot).  @i(Extra-val) can be anything 
and is the same value passed into @pr(Call-Func-On-Slot-Set).

@index(Clear-Slot-Set)
@begin(programexample)
gd:Clear-Slot-Set &key @i(object  slot  value) @value(function)
@end(programexample)
Clear the break or notify for the object, slot and value.  If nothing is
specified or object is NIL, then clears all breaks and notifies.



@chapter(Opal Update Failures)
@index(update)
@label(fix-up-windows)
Opal assumes that graphical objects have valid display parameters
such as @pr(:top) or @pr(:width).  If a parameter is computed
by formula and there is a bug, the problem will often cause an
error within Opal's @pr(update) function.

A "quarantine slot" named @pr(:in-progress) exists in all Garnet windows.
If there was a crash during the last update of the window, then the window
will stop being updated automatically along with the other Garnet windows,
until you can fix the problem and update the window successfully.  The
quarantine slot is discussed in detail in the Opal Manual.

There are several ways to proceed after an update failure.
The first and easiest action is to run @pr(opal:update) with
the optional parameter @pr(t):
@begin(programexample)
(opal:update window t)
@end(programexample)
This forces @pr(opal:update) to do a complete update of @pr(window)
as opposed to an incremental update.  This may fix your problem
by bringing all slots up-to-date and expunging previous display
parameters.

Another possibility is, after entering the debugger, call
@begin(programexample)
gd:Explain-NIL@value(function)
@end(programexample)
This function@index(explain-nil) 
will check to see if a formula tried to follow a
null link @index(null link)
(a typical cause of Opal object slots becoming @pr(nil)).
If so, the object and slot associated with the formula will be
printed followed by objects and slots on which the 
formula depends@foot[@pr(Explain-nil) does not use the same
technique for finding dependencies as @pr(explain-slot), which
uses forward pointers from the formula's @pr(:depends-on) slot. 
Since @pr(:depends-on) is currently cleared when a null link is
encountered, @pr(explain-nil) uses back pointers from the
objects back to the formula.  These are in the @pr(:depended-upon)
slot of objects.  To locate the back pointers, @pr(explain-nil)
searches for all components of all Opal windows.  Only objects in
windows are searched, so dependencies on non-graphical objects
will be missed.].
One of the slots depended upon will be the null link that caused
the formula to fail. 

@p(Warning:) @pr(explain-nil) will always
attempt to describe the last formula that failed due to a null
link @i(since the last time @pr(explain-nil) was evaluated).
This may or may not be relevant to the bug you are searching
for.  The last error is cleared every time @pr(explain-nil) is
evaluated to reduce confusion over old errors.
If there has been no failure, @pr(explain-nil) will print
@begin(programexample)
No errors in formula evaluation detected
@end(programexample)

A third possibility is to run@index(fix-up-window)
@begin(programexample)
gd:Fix-Up-Window @i(window)@value(function)
@end(programexample)
on the window in question.  (You may want to use @pr(windows) to
find the window object.)  @pr(fix-up-window) will do type checking
@index(type checking)
without attempting a redisplay.  If an error is detected, 
@pr(fix-up-window) will allow you to interactively remove 
objects with problems from the window.

After fixing the problem that caused @pr(update) to crash, you should be
able to do a successful total update on the window (discussed above).
A successful total update will clear the quarantine slot, and will allow
interactions to take place in the window normally.



@chapter(Inspecting Interactors)
@index(interactors)
@section(Tracing)
@index(tracing)
A common problem is to create some graphical objects and an interactor 
but to discover that nothing happens when you try to interact with
the program.  If you know what interactor is not functioning, then
you can trace its behavior using the function
@begin(programexample)
inter:Trace-Inter @i(interactor)@value(function)
@index(trace-inter)
@end(programexample)
This function enables some debugging printouts in the interactors 
package that should help you determine what is wrong.  A set of
things to trace is maintained internally, so you can call
@pr(inter:trace-inter) several times to trace several things.
In addition to interactors, the parameter can be one of:
@begin(itemize)
@pr(t) @dash trace everything.

@pr(NIL) @dash untrace everything, same as calling @pr(inter:untrace-inter).

@pr(:window) @dash trace things about interactor windows such as
@pr(create) and @pr(destroy) operations.

@pr(:priority-level) @dash trace changes to priority levels.

@pr(:mouse) @dash trace @pr(set-interested-in-moved) and @pr(ungrab-mouse).

@pr(:event) @dash show all events that come in.

@pr(:next) @dash start tracing when the next interactor runs, and trace
that interactor.

@pr(:short) @dash report only the name of the interactor that runs, so that
the output is much less verbose.  This is very useful if you suspect that more
than one interactor is accidentally running at a time.
@end(itemize)
Tracing any interactor will turn on @pr(:event) tracing by default.  Call
@pr[(inter:untrace-inter :event)] (see below) to stop @pr(:event) tracing.

Just typing
@begin(programexample)
(inter:trace-inter)
@end(programexample)
will print out the interactors currently being traced.

@begin(programexample)
inter:Untrace-Inter @i(interactor)@value(function)
@index(untrace-inter)
@end(programexample)
can be used to selectively stop tracing a single interactor or
other category.  You can also
pass @pr(t) or @pr(nil), or no argument to @pr(untrace) to stop all tracing:
@begin(programexample)
(inter:untrace-inter)
@end(programexample)


@section(Describing Interactors)
If you are not debugging a particular interactor, there are a few
ways to proceed other than wading through a complete interactor trace.
First, you can find out what interactors are active by calling:
@begin(programexample)
gd:Look-Inter &optional @i(interactor-or-object) @i(detail)@value(function)
@index(look-inter)
@end(programexample)

The parameter @i(interactor-or-object) can be:

@begin(itemize)
NIL to list all active interactors (see below),

an interactor to describe,

a window, to list all active interactors on that window,

an interactor priority-level, to list all active interactors on that level,

a graphical object, to try to find all interactors that affect that object,

@pr(:next) to wait and describe the next interactor that runs
@end(itemize)

With no arguments (or NIL as an argument), @pr(look-inter) will print all
active interactors
(those with their @pr(:active) and @pr(:window) slots set to something)
sorted by priority level @index(priority level):
@begin(programexample)
* (look-inter)
Interactors that are :ACTIVE and have a :WINDOW are:
Level #k<RUNNING-PRIORITY-LEVEL>: 
Level #k<HIGH-PRIORITY-LEVEL>: #k<DEMO-GROW::INTER2> 
Level #k<NORMAL-PRIORITY-LEVEL>: #k<MYTYPER> #k<MYMOVER> #k<DEMO-GROW::INTER3>
 #k<DEMO-GROW::INTER4> #k<DEMO-GROW::INTER1>
--> NIL
@end(programexample)

If @i(detail) is 1, @pr(look-inter) will show the @pr(:start-event) and 
@pr(:start-where) of each active interactor:
@begin(programexample)
* (look-inter 1)
Interactors that are :ACTIVE and have a :WINDOW are:
Level #k<RUNNING-PRIORITY-LEVEL>: 
Level #k<HIGH-PRIORITY-LEVEL>: #k<DEMO-GROW::INTER2> 
Level #k<NORMAL-PRIORITY-LEVEL>: #k<MYTYPER> #k<MYMOVER> #k<DEMO-GROW::INTER3>
 #k<DEMO-GROW::INTER4> #k<DEMO-GROW::INTER1>
#k<DEMO-GROW::INTER2> (MOVE-GROW-INTERACTOR)
   starts when :LEFTDOWN (:ELEMENT-OF #k<AGGREGATE-164>)
#k<MYTYPER> (TEXT-INTERACTOR)
   starts when :RIGHTDOWN (:IN #k<MYTEXT>)
#k<MYMOVER> (MOVE-GROW-INTERACTOR)
   starts when :LEFTDOWN (:IN #k<MYRECT>)
#k<DEMO-GROW::INTER3> (MOVE-GROW-INTERACTOR)
   starts when :MIDDLEDOWN (:ELEMENT-OF #k<AGGREGATE-136>)
#k<DEMO-GROW::INTER4> (MOVE-GROW-INTERACTOR)
   starts when :RIGHTDOWN (:ELEMENT-OF #k<AGGREGATE-136>)
#k<DEMO-GROW::INTER1> (BUTTON-INTERACTOR)
   starts when :LEFTDOWN (:ELEMENT-OF-OR-NONE #k<AGGREGATE-136>)
--> NIL
@end(programexample)
To get information about a single interactor, pass
the interactor as a parameter:
@begin(programexample)
* (look-inter mymover)
#k<MYMOVER>'s :ACTIVE is T, :WINDOW is #k<MYWINDOW>
#k<MYMOVER> is on the #k<NORMAL-PRIORITY-LEVEL> level
#k<MYMOVER> (MOVE-GROW-INTERACTOR)
   starts when :LEFTDOWN (:IN #k<MYRECT>)
--> NIL
@end(programexample)

In some cases you need to know what interactor will affect a given
object (perhaps located using the @pr(ident) function).  This is
not possible in general since the object(s) an interactor
changes may be referenced by arbitrary application code.  However,
if you use interactors in fairly generic ways, you can call
@pr(look-inter) with a graphical object as argument to search
for relevant interactors:
@begin(programexample)
* (look-inter myrect)
#k<MYMOVER>'s :start-where is (:IN #k<MYRECT>)
--> NIL
* (look-inter mytext)
#k<MYTYPER>'s :start-where is (:IN #k<MYTEXT>)
--> NIL
@end(programexample)

The search algorithm used by look-inter is fairly simple:
the current value of @pr(:start-where) is interpreted to see
if it could refer to the argument.  Then the @pr(:feedback-obj)
and @pr(:obj-to-change) slots are examined for an exact match
with the argument.  If formulas are encountered, only the current
value is considered, so there are a number of ways in which
@pr(look-inter) can fail to find an interactor.

@chapter(Sizes of Objects)
@index(size)
Several functions are provided to help make size measurements of 
Opal objects and aggregates.
@begin(programexample)
gd:ObjBytes @i(object)@value(function)
@end(programexample)
will measure the size of a single Opal object or interactor in bytes.

@begin(programexample)
gd:AggBytes @i(aggregate) &optional @i(verbose)@value(function)
@end(programexample)
will measure the size of an Opal aggregate and all of its
components in bytes.  The first
argument may also be a list of aggregates, a window, or a list
of windows.  For example, to compute the total size of all graphical
objects, you can type this:
@begin(programexample)
(aggbytes (windows))
@end(programexample)
The output will include various statistics on size according
to object type.  Sizes are printed in bytes, and
the returned value will be the total size in 
bytes.  The size information @i(does not) include any interactors
because interactors can exist independent of the 
aggregate hierarchy.  
The optional @i(verbose) flag defaults to @pr(t); setting
it to nil will reduce the detail of the printed information.

@begin(programexample)
gd:InterBytes &interactor @i(window) @i(verbose)@value(function)
@end(programexample)
will report size information on the interactors whose @pr(:window)
slot @i(currently) 
contains the specified window.  If the @i(window) parameter is
omitted, @pr(t) or @pr(nil), then the size of all interactors is
computed.  (Use @pr(objsize) for a single interactor.)  Note that
an interactor may operate in more than one window and that interactors
can follow objects from window to window.  As with
@pr(aggbytes), the @i(verbose) flag defaults to @pr(t); setting
it to nil will reduce the detail of the printed information.

@begin(programexample)
gd:*Avoid-Shared-Values*@value(variable)
@end(programexample)
@index(*avoid-shared-values*)
Normally, @pr(aggbytes) does not consider the fact that list
structures may be shared, so shared storage is counted multiple
times.  To avoid this (at the expense of using a large hash table),
set @pr(avoid-shared-values) to @pr(t).

@begin(programexample)
gd:*Avoid-Equal-Values*@value(variable)
@end(programexample)
@index(*avoid-equal-values*)
To measure the potential for sharing, set this variable to @pr(t).
This will do hashing using @pr(#'equal) so that equal values will
be counted as shared instead of @pr(#'eq), which
measures actual sharing.

@begin(programexample)
gd:*Count-Symbols*@value(variable)
@end(programexample)
@index(*count-symbols*)
Ordinarily, storage for object names is not counted as part of the
storage for objects.  By setting this variable for true, the
sizes reported by @pr(objbytes) and @pr(aggbytes) will include
this additional symbol storage overhead.

@b(Note:) Size information for an object includes the size of
any attached formulas.  At present, only objects and cons cells
are counted.  Storage for structures (other than KR
schema), strings, and arrays is @i(not) counted.  


@UnNumbered(References)
@bibliography
