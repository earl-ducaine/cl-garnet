@device(postscript)
@Make(manual)
@disable(figurecontents)
@LibraryFile(Garnet)
@String(TitleString = "KR")
@use(Bibliography = "garnet.bib")
@begin(TitlePage)
@begin(TitleBox)
@blankspace(0.6 inch)
@Bg(KR: Constraint-Based Knowledge Representation)

@b(Dario Giuse)
@BlankSpace(0.3 line)
@value[date]
@end(TitleBox)
@BlankSpace(0.1 inch)
@center[@b(Abstract)]
@begin(Text, spacing=1.1)
KR is a very efficient knowledge representation language implemented in
Common Lisp.  It provides powerful frame-based knowledge representation
with user-defined inheritance and relations, and an integrated
object-oriented programming system.  In addition, the system supports a
constraint maintenance mechanism which allows any value to be computed from
a combination of other values.  KR is simple and compact and does not
include some of the more complex functionality often found in other
knowledge representation systems.  Because of its simplicity, however, it
is highly optimized and offers good performance.  These qualities make it
suitable for many applications that require a mixture of good performance
and flexible knowledge representation.


@blankspace(0.3 inch)
@include(creditetc.mss)
@End(Text)
@end(TitlePage)



@string(version = "2.3.1")
@string(nil = "@c[nil]")
@string(t = "@c[t]")
@string(top = "")
@string(s-top = "@blankspace(0.1 inch)")
@string(f-top = "@blankspace(0.2 inch)")

@include(pagenumbers.mss)
@set(page = kr-first-page)

@chapter(Introduction)
@value(top)


This document is the reference manual for the KR system.  KR implements
objects, also known as @i(schemata), which can contain any amount of
information
and which can be connected in arbitrary ways.  All Garnet objects are
implemented as KR schemata.  KR @cite(kr) can also be used as a very
efficient frame-based representation system.  Simplicity and efficiency are
its main design goals and differentiate it sharply from more
conventional frame systems, as discussed in @cite(KR-KER).

In addition to the basic representation of knowledge as a network of
schemata, KR provides object-oriented programming and an integrated
constraint maintenance system.  Constraint maintenance is implemented
through @i(formulas), which constrain certain values to combinations
of other values.  The constraint system is closely integrated with the
basic object system and is part of the same program interface.

Close integration between objects and constraint maintenance yields
several advantages.  First of all, constraint maintenance is seen as a
natural extension of object representation; the same access functions work
on regular values and on values constrained by a formula.  Second, the full
power of the representation language is available in the specification of
constraints.  Third, since the two mechanisms are integrated at a fairly
low level, the constraint maintenance system offers very good performance.
These advantages make the KR constraint maintenance system a
practical tool for the development of applications that require
flexibility, expressive power, and performance comparable to that obtained
with conventional data structures.

In addition to being one of the building blocks of the Garnet project, KR
can be used as a self-contained knowledge representation system.  Besides
Garnet, KR is used in the Chinese Tutor @cite(CHINESE-TUTOR)
@cite(CHINESE-TUTOR-SHANGHAI), an intelligent tutoring system designed to
teach Chinese to English speakers, and in speech understanding research
@cite(MINDS) currently underway at Carnegie Mellon.

This document describes version @value(version) of KR, which is part of
release 2.2 of the Garnet system.  Several aspects of this version differ
from previous versions of the system, such as the ones described in
previous reports @cite(KRTR2) @cite(KR).  The present document overrides
all previous descriptions.

The orientation of this manual is for users of KR as an object system.
Users who are more interested in using KR as a knowledge representation
system should consult a previous paper @cite(kr-ker).  This manual begins
with a description of the features of the system that beginners are most
likely to need.  Some of the less common features are only presented near
the end of the document, in order to avoid obscuring the description with
irrelevant details.  Sections 6 and 7 contain the detailed description of
the program interface of KR.  This is a complete description of the system
and its features.  Most application programs will only need a small number
of features, described in section 6.





@chapter(Structure of the System)
@value(top)

KR is an object system implemented in Common Lisp @cite(CommonLisp).  It
includes three closely integrated components: @i(object-oriented
programming), @i(constraint maintenance), and @i(knowledge representation).

The first component of KR is an object oriented programming system based on
the prototype-instance paradigm.  Schemata can be used as objects, and
inheritance can be used to determine their properties and behavior.
Objects can be sent @i(messages), which are implemented as procedural
attachments to certain slots; messages are inherited through the same
mechanism as values.

Instead of the class-instance paradigm, common in object-oriented
programming languages, KR implements the more flexible prototype-instance
paradigm @cite(liebermanprototypes), which allows properties of instances
to be determined dynamically by their prototypes.  This means that the
class structure of a system can be modified dynamically as needed, without
any need for recompilation.

The second component of KR implements constraint maintenance.  Constraint
maintenance is implemented through @i(formulas), which may be attached to
slots and determine their values based on the values of other slots in the
system.

Constraint maintenance is closely integrated with the other components.
The user, for example, does not need to know which slots in a schema
contain ordinary values and which ones are constrained by a formula, since
the same access primitives may be used in both cases.

The third component, frame-based knowledge representation, stores knowledge
as a network of chunks of information.  Networks in KR are built out of
unstructured chunks, i.e., @i(schemata).  Each schema can store
arbitrary pieces of information, and is not restricted to a particular
format or data structure.  Information is encoded via attribute-value
pairs.

Values in a schema can be interpreted as links to other schemata.  This
enables the system to support complex network structures, which can be
freely extended and modified by application programs.  KR provides simple
ways to specify the structure of a network and the relationship among its
components.



@chapter(Basic Concepts)
@value(top)

This section describes the basic elements of KR, i.e., objects.  More
details about the design philosophy of the system and some of the
internal implementation may be found in @cite(KR), which describes a
previous version of the system that did not support constraint
maintenance.


@section(Main Concepts: Schema, Slot, Value)
@value(s-top)

An object in KR is known as a @i(schema).  A schema@index(schema) is the
basic unit of representation and consists of an optional @i(name), a
set of @i(slots), and a @i(value) for each slot.  The user can
assemble networks of schemata by placing a schema as the value in a slot of
another schema; this causes the two schemata to become linked.

A schema@index(schema names) may be named@index(named schemata) or
unnamed.  Named schemata are readily accessible and are most useful
for interactive situations or as the top levels of a hierarchy, since
their names act as global handles.  Unnamed schemata@index(unnamed
schemata) do not have meaningful external names.  They are, however,
more compact than named schemata and account for the vast majority of
schemata created by most applications.  Unnamed schemata are
automatically garbage-collected when no longer needed, whereas named
schemata have to be destroyed explicitly by the user.

The name of a named schema is a symbol.  When a named schema is created, KR
automatically creates a special variable by the same name @index(schemata
and variables) and assigns the schema itself as the value of the special
variable.  This makes named schemata convenient to use.

A schema may have any number of @i(slots), which are simply
@index(slot) attribute-value pairs.  The slot name indicates the
attribute name; the slot value (if there is one) indicates its value.  Slot
names @index(slot names) are keywords, and thus always begin with a
colon.  All slots in a schema must have distinct names, but different
schemata may very well have slots with the same name.

Each slot may contain only one value.
A value@index(value) is the actual data item stored in the schema, and may
be of any Lisp type.  KR provides functions to add, delete, and retrieve
the value from a given slot in a schema.

The printed representation of a schema shows the schema name followed by
slot/value pairs, each on a separate line.  The whole schema is surrounded
by curly braces.  For example,
@begin(programexample)
{#k<fido>
  :owner =  #k<john>
  :color =  #k<brown>
  :age =  5
}
@end(programexample)

The schema is named FIDO and contains three slots named
@pr(:owner), @pr(:color), and @pr(:age).  The slot @pr(:age) contains one
value, the integer 5.

The default printed name of a schema is of the form @c(#k<name>), where
@i(name) is the actual name of the schema.  This representation makes it
very easy to distinguish KR schemata from other objects.  Note, however,
that this convention is only used when printing, and is not used when
typing the name of a schema.



In order to illustrate the main features of the system, we will repeatedly
use a few schemata.  We present the definition of those schemata at this
point and will later refer to them as needed.  These schemata might be part
of some graphical package, and are used here purely for explanation
purposes.  In practice, there is no need to define such schemata in a
Garnet application, since the Opal component of Garnet (see the Opal
manual) already provides a complete graphical object system.

The following KR code is the complete definition of the example schemata:
@label(kr-examples)

@begin(programexample)
@index(my-graphical-object)
(create-instance 'MY-GRAPHICAL-OBJECT NIL
  (:color :blue))

@index(box-object)
(create-instance 'BOX-OBJECT MY-GRAPHICAL-OBJECT
  (:thickness 1))

@index(rectangle-1)
(create-instance 'RECTANGLE-1 BOX-OBJECT
  (:x 10)
  (:y 20))

@index(rectangle-2)
(create-instance 'RECTANGLE-2 BOX-OBJECT
  (:x 34)
  (:y (o-formula (+ (gvl :left-obj :y) 15)))
  (:left-obj RECTANGLE-1))
@end(programexample)

The exact meaning of the expressions above will become clear after we
describe the functional interface of the system.  Briefly, however,
the example can be summarized as follows.  The schema
@c(my-graphical-object) is at the top of a hierarchy of graphical
objects.  The schema @c(box-object) represents an intermediate level
in the hierarchy, and describes the general features of all graphical
objects which are rectangular boxes.  @c(box-object) is placed below
@c(my-graphical-object) in the hierarchy, and its @pr(:is-a) slot
points to the schema @c(my-graphical-object).  This is done
automatically by the macro create-instance.

Finally, two rectangles (@c(rectangle-1) and @c(rectangle-2)) are
created and placed below @c(box-object) in the hierarchy.  @c(rectangle-1)
defines the values of the two slots @pr(:x) and @pr(:y) directly, whereas
@c(rectangle-2) uses a formula for its @pr(:y) slot.  The formula states
that the value of @pr(:y) is constrained to be the @pr(:y) value of another
schema plus 15.  The other schema can be located by following the
@pr(:left-obj) slot of @c(rectangle-2), as specified in the formula, and
initially corresponds to @c(rectangle-1).

Figure @ref(all-schemata) shows the four schemata after the definitions
above have been executed.  Relations are indicated by an arrow going from a
schema to the ones to which it is related.

@begin(figure)
@bar()
@center(@graphic(Postscript = "kr/schemata.ps",magnify=.75,boundingbox=File))
@caption(The resulting network of schemata)
@bar()
@tag(all-schemata)
@end(figure)


Asking the system to print out the
current status of schema @c(rectangle-2) would produce the following output:
@begin(programexample)
{#k<RECTANGLE-2>
  :IS-A =  #k<BOX-OBJECT>
  :LEFT-OBJ =  #k<RECTANGLE-1>
  :Y =  #k<F2289>(NIL . NIL)
  :X =  34
}
@end(programexample)

Note that slot @pr(:y) contains a formula, which is printed as
"#k<F2289>(NIL . NIL)".  This is simply an internal representation for the
formula and will yield the correct value of @pr(:y) when needed.




@section(Inheritance)
@value(s-top)

The primary function of values is to provide information about the object
represented by a schema.  In the previous example, for instance, asking the
system for the @pr(:x) value of @c(rectangle-1) would simply return the
value @pr(10).

Values@index(values as links) can also perform another function,
however: They can establish @i(connections between schemata).
Consider the @pr(:left-obj) slot in the example above: if we interpret
@c(rectangle-1) as a schema name, then the slot tells us that the
schema @c(rectangle-2) is somehow related to the schema
@c(rectangle-1).  Graphically, this will mean that the position of
@c(rectangle-2) is partially determined by that of @c(rectangle-1).


KR also makes it possible to use values to perform @i(inheritance),
@index(inheritance)
i.e., to control the way information is inherited by a
particular schema from other schemata to which it is connected.
Inheritance allows information to be arranged in a hierarchical fashion,
with lower-level schemata inheriting most of their general features from
higher-level nodes and possibly providing local refinements or
modifications.  A connection that enables inheritance of values is called an
@i(inheritance relation)@index(relation).  Inheritance relations
always contain a list of values;  in many cases, this is a list of
only one value.

The most common example of inheritance is provided by the @pr(:is-a)
relation@index(is-a relation).  If schema A is connected to schema B by the
@pr(:is-a) relation,@footnote(In other words, if schema B appears as a value
in the @pr(:is-a) slot of schema A.) then values that are not present in A
may be inherited from B.

Consider the schema @c(rectangle-1) in our example.  If we were to ask
"What is the color of @c(rectangle-1)?", we would not be able to find the
answer by just looking at the schema itself.  But since we stated that
@c(rectangle-1) is a box object, which is itself a graphical object,
the value can be inherited from the schema @c(my-graphical-object)
through two levels of @pr(:is-a).  The answer would thus be
"@c(rectangle-1) is blue."  Inheritance is possible in this case because
the slot @pr(:is-a) is pre-defined by the system as a relation.





@chapter(Object-Oriented Programming)
@label(object-oriented-prog)
@value(top)


This section describes the object-oriented programming component of KR.
@index(object-oriented programming) This component entails two concepts:
the concept of message sending, and the concept of prototype/instance.

@Section(Objects)

The fundamental data structure in KR is the @i(schema), which is equivalently
referred to as an @i(object).  Objects consist of data
(represented by values in slots) and methods (represented by
procedural attachments, again stored as values in slots).  Methods are
similar to functions, except that a method can do something different
depending on the object that it is called on.
A procedural attachment is invoked by "sending@index(sending messages) a
message" to an@index(messages) object; this means that a method by the
appropriate name is sought and executed.  Different objects often
provide different methods by the same name, and thus respond to the
same message by performing different actions.

The data and methods associated with an object can be either stored
within the object or inherited.  This allows the behavior of objects
to be built up from that of other objects.  The object-oriented
component of KR allows some combination of methods@index(method
combination), since a method is allowed to invoke the corresponding
method from an ancestor schema and to explicitly refer to the object
which is handling the message.  Method combination, however, is not as
developed as in full-fledged object-oriented programming systems such
as CLOS @cite(CLOS-X3J13).


@Section(Prototypes vs. Classes)

The notion of @index(prototype/instance) @i(prototype)
@index(prototypes) in KR is superficially similar to that of @i(class) in
conventional object-oriented programming languages, since a prototype
object can be used to partially determine the behavior of other
objects (its @i(instances))@index(instance).  A prototype, however,
plays a less restricting role than a class.  Unlike classes,
prototypes simply provide a place from which the values of certain
slots may be inherited.  The number and types of slots which actually
appear in an instance is not in any way restricted by the
prototype@index(objects and inheritance).  The same is true for
methods, which are simply represented as values in a slot.

Prototypes in KR serve two specific functions: they provide an
initialization method@index(object initialization), and they provide
default@index(default constraints) constraints@index(object constraints).
When a KR schema is created via the function
@pr(create-instance)@index(create-instance), and its prototype has an
@pr(:initialize)@index(initialize method) method, the method is invoked
on the instance itself.  This results in a uniform mechanism for handling
object-dependent initialization tasks.


@Section(Inheritance of Formulas)
@IndexSecondary(Primary="Formulas", Secondary="Inheritance")

If a prototype provides a constraint for a certain slot, and the slot is
not explicitly redefined in an instance, the formula@index(inherited
formulas) which implements the constraint is copied down and installed in
the instance itself.  The formula, however, is not actually copied down until
a value is requested for that slot (e.g., when @pr(gv) is used).
This is a convenient mechanism through which a
prototype may partially determine the behavior of its instances.  Note that
this behavior can be overridden both at instance-creation time (by
explicitly specifying values for the instance) and at any later point in
time.




@chapter(Constraint Maintenance)
@value(top)


This section describes the constraint maintenance component of KR.
The purpose of constraint maintenance is to ensure that changes to a
schema are automatically propagated to other schemata which depend on
it.



@section(Value Propagation)
@value(s-top)

The KR constraint system@index(constraint maintenance) offers two distinct
mechanisms to cause changes in a part of network to propagate to other
parts of the network.  The first mechanism, @i(value propagation), ensures
that the network is kept in a consistent state after a change.  The second
mechanism, @i(demon invocation), allows certain actions to be triggered
when parts of a network are modified.  Demons are described in section
@ref(demons).

Value propagation is based on the notion of @i(dependency) of a value
on another.  Value dependencies are embodied in formulas.  Whenever a
value in a slot is changed, all slots whose values depend on it are
immediately invalidated, although not necessarily re-evaluated.  This
strategy, known as lazy evaluation, does not immediately recompute the
values in the dependent slots, and thus it typically does less
work@index(lazy evaluation) than an
eager@index(eager evaluation) re-evaluation strategy.  The system simply
guarantees that correct
values are recomputed when actually needed.



@section(Formulas)
@value(s-top)
@Index(Formulas)

Formulas represent one-directional connections between a
@i(dependent value) and any number of @i(depended values).  Formulas
specify an expression which computes the dependent value based upon the
depended values, as well as a permanent dependency which causes the
dependent value to be recomputed whenever any of the other values change.

Formulas can contain arbitrary Lisp expressions, which generally
reference at least one particular depended value.  The Lisp
expression is used to recompute the value of the formula whenever a
change in one of the depended values makes it necessary.

Formulas are not recomputed immediately@index(lazy evaluation) when one of
the depended values changes.  This reduces the amount of unnecessary
computation.  Moreover, formulas are not recomputed every time their value
is accessed.  Each formula, instead, keeps a cache of the last value it
computed.  Unless the formula is marked invalid, and thus needs to be
recomputed, the cached value@index(cached values) is simply reused.  This
factor causes a dramatic improvement in the performance of the constraint
maintenance system, since under ordinary circumstances the rate of change
is low and most changes are local in nature.

@begin(figure)
@bar()
@center(@graphic(Postscript="kr/formulas.ps",magnify=.75,boundingbox=file))
@caption(Successive changes in depended values)
@bar()
@tag(formulas)
@end(figure)


Figure @ref(formulas), part @b{(a)}, shows an example of a formula
installed on slot @pr(:y) of schema @c(point-2).  The formula depends on
two values, i.e., the value of slots @pr(:y1) and @pr(:y2) in schema
@c(point-1).  The formula specifies that slot @pr(:y) is constrained to be
the sum of the two values divided by 2, i.e., the average of the two
values.  Figure @ref(formulas), part @b{(b)}, shows the internal state of
the formula in a steady-state situation where the formula contains a valid
cached value.  Under these circumstances, any request for the value of slot
@pr(:y) would simply return the cached value, without recomputing the
formula.

Parts @b{(c)} and @b{(d)} show the effects of changes to the depended
values.  Changes@index(value propagation) are illustrated by small
rectangles surrounding the modified information.  The first change is to
slot @pr(:y1) and causes the value in the formula to be marked invalid.
Note that the formula is not actually recomputed at this point, and the
cached value is left untouched.  The second change is to slot @pr(:y2) and
does not cause any action to occur, since the formula is already marked
invalid.

Finally, part @b{(e)} shows what happens when the value in slot @pr(:y) is
eventually needed.  The value of the formula is recomputed and again cached
locally; the cache is marked as valid.  The system is then back to steady
state.  Note that the formula was recomputed only once, when needed, rather
than eagerly after each value changed.



@subsection(Circular Dependencies)

Constraints may involve circular chains@index(circular constraints) of
dependency.  Slot A, for instance, might depend on slot B, which in turn
depends on slot A; see section @ref(degrees) for an example of a situation
where this arises fairly naturally.  Circular dependencies may also be used to
provide a limited emulation of two-way constraint maintenance.

KR is able to deal with circular dependencies without any trouble.  This is
handled during formula evaluation; if a formula is evaluated and requests a
value which depends of the formula itself, the cycle is broken and the
cached value of the formula is used instead.  This algorithm guarantees
that the network is left in a consistent state, even though the final
result may of course depend on where evaluation started from.



@subsection(Dependency Paths)

Typical formulas contain embedded references to other values and
schemata.  The formula in Figure @ref(formulas), for example, contains
an indirect reference to schema @c(point-1) through the contents of
the @pr(:other) slot.  Such references are known as
dependency@index(dependency paths) paths@index(paths in formulas).
Whenever a formula is evaluated, its dependency paths are used to
recompute the updated value.

It is possible for a dependency path to become temporarily unavailable.
This would happen, for instance, if schema @c(point-1) in Figure
@ref(formulas) was deleted, or if slot @pr(:other) in schema
@c(point-2) was temporarily set to @value(nil).  KR handles such
situations automatically.  If a formula needs to be evaluated but one
of its dependency paths is broken, the current cached value of the
formula is simply reused.  This makes it completely safe to modify
schemata that happen to be involved in a dependency path, since the
system handles the situation gracefully.


@begin(comment)
@subsection(Constraints and Multiple Values)

Unlike earlier versions of KR, version @value(version) supports constraints
on multiple values@index(multiple values) in a slot.  The functional
interface, however, is not complete and therefore certain operations are
not fully supported at the time of this writing.  Functions which support
constraints on multiple values are easily identified because they accept a
@i(position) parameter which determines what value is affected.

The interaction between constraints and multiple values will be completely
specified in future versions of KR.  For the time being, most applications
should simply be aware that constraints on the first value in a slot are
supported universally, whereas some of the functionality may be unavailable
for constraints on values other than the first one.
@end(comment)





@chapter(Functional Interface: Common Functions)
@value(top)


This section contains a list of the more common functions and macros
exported by the KR interface.  It includes the functionality that most
Garnet users are likely to need and covers schema representation,
object-oriented programming, and constraint maintenance.  Section
@ref(additional-functions) describes parts of the system that are less
commonly used.

All functions and variables are defined and exported by the KR package.
The easiest way to make them accessible to an application program is to
execute the following line:
@programexample[(use-package "KR")]

Throughout this and the following section, we will use the schemata defined
in section @ref(kr-examples) as examples.  All examples assume the initial
state described there.



@section(Schema Manipulation)
@value(s-top)

This group@index(schema manipulation) includes functions that create,
modify, and delete whole schemata.


@value(f-top)
@begin(example)
kr:Create-Instance @i(object-name prototype) &rest @i(slot-definitions)@value(macro)
@index(create-instance)@index(creating schemata)@index(creating objects)
@end(example)

This macro creates an instance of the @i(prototype) with certain slots
filled in;  if @i(prototype) is @value(nil), the instance will have no
prototype.  The instance is named @i(object-name).  If @i(object-name) is
@value(nil), an unnamed object is created and returned.  If @i(object-name)
is a symbol, a special variable by that name is created and bound to the
new object.

The @i(slot-definitions), if present, are used to create initial slots and
values for the object.  Each slot definition should be a list whose first
element is the name of a slot, and whose second element is a value for that
slot.

In addition to this basic slot-filling behavior, this macro also performs
three operations that are connected to inheritance and constraint
maintenance.  First of all, create-instance links the newly created
object to the @i(prototype) via the @pr(:is-a) link, thus making it
an instance.

Second, if the @i(prototype) contains any slot with a formula, and the
@i(slot-definitions) do not redefine that slot, create-instance copies
the formula down into the instance.  This means that the @i(prototype) can
conveniently provide default formulas@index(default formulas) for any slot
that is not explicitly defined by its instances.

Third, if either the @i(prototype) or the object itself defines the
@pr(:initialize) method@index(initialize method), create-instance sends
the newly created object the @pr(:initialize) message.  This is done after
all other operations have been completed, and provides an automatic way to
perform object-dependent initializations.

Example:
@begin(programexample)
(create-instance 'RECTANGLE-4 BOX-OBJECT (:x 14) (:y 15))
@end(programexample)

The following example@index(defining methods) demonstrates the use of the
@pr(:initialize)
method at the prototype@index(prototypes) level@foot(Defining methods
on Garnet objects is seldom necessary in practice, since real Opal
prototypes already have built-in @pr(:initialize) methods.):
@begin(programexample)
(define-method :initialize BOX-OBJECT (schema)
   (s-value schema :color :RED)
   (format t "~S initialized~%" schema))

(create-instance 'RECTANGLE-4 BOX-OBJECT (:x 14) (:y 15))
@i(;; prints out:)
#k<RECTANGLE-4> initialized
@end(programexample)

Create-instance understands the  @b(:override) keyword and the
@b(:name-prefix) keyword; see @ref(create-options) for more details.
The uniform declaration syntax with the @b(:declare) keyword is used
to define "local only slots", constant slots, and many others (see section
@ref(uniform-syntax)).


@value(f-top)
@begin(example)
kr:PS @i(schema)@value(function)
@end(example)
@index(ps)@index(printing schemata)

This function prints the contents of the @i(schema).  In its simplest form,
described here, the function is called with the @i(schema) as its sole
argument, and prints out the contents of the schema in a standard format.
Optional arguments also allow you to control precisely what is printed out; the
more complicated form is described in section @ref(print-control-slots).

The following example shows the simple form of @pr(ps):
@begin(programexample)
(ps RECTANGLE-1)
@i(;; prints out:)
{#k<RECTANGLE-1>
  :Y =  20
  :X =  10
  :IS-A =  #k<BOX-OBJECT>
  }
@end(programexample)



@value(f-top)
@begin(example)
kr:Schema-P @i(thing)@value(function) 
@end(example)
@index(schema-p)

This predicate returns @value(t) if @i(thing) is a valid KR schema,
@value(nil) otherwise.  It returns @value(nil) if @i(thing) is a
destroyed schema.  It also returns @value(nil) if @i(thing) is a formula.

@begin(programexample)
(schema-p RECTANGLE-1) ==> T
(schema-p 'random) ==> NIL
@end(programexample)


@value(f-top)
@begin(example)
kr:Is-A-P @i(schema) @i(thing)@value(function)
@end(example)
@index[is-a-p (function)]

This predicate returns @value(T) if @i(schema) is related to @i(thing)
(another schema) via the @pr(:is-a) relation, either directly or through an
inheritance chain.  It returns @value(nil) otherwise.

Note that @i(thing) may have the special value @value(T), which is used as
a "super-class" indicator; in this case, @pr(is-a-p) returns @value(T) if
@i(schema) is any schema.  If the @i(schema) is identical to the @i(thing),
@pr(is-a-p) also returns @value(T).@* Examples:
@begin(programexample)
(is-a-p RECTANGLE-1 BOX-OBJECT) ==> T
(is-a-p RECTANGLE-1 MY-GRAPHICAL-OBJECT) ==> T
(is-a-p RECTANGLE-1 RECTANGLE-2) ==> NIL
(is-a-p RECTANGLE-1 T) ==> T
@end(programexample)



@section(Slot and Value Manipulation Functions)
@value(s-top)

This group includes the most commonly used KR functions, i.e., the ones
that retrieve or modify the value in a slot.  This section presents KR value
manipulation functions that deal with constraints.  A different
set of primitive functions, which do not deal with constraints, is
described in Section @ref(additional-functions).


@Subsection(Getting Values with G-Value and GV)
@label(g-value-and-gv)

When called outside of a formula, @pr(g-value) and @pr(gv) behave identically.
When used inside a formula, the function @pr(gv) not only returns the value
of a slot, but also establishes a dependency for the formula on the slot.
This special property of @pr(gv) is discussed in section @ref(gv-in-form).

Novice Garnet users only need to learn about @pr(gv), but @pr(g-value) is
supplied for the rare case in which you want to retrieve a slot value from
inside a formula without establishing a dependency.

@begin(example)
kr:Gv @i(object) &rest @i(slot-names)@value(macro)

kr:G-Value @i(object) &rest @i(slot-names)@value(macro)
@end(example)
@index(g-value)

These macros return the value in a slot of an object.  If the slot is
empty or not present, they return @value(nil).  Inheritance may be used
when looking for a value.  @pr(G-value) and @pr(gv) handle constraints
properly:  If a formula is currently installed in the slot, the value is
computed (if needed) and returned.  @pr(G-value) will work in place of @pr(gv)
in any of the following examples:

@begin(programexample)
(gv RECTANGLE-1 :is-a) ==> #k<BOX-OBJECT>
(gv RECTANGLE-1 :thickness) ==> 1   @i(; inherited)
(gv RECTANGLE-1 :color) ==> :BLUE
(gv RECTANGLE-2 :y) ==> 35          @i(; computed formula)
@end(programexample)

Although it is common to call @pr(g-value) and @pr(gv) with only one slot name,
these macros may actually be given any number of @i(slot-names).  The macros
expand into repeated calls to @pr(g-value) and @pr(gv), where each slot is
used to retrieve another object.  The given slot in the final object (which
is, in general, different from the @i(object)) is then
accessed.  For example:
@begin(programexample)
(gv RECTANGLE-2 :left-obj :x)
@end(programexample)
is equivalent to
@begin(programexample)
(gv (gv RECTANGLE-2 :left-obj) :x)
@end(programexample)

Both expressions return the value of the @pr(:x) slot of the object
which is contained in the @pr(:left-obj) slot of @c(RECTANGLE-2).  One can
think of the slot @pr(:left-obj) as providing the name of the place from which
the next slot can be accessed.  Such a slot is often called a
@i(link), since it provides a link to another object which can be used to
compute values.


@Subsection(Setting Values with S-Value)

@value(f-top)
@begin(example)
kr:S-Value @i(object slot) [@i(more-slots)] @i[value]@value(function)
@end(example)
@index(s-value)

This function is used to set a slot with a given value or formula.  The
@i(slot) in the @i(object) is set to contain the @i(value).  Like with
@pr(g-value)
and @pr(gv), @pr(s-value) can be given multiple slots in argument list, when
the slot to be set is several levels away from @i(object).  In the normal
case, @i(value) is an ordinary LISP value and simply supersedes any previous
value in the slot.  If @i(value) is a formula (i.e.  the result of a call
to @pr(o-formula) or @pr(formula)), the formula is installed@index(installing
formulas) in the @i(slot) and internal bookkeeping information is set up
appropriately.

If the @i(slot) already contains a formula, the following two cases arise.
If @i(value) is also a formula, the old formula is replaced and any
dependencies are removed.  If @i(value) is not a formula, the
old formula is kept in place, but the new @i(value) is used as
its new, temporary cached value.  This means that the @i(slot) will keep
the @i(value) until such time as the old formula needs to be re-evaluated
(because some of the values on which it depends are modified).

@pr(S-value) returns the new value of the @i(slot).

Note that a @b(setf) form is defined for @pr(gv) and @pr(g-value), and
expands into
@pr(s-value).  This allows a variety of LISP constructs to be
used in combination with @pr(gv) and @pr(g-value), such as the idiom 
@programexample{(incf (gv object slot))}
which increments the value of a slot in the object.  For example,
@begin(programexample)
@i(;; Change value in depended slot from 20 to 21)
(incf (gv RECTANGLE-1 :y))
@i(;; The constraint is propagated to RECTANGLE-2:)
(gv RECTANGLE-2 :y) ==> 36          @i(; recomputed)
@end(programexample)

Constraint propagation is fully enforced during this operation, just as it
would be in the equivalent expression
@begin(programexample)
(s-value RECTANGLE-1 :y (1+ (gv RECTANGLE-1 :y)))
@end(programexample)


@Subsection(Formula and O-Formula)
@label(formula-and-o-formula)

@value(f-top)
@begin(example)
kr:O-Formula @i(form) &optional @i(initial-value)@value(macro)
@end(example)
@index(o-formula)
@IndexSecondary(Primary="Formulas", Secondary="O-formula")

Given a @i(form), this macro returns a formula (formulas are internally
represented by special structures).  The @i(form) typically consists of Lisp
expressions and @pr(gv) or @pr(gvl) references (see below).@*Examples:
@begin(programexample)
(o-formula (gvl :ABOVE-GADGET :x))
(o-formula (min (gvl :ABOVE-GADGET :x)
                (+ (gvl :OTHER-GADGET :width) 10)))
@end(programexample)
The first example@index(expressions in formulas) creates a formula which
causes the slot on which it is installed to have the same value as slot
@pr(:x) of the object contained in slot @c(:above-gadget) of the current
object.  The second formula is more complex and constrains the slot on
which it is installed to have as its value the minimum of two values.  One
value is computed as before, and the other is computed by adding 10 to the
@pr(:width) slot of the object contained in slot @c(:other-gadget) of the
current object.

The @i(form) can also be an existing formula, rather than a Lisp
expression.  In this case, the new formula is linked to the
existing formula, and inherits the expression from it.  No local state is
shared by the two formulas.  This form of the call should be used as often
as possible, since inherited formulas are smaller and more efficient than
top-level formulas.
An illustration of this case is given by the
second call in the following example, which creates a new formula
that inherits its expression from the first one:
@begin(programexample)
(setf f (o-formula (+ (gvl :ABOVE :y)
                      (floor (gvl :ABOVE :height) 2))))
(setf g (o-formula f))
@end(programexample)

@IndexSecondary(Primary="Formulas", Secondary="Initial values")
If an @i(initial-value) is specified, it
is used as the initial cached value@index(cached values) for the formula.
This cached value is recorded in the formula but marked invalid, and thus
it is never used under normal circumstances.  The initial value is only
used if the formula is part of a circular dependency, or if one of its
dependency paths is invalid.  Most applications need not be concerned about
this feature.

A reader macro has been defined to abbreviate the definition of o-formulas.
Instead of typing @pr[(o-formula (...))], you could type @pr[#f(...)], which
expands into a call to @pr(o-formula).  For example, one may write:
@begin(programexample)
   (s-value a :left #f(gvl :top))
@end(programexample)
instead of the equivalent expression
@begin(programexample)
   (s-value a :left (o-formula (gvl :top)))
@end(programexample)

@value(f-top)
@begin(example)
kr:Formula @i(form) &optional @i(initial-value)@value(function)
@end(example)
@IndexSecondary[Primary="Formulas", Secondary="Formula (function)"]

Given a @i(form), this function returns a formula.  It is similar to
@pr(o-formula), except that the code in @i(form) is not compiled until
run-time, when the @pr(formula) call is actually executed.

Code that can be compiled early should use @pr(o-formula), which
yields more efficient formula evaluation and reduces the amount of storage.
@pr(Formula) might be required when local variables are used in @i(form),
and are not set until run-time.  See the "Hints and Caveats" section of
the Tutorial for more discussion of when a
formula created with @pr(formula) might be needed.


@value(f-top)
@begin(example)
kr:Formula-P @i(thing)@value(macro)
@end(example)
@index(formula-p)@index(predicates)
@IndexSecondary(Primary="Formulas", Secondary="Formula-p")

A predicate that returns @value(T) if the @i(thing) (any Lisp object) is a
formula created with @pr(o-formula) or @pr(formula), @value(NIL) otherwise.


@Subsection(GV and GVL in Formulas)
@label(gv-in-form)

@value(f-top)
@begin(example)
kr:Gv @i(object) &rest @i(slot-names)@value(macro)
@end(example)
@index(gv)

This macro, which we saw in section @ref(g-value-and-gv), serves a special
purpose when used within formulas.
@IndexSecondary(Primary="Formulas", Secondary="Gv in formulas")
In addition to returning a value like @pr(g-value), @pr(gv) records the
dependency@index(value dependency) path and ensures that the formula in
which it is embedded is recomputed whenever the dependency path or the
value changes.

Note that the @i(object) can be any object, not just the one on
which the formula containing @pr(gv) is installed.  Specifying the reserved
name @pr(:self) for @i(object) ensures that the path@index(paths in
formulas) starts from the object on which the formula is installed.  This
can be achieved more simply via @pr(gvl), as explained below.

The following examples show how to use @pr(gv) within formulas:
@begin(programexample)
(o-formula (gv RECTANGLE-1 :y))
(o-formula (+ (gv :self :x) 15))
(o-formula (equal (gv :self :other :other :color)
                  (gv :self :color)))

@end(programexample)

As a special case, the expression @pr{(gv :self)} (without any slot name) may
be used within a formula to refer to the object to which the formula is
attached.  This is sometimes useful for formulas which need a way to
explicitly reference the object on which they are currently installed.


@value(f-top)
@begin(example)
kr:Gvl @i(slot) &rest @i(more-slots)@value(macro)
@end(example)
@index(gvl)

This is a useful shorthand notation for @pr{(gv :self @i(slot
more-slots))}.  It may only be used within formulas, since it looks for
@i[slot] in the object on which the surrounding formula is installed.  For
example, the expression @pr{(gvl :color)} returns the current value of the
@pr(:color) slot in the object which contains the formula, and
is equivalent to the expression @pr{(gv :self :color)}.



@section(Object-Oriented Programming)
@value(s-top)

This group includes functions which support objected-oriented
programming@index(object-oriented programming) within KR.


@value(f-top)
@begin(example)
kr:Define-Method @i(slot-name object arg-list) &rest @i(body)@value(macro)
@end(example)
@index(define-method)

This macro defines a method@index(methods) named @i(slot-name) for the
@i(object).  While @i(object) can be any object, and in particular any
instance, it is customary to define methods at the level of prototypes;
this allows prototypes to provide methods for all their instances.

The method is defined as a function whose argument list is @i(arg-list) and
whose body is given by the @i(body).  The method is installed on slot
@i(slot-name), which is created if needed.  In order to facilitate debugging,
the function which implements the method is given a meaningful name formed
by concatenating the @i(slot-name), the string "-METHOD-", and the name of the
@i(object).  Example:
@begin(programexample)
(define-method :print BOX-OBJECT (object)
  (format t "A rectangle at (~D,~D).~%"
    (gv object :x) (gv object :y)))
@end(programexample)
After this, the @pr(:print) method can be inherited by any instance of
@c(box-object).  Sending the message to @c(rectangle-2), for example, would
cause the following to happen:
@begin(programexample)
(kr-send RECTANGLE-2 :print RECTANGLE-2)
@i(;; prints out:)
A rectangle at (34,35).
@end(programexample)
The generated name of the @pr(:print) method, in this example,
would be @c(print-method-box-object).



@value(f-top)
@begin(example)
kr:KR-Send @i(object) @i(slot) &rest @i(arguments)@value(macro)
@end(example)
@index(kr-send)@index(sending messages)

This macro implements the primitive level of message sending.  The
@i(slot) in @i(object) should yield a Lisp function
@index(methods)
object; the function is then called with the arguments specified in
@i(arguments).  Note that the function may be local to the @i(object), or
it may be inherited.

If the function, i.e., the value of the expression @t{(g-value object
slot)}, is @value(NIL), nothing happens and @pr(kr-send) simply returns
@value(NIL).  Otherwise, the function is invoked and the value(s) it
returns are returned by @pr(kr-send).



@value(f-top)
@begin(example)
kr:Call-Prototype-Method &rest @i(arguments)@value(macro)
@end(example)
@index(call-prototype-method)@index(sending messages)

This macro can be used inside an object's method to invoke the method
attached to the object's prototype.  It can only be used inside object
methods.  If a prototype of the current object (i.e, the one which supplied
the method currently being executed) also defines a method by the same
name, the prototype's method is invoked with @i(arguments) as the list of
arguments.  For example,
@begin(programexample)
(define-method :notify A (object level)
  ;; Execute object-specific code:
  ;; ...
  ;; Now invoke :notify method from prototype, if any:
  (call-prototype-method object level))))

(kr-send A :notify A 10)
@end(programexample)
First of all, @pr(kr-send) invokes the method defined locally by object
@c(a).  Since the method itself contains a call to
@pr(call-prototype-method), the hierarchy is searched for a prototype
of object @c(a) which also defines the @pr(:notify) method.  If one exists,
that method is invoked.

A method is free to supply a prototype method with any parameters it
wants; this can be accomplished simply by using different values in the
call to @pr(call-prototype-method).  In the example above, for instance, we
could have written @pr{(call-prototype-method object (+ level 1))}.  It is
customary, however, to invoke @pr(call-prototype-method) with exactly the
same parameters as the original call. 

Note that the name of the original object and the message name are not
specified in @pr(call-prototype-method).  KR automatically provides the
right values.


@value(f-top)
@begin(example)
kr:Apply-Prototype-Method &rest @i(args)@value(macro)
@end(example)

@index(apply-prototype-method)
The macro @pr[apply-prototype-method] is similar to
@pr[call-prototype-method], but the method defined by the prototype is
invoked using @pr[apply] rather than @pr[funcall].  This macro may be
useful for methods that take @pr[&rest] arguments.



@value(f-top)
@begin(example)
kr:Method-Trace @i(object message-name)@value(macro)
@end(example)
@index(method-trace)@index(methods)@index(tracing methods)

This macro can be used to trace method execution.  Trace information
is printed every time an instance of the @i(object) is sent the message
named @i(message-name).  Since this expands into a call to the
primitive macro @b(trace), the Lisp expression @b{(untrace)} may
be used later to eliminate trace information.@*Example:
@begin(programexample)
(method-trace BOX-OBJECT :print)
@end(programexample)


@Section(Reader Macros)
@index(reader macros)

A reader macro is defined by default for the @pr(#k<...>) notation, which is
produced by the functions @pr(ps) and @pr(gv) when the variable
@pr[kr::*print-as-structure*] is non-NIL.  This macro allows objects written
with the @pr(#k) notation to be read back in as a KR object.
If necessary, this feature may be disabled by recompiling KR after
pushing the keyword @pr[:no-kr-reader] onto the @pr[*features*] list.

A second reader macro is defined for convenience, as discussed previously.
This reader macro
allows o-formulas to be entered using the @pr[#f()] notation, which expands
into a call to @pr[o-formula].  For example, one may write:
@begin(programexample)
   (s-value a :left #f(gvl :top))
@end(programexample)
instead of the equivalent expression
@begin(programexample)
   (s-value a :left (o-formula (gvl :top)))
@end(programexample)



@Chapter[The Type-Checking System]
@label(type-system)
@index(type-checking)

KR supports complete type-checking for slots.  Any
slot in any object can be declared of a certain type.  Slots can
be declared with one of the pre-defined types Garnet provides, which
cover most of the commonly occurring situations, or new types may be
created as needed using the macro @pr[def-kr-type] (see section
@ref[creating-types]).  Type
expressions use the same syntax as in the Common Lisp type system.
Type declarations are inherited, so it is generally not necessary to
specify types for the slots of an instance (unless, of course, the
instance is to behave differently from the prototype).

Every time the value of a typed slot changes, KR checks that the new
value is compatible with the declared type of the slot.  If not, a
continuable error is generated.  More specifically, the type of a
value is checked against the type specification for a slot under the
following circumstances:
@begin[itemize]
when the slot is first created using @pr[create-instance]: if a value
is specified and the value is of the wrong type, an error is
generated;

when a slot is set to a certain value using @pr[s-value];

when the value in a slot is computed by a formula, and the formula is
evaluated;

when the type of a slot that already contains a value is changed using
@pr[s-type] (see below).
@end[itemize]

This mechanism ensures that potential problems are detected
immediately; without type-checking, it would be possible for a bad
value in a slot to cause hard-to-track errors later on.  For example,
if a slot in an object is supposed to contain an integer value, a
formula in another object would typically assume that the value is
correct, and compute an expression such as @pr[(+ 10 (gv obj :left))].
If the value in the slot @pr[:left] is incorrectly set to NIL, however,
this would not cause an error until much later, when the formula is
actually recomputed and the operator + is given a null value.  When
type-checking is enabled, on the other hand, the user would see an
error immediately when the value is set to NIL, because NIL does not
meet the "integer" declaration.

The KR type-checking mechanism is independent of the lisp type system.
Even if a type is defined with lisp's @pr(deftype), another corresponding
type must be defined with KR's @pr(def-kr-type).  The two types may have the
same name.  The important thing is that the new type must be registered with
KR's type system.

Type-checking may be turned off completely, for maximum performance in
finished systems, by setting the variable @pr[kr::*types-enabled*] to
NIL.  However, the performance overhead associated with type-checking
is small, and we recommend that you always keep type-checking enabled.
This ensures early detection of problems that might otherwise be
difficult to track down.


@Section[Creating Types]
@label(creating-types)

New types may be declared as needed with the macro @pr[def-kr-type],
which is exported from the KR package.  The syntax of the macro is as
follows:

@value(f-top)
@index(def-kr-type)
@begin(example)
kr:Def-KR-Type @i[name-or-type] &optional @i[args body doc-string] @value(macro)
@end(example)

This macro defines a new type for KR's type-checking mechanism.  Every
type used in slot declarations must have been defined with
@pr[def-kr-type].  However, Garnet already predefines the most common
types, so you do not have to worry about those.

The macro may be called in two different styles, one named, one
unnamed.  The first style is used to define types that have a name; you
may then use either the name or the corresponding expression in actual
type declarations.  The second style simply defines a type expression,
which is not named and hence must be used verbatim in type
declarations.  Here are examples of the two styles:

@blankspace(1 line)
@begin(programexample)
  (def-kr-type my-type () '(or keyword null))

  (def-kr-type '(or keyword null))
@end(programexample)

The first style uses the same syntax as Lisp's @pr[deftype]; the
@i[body] should be a type expression acceptable to @pr[deftype], and is
used for typechecking when the name is used.  In the current
implementation of the type system, the @i(args) parameter should
always be NIL.@foot[The presence of the @i(args) parameter is to maintain
consistency of syntax with the standard lisp function @pr(deftype).  If you
need to pass a parameter to your predicate, then define the predicate using
@pr(satisfies).]  With either example
above you could then specify some object's type to be @pr['(or keyword
null)].  With the first style, however, you could also specify the
type to be @pr['my-type], which may be more convenient and easier to
maintain in the long run.

@begin(group)
The named style also allows a @i[doc-string] to be specified.  This is
a human-readable documentation string that is associated with the
type, and is useful for debugging purposes.  For example, the first
call above could be written as:

@blankspace(1 line)
@begin(programexample)
  (def-kr-type my-type () '(or keyword null)
     "Either NIL or a keyword")
@end(programexample)
@end(group)


@Section[Declaring the Type of a Slot]


Types are associated with slots either statically or dynamically.  The
former mechanism is by far the most common, and is done at object
creation time using the @c[:declare] option in @pr[create-instance].
For example, consider the following code:

@blankspace(1 line)
@begin(programexample)
(create-instance 'R1 opal:rectangle
  :declare (:type (integer :left :top)
		  ((integer 0) :width :height)
		  ((or keyword null) :link-name))
  (:link-name :PARENT)
  (:left 10) (:height (+ 15 (o-formula (gvl :width)))))
@end(ProgramExample)

The example declares that the values contained in slots @pr[:left] and
@pr[:top] must be integers, the values in slots @pr[:width] and
@pr[:height] must be positive integers, and the value in slot
@pr[:link-name] must be either a keyword or NIL.  Note that this
declaration is legal, as the type @pr[(or keyword null)] was declared
above using @pr[def-kr-type].  Note also that the declarations for
slots @pr(:left), @pr(:top), @pr(:width), and @pr(:height) are, in fact,
not necessary, as they would normally be inherited from the prototype.


Types can also be associated with slots dynamically, i.e., after
object creation time.  This is done with the function
@index(s-type)
@begin(example)
kr:S-Type @i[object slot type] &optional (@i[check-p] T) @value(function)
@end(example)

This function changes the type declaration for the @i[slot] in the
@i[object] to the given @i[type].  If @i[check-p] is non-NIL (the
default), the function signals a continuable error if the value
currently in the @i[slot] does not satisfy the new type.  Setting
@i[check-p] to NIL disables the error; note that this should only be
used with caution, as it may leave the system in an inconsistent state
(i.e., the @i[slot] may in fact contain an illegal value).  The
function returns the @i[type] it was given.


The type associated with a slot can be retrieved by the function
@index(g-type)
@begin(example)
kr:G-Type @i[object slot]	@value(function)
@end(example)
If a type is associated with the slot, it is returned (more precisely,
if the type is named, the name is returned; otherwise, the type
expression is returned).  If there is no type, the function returns
NIL.


@Section[Type Documentation Strings]


Given a type (for example, something returned by @pr[g-type]), its
associated documentation string can be retrieved using:
@index(get-type-documentation)
@begin(example)
kr:Get-Type-Documentation @i[type]	@value(function)
@end(example)

This function returns the human-readable type documentation string, or
NIL if there is none.


Given a type, it is also possible to modify its string documentation,
using the function:
@index(set-type-documentation)
@begin(example)
kr:Set-Type-Documentation @i[type doc-string]	@value(function)
@end(example)

This function associates the @i[doc-string] with the @i[type].  When
an error message which concerns the type is printed, the documentation
string is printed in addition to the raw type.


@begin(group)
@Section(Retrieving the Predicate Expression)

When types are named, @pr(g-type) returns just the name of the type, rather
than its associated expression.  Sometimes it is useful to retrieve the
predicate of the type associated with the type name.  The following function
serves this purpose:

@index(get-type-definition)
@begin(example)
kr:Get-Type-Definition @i[type-name] @value(function)
@end(example)

Given a symbol which names a KR type (i.e., a named type defined with
@pr[def-kr-type]), this function returns the type expression that was used
to define the type.  If no such expression is found, the function returns NIL.
@end(group)

@begin(group)
@Section[Explicit Type-Checking]

In addition to KR's built-in type checking, which happens when the
value in a slot is changed, it is also possible to check whether a
value is of the right type for a slot.  This can be done with
the function:
@index(check-slot-type)
@begin(example)
kr:Check-Slot-Type @i[object slot value] &optional (@i[error-p] T) @value(function)
@end(example)

The function checks whether the given @i[value] is of the right type
for the @i[slot] in the @i[object].  If not, it raises a continuable
error, unless @i[error-p] is set to NIL; in this case, it returns a
string which describes the error.  This function is called
automatically by KR any time a slot is modified, so you normally do
not have to call it explicitly.
@end(group)


@Section[Temporarily Disabling Types]

It is possible to execute a piece of code with type-checking
temporarily disabled, using the macro
@index(with-types-disabled)
@begin(example)
kr:With-Types-Disabled &body @i[body] @value(macro)
@end(example)
This macro is similar to others, such as @pr[with-constants-disabled].
During the execution of the @i[body], type-checking is disabled, and
no errors are given if a value does not meet the type specification of
its slot.  Just as with @pr[with-constants-disabled], this macro should
only be used with caution, as it may leave the system in an
inconsistent state.


@Section[System-Defined Types]

The following type predicate can be used to declare types:
@index[is-a-p (type predicate)]
@begin(example)
kr:Is-A-P @i[prototype]	@>[@i(Type Predicate)]
@end(example)
This is a type predicate, NOT a function or macro; it can only be
used within type specifiers.  This predicate declares that the value
in a slot should be an instance of the @i[prototype], either directly
or indirectly.  The predicate is true of all objects for which a call
to the function @pr[kr:is-a] would return true.  For example, the
following definition can be used as the type of all rectangles:

@blankspace(1 line)
@begin(programexample)
(def-kr-type rect-type () '(is-a-p opal:rectangle))
@end(programexample)
@blankspace(1 line)

@begin(group)
Garnet defines a number of types, which cover the types of the most
commonly used slots.  This is the list of pre-defined basic types:
@begin[description, indent=-2]
@index[T (type)]
@c(T) - Any value satisfies this type.

@index[kr-boolean (type)]
@c(kr-boolean) - Same as @pr(T), but specifically intended for
slots which take a NIL or non-NIL value, often used as boolean variables.

@index[null (type)]
@c(null) - Only the value NIL satisfies this type.

@index[string (type)]
@c(string) - Strings satisfy this type.

@index[keyword (type)]
@c(keyword) - All Lisp keywords satisfy this type.

@index[integer (type)]
@c(integer) - All integers (fixnums and bignums) satisfy this type.

@index[number (type)]
@c(number) - This type includes all numbers: integers, floating point,
complex numbers, and fractions.

@index[list (type)]
@c(list) - Any list satisfies this type.

@index[cons (type)]
@c(cons) - Any cons cell (lists and dotted pairs) satisfies this type.

@index[schema (type)]
@c(schema) - Any non-destroyed KR object satisfies this type.
@end[description]
@end(group)
@blankspace(1 line)


@begin(group)
Garnet also defines many non-basic types, which are typically used
by many objects throughout the system.  The following types do not have a
name.  They are often used for slots in
Opal fonts, line styles, etc.  Because they are predefined, you don't
need to call @pr[def-kr-type] for them.

@blankspace(.5 line)
@Begin(description, columns = 2, columnmargin = 0.33 in, linewidth = 3.33 in,
       boxed, columnbalance=on, size=8, spread=0, indent=-2, FaceCode T)
'(real 0 1)

'(integer 0 1)

'(integer 0)

'(integer 1)

'(integer 2)

'(member 0 1 2 3)

'(or null integer)

'(or null (integer 0))

'(or keyword (integer 0))

'(or number null)

'(member :even-odd :winding)

'(or (member :below :left :right) list)

'(or keyword character list)

'(or list string)

'(or list (member t))

'(or list (satisfies schema-p))

'(or string atom)

'(or string (satisfies schema-p))

'(or function symbol)

'(or list integer function symbol)

'(or null function symbol)

'(or null keyword character)

'(or null string)

'(or null (satisfies schema-p))

'(or null string keyword (satisfies schema-p))

'(or string keyword (satisfies schema-p))

@end(description)
@end(group)





@begin(group)
The following non-basic types are named, and have associated documentation
strings.  Users can reference these types anywhere in Garnet programs.
To access each type's own documentation string, use
@pr(get-type-documentation).

@begin[description, indent=-2]

@index[known-as-type]
@c(known-as-type) - a keyword (this type is used in the @pr[:known-as] slot)

@index[aggregate(-or-nil) type]
@c(aggregate) - an instance of @pr(opal:aggregate)

@c(aggregate-or-nil) - either an instance of @pr(opal:aggregate) or NIL

@index[bitmap(-or-nil) type]
@c(bitmap) - an instance of @pr(opal:bitmap)

@c(bitmap-or-nil) - either an instance of @pr(opal:bitmap) or NIL

@index[color(-or-nil) type]
@c(color) - an instance of @pr(opal:color)

@c(color-or-nil) - either an instance of @pr(opal:color) or NIL

@index[font(-...) type]
@c(font) - either an instance of @pr(opal:font) or @pr(opal:font-from-file)

@c(font-family) - one of @pr(:fixed), @pr(:serif), or @pr(:sans-serif)

@c(font-face) - one of @pr(:roman), @pr(:bold), @pr(:italic), or
@pr(:bold-italic)

@c(font-size) - one of @pr(:small), @pr(:medium), @pr(:large), or
@pr(:very-large)

@index[filling-style(-or-nil) type]
@c(filling-style) - an instance of @pr(opal:filling-style)

@c(filling-style-or-nil) - either an instance of opal:filling-style or NIL

@index[line-style(-or-nil) type]
@c(line-style) - an instance of @pr(opal:line-style)

@c(line-style-or-nil) - either an instance of @pr(opal:line-style) or NIL

@index[inter-window-type]
@c(inter-window-type) - a single @pr(inter:interactor-window), or a list of
windows, or T, or NIL.

@index[window(or-nil) type]
@c(window) - an instance of @pr(inter:interactor-window)

@c(window-or-nil) - either an instance of @pr(inter:interactor-window) or NIL

@index[fill-style type]
@c(fill-style) - one of @pr(:solid), @pr(:stippled), or @pr(:opaque-stippled)

@index[draw-function type]
@c(draw-function) - one of @pr(:copy, :xor, :no-op, :or, :clear, :set,
:copy-inverted, :invert, :and, :equiv, :nand, :nor,
:and-inverted, :and-reverse, :or-inverted, :or-reverse)

@index[h-align type]
@c(h-align) - one of @pr(:left), @pr(:center), or @pr(:right)

@index[v-align type]
@c(v-align) - one of @pr(:top), @pr(:center), or @pr(:bottom)

@index[direction(-or-nil) type]
@c(direction) - either @pr(:vertical) or @pr(:horizontal)

@c(direction-or-nil) - either @pr(:vertical), @pr(:horizontal), or NIL

@index[items-type]
@c(items-type) - list of items: ("Label2"...)

@index[accelerators-type]
@c(accelerators-type) - list of lists: ((#\r "Alt-r" #\meta-r)...)

@index[filename-type]
@c(filename-type) - a string that represents a pathname

@index[priority-level type]
@c(priority-level) - an instance of @pr(inter:priority-level)

@end[description]
@end(group)




@chapter(Functional Interface: Additional Topics)
@value(top)

@label(additional-functions) This section describes features of KR that
are seldom needed by casual Garnet users.  These features are useful for large
application programs, especially ones which manipulate constraints
directly, or for application programs which use the more advanced knowledge
representation features of KR.


@section(Schema Manipulation)
@value(s-top)

@begin(example)
kr:Create-Schema @i(object-name) &rest @i(slot-definitions)@value(macro)
@end(example)
@index(create-schema)

This macro@index(creating schemata) creates and returns a new object
named@index(object names) @i(object-name).  It is much more primitive than
create-instance, since it does not copy down formulas from a prototype
and does not call the @pr(:initialize) method.

If @i(object-name) is @value(nil), an unnamed object is created and
returned.  If @i(object-name) is a symbol, a special variable by that name
is created and bound to the new object.  The @i(slot-definitions), if
present, are used to create initial slots and values for the object.  Each
slot definition should be a list whose first element is the name of a slot,
and whose second element is the value for that slot.

@pr(Create-schema) understands the @b(:override) keyword and the
@b(:name-prefix) keyword; see @ref(create-options) for more details.

Examples:
@begin(programexample)
(create-schema 'RECTANGLE-3 (:is-a BOX-OBJECT) (:x 70))
(create-schema 'RECTANGLE-3 :override (:y 12))   @i(; add a slot)
(create-schema NIL (:is-a MY-GRAPHICAL-OBJECT))
@end(programexample)



@value(f-top)
@begin(example)
kr:Create-Prototype @i(object) &rest @i(slot-definitions)@value(macro)
@end(example)
@index(create-prototype)@index(creating schemata)@index(creating objects)

This macro is slightly more primitive than create-instance.  Unlike
create-instance, it does not allow a prototype to be specified
directly.  Moreover, it does not automatically send the @pr(:initialize)
message to the newly created @i(object).  Like create-instance, it
copies formulas from any prototype into the newly created @i(object).

The following two examples are roughly equivalent:
@begin(programexample)
(create-instance NIL BOX-OBJECT (:x 12))

;;; The hard way to do the same thing
(let ((a (create-prototype NIL
	   (:is-a BOX-OBJECT) (:x 12))))
  (kr-send a :initialize a))
@end(programexample)

Most applications will find create-instance much more convenient.  The
only case when @pr(create-prototype) should be used is when it is important
that the @pr(:initialize) message @i(not) be sent to an object at creation
time.

@pr(Create-prototype) also understands the  @b(:override) keyword and the
@b(:name-prefix) keyword; see @ref(create-options) for more details.



@value(f-top)
@begin(example)
kr:Destroy-Schema @i(object)@value(Function)
@end(example)
@index(destroy-schema)

Destroys the @i(object).  Returns @value(T) if the object was destroyed,
@value(nil) if it did not exist.  This function takes care of properly
removing all constraint dependencies to and from the @i(object).  Any
formula installed on any slot of the @i(object) is also destroyed.

Usually, Garnet users do not call this function directly.  Instead, they
use @t{(opal:destroy object)}, which performs all necessary clean-up
operations and eventually calls @pr(destroy-schema).



@value(f-top)
@begin(example)
kr:Destroy-Slot @i(object slot)@value(Function)
@end(example)
@index(destroy-slot)

Destroys the @i(slot) from the @i(object).  The value previously stored in
the slot, if there was one, is lost.  All constraints to and from @i(object)
are modified accordingly.  The invalidate demon is run on the slot before
it is destroyed, ensuring that any changes caused by this action become
visible to formulas that depend on the slot.  Using @pr(destroy-slot) on slots
that are declared constant gives a continuable error.  Continuing from the
error causes the slot to be destroyed anyway.  This behavior can be overridden
by using the macro @pr(with-constants-disabled).






@value(f-top)
@begin(example)
kr:Name-For-Schema @i(object)@value(function)
@end(example)
@index(name-for-schema)@index(schema names)

Given a @i(object), this function returns its name as a string.  The
special notation @i(#k<>) is never used, i.e., the name is the actual name
of the object.  The return value should never be modified by the calling
program.



@begin(group)
@section[Uniform Declaration Syntax]
@label(uniform-syntax)
@index(declare syntax) @index(uniform declaration syntax)

One syntax can be used for all kinds of declarations
associated with slots in an object.  Declarations are generally specified at
object creation time.  In some
cases (notably, in the case of types), it is also meaningful to
modify declarations after an object has been created; in such cases, a
separate function (such as @pr[s-type]) is provided.  (For details on the
type-checking mechanism, see Chapter @ref[type-system].)
@end(group)
@blankspace(1 line)

@begin(group)
The general syntax for declarations in @pr[create-instance] is as follows:
@begin(programexample)
(create-instance instance prototype
  [:DECLARE ((declaration-1 [slot1 slot2 ...])
             (declaration-2 [slot1 ...])
             ...)]
  [:DECLARE ((declaration-3 [slot1 ...])
            ...)]
  slot-specifiers ...)
@end(programexample)
@end(group)
@blankspace(1 line)

The keyword @c[:declare] introduces a list of declarations.  The
keyword may appear more than once, which allows separate groups of
declarations.  Each group of declarations may contain one or more
declarations; if there is only one, a level of parentheses may be
omitted.  Each declaration in a list consists of a keyword, which
specifies what property is being declared, followed by any number of
slot names (including zero).  All slots are declared of the given
property.

Consider the following, rather complex example:
@begin(programexample)
(create-instance 'REC A
  :declare ((:type (vector :BOX)
		   (integer :LEFT :TOP)
		   ((or (satisfies schema-p) null) :PARENT))
	    (:type ((member :yes :no) :VALUE))
	    (:update-slots :LEFT :TOP :WIDTH :HEIGHT :VALUE))
  :declare (:type (list :IS-A-INV))

  (:left (o-formula (+ (gvl :parent :left) (floor (gvl :width) 2))))
  (:top 10))
@end(programexample)

The first declaration group defines types (in two separate lists) and
the list of update-slots for the object.  Slot @pr[:box] is declared as
a Lisp vector; left and top are declared as integers; slot @pr[:parent]
must be either null or a valid KR object; and slot @pr[:value] must
contain either the value :yes or the value :no.  The second
declaration group shows the simplified form, in which only one
declaration is used and therefore the outside parentheses are dropped.

@blankspace(1 line)
@begin(group)
The following keywords can be used to declare different slot properties:

@begin[description, indent=-2]
@c(:constant) - The slots that follow are declared constant.  Note that (in
this case only) the special value T indicates that the slots in the
prototype's @pr[:maybe-constant] slot should be used.  (See section
@ref[constant-slots].)

@c(:ignored-slots) - The slots that follow will not be printed by the
function @pr[ps].  (See section @ref[print-control-slots].)

@c(:local-only-slots) - The values that follow should be lists of the form
@i[(slot-name  copy-down-p)].  The @i(slot-name) specifies the name of a slot
which should be treated as local-only, i.e., should not be inherited by the
object's instances.  If @i(copy-down-p) is NIL, the slot will have value NIL in
the instances.  Otherwise, the value from the object will be copied
down when instances are created and marked as local; this prevents
further inheritance, even if the value in the prototype is changed.
(See section @ref[local-only].)

@c(:maybe-constant) - Specifies the list of slots that can be made constant
in this object's instances simply by specifying the special value T.
(See section @ref[constant-slots].)

@c(:output) - Specifies the list of output slots for the object, i.e., the
slots that are computed by formulas and may provide useful output
values for communication with other objects.

@c(:parameters) - Specifies the list of parameters for the object, i.e.,
the slots designed to allow users to customize the appearance or
behavior of the object.  This slot is used extensively in the Garnet Gadgets
to indicate user-settable slots.

@c(:sorted-slots) - Specifies the list of slots (in the appropriate order)
that @pr[ps] should always print first.
(See section @ref[print-control-slots].)

@c(:type) - Introduces type declarations for one or more slots.
(See chapter @ref[type-system].)

@c(:update-slots) - The list of update slots for the object, i.e., the
slots that should trigger the @pr[:invalidate-demon] when modified.
(See section @ref[update-slots].)
@end[description]
@end(group)


@subsection[Declarations in Instances]

Most inherited declarations follow the standard KR scheme, where a
@c(:maybe-constant) or @c(:update-slots) declaration in an instance
will completely
override the declaration in the prototype.  One important exception is
the @c(:type) declaration, which is @i(additive) from prototype to
instance.  That is, all of the types declared in a prototype will be
valid in its instances, along with any new type declarations in the
instance.  So you do not need to repeat type declarations in the
instances of an object.

For other kinds of declarations besides @c(:type), a convenient syntax has
been provided for specifying declarations in instances.  If you want all the
declarations in a prototype to be inherited by the
instance along with several new ones, you could either retype all the 
declarations in the instance, or you could use the @pr(T) and @pr(:except)
syntax.  For example, it is possible to write
@begin(programexample)
(create-instance 'REC A
  :declare ((:output T :new-slot)
	    (:parameters T :except :left)))
@end[programexample]
to indicate that object REC's list of output slots includes all the
ones declared in object A, plus the @pr[:new-slot].  Also, the list of
parameter slots is equal to the one in A, minus the slot @pr[:left].

@blankspace(1 line)
@begin(group)
Declarations made in a prototype can be eliminated with an empty
declaration in an instance.  This may be particularly convenient for
declarations such as @c[:maybe-constant].  For example, the expression
@begin(programexample)
  :declare ((:TYPE) (:MAYBE-CONSTANT))
@end[programexample]
in a call to @pr[create-instance] would clear the @c(:maybe-constant)
declarations from the prototype, and eliminate all type declarations.
@end(group)
@blankspace(1 line)

However, note that redefining the @c(:constant) declaration may
not yield the expected results.  When a slot becomes constant in a
prototype, that slot will be constant for all instances.  This makes
sense because any formulas in the prototype that relied on the
constant slot have been eliminated, and cannot be restored in the
instance.  See section @ref(constant-slots) for an elaborate
discussion of constant slots.


@subsection[Examining Slot Declarations]

The following functions may be used to determine what slot
declarations are associated with a particular slot in an object, or to
retrieve all slot declarations for an object.  Note that there is no
function to alter the declaration on an object after the object has
been created, as most properties can only be set meaningfully at
object creation time.


@index(get-declarations)
@value(f-top)
@begin(example)
kr:Get-Declarations @i[object selector]		@value(function)
@end(example)

Returns a list of all the slots in the @i[object] that have associated
declarations of the type given by @i[selector], which should be one of
the keywords listed above.  If @i[selector] is @pr[:type], the return value
is a list of lists, such as
@begin(programexample)
((:left (or integer null)) (:top (or integer null)))
@end[programexample]
If @i[selector] is one of other keywords, the function returns a list of all
the slots that have the corresponding declaration.


@index(get-slot-declarations)
@value(f-top)
@begin(example)
kr:Get-Slot-Declarations @i[object slot]		@value(function)
@end(example)

This function returns a list of all the declarations associated with
the @i[slot] in the @i[object].  The list consists of keywords, such
as @c[:constant] and @c[:update-slot], or (in the case of type declarations)
a list of the form @pr[(:type @i{type-specification})].



@section(Relations and Slots)
@value(s-top)

KR supports special slots called @i[relations].  Relations serve two
purposes: allowing inheritance, and automatically creating inverse
connections.  In addition to a handful of predefined relations,
application programs can create new relations as needed via the
function @pr(create-relation) (see below).

Slots such as @pr(:is-a), which enable knowledge to be inherited from
other parts of a network, are called @i(inheritance relations).
Inheritance along such relations proceeds depth-first
@index(inheritance search) and may include any number of steps.  The
search terminates if a value is found, or if no other object can be
reached.

Any relation, including user-defined ones, may also be declared to have an
inverse relation@index(inverse relations).  If this is the case, KR
automatically generates an inverse link any time the relation is used to
connect one object to another.  Imagine, for instance, that we defined
@pr(:part-of) to be a relation having @pr(:has-parts) as its inverse.  Adding
object A to the slot @pr(:part-of) of object B would automatically add B to
the slot @pr(:has-parts) of object A, thereby creating a reverse link.

KR automatically maintains@index(relation maintenance) all relations and
inverse relations, and the application programmer does not
have to worry about them.  In the example above, if slot @pr(:part-of)
in object B is deleted, the value B is also removed from the slot
@pr(:has-parts) of object A.  The same would happen is object B is deleted.
This ensures that the state of the system is consistent at any
point in time, independent of the particular sequence of operations.


The following functions handle user-defined relations and slots:

@value(f-top)
@begin(example)
kr:Create-Relation @i(name inherits-p) &rest @i(inverses)@value(Macro)
@end(example)
@index(create-relation)

Declares@index(relations) the slot @i(name) to be a
relation.  The new relation will have @i(inverses) (a possibly empty
list of slot names)
as its inverse@index(inverse relations) relations.  If @i(inherits-p) is
non-@value(nil), @i(name) becomes a relation with inheritance, and
values may be inherited through it.

The following form defines the non-inheritance relation
@pr(:has-parts) and its two inverses, @pr(:part-of) and @pr(:subsystem-of):
@begin(programexample)
(create-relation :has-parts NIL :part-of :subsystem-of)
@end(programexample)



@value(f-top)
@begin(example)
kr:Relation-P @i(thing)@value(Macro)
@end(example)
@index(relation-p)

This predicate returns @value(nil) if @i(thing) is not a
relation, or a non-@value(nil) value if it is the name of a relation slot.@*
Examples:
@Begin(programexample)
(relation-p :is-a) ==> non-NIL value
(relation-p :color) ==> NIL
@End(programexample)



@value(f-top)
@begin(example)
kr:Has-Slot-P @i(object) @i(slot)@value(function)
@end(example)
@index(has-slot-p)

A predicate that returns @value(T) if the @i(object) contains a slot named
@i(slot), @value(NIL) otherwise.  Note that @i(slot) must be local to the
@i(object); inherited slots are not considered.@*
Examples:
@begin(programexample)
(has-slot-p RECTANGLE-1 :is-a)  ==> T
(has-slot-p RECTANGLE-1 :thickness) ==> NIL   @i(; not local)
@end(programexample)



@value(f-top)
@begin(example)
kr:DoSlots (@i(slot-var object) &optional @i(inherited)) &rest @i(body)@value(Macro)
@end(example)
@index(doslots)

Iterates@index(slot iterator)@index(iterators) the @i(body) over all the
slots of the @i(object).  The @i(slot-var) is bound to each slot in turn.
The @i(body) is executed purely for side effects, and @pr(doslots) returns
@value(NIL).  Example:

@begin(programexample) 
(doslots (slot RECTANGLE-1)
  (format t "Slot ~S has value ~A~%"
          slot (gv RECTANGLE-1 slot)))
@i(;; prints out:)
Slot :Y has value 20
Slot :X has value 10
Slot :IS-A has value #k<BOX-OBJECT>
@end(programexample)

By default, @pr(doslots) only iterates over the local slots of @i(object).
But if the @i(inherited) parameter is T, then all slots that have been
inherited from the @i(object)'s prototype will be iterated over as well.
Note: Only those slots that have actually been inherited will be included
in the list of inherited slots.  If they are merely defined in the prototype
and have not been @pr(gv)'d in the instance, then they will not be
included in the iteration list.  See the description of the function @pr(ps)
in section @ref(print-control-slots) for a way to display all the slots that
could possibly be inherited by the object.



@section(Constraint Maintenance)
@value(s-top)


These functions are concerned with the constraint maintenance part of KR.


@value(f-top)
@begin(example)
kr:Change-Formula @i(object slot) @i(form)@value(function)
@end(example)
@index(change-formula)
@IndexSecondary(Primary="Formulas", Secondary="Change-formula")

If the @i(slot) in @i(object) contains a formula, the formula is modified
to contain the @i(form) as its new function.  @pr(change-formula) works
properly on any formula, regardless of whether the old function was local
or inherited from another formula.  If formula inheritance is involved,
this function makes sure that all the links are modified as appropriate.
If the @i(slot) does not contain a formula, nothing happens.

Note that this function cannot be used to install a fixed value on a slot
where a formula used to be; @pr(change-formula) only modifies the expression
within a formula.



@value(f-top)
@begin(example)
kr:Recompute-Formula @i(object slot)@value(function)
@end(example)
@index(recompute-formula)
@indexsecondary(Primary="Formulas", Secondary="Recompute-formula")

This function can be called to force a formula to be recalculated.  It may
be used in situations where a formula depends on values which are outside
of KR (such as application data, for example).  The formula stored in the
@i(slot) of the @i(object) is recalculated.  Formulas which depend on the
@i(slot), if any, are then marked invalid.



@value(f-top)
@begin(example)
kr:Mark-As-Changed @i(object slot)@value(function)
@end(example)
@index(mark-as-changed)@index(value propagation)
@indexSecondary(Primary="Formulas", Secondary="Mark-as-changed")

This function may be used to trigger constraint propagation for a
@i(object) whose @i(slot) has been modified by means other than
@pr(s-value).  Some applications may need to use destructive operations on
the value in a slot, and then notify the system that certain values were
changed.  @pr(Mark-as-changed) is used for this purpose.


@value(f-top)
@begin(example)
kr:Copy-Formula @i(formula)@value(function)
@end(example)
@index(change-formula)
@indexsecondary(Primary="Formulas", Secondary="Copy-Formula")

This function returns a copy of the given @i(formula), which should be a
formula object.  The copy shares the same prototype with the @i(formula), and
its initial value is the current cached value of the @i(formula).


@value(f-top)
@begin(example)
kr:Move-Formula @i(from-object from-slot to-object to-slot)@value(function)
@end(example)
@index(move-formula)

This function takes a formula from a
slot in an object and moves it to another slot in another object.  This
function is needed because one cannot move a formula from one slot to
another simply by storing the formula in some temporary variable (this
creates potentially serious problems with formula dependencies).


@value(f-top)
@begin(example)
kr::Make-Into-O-Formula @i[formula] &optional @i[compile-p]  @value(function)
@end(example)

This function modifies formulas created using the
function @pr[formula] to behave as if they were created using
@pr[o-formula].  This is useful for tools like Lapidary that need to
construct formulas on the fly.  The converted formulas will be handled
properly by
functions such as @pr[opal:write-gadget].  It is also possible to specify that
the formula's expression be compiled during the transformation.
If @i[compile-p] is non-NIL, the
@i[formula]'s expression is compiled in the process.


@value(f-top)
@begin(example)
kr:G-Cached-Value @i(object slot)@value(function)
@end(example)
@index(g-cached-value)

This function is similar to @pr(gv) if the @i(slot) contains an
ordinary value.  If the @i(slot) contains a formula, however, the cached
value@index(cached values) of the formula is returned even if the formula
is invalid; the formula itself is never re-evaluated.  Only advanced
applications may need this function, which in some cases
returns out-of-date values and therefore should be used with care.



@value(f-top)
@begin(example)
kr:Destroy-Constraint @i(object slot)@value(function)
@end(example)
@index(destroy-constraint)

If the @i(slot) of the @i(object) contains a formula, the constraint is
removed and replaced by the current value of the formula.  The formula is
discarded and all dependencies are updated.  Dependent formulas are notified
that the formula has been replaced by the formula's value, even if the actual
value does not change.  If the @i(slot) contains an ordinary value, this
function has no effect.

Note that the expression @t{(s-value object slot (gv object slot))}
cannot be used to simulate @pr(destroy-constraint).  This is because
@pr(s-value) does not remove a formula when it sets a slot to an ordinary
value, and thus the expression above would simply set the cached value of
the formula without removing the formula itself.


@value(f-top)
@index(with-dependencies-disabled)
@begin(example)
kr::With-Dependencies-Disabled &body @i[body] @value(macro)
@end(example)
This macro can be used to prevent the evaluation of @pr[gv] and @pr[gvl] inside
formulas from setting up dependencies.  Inside the body of the macro, @pr[gv]
and @pr[gvl]
effectively behave (temporarily) exactly like @pr[g-value]. 
This macro should be used with great care, as it may cause formulas
not to be re-evaluated if dependencies are not set up correctly.



@section[Constant Formulas]
@value(s-top)
@label(constant-slots)
@index(constant slots) 
It is possible to declare that certain slots are constant, and
cause all formulas that only depend on constant slots to be
eliminated automatically.  The main advantage of this approach is 
that it reduces storage and execution time.

A slot in an object can be declared constant at object creation time.
This guarantees that the application program will never change the
value of the slot after the object is created.  When a formula is
evaluated for the first time, KR checks whether it depends exclusively
on constant slots.  If this is the case, the formula is eliminated and
its storage is reused.  The slot on which the formula was originally
installed takes the value that was computed by the formula.

A slot can become constant in one of three ways.  First, the slot may
be declared constant explicitly.  This is done by listing the name of
the slot in the @pr[:constant] slot of an object (see below for more
details), or calling @pr(declare-constant) on the slot after its object has
already been created.  For example, adding the following code to
@pr[create-instance] for object @i(A) will cause slots @pr[:left] and
@pr[:top] to be declared constant in object @i(A): @w{@pr[(:constant '(:left
:top))]}.  Note that it is possible for the value of the @pr[:constant]
slot to be computed by a formula, which is evaluated once at object
creation time.

Second, a slot may become constant because it is declared constant in
the object's prototype.  In the example above, if object @i(B) is created
with @i(A) as its prototype, slots @pr[:left] and @pr[:top] will be declared
constant in @i(B), even if they are not explicitly mentioned in object @i[B]'s
@pr[:constant] slot.

Third, a slot may become constant because it contains a formula which
depends exclusively on constant slots.  After the formula is removed,
the slot on which it was installed is declared constant.  Thus,
constants propagate recursively through formulas.@foot[In the most elegant
programming style, a minimum number of constants will be declared in an object,
and formulas will be allowed to become constant because of their dependencies
on the constant slots (rather than bluntly declaring the formulas constant).
This is certaintly not a requirement of programming with constants, however.]
If you cannot figure out why a formula is not being eliminated, the function
@pr(garnet-debug:why-not-constant) and related functions in the Debugging Tools
Reference Manual may be useful.

To facilitate the creation of the list of constant slots for an
object, the syntax of the @pr[:constant] slot is extended as follows.
First, a prototype may specify a list of all the slots that its
instances may choose to declare constant.  This is done by specifying
a list of slot names in the prototype, using the slot
@pr[:maybe-constant].  When this is done in the prototype, an instance
may choose to declare all of those slots constants by simply adding
the value @value(t) to its @pr[:constant] slot.  Note that @value(t)
does @i(not) mean that @i(all) slots are constant; it only means that
all slots in the @pr(:maybe-constant) list become constant.
 
@index(maybe-constant)
It is also possible for the
instance to add more constant slots as necessary.  Consider the
following example:
@begin(programexample)
(create-instance 'PROTO NIL (:maybe-constant '(:left :x1 :x2 :width)))
(create-instance 'INST PROTO (:constant '(:top :height T)))
@end(programexample)
No slot is declared constant in the prototype, i.e., object PROTO,
because the @pr[:maybe-constant] slot does not act on the object
itself.  However, because object INST includes the value @value(t) in its
@pr[:constant] slot, the list of constant slots in the instance is the
union of the slots that are declared constant locally and the slots
named in the @pr[:maybe-constant] slot of the prototype.  Therefore,
the following slots are constant in INST: @pr[:left, :top, :width,
:height, :x1,] and @pr[:x2].

The slot @pr[:maybe-constant] is typically used in prototypes to
specify the list of all the parameters of the instances, i.e., the
slots that an instance may customize to obtain gadgets with the
desired appearance.  Consider, for example, the prototype of a gadget.
If the application is such that a gadget instance will never be
changed after it is created, the application programmer may simply
specify @w{@pr[(:constant '(T))]}.  This informs the system that all
parameters declared by the creator of the prototype are, in fact,
constant, and formulas that depend on them can be eliminated once the
gadget is created.  All of the standard objects and gadgets supply a
@pr(:maybe-constant) slot.

The syntax of the @pr[:constant] slot also allows certain slots that
appeared in the @pr[:maybe-constant] list to be explicitly excluded from the
constant slots in an object.  This can be done by using the marker
@pr[:except] in the @pr[:constant] slot.  The slots following this marker
are removed from the list that was specified by the prototype.  If a
slot was not mentioned in the prototype's @pr[:maybe-constant] slot,
the @pr[:except] marker has no effect on the slot.  The following is a
comprehensive example of the syntax of the @pr[:constant] slot:
@begin(programexample)
(create-instance 'INST-2 PROTO
   (:CONSTANT '(:top :height T :EXCEPT :width :x2)))
@end(programexample)

As a result, these slots are declared constant in object
INST-2: @pr[:left, :top, :height,] and @pr[:x1].


It is an error to set slots that have been declared constant.  This
can happen in three cases: a slot may be set using @pr[s-value] after
having been declared constant, a call to @pr[create-instance] may
redefine in the instance a slot that was declared constant in the
prototype, or @pr(destroy-slot) may be used.  In all cases, a
continuable error is signaled.  Note that 
this behavior can be overridden by wrapping the code in the macro
@pr[with-constants-disabled] (see below).


@value(f-top)
@begin(example)
kr:Declare-Constant @i(object slot)@value(function)
@end(example)

@index(declare-constant)
The function @pr[declare-constant] may be used to declare slots
constant in an object after creation time.  The function takes an
object and a slot, which is declared constant.  The behavior is the
same as if the slot had been declared in the @pr[:constant] slot at
instance creation time, although of course the change does not affect
formulas which have already been evaluated.  The @pr[:constant] slot
of the object is modified accordingly: the new slot is added, and it
is removed from the @pr[:except] portion if it was originally declared
there.  As a special case, if the second argument is @value(t) all the
slots that appear in the slot @pr[:maybe-constant] (typically
inherited from a prototype) are declared constant.  This is similar to
specifying T in the @pr[:constant] slot at instance creation time.

If @pr(declare-constant) is executed on a slot while constants are disabled
(i.e., inside of a @pr(with-constants-disabled) body), the call will have no
effect and the slot will not become constant.


@value(f-top)
@begin(example)
kr:With-Constants-Disabled &body @i[body]@value(macro)
@end(example)

@index(with-constants-disabled)
The macro @pr[with-constants-disabled] may be used to cause all
constant declarations to be temporarily ignored.  During the execution
of the body, no error is given when slots are set that are declared
constant.  Additionally, constant declarations have no effect when
@pr[create-instance] is executed inside this macro.  This macro,
therefore, is intended for experienced users only.

Several functions in the @pr(garnet-debug) package (loaded with Garnet by
default) can be helpful in determining which slots in your application should
be declared constant for maximum benefit, and can help you determine why some
slots are not becoming constant.  These functions are documented in the
Debugging Tools Reference Manual, which starts on page @value(debug):

@begin(programexample)
gd:Record-From-Now@value(function)

gd:Suggest-Constants @i(object) &key @i(max) (@i[recompute-p] T) (@i[level] 1)@value(function)

gd:Explain-Formulas @i(aggregate) &optional (@i[limit] 50) @i[eliminate-useless-p]@value(function)

gd:Find-Formulas @i(aggregate) &optional (@i[only-totals-p] T) (@i[limit] 50) @i[from]@value(function)

gd:Count-Formulas @i(object)@value[function]

gd:Why-Not-Constant @i(object  slot)@value[function]
@end(programexample)


@section(Efficient Path Definitions)
@value(s-top)

@index(kr-path)
The function @pr[kr-path] can be used to improve the efficiency of
formula access to slots that are obtained via indirect links.
Inside formula expressions, macros such as @pr[gv] are used to access a slot
indirectly, traversing a number of objects until the last slot is
obtained.  This is sometimes called a @i[link] or a @i[path].  For
example, the expression @pr[(gvl :parent :parent :left)] will access
the @pr[:left] slot in the parent's parent.  If the application program
can guarantee that the intermediate path will not change, the
function @pr[kr-path] provides better performance.  The expression
above could be written as:
@begin(programexample)
(gv (kr-path 0 :parent :parent) :left)
@end(programexample)
The call to @pr[kr-path] computes the object's parent's parent
only once, and stores the result as part of the formula.  Subsequent
evaluations of the formula only need to access the @pr[:left]
slot of the target object.  The syntax is:
@blankspace(.5 line)
@begin(example)
kr:KR-Path @i(path-number) &rest @i(slots)@>[@i(Macro)]
@end(example)
@blankspace(.5 line)
The path-number is a 0-based integer which indicates the number of
this path within the formula expression.  In most cases, a formula
contains only one call to @pr[kr-path], and path-number is 0.  If more
than one path appears in a formula expression, different numbers
should be used.  For example,

@begin(programexample)
(or (gv (kr-path 0 :parent :parent) :left)
    (gv (kr-path 1 :alternate :parent) :left))
@end(programexample)
Note that @pr[kr-path] can only be used inside a formula expression.


@begin(group)
@section[Tracking Formula Dependencies]

The function @pr[kr::i-depend-on] can be used to find out all the
objects and slots upon which a certain formula depends directly.  The
syntax is:

@index(i-depend-on)
@blankspace(.5 line)
@begin(example)
kr::I-Depend-On @i[object slot]  @value(function)
@end(example)
@end(group)
@blankspace(.5 line)

If the @i[slot] in the @i[object] does not contain a formula, this
function returns NIL.  Otherwise, the function returns a list of
dotted pairs of the form @pr[(obj . slot)], which contains all the slots
upon which the formula depends.  Note that this is the list of only
those slots that are used by the formula directly; if some of those
slots contain other formulas, @pr[kr::i-depend-on] does not pursue
those additional formulas' dependencies.

@blankspace(1 line)
@begin(group)
Example:
@begin(programexample)
(create-instance 'A NIL (:left 7))
(create-instance 'B A (:left 14) (:top #f(+ (gvl :left) (gv a :left))))
(gv b :top)	; set up the dependencies

(kr::i-depend-on B :top)
    ==> ((B . :LEFT) (A . :LEFT))
@end(programexample)
@end(group)



@section[Formula Meta-Information]
@index(meta-information)

It is possible to associate arbitrary information (sometimes known
as meta-information) with formulas, for example for documentation or
debugging purposes.  Meta-information is internally represented by a
KR object which is associated with the formula; this allows
essentially any slot to be added to formulas.  Meta-information can be
inherited from parent formulas, and is copied appropriately by
functions such as @pr[copy-formula].

In addition, it is possible to access built-in formula information
(such as the lambda expression that was used to create the formula)
using exactly the same mechanism that is used to access
meta-information.  This provides a single, well-documented way to
access all information associated with a formula.

@subsection[Creating Meta-Information]

Meta-information can be specified statically at formula creation time,
and also dynamically for already existing formulas.  Static
meta-information is specified by additional parameters to the
functions @pr[formula] and @pr[o-formula].  The additional parameters
are slot specifications, in the style of @pr[create-instance] (except
that, of course, special @pr[create-instance] keywords such as
@c[:declare] or @c[:override] are not supported).  For example, the
expression:
@begin(programexample)
 (o-formula (gv a :top) 15
   (:creator 'GILT) (:date "today"))
@end(programexample)
creates a new formula with initial value 15, and two meta-slots named
@pr[:creator] and @pr[:date].

Note that in order to specify meta-information statically, one has to
specify the default initial value for the formula, which is also an
optional parameter.

@index(s-formula-value)
Meta-information may also be created dynamically, using the function
@begin(example)
kr:S-Formula-Value @i[formula slot value]		@value(function)
@end(example)

This function sets the value of the meta-slot @i[slot] in the
@i[formula] to be the specified @i[value].  If the @i[formula] does
not already have an associated meta-object, one is created.

It is not possible to use this function to alter one of the built-in
formula slots, such as the formula's lambda expression or its list of
dependencies.


@begin(group)
@subsection[Accessing Meta-Information]
@index(g-formula-value)

Meta-information can be retrieved using the function
@pr[g-formula-value].  In addition to slots that were specified
explicitly, this function also makes it possible to retrieve the
values of all the special formula slots, such as the formula's parent
or its compiled expression.
@begin(example)
kr:G-Formula-Value @i[formula slot]		@value(function)
@end(example)
@end(group)

The function returns the value of meta-slot @i[slot] for the @i[formula].
If the latter is not a formula, or the meta-slot is not present, the
function returns NIL.  If the @i[formula] inherits from some other
formula, inheritance is used to find the meta-slot.

@begin(group)
As a convenience, @i[slot] can also be the name of an internal formula
slot, i.e., one of the structure slots used by KR when handling
formulas.  Such slots should be treated strictly as read-only, and should never
be modified by application programs.  The built-in slot names are:

@begin[description, indent=-2]
@c(:depends-on) - returns the object, or list of objects, on which the
formula depends.

@c(:schema) - returns the object on which the formula is currently installed.

@c(:slot) - returns the slot on which the formula is currently installed.

@c(:cached-value) - returns the current cached value of the formula,
whether or not the formula is currently valid.

@c(:valid) - returns T if the formula is currently valid, NIL otherwise.

@c(:path) - returns the path accessor associated with the formula, if any.

@c(:is-a) - returns the parent formula, or NIL if none exists.

@c(:function) - returns the compiled formula expression.

@c(:lambda) - returns the original formula expression, as a lambda list.

@c(:is-a-inv) - returns the list of formulas that inherit from the
@i[formula], or NIL.  If there is only one such formula, a single value
(not a list) is returned.

@c(:number) - returns the internal field which encodes the valid/invalid
bit, and the cycle counter.

@c(:meta) - returns the entire meta-object associated with the formula, or
NIL if none exists.
@end[description]
@end(group)
@blankspace(1 line)

@begin(group)
When the function @pr(ps) is given a formula, it can print
associated meta-information.  The latter is printed as an
object, immediately after the formula itself.  For example:
@begin(programexample)
@b[lisp>] (create-instance 'A NIL
        (:left (o-formula (gvl :parent :left) 100
                          ;; Supply meta-information here
                          (:name "Funny formula")
                          (:creator "Application-1"))))
#k<A>

@b[lisp>] (ps (get-value A :left))           ; prints the following:
{F8
  lambda:        (gvl :parent :left)
  cached value:  (100 . NIL)
  on schema A, slot :LEFT
  }
  ---- meta information (S7):
{S7
  :NAME =  "Funny formula"
  :CREATOR =  "Application-1"
  }
@end(programexample)
@end(group)




@section(Demons)
@value(s-top)
@label(demons)

The demon mechanism allows an application program to perform a certain
action when a value is modified.  This mechanism, which is totally
controlled by the application program, is independent from value
propagation.  Regular Garnet users do not need to know the contents of
this section, since Garnet already defines all appropriate demons.
Garnet applications should never modify the default demons, which are
defined by Opal and automatically update the graphical representation
of the application's objects.


@Subsection(Overview of the Demon Mechanism)

A demon is an application-defined @index(procedural attachments)
procedural attachment to a KR schema.  Demons are user-defined fragments
of code which are invoked when certain actions are performed on a
schema.  Whenever the value of a slot in a schema is modified (either
directly or as the result of value propagation), KR checks whether a
demon should be invoked.  This allows application programs to 
be notified every time a change occurs.

Two separate demons invoked at different times allow an application
program to have fine control over the handling of value changes.
These demons are only invoked on slots that are listed in the
@c(:update-slots) list of a schema (see section @ref[update-slots]).

The first demon is the @i(invalidate demon)@index(invalidate demon).  This
demon is invoked every
time a formula is invalidated.  At the time the demon is invoked, the
formula has not yet been re-evaluated, and thus it contains the old cached
value.  This demon is contained in the @pr(:invalidate-demon) slot of
an object.  This makes it possible for different objects to provide
customized demons to handle slot invalidation.

@index(pre-set demon)
The second demon is the pre-set demon.  It is
invoked immediately before the value in a formula is actually
modified, and it is passed the new value.  This allows the pre-set
demon to record the difference between the old and the new value, if
needed.  This demon is stored in the variable
@index(*pre-set-demon*) @pr(kr::*PRE-SET-DEMON*).  Garnet does not use
the pre-set demon.


The relationship between value propagation and demon invocation is best
illustrated by showing the complete sequence of events for the invalidate
demon.  This is what
happens when @pr(s-value) is called to set slot @b(s) of schema S to value
@b(v):
@begin(enumerate)
If slot @b(s) already contains value @b(v), nothing happens.

Otherwise, if slot @b(s) should trigger demons, the demon is
invoked.  The demon is called with schema S in its @i(old) state,
which means that slot @b(s) still contains its old value.

The change is recursively propagated.  All slots whose value is a formula
that depends on slot @b(s) are invalidated.  The process is similar to the
one described in step 2, but there is no check corresponding to step 1 at
this point.  Demons are invoked normally on any slot that is modified
during this phase.

The value of slot @b(s) is finally changed to @b(v).
@end(enumerate)

Both the invalidate demon and the pre-set demon should be functions of
three arguments.  The first argument is the schema which is being modified.
The second argument is the name of the slot which is being modified.  The
third argument is always @value(nil) for the invalidate demon.  For the
pre-set demon, the third argument is the new value which is about to be
installed in the slot.  This allows the pre-set demon to examine both the
old value (which is still in the slot) and the new value.


@Subsection(The :Update-Slots List)
@label(update-slots)
@index(update-slots)

The KR demons are only invoked on slots that are listed in the
@c(:update-slots) list of the schema containing them.  For example,
Garnet defines a particular demon that is responsible for redrawing the
objects in a window as the values of their "interesting" slots change.
These "interesting" slots are declared in each object's @c(:update-slots)
declaration during @pr(create-instance) (the declaration is usually inherited
from the prototype, so that typical Garnet users will never see this
declaration).  The @c(:update-slots) list contains all the slots in an object
that should cause Opal's special demon to be invoked when they are modified.
When an update-slot is modified, Opal's demon will "invalidate"
the @u(object), causing it to be redrawn during the next pass of the update
algorithm.

The @c(:update-slots) list can only be set directly at @pr(create-instance)
time.  That is, after an object is created it is no longer sufficient to
modify the value of the @pr(:update-slots) slot to change whether a slot is an
update-slot or not.  This is because update-slots are internally represented
by a bit associated with the slot, which is set during the @pr(create-instance)
call.  Instead of setting the @pr(:update-slots) slot, you must call the
function:

@index(add-update-slot)
@begin(example)
kr::Add-Update-Slot @i[object slot] &optional (@i[turn-off] NIL)  @value(function)
@end(example)

If @i[turn-off] is NIL (the default), the @i[slot] in the @i[object]
is declared as an update-slot; if @i[turn-off] is non-NIL, the slot is
no longer an update slot.  In addition to setting or resetting the
internal bit, the function also modifies the @c[:update-slots] slot
accordingly, by adding or removing the @i[slot] from the list.



@Subsection(Examples of Demons)


The following example shows how to define the invalidate demon for an
object, and how the demon is invoked.

@begin(programexample)
;;; Define an invalidate demon
;;;
(defun inv-demon (schema slot v)
  (declare (ignore v))     ; v is not used
  (format t
	  "schema ~s, slot ~s is being invalidated.~%"
	  schema slot))

(create-schema 'A (:left 10)
  (:top (o-formula (1+ (gvl :left))))
  (:update-slots '(:top))
  (:invalidate-demon 'inv-demon))

(gv A :top)  ==> 11
(s-value A :left 1)
;; prints out:
schema #k<A>, slot :TOP is being invalidated.
(gv A :top)  ==> 2
@end(programexample)


@Subsection(Enabling and Disabling Demons)

@value(f-top)
@begin(example)
kr:With-Demons-Disabled &body @i(body)@value(macro)
@end(example)
@index(with-demons-disabled)

The @i(body) of this macro is executed with demons disabled.  Constraints are
propagated as usual, but demons are not invoked.

This macro is often useful when making temporary changes@index(action
propagation) to schemata which have un update demon.  This happens,
for instance, when a program is changing graphical objects but does
not want to display the changes to the user, or when some of the
intermediate states would be illegal and would cause an error if
demons were to run.  Objects may be freely modified inside the
@i(body) of this macro without interference from the demons.


@value(f-top)
@begin(example)
kr:With-Demon-Disabled @i(demon) &body @i(body)@value(macro)
@end(example)
@index(with-demon-disabled)

This is similar to @pr[with-demons-disabled], except that it allows a
specific demon to be disabled.  Normally, when
@pr[with-demons-disabled] is used, all demons are disabled.  This
macro allows all demons except a specific one to execute normally;
only the specific demon is disabled. 

The forms in the @i[body] are executed, but the
given @i[demon] is not invoked.  For example, the following will
selectively disable the invalidate demon provided by object FOO:
@begin(programexample)
(with-demon-disabled (gv FOO :invalidate-demon)
  (s-value FOO :left 100))
@end(programexample)
While FOO's own demon is not
executed, formulas in other objects which depend on FOO's
@pr[:left] slot will be invalidated, and the corresponding invalidate
demons will be invoked normally.



@value(f-top)
@begin(example)
kr:With-Demon-Enabled @i(demon) &body @i(body)@value(macro)
@end(example)
@index(with-demon-enabled)

This macro enables a particular demon if it had been disabled, either
explicitly or with @pr(with-demons-disabled).





@section(Multiple Inheritance)
@value(s-top)

KR supports multiple inheritance@index(multiple inheritance): a schema
may inherit values from more than one direct ancestor.  This can be
accomplished in two ways.  The first way is simply to connect the
schema to more than one ancestor schema through a relation.  The
relation slot, in other words, may contain a list of slots.  When
performing inheritance, KR searches each ancestor slot in turn until a
value is found.

The second way to achieve multiple inheritance is by using more than one
relation with inheritance.  Any schema may have several slots defined as
relations with inheritance; in this case, all relations are searched in
turn until a value is found.  The two mechanisms may be combined, of
course.

Application programs should not rely on the order in which KR searches
different relations.  The particular order is implementation-dependent.




@begin(comment)
@subsection(Inheritance: Implementation Notes)

KR uses a mechanism which enables inheritance to behave in the
dynamic fashion describe above and, at the same time, to provide extremely
efficient performance.  This mechanism is named @i(eager
inheritance)@index(eager inheritance).

Eager inheritance works as follows.  The first time the value of a slot is
requested, but the value is not present locally, the value is obtained by
inheritance as described above.  At this point, however, the value is also
copied into the local schema (and in any intervening schema, if necessary)
with a special marker which indicates that the value was inherited.

The second time the value is requested, inheritance is no longer required
and the value is immediately found locally.  This makes successive accesses
to inherited values much faster, and causes inheritance to be essentially
as efficient as local values, no matter how many levels of inheritance were
originally used.

It is vital that inherited values which were copied down into children
schemata be kept up to date.  Any change in the upper portions
of the schema hierarchy might change what values can be inherited by the
lower levels, and inherited values which were copied down must be modified.
KR performs this task immediately when a value which was inherited is
changed, thus justifying the term @i(eager inheritance).  This technique
ensures minimal overhead for both access and update of inherited values,
and provides superior performance for the inheritance mechanism.
@end(comment)


@section(Local Values)
@value(s-top)

This group contains functions which deal with local values in a slot.
Some of these functions do not
treat formulas as special objects, and thus can be used to access formulas
stored in a slot (remember that functions like @pr(gv), for example,
return the @i(value) of a formula, rather than the formula object itself).


@value(f-top)
@begin(example)
kr:Get-Value @i(object slot)@value(Macro)
@end(example)
@index(get-value)

Returns the value in the @i(slot) from @i(object).  If the slot is
empty or not present, it returns @value(nil).  Inheritance may be used when
looking for a value.  Given a slot that contains a formula, @pr(get-value)
returns the formula itself, rather than its value.  Therefore, its use is
limited to applications that manipulate formulas explicitly.

@begin(comment)
@value(f-top)
(GET-VALUES @i(object slot))@value(Macro)
@index(get-values)

This macro returns a list of all the values in the @i(slot) of the
@i(object).  If the @i(slot) is empty or not present, it returns
@value(nil).  Inheritance may be used when looking for values.  This macro
does not deal with constraints, i.e., it does not cause formulas to be
evaluated.@*Examples:
@begin(example)
(get-values my-graphical-object :is-a-inv) ==> 
	 (#k<BOX-OBJECT>)
(get-values box-object :is-a-inv) ==>
	 (#k<RECTANGLE-2> #k<RECTANGLE-1>)
@end(example)

Since @pr(get-values) does not deal with constraints, @pr(dovalues) (see
below) is the preferred way to access all values in a slot.  An additional
advantage is that the expression
@begin(example)
(dovalues (item object slot) ...)
@end(example)
is potentially more efficient than the equivalent idiom
@begin(example)
(dolist (item (get-values object slot))  ...)
@end(example)
which may create garbage in some situations.

A @b(setf) form@index[setf form for @pr(get-values)] is defined for
@pr(get-values) and expands into a call to @pr(set-values).



@value(f-top)
(SET-VALUES @i(object slot values))@value(Function)
@index(set-values)

This function stores a list of values in the @i(slot) of the @i(object).
The entire list may subsequently be retrieved with @pr(get-values), or the
first value may be retrieved with @pr(g-value).





@value(f-top)
(DOVALUES (@i(variable object slot) &key @i(local result formulas in-formula)) &rest @i(body))@value(macro)
@index(dovalues)

DOVALUES executes@index(value iterator)@index(iterators) the @i(body) with
the @i(variable) bound in turn to each value in the @i(slot) of the
@i(object).  The @i(body) is executed purely for side effects, and DOVALUES
normally returns @value(nil); if the keyword argument @pr(:result) is
specified, however, the given value is returned.
The @i(body) of @pr(dovalues) should never alter the contents of the
@i(slot), since this may cause unpredictable results.

If @pr(:local) (default @value(nil)) is non-@value(nil), DOVALUES only
considers local values; otherwise, it iterates over inherited values
if no local values are present.  If @b(:formulas) is @value(T) (the
default), any value which is expressed by a formula is computed and
returned; otherwise, the formula itself is returned.  The latter is
only useful for more advanced applications.@*Examples:
@begin(example)
(set-values rectangle-1 :vertices '(3 6 72))

(dovalues (v rectangle-1 :vertices)
   (format t "rectangle-1 has vertex ~S~%" v))
@i(;; prints out:)
rectangle-1 has vertex 3
rectangle-1 has vertex 6
rectangle-1 has vertex 72
@end[example]

@begin[example]
(s-value-n rectangle-1 :vertices 2
   (o-formula (+ (gvl :vertices) 15)))
(dovalues (v rectangle-1 :vertices)
   (format t "rectangle-1 has vertex ~S~%" v))
@i(;; prints out:)
rectangle-1 has vertex 3
rectangle-1 has vertex 6
rectangle-1 has vertex 18

;;; Example of :formulas NIL
(dovalues (v rectangle-1 :vertices :formulas NIL)
   (format t "rectangle-1 has vertex ~S~%" v))
rectangle-1 has vertex 3
rectangle-1 has vertex 6
rectangle-1 has vertex #k<F2297>
@end(example)

@pr(dovalues) may also be used inside formulas; in this case,
@pr(:in-formula) should be set to @value(T).  The surrounding formula
is then re-evaluated when any of the values in the @i(slot) is
changed, or whenever a value is added or deleted.  @pr(dovalues) with a
non-@value(nil) @pr(:in-formula) option, therefore, behaves more like @pr(gv)
than like @pr(g-value).  When @pr(dovalues) is used in this fashion, the
keyword @pr(:self) may be used to stand for the object to which the
formula is attached.

The following is an example of a formula which uses @pr(dovalues) and is
re-evaluated when one of the values in the @pr(:components) slot changes:@*
@begin(example)
(o-formula (let ((is-odd NIL))
	    (dovalues (value :SELF :components 
		       :in-formula T)
	      (if (odd value) (setf is-odd T)))
	    is-odd))
@end(example)
@end(comment)




@value(f-top)
@begin(example)
kr:Get-Local-Value @i(object slot)@value(Macro)
@end(example)
@index(get-local-value)

Returns the value in the @i(slot) from @i(object).  If the slot is
empty or not present, it returns @value(nil).  Inheritance is not used, and
only local values are considered.  
Given a slot that contains a formula, @pr(get-local-value) returns the
formula itself, rather than the formula's value.  Therefore, use of this
macro is limited to applications that manipulate formulas explicitly.



@begin(comment)
@value(f-top)
(GET-LOCAL-VALUES @i(object slot))@value(Macro)
@index(get-local-values)@index(inheritance)@index(local values)

Similar to GET-VALUES, but only local slots are examined and inheritance is
never used.@*
Examples:
@begin(example)
(get-values rectangle-1 :thickness) ==> (1)
(get-local-values rectangle-1 :thickness) ==> NIL @i(; not local)
@end(example)

This macro does not deal with constraints, i.e., it never causes formulas
to be evaluated.
@end(comment)



@value(f-top)
@begin(example)
kr:G-Local-Value @i(object) @i(slot) &rest @i(other-slots)@value(macro)
@end(example)
@index(g-local-value)

This macro is very similar to @pr(g-value), except that it only considers
local values.  Inheritance is never used when looking for a value.



@value(f-top)
@begin(example)
kr:Gv-Local @i(object slot) &rest @i(more-slots)@value(macro)
@end(example)
@index(gv-local)

This macro is similar to @pr(gv), except that it only considers local
values, and it never returns an inherited value.  @pr(Gv-local) should be 
used in situations where it is important to only retrieve values that are
local to the @i(object).



@begin(comment)
@value(f-top)
(APPEND-VALUE @i(object slot value))@value(Function)
@index(append-value)

This function adds the @i(value) to the end of the list of values in
the @i(slot) of the @i(object).



@value(f-top)
(DELETE-VALUE-N @i(object slot position))@value(Function)
@index(delete-value-n)

This function deletes the @i(position)-th value from the @i(slot) of the
@i(object).  @i(position) is a 0-based non-negative integer.  This function
does not deal with constraints properly, and should not be used when the
@i(slot) may contain formulas.
@end(comment)


@section(Local-only Slots)
@value(s-top)

@label(local-only)
There are cases when certain slots in an object should not be inherited by
any instance of the object.  An example of this situation might be a slot
which is used as a unique identifier; clearly, the slot should never be
inherited, or else errors will occur.  Such slots are called @i(local-only
slots).

This effect can be achieved in KR by listing the names of all such slots in
the prototype object.  The names are listed in the @c(:local-only-slots)
declaration (the general declaration syntax is discussed in section
@ref(uniform-syntax)).  This declaration should contain a list of two-element
sub-lists.  The first element in each sub-list specifies the name of a
local-only slot.  The second element can be @value(T) or @value(NIL).

The value @value(NIL) specifies that the local-only slot is always
initialized to @value(NIL) in any instance which does not define it
explicitly.  The value @value(T), on the other hand, specifies that the
current value of the local-only slot in the prototype will be used to
initialize the slot in the instance.  The value, however, is physically copied
down into the instance, and thus inheritance is no longer used for that
instance. Modifying the value in the prototype, in particular, will have no
effect on the instance.  This second option is used more rarely than
@value(NIL).

Note that none of the above applies to slots whose value (in the prototype) is
a formula.  Slots which contain formulas are always inherited, independent
of whether the slot is listed in @pr(:local-only-slots).



@section(Schema Creation Options)
@value(s-top)

Two special keywords can be used in the macros that create schemata.  These
options are recognized by @pr(create-instance), @pr(create-schema), and
@pr(create-prototype).  They are@label(create-options):

@value(f-top)
@example{:OVERRIDE@value(keyword)}
@index(override)

If the @i(object-name) in one of the object-creating macros names an
existing object, that object is normally deleted, together with its
instances, and replaced by a brand new object.  The default
behavior may be modified by using the keyword @b(:override) as part of the
@i(slot-definitions).  This keyword causes the existing object to be
modified in place and contain the union of its previous slots and those
specified by @pr(create-schema).  Previous slots that are not mentioned in the
call retain whatever values they had before the operation.  For example,

@begin(programexample)
(create-schema 'RECTANGLE-1 :override (:color :magenta))
@end(programexample)
adds a slot to the object RECTANGLE-1 if it already exists.  Without
the @pr(:override) keyword, this would have destroyed the object and created
a new one with a single slot.



@value(f-top)
@example{:NAME-PREFIX @i(string)@value(keyword)}
@index(name-prefix)

The keyword @b(:name-prefix)@index(name-prefix keyword) may be used to
specify a name prefix for unnamed objects.  Unnamed objects are normally
named after the object they are an instance of; this option allows a
specific string to be used as the name prefix.  The option, if specified,
should be immediately followed by a string, which is used as the prefix.
Example:
@Begin(programexample)
(create-schema NIL :name-prefix "ORANGE"
  (:left 34)) ==> #k<ORANGE-2261>
@end(programexample)




@section(Print Control)
@value(s-top)

This section describes the slots that control what portions of an object are
printed, and how they are printed.  The need for fine control over printing
arises, for example, when certain slots contain very large data structures
that take a long time to print.

The print control slots are taken from the object which is specified as the
@i(print-control) in the complicated form of @pr(ps), described below.  In
many cases, the slots are actually inherited by the object being printed.


@value(f-top)
@begin(example)
kr:PS @i(object) &key @i{types-p  all-p} (@i{control} T) (@i{inherit} NIL) @value(function)
                (@i{indent} 0) (@i{stream} *standard-output*)
@end(example)
@index(ps)@index(printing schemata)

This form of @pr(ps) prints the contents of the @i(object), and allows fine
control over what to print and how.  A possible behavior is to print out
all slots and all values in @i(object); this happens when the @i(control)
object is @value(nil).  It is possible, however, to cause @pr(ps) to ignore
certain slots and to specify that others should be printed in a given
order.  It is also possible to limit the number of elements printed for list
values, thus preventing annoyingly long lists of values.

The function @pr[ps] can print out type information, if desired.  This
can be specified with a non-null value for the new keyword parameter
@i[types-p] (the default value is NIL).  Type declarations are printed in
square brackets.

Supplying a non-NIL value for the @i(all-p) parameter will cause @pr[ps]
to print out all slots of the @i{object}, including slots that do not currently
have any value.  The default for @i(all-p) is NIL.

The value of @i(control)@index(controlling printing) should be one of four
things:
@begin(enumerate)
@value(NIL), which means that the @i(object) is printed in its entirety.

@value(t), the default, which means that the @i(object) itself is used as
the control object.  In most cases, the control slots are inherited from an
ancestor of the @i(object).  All Opal prototypes, for example, define
appropriate slots which reduce the amount of information that is shown by
@pr(ps).

an object, which is used directly as the control object.

the keyword @pr(:default), which indicates that the KR-supplied default
print control object should be used.  The name of the default print control
object is @c(print-schema-control) @index(print-schema-control), an object
in the KR package.  This default object limits the length of lists that
are printed by @pr(ps) to a maximum of ten for ordinary slots, and five for
the @pr(:is-a-inv) slot.
@end(enumerate)

If the @i(inherit) option is @value(nil) (the default), only local slots
are printed.  Otherwise, all inheritable values from all prototypes of
@i(object) are inherited and printed; inherited values are clearly indicated
in the printout.  As discussed in Chapter @ref(object-oriented-prog), formulas
are not copied down from prototypes until they are requested by @pr(gv) or
@pr(g-value).  Formulas that have not yet been copied down will not be shown
by @pr(ps), unless the @i(inherit) parameter is non-@value(nil).

The @pr(:indent) option is only used by debugging code which needs to
specify an indentation level.  This option is not needed by regular
application programs.

@pr(Ps) prints slots whose value is a formula in a special way.  Besides the
name of the formula, the current cached value of the formula is
printed in parentheses, followed by @value(t) if the cache is valid or
@value(nil) otherwise.  Example:
@begin(programexample)
(create-schema 'A
  (:left 10) (:right (o-formula (+ (gvl :left) 25))))
(gv A :right)  ==> 35

(ps A)
@i(;; prints out:)
{#k<A>
  :DEPENDED-SLOTS =  (:LEFT #k<F2285>)
  :RIGHT =  #k<F2285>(35 . T)
  :LEFT =  10
}
@end[programexample]

@begin[programexample]
(s-value A :left 50)
(ps A)
@i(;; prints out:)
{#k<A>
  :DEPENDED-SLOTS =  (:LEFT #k<F2285>)
  :RIGHT =  #k<F2285>(35 . NIL)
  :LEFT =  50
}
@end(programexample)
The cached value is not correct, of course, but it will be recomputed as
soon as its value is requested because formula @pr(F2285) is marked invalid.

The function @pr[ps] prints the expression of a formula, when given
the formula as argument.  A formula is printed with three pieces
of information: the expression, the cached value (which is printed as
before), and the object and slot on which the formula is installed.

If @i(stream) is specified, it is used for printing to a stream other than
standard output.


@subsection(Print Control Slots)
@label(print-control-slots)

If a control object is specified in a call to @pr(ps), it should contain (or
inherit) six special slots.  These slots determine what @pr(ps) does.  The
meaning of the print control slots is as follows:

@begin(itemize)
@pr(:sorted-slots)@index(sorted-slots slot) contains a list of names
of slots that should be printed before all other slots, in the desired
order.

@pr(:ignored-slots)@index(ignored-slots slot) contains a list of names of
slots that should not be printed.  A summary printed at the end of the
object indicates which slots were ignored.

@pr(:global-limit-values)@index(global-limit-values slot) contains an
integer, the maximum number of elements that should be printed for each list
that is a value for a slot.
If a list contains more than that many elements, ellipsis are printed after
the given number to indicate that not all elements of the list were actually
displayed.

@pr(:limit-values)@index(limit-values slot) allows the same control on a
slot-by-slot basis.  It should contain lists of the form @t{(slot number)}.
If a slot name appears in one of these lists, the number specified there is
used instead of the one specified in @pr(:global-limit-values).

@pr(:print-as-structure)@index(print-as-structure slot) can be @value(t),
in which case the #k<> notation is used when printing object names, or
@value(nil), in which case only pure object names are printed.

@pr(:print-slots)@index(print-slots slot) is a list of the slots that are
printed as part of the @i(#k<>) notation.  It is possible to cause @pr(ps)
to print a few slots from each object, inside the @i(#k<>) printed
representation; this may make it easier to identify different schemata.
@pr(:print-slots) should contain a list of the names of the slots which
should be printed this way.  Note that this option has no effect if schema
names are not being printed with the @i(#k<>) notation.
@end(itemize)



The following is a rather comprehensive example of fine control over what
@pr(ps) prints.
@begin(programexample)
@i(; Use top level of the hierarchy to control printing.)
(create-schema 'TOP-OBJECT
  (:ignored-slots :internal :width))

(create-schema 'COLORED-THING (:color :blue) (:x 10)
  (:is-a TOP-OBJECT) (:width 12.5) (:y 20) 
  (:internal "Some information"))

(dotimes (i 20) (create-instance NIL COLORED-THING))
@end(programexample)
Using @pr(ps) with a null @i(control) prints out the whole contents of
the schema:
@begin(programexample)
(ps COLORED-THING :control NIL)
@i(;; prints out:)
@end[programexample]
@begin[programexample]
{#k<COLORED-THING>
  :IS-A-INV =  #k<COLORED-THING-2265> 
    #k<COLORED-THING-2266> #k<COLORED-THING-2267>
    #k<COLORED-THING-2268> #k<COLORED-THING-2269>
    #k<COLORED-THING-2270> #k<COLORED-THING-2271>
    #k<COLORED-THING-2272> #k<COLORED-THING-2273>
    #k<COLORED-THING-2274> #k<COLORED-THING-2275>
    #k<COLORED-THING-2276> #k<COLORED-THING-2277>
    #k<COLORED-THING-2278> #k<COLORED-THING-2279>
    #k<COLORED-THING-2280> #k<COLORED-THING-2281>
    #k<COLORED-THING-2282> #k<COLORED-THING-2283>
    #k<COLORED-THING-2284>
  :INTERNAL =  "Some information"
  :Y =  20
  :X =  10
  :COLOR =  :BLUE
  :WIDTH =  12.5
  :IS-A =  #k<TOP-OBJECT>
}
@end(programexample)
Using the system-supplied default control object reduces the clutter in the
@pr(:is-a-inv) slot, and also eliminates printing of schemata with the
special #k<> convention:
@begin(programexample)
(ps COLORED-THING :control :default)
{COLORED-THING
  :WIDTH =  12.5
  :IS-A-INV =  COLORED-THING-2265 COLORED-THING-2266 
     COLORED-THING-2267 COLORED-THING-2268
     COLORED-THING-2269 ...
  :INTERNAL =  "Some information"
  :Y =  20
  :X =  10
  :COLOR =  :BLUE
  :IS-A =  TOP-OBJECT
}
@end(programexample)
We can make things even better by using the object itself to inherit the
control slots.  We add sorting information and a global limit to the number
of elements to be printed for each list.  We do this at the highest level in
the hierarchy, so that every object can inherit the information:
@begin(programexample)
(s-value TOP-OBJECT :global-limit-values 3)
(s-value TOP-OBJECT :sorted-slots 
  '(:is-a :color :x :y))

(ps COLORED-THING)
@i(;; prints out:)
{COLORED-THING
  :IS-A =  TOP-OBJECT
  :COLOR =  :BLUE
  :X =  10
  :Y =  20
  :IS-A-INV =  COLORED-THING-2265 COLORED-THING-2266
     COLORED-THING-2267 ...
  List of ignored slots:   WIDTH INTERNAL
}
@end(programexample)



@subsection[Slot Printing Functions]

It is possible to use the basic mechanism used by the function
@pr[ps] to print or format objects in a customized way.  This facility
is used by applications such as the @pr(garnet-@|debug:Inspector),
which need full control
over how objects are displayed.  This mechanism is supported by the
following function.

@index(call-on-ps-slots)
@begin(example)
kr::Call-On-PS-Slots @i[object  function] &key (@i[control] T) @i[inherit] @value(function)
                     (@i[indent] NIL) @i[types-p  all-p]
@end(example)

The @i[function] is called in turn on each slot that would be printed
by @pr[ps].  The keyword arguments have exactly the same meaning as in
@pr[ps].  The @i[function] should take nine arguments, as follows:
@begin(programexample)
(lambda (object slot formula is-inherited valid real-value types-p bits indent limits))
@end(programexample)
When the function is called, the first argument is the object being
displayed; the second argument is bound to each slot in the object, in
turn.  The @i[formula] is set to NIL (for slots that contain
non-formula values), or to the actual formula in the @i[slot].  The
parameter @i[is-inherited] is T if the value in the @i[slot] was
inherited, NIL if the value was defined locally.  The parameter
@i[valid] is NIL if the @i[slot] contains a formula whose cached value
is invalid; it contains T if the slot contains a valid formula, or any
non-formula value.  The parameter @i[real-value] is whatever g-value
would actually return.  The parameter @i[types-p] is set to T if the
@i[function] should process type information for the @i[slot]; its
value simply reflects the value passed to @pr[kr::call-on-ps-slot].
The parameter @i[bits] contains the internal bitwise representation of
the slot's features and type, as an integer.  The parameter @i[indent]
is the level of indentation.  The parameter
@i[limits] is a number (the maximum number of values from the @i[slot]
that are to be processed by the @i[function]), or NIL if all values in
the slot should be processed.

@blankspace(1 line)
@begin(group)
@index(call-on-one-slot)
A similar function is used when only one slot in an object is to be processed:
@begin(example)
kr::Call-On-One-Slot @i[object slot function]  @value(function)
@end(example)

This function returns T if the slot exists and
the @i[function] was called, and NIL otherwise.
@end(group)




@section(Control Variables)
@value(s-top)


The following variable can be set globally to achieve the same effect as
the slot @pr(:print-as-structure) described above:

@value(f-top)
kr::*PRINT-AS-STRUCTURE*@value(variable)
@index(*print-as-structure*) @index(print-as-structure)

This variable may be used to determine whether schema names are printed
with the notation @c(#k<name>) (the default) or simply as @c(name).  The
former notation is more perspicuous, since it makes it immediately clear
which objects are KR schemata.  The second notation is more compact, and is
obtained by setting kr::*PRINT-AS-STRUCTURE* to @value(nil).

@value(f-top)

In addition to kr::*PRINT-AS-STRUCTURE*, other special variables
can be used to control the behavior of the system.  The following
variables are used to control what debugging information is printed.
The default settings are such that very little debugging information
is printed.


@value(f-top)
kr::*PRINT-NEW-INSTANCES*@value(variable)
@index(*print-new-instances*) @index(print-new-instances)

This variable controls whether a notification is printed when
@pr(create-schema) or create-instance are compiled from a file.  The
message is printed when kr::*PRINT-NEW-INSTANCES* is @value(t) (the
default), and may be useful to determine how far into a file compilation
has progressed.  Setting this variable to @value(nil) turns off the
notification.


@value(f-top)
kr::*WARNING-ON-NULL-LINK*@value(variable)
@index(*warning-on-null-link*) @index(warning-on-null-link)

This variable controls whether a notification is printed when a null link
is encountered during the evaluation of a formula.  When the variable is
@value(nil) (the default), the stale value of the formula is simply reused
without any warning.  Setting the variable to @value(t) causes a
notification describing the situation to be printed; the formula then
returns the stale value, as usual.



@value(f-top)
kr::*WARNING-ON-CIRCULARITY*@value(variable)
@index(*warning-on-circularity*) @index(warning-on-circularity)

This variable controls whether a notification is printed when a circularity
is detected during formula evaluation.  When the variable is @value(nil)
(the default), no warnings are generated.  Setting the variable to
@value(t) causes a notification describing the situation to be printed.



@value(f-top)
kr::*WARNING-ON-CREATE-SCHEMA*@value(variable)
@index(warning, create-schema)
@index(*warning-on-create-schema*)

This variable controls whether a notification is printed when
@pr(create-instance) creates an object that has the same name as an old object,
and the old object is destroyed.  If @value(t) (the default), then a
warning will be printed when an object is redefined.


@value(f-top)
kr::*WARNING-ON-EVALUATION*@value(variable)
@index(*warning-on-evaluation*)

This variable controls whether a warning is printed whenever a formula
is evaluated.  If its value is non-@value(nil), then a warning will
describe the object, slot, and name of any formula that is evaluated.
This can be useful for debugging.


@value(f-top)
kr::*STORE-LAMBDAS*@value(variable)
@index(*store-lambdas*)
@index(store-lambdas)

The variable kr::*STORE-LAMBDAS* (default @value[t]) may be set
to @value(nil) to prevent the expression of a formula from being stored in the
formula itself.  This produces smaller run-time programs, but because
the expression is lost it may be impossible to dump a set of objects
to a file using @pr(opal:write-gadget).





@chapter(An Example)
@value(top)


This section develops a more comprehensive example than the ones so far,
and highlights the operations with which most users of the system should be
familiar.  Note that this example does not use graphical operations at all;
refer to the Opal manual for examples of graphical applications.

We will first construct a simple example of constraints and show how
constraints work.  The example uses constraints to compute the equivalence
between a temperature expressed in degrees Celsius and in degrees
Fahrenheit.  This first part also illustrates how KR deals with circular
chains of constraints.

The second part of the example shows simple object-oriented programming
techniques, and illustrates many of the dynamic capabilities of KR.  Note
that this example is purely indicative of a certain way to program in KR,
and different programming styles would be possible even for such a simple
task.

@section(The Degrees Schema)
@value(s-top)

First of all, we will create the @c(degrees) schema@index(degrees
schema) as a demonstration of constraints in KR.  This is a schema with two
slots, namely, @pr(:celsius) and @pr(:fahrenheit).  The schema can be created
with the following call to @pr(create-schema):
@begin(programexample)
@label(degrees)
(create-schema 'DEGREES
  (:fahrenheit (o-formula (+ (* (gvl :celsius) 9/5) 32)
                          32))
  (:celsius (o-formula (* (- (gvl :fahrenheit) 32) 5/9)
                       0)))
@i(;; and now:)
(gv DEGREES :celsius)    ==> 0
(gv DEGREES :fahrenheit) ==> 32

@end(programexample)

Each of the two slots contains a formula.  The formula in the @pr(:celsius)
slot, for instance, indicates that the value is computed from the value in
the @pr(:fahrenheit) slot, using the appropriate expression.  The initial
value, moreover, is 32.  The formula in the @pr(:fahrenheit) slot,
similarly, is constrained to be a function of the value in the @pr(:celsius)
slot and is initialized with the value 0.

It is clear that this example involves a circular chain of constraints.
The value of @pr(:celsius) depends on the value of @pr(:fahrenheit), which
itself depends on the value of @pr(:celsius).  This circularity, however, is
not a problem for KR.  The system is able to detect such circularities and
reacts appropriately by stopping value propagation when necessary.

Consider, for instance, setting the value of the @pr(:celsius) slot:
@begin(programexample)
(s-value DEGREES :celsius 20)
(gv DEGREES :celsius)             ==> 20
(gv DEGREES :fahrenheit)          ==> 68
@end(programexample)

As the example shows, KR propagates the change to the @pr(:fahrenheit) slot,
which is given the correct value.  Similarly, if we modify the value in the
@pr(:fahrenheit) slot, we have correct propagation in the opposite
direction:
@begin(programexample)
(s-value DEGREES :fahrenheit 212)
(gv DEGREES :celsius)             ==> 100
(gv DEGREES :fahrenheit)          ==> 212
@end(programexample)


@section(The Thermometer Example)
@value(s-top)

Let us now build an example of a thermometer from which one can read the
temperature in both degrees Celsius and Fahrenheit, and show a more
extensive application of constraints.  This example also shows the role of
inheritance in object-oriented programming, and a simple method
combination.

We begin with @c(temperature-device), a simple
prototype@index(temperature-device schema) which contains a
formula to translate degrees Celsius into Fahrenheit (the formula is the
same we used in the previous example) and a @pr(:print) method which prints
out both values:
@begin(programexample)
(create-schema 'TEMPERATURE-DEVICE
  (:fahrenheit
    (o-formula (+ (* (gvl :celsius) 9/5) 32) 32)))


(define-method :print TEMPERATURE-DEVICE (schema)
  (format t "Current temperature: ~,1F C (~,1F F)~%"
	  (gv schema :celsius)
	  (gv schema :fahrenheit)))
@end(programexample)

We now create two objects to hold the current temperature outdoors and
indoors, and we create the schema @c(thermometer),
@index(thermometer schema)
which will be the basic building block for other
thermometers:
@begin(programexample)
(create-schema 'OUTSIDE
  (:celsius 10))

(create-schema 'INSIDE
  (:celsius 21))

(create-instance 'THERMOMETER TEMPERATURE-DEVICE
  (:celsius (o-formula (gvl :location :celsius))))
@end(programexample)
Note that @c(thermometer) can act as a prototype, since it provides a
formula which constrains the value of the @pr(:celsius) slot to follow the
value of the @pr(:celsius) slot of a particular location.  Thermometer
schemata created as instances of @c(thermometer) will then simply track the
value of temperature at the location with which they are associated.  Note
that instances of @c(thermometer) inherit the @pr(:print) method from
@c(temperature-device).
@begin(programexample)
(create-instance 'TH1 THERMOMETER
		 (:location outside))

(create-instance 'TH2 THERMOMETER
		 (:location inside))

(kr-send TH2 :print TH2)
@i(;; prints out:)
Current temperature: 21.0 C (69.8 F)

(kr-send TH1 :print TH1)
@i(;; prints out:)
Current temperature: 10.0 C (50.0 F)
@end(programexample)
Since the temperature in the @c(outside) schema is 10, and thermometer
@c(th1) is associated with @c(outside), it prints out the current
temperature outside.  Changing the slot @pr(:location) of @c(th1) to
@c(inside) would automatically change the temperature reading, because of
the dependency built into the formula in that slot.

We now want to specialize@index(prototypes) the @c(thermometer) in order to
provide a new kind of thermometer that keeps track of minimum and maximum
temperature, as well as the current temperature.  We do this by creating an
instance, @c(min-max-thermometer), which inherits all the features of
@c(thermometer) and defines two new formulas for computing minimum and
maximum temperatures.  Note the initial values in the formulas.  Also, we
create an instance of @c(min-max-thermometer) named @c(min-max), and
send it the @pr(:print) message.
@begin(programexample)
(create-instance 'MIN-MAX-THERMOMETER THERMOMETER
  (:min (o-formula (min (gvl :min)
		        (gvl :location :celsius))
		   100))
  (:max (o-formula (max (gvl :max)
		        (gvl :location :celsius))
		   -100)))

(create-instance 'MIN-MAX MIN-MAX-THERMOMETER
   (:location outside))

(kr-send MIN-MAX :print MIN-MAX)
@i(;; prints out:)
Current temperature: 10.0 C (50.0 F)
@end(programexample)
The @pr(:print) method inherited from @c(temperature-device) is not
sufficient for our present purpose, since it does not show minimum and
maximum temperatures.  We thus specialize@index(combining methods) the
@pr(:print) method, but we still use the default @pr(:print) method to print
out the current values.  Let us specialize the method, print out the
current status, change the temperature outside a few times, and then print
out the status again:
@begin(programexample)
(define-method :print MIN-MAX-THERMOMETER (schema)
  ;; print out temperature, as before
  (call-prototype-method schema)
  ;; print out minimum and maximum readings.
  (format t "Minimum and maximum: ~,1F  ~,1F~%"
	  (gv schema :min)
	  (gv schema :max)))

(kr-send MIN-MAX :print MIN-MAX)
@i(;; prints out:)
Current temperature: 10.0 C (50.0 F)
Minimum and maximum: 10.0  10.0

(s-value OUTSIDE :celsius 14)
(kr-send MIN-MAX :print MIN-MAX)
@i(;; prints out:)
Current temperature: 14.0 C (57.2 F)
Minimum and maximum: 10.0  14.0

(s-value OUTSIDE :celsius 12)
(kr-send MIN-MAX :print MIN-MAX)
@i(;; prints out:)
Current temperature: 12.0 C (53.6 F)
Minimum and maximum: 10.0  14.0
@end(programexample)
Note that the @pr(:fahrenheit) slot in any of these schemata can be
accessed normally, and the constraints keep it up to date at all times:
@begin(programexample)
(gv MIN-MAX :fahrenheit)  ==> 268/5 @i{(53.6)}
@end(programexample)

Finally, we can add a method to reset the minimum and maximum temperature,
in order to start a new reading.  This is shown in the next fragment of
code:
@begin(programexample)
(define-method :reset MIN-MAX-THERMOMETER (schema)
  (s-value schema :min (gv schema :celsius))
  (s-value schema :max (gv schema :celsius)))

(kr-send MIN-MAX :reset MIN-MAX)  @i(; reset min, max)

(kr-send MIN-MAX :print MIN-MAX)
@i(;; prints out:)
Current temperature: 12.0 C (53.6 F)
Minimum and maximum: 12.0  12.0

(s-value OUTSIDE :celsius 14)

(kr-send MIN-MAX :print MIN-MAX)
@i(;; prints out:)
Current temperature: 14.0 C (57.2 F)
Minimum and maximum: 12.0  14.0
@end(programexample)

Other choices of programming style would have been possible, ranging from
entirely object-oriented (i.e., without using constraints at all) to
entirely demon-based.



@chapter(Summary)
@value(top)


KR provides excellent performance and three powerful paradigms:
object-oriented programming, knowledge representation, and constraint
maintenance.  The system is designed for high performance and has a very
simple program interface, which makes it easy to learn and easy to use.

The object-oriented programming component of KR is based on the
prototype-instance paradigm, which is more flexible than the class-instance
paradigm.  Prototypes are simply
objects from which other objects (called instances) may inherit values or
methods.  This relationship is completely dynamic, and an object can be
made an instance of a different prototype as needed.  Object methods are
implemented as procedural attachments which are stored in an object's
slots.  Methods are inherited through the usual mechanism.

The knowledge representation component of KR offers
multiple inheritance and user-defined relations.  This component provides
completely dynamic specification of a network's characteristics:
inheritance, for example, is determined through user-specified relations,
which the user may modify at run-time as needed.  The performance of this
component is very good and compares favorably with that of basic Lisp data
structures.  Inheritance, in particular, is efficient enough to provide the
basic building block across a wide variety of application programs.

The constraint maintenance component of KR provides integrated, efficient
constraint maintenance and is implemented through formulas, i.e,
expressions which compute the value of a slot based on the values in other
slots.  Constraint maintenance uses lazy evaluation and value caching
to yield excellent performance in a completely transparent way.  Constraint
maintenance is totally integrated with the rest of the system and can be
used even without any knowledge of its internal details.  The same access
functions, in particular, work on both regular values and on values which
are constrained by formulas.

In spite of its power, KR is small and simple.  This makes it easy to
maintain and extend as needed, and also makes it ideally suited for
experimentation on efficient knowledge representation.  The system is
entirely written in portable Common Lisp and can run efficiently on any
machine which supports the language.  These features make KR an attractive
foundation for a number of applications which use a combination of
frame-based knowledge representation, object-oriented programming, and
constraint maintenance.


@UnNumbered(References)
@bibliography

