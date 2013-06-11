@device(postscript)
@make(manual)
@disable(figurecontents)
@LibraryFile(Stable)
@style(spacing 1.0, BottomMargin .5 inch)  @comment[ Change BottomMargin so
                                                     that all apps fit on one
                                                     page ]
@LibraryFile(Garnet)
@String(TitleString = "Garnet Applications")
@Modify(Hd1, PageBreak Off)
@Modify(description, size 9)
@Comment(****)

@begin(TitlePage)
@begin(TitleBox)
@blankspace(0.6 inch)
@Bg(Pictures of Applications
Using Garnet)

@b(Edited by
Brad A. Myers)
@BlankSpace(0.3 line)
@value[date]
@end(TitleBox)
@BlankSpace(0.5 inch)
@center[@b(Abstract)]
@begin(Text, spacing=1.1)
This is a collection of pictures of real applications written by Garnet users.
You can print just this section of the manual on a color printer to see some
dramatic color images.

@blankspace(0.5 inch)
@include(creditetc.mss)
@End(Text)
@end(TitlePage)


@comment[ The @PageHeading command is needed because this manual does not use
          @Chapter, which normally performs this command. ]
@PageHeading(Even,Immediate, left "@ux[Page @value(page)@hsp(0.15 in)@Value(TitleString) @>]")


@include(pagenumbers.mss)
@set(Page = apps-first-page)


@comment[ Use @MajorHeading instead of @Chapter to avoid Table of Contents! ]

@PageHeading(Even, Immediate, left "@ux[Page @value(page)@>@Value(TitleString)]")
@PageHeading(Odd, Immediate, Left "@ux[@Value(TitleString) @> Page @value(page)]")


@MajorHeading(Some Garnet Applications)


This section presents some examples of applications written using
Garnet.  As of September 1994, we know of over 50 applications
written using Garnet, listed below.  If you
are using Garnet and you are not in the list, please send mail to
@pr(garnet@@cs.cmu.edu).  The figures on the following pages show some
of the applications.  These were generated using the Garnet utility
@pr(Opal:make-ps-file).  Many of these figures are in color, so if you
have a color printer or postscript previewer, you might try looking at
this file in color. 
@define(figcol, columns=3, boxed, justification=off,
        flushleft, size=6, spacing = 6 points)
@begin(figure)
@begin(figcol)
@begin(b, size +2)
Companies
@end(b)
1. Corporation For Open Systems
     Automated Protocol Analysis/Reference Tool
     Frank J. Wroblewski
2. Design Research Institute (Cornell & Xerox)
     (various)
     Jim Davis
3. Deutches Forschungszentrum fuer Kuenstliche
        Intelligenz GmbH
     COSMA
     Stephen P. Spackman
4. GE Research and Development Center
     Metallurgical Expert Systems for Manufacturing
     K. J. Meltsner
5. GE Research and Development Center
     SKETCHER
     K. J. Meltsner
6. Hughes AI Center
     NLP Project
     Seth Goldman or Charles Dolan
7. Institute of Knowledge Engineering, Madrid, Spain
     KIISS: Knowledge Interface Interactive
        Surgery System
     Roberto Moriyon
8. Istituto di Psicologia, Consiglio Nazionale
        delle Ricerche
     GUI for a Knowledge Base
     Daniela D'Aloisi & Vittorio Giannini
9. IRISA (Inst. de recherche en info. et systemes
        aleatoires), France
      Handwriting recognition pen-based editor
      Annick Leroy
10. Lawrence Livermore National Lab
      PLANET (Pump Layout ANd Evaluation Tool)
      Tom Canales
11. Microelectronics and Computer Technology Corp.
      Scan: Intelligent Text Retrieval
      Elaine Rich
12. MITRE Corporation
      AIMI (An Intelligent Multimedia Interface)
      John D. Burger
13. Mitsubishi Electric Research Laboratories
      A toolkit for building collaborative agents
      Charles Rich
14. NASA Ames Research Center, AI
      AMPHION (Knowledge-based software
         engineering system)
      Andrew Philpot & Michael Lowry
15. NASA Ames Research Center, AI
      Dedal (Design Information Reuse and Retrieval
      Aseem Das, C.Baudin
16. National Science Center Foundation
      Learning Logic
      Lawrence Freil
17. PTT Research, Groningen, The Netherlands
      DESSERT (requirements capture for the clients
         of telcommunications)
      Colin Tattersall
18. Rank Xerox EuroPARC
      (various CSCWrelated projects)
      Paul Dourish
19. Siemens AG, Corporate Research, Germany
      Programming by Demonstration
      Michael Sassin
20. StatSci
      GRAPHICAL-BELIEF
      Russell Almond
21. Transarc Corporation
      Lisp GUI for Encina
      Mark Sherman
22. UNISYS, CAD/CAE group of Computer Systems
         Products
      GLL (Geometric Layout Language)
      Shelly Evans
23. U S WEST Advanced Technologies
      KBNL natural language system
      Randall Sparks
24. USC/ISI
      Humanoid
      Pedro Szekely
25. USC/ISI
      SHELTER knowledge-based development environ.
      Pedro Szekely

@newcolumn()
@begin(b, size +2)
Universities
@end(b)
26. Carnegie Mellon University, CS
      Miro
      J. D. Tygar & J. M. Wing
27. Carnegie Mellon University, CS
      Learning Calendar System
      Conrad Poelman
28. Carnegie Mellon University, CS
      Interactive Fiction Editor
      Merrick Furst
29. Carnegie Mellon University, CS
      Redstone
      Jeff Schlimmer
30. Carnegie Mellon University, CS
      PURSUIT
      Francesmary Modugno
31. Carnegie Mellon University, CS
      KATIE
      David Kosbie
32. Carnegie Mellon University, ERDC
      LOOS
      Ulrich Flemming & Robert Coyne
33. Carnegie Mellon University, Math
      Educational Theorem Proving System
      Peter Andrews
34. Carnegie Mellon University, Philosophy
      Carnegie Mellon Proof Tutor II
      Richard Scheines, Wilfried Sieg, & John Byrnes
35. Carnegie Mellon University, Psychology
      Soar Graphics Interface
      Frank Ritter
36. Carnegie Mellon University, Robotics
      MICRO-BOSS
      Norman Sadeh
37. Carnegie Mellon University, Robotics
      SAGE Data Visualization
      Steve Roth
38. Georgia Institute of Technology
      KritikTutor (interactive learning environment)
      Andres Gomez de Silva Garza, Nathalie Grue
39. MIT, Dept. of Brain and Cognitive Sciences
      SURF-HIPPO Neuron Simulator
      Lyle J. Borg-Graham
40. MIT, LCS, Computation Structures Group
      Debugging tools for the Id Language
      Steve Glim & R. Paul Johnson
41. MIT, LCS
      DYNAMOL: A DYNAmic decision MOdeling
         Language
      Tze-Yun Leong
42. Monash University, Dept. of CS, Australia
      Multimedia Presentation Planning
      Ingrid Zukerman & Yi Han
43. New York University, Courant Institute
      COMLEX (Common Lexicon) dictionary
      R. Grishman, C.Macleod, S. Wolff, & K. Rajan
44. Oregon State University, Computer Science Dept
      Forms/3 (visual language)
      Margaret Burnett
45. Oregon State University, Computer Science Dept
      CarGo (Game of Go)
      Peter Dudey
46. State University of New York at Buffalo, CS
      Air Battle Simulation
      Henry Hexmoor
47. State University of New York at Buffalo, CS
      SNePS Graphical UI
      Stuart C. Shapiro
48. Tulane University, CS
      Natural Language Processing
      Robert Goldman
49. Tulane University, CS
      THESEUS
      Raymond Lang
50. Universidad de las Americas-Puebla (Mexico)
      UDLA-PLV (Prototipo de un Lenguaje Visual)
      A. Gaspar Contreras & M. Sanchez Sandoval
51. Universite de Nice Sophia-Antipolis
      Environment for distributed languages
      Jean-Christophe Pazzaglia, Jean-Pierre Regourd,
         & Michel Claude
52. University College London
      The Cognitive Browser
      Gordon Joly
53. Univ. of CA at Santa Barbara, CS & ECE
      Graph. Tools for the Dev. of Concurrent Systems
      L.Dillon, G.Kutty, P.Melliar-Smith, L.Moser,
         & Y.Ramakrishna

@newcolumn()
54. University of CA at Santa Barbara, CS & ECE
      GIL (Graphical Interval Logic) (visual spec. lang.)
      G. Kutty, Ron Dolin, Laurie Dillon,
         & Louise Moser
55. University of Chicago, CS
      Shopper (planning and visual navigation)
      Daniel Fu
56. University of Chicago, CS
      SEAL (situation-driven execution and
         constructivist learning)
      Charles Earl
57. University of Chicago, AI Lab
      Roentgen (case-based aid for radiation therapy)
      Jeff Berger
58. University of Edinburgh, Human Communication
          Research Centre (HCRC)
      SIGNAL
      Robert Inder & Keith Stenning
59. University of Iowa
      Failure-Resistant Induction with Window
         Interfaces
      Sitao Yang & Hantao Zhang
60. University of Kaiserslautern, Germany
      PLATIN (planning and automatic theorum proving
         tool)
      Robert Eschbach & Carlo Reiffers
61. University of Leeds
      Graphical Multi-User Domain Designer
      Roderick J. Williams
62. University of Leeds
      CLARE
      Nikos Drakos
63. University of Leeds
      ADVISOR
      Andrew J. Cole
64. University of Leeds
      PORSCHE
      Colin Tattersall
65. University of Manchester Institute of
         Science and Technology
      Multi-User-Garnet
      Reza Hazemi & Linda Macaulay
66. University of Michigan, EE&CS
      Science works: Nuclear Engineering
      Kasem Abotel, Elliot Solloway, Chris Quintaina,
         & William Martin
67. University of Michigan, AI Lab
      Symbiosis Testbed
      Jason Daida
68. University of New South Wales Sydney, Australia
      TURNINT (Process Planning Expert System for
         Turning Operation)
      Bela Pecsek
69. University of Queensland, Australia
      Conceptual Modeling CASE tool
      Anthony Berglas
70. University of Pittsburgh
      DIPART (Distr. Intera. Planner's Asst. for
         Real-time Transp. Planning)
      Martha Pollack, Taieb Znati, & Nilufer Onder
71. University of Queensland, Australia
      Interoperating Network Servers
      Hung Wing
72. University of Saskatchewan
      DISCUS (DIstributed Computing at the Uof S)
      Beth Protsko (User Interface only)
73. University of Southern California
      Dynamic Aggregation in Qualitative Simulation
      Nicolas Rouquette
74. University of Southern California
      Rev-Enge: Knowledge-based S/W for
        (Re)Design-for-Assembly
      Gerard Kim & George Bekey
75. University of Washington, CS
      Multi-Garnet
      Michael Sannella
76. University of Washington, CS
      Electronic Encyclopedia Exploratorium
      Mike Salisbury
77. Victoria University of Wellington, New Zealand
      Caselaw (argument representation & legal
        reasoning)
      Paul Hosking & Eric Jones
78. Vrije Universiteit, Computer/Law Institute,
         The Netherlands
      PROLEXS (legal expert system shell)
      Geert-Jan van Opdorp
@end(figcol)
@end(figure)


@comment{---------------  drakosclare ----------------------}
@newpage()
@center{@graphic(Postscript="apps/drakosnewclare.ps", magnify=0.6,
         boundingbox=file)}
@center{@graphic(Postscript="apps/drakosclaregraph.ps", magnify=0.5,
         boundingbox=file)}
@begin(format)
@b(Nikos Drakos)
Computer Based Learning Unit, University of Leeds, UK.
@i(CLARE)
@pr(nikos@@cbl.leeds.ac.uk)
@end(format)

A collaborative project on an environment for the specification, testing, 
maintenance and automatic generation of application software. 
The context is batch process control in Chemical Engineering although 
it is envisaged that the applicability of the environment will be more general.
A `domain expert' will be able to specify knowledge about plant 
subsystems, plant configurations, and the allowable generic operations and
constraints on each plant subsystem. An `application engineer' will then use 
the system to `glue' together predefined operations in order to make specific 
products. The system will then generate process control code for particular 
target hardware. Garnet is being used to capture and visualise plant and 
process information through schematics, process diagrams, interactive 
simulations and simple animations. 
@begin(description)
Nikos Drakos, ``Object Orientation and Visual Programming'', in
Mamdouh Ibrahim, editor, @i(OOPSLA '92 Workshop on Object-Oriented
Programming Languages: The Next Generation), Vancouver, B.C.
Canada, October 18 1992. Extended Abstract. pp. 85-93.
@end(description)



@comment{---------------  ge mesh ----------------------}

@newpage()

@center{@graphic(Postscript="apps/ge.ps", magnify=1.0, boundingbox=file)}
@begin(format)
@b(Kenneth Meltsner)
General Electric Company, Corporate Research and Development
@i(Metallurgical Expert System)
@pr(meltsner@@crd.ge.com)
@end(format)
A mesh created using a virtual aggregate for the
polygons and another virtual aggregate for the square knobs.  For the
polygons, the virtual aggregate is passed a prototype for a
polygon, and an array containing the list of points and the color for
each polygon.   The virtual aggregate then pretends to
allocate an object for each element of the array, but actually just
draws the prototype object repeatedly.

@begin(description)
Kenneth J. Meltsner. 
``A Metallurgical Expert System for Interpreting FEA,'' @i{Journal of Metals},
Oct, 1991, vol. 43, no. 10, pp. 15-17.
@end(description)

@newpage()

@comment{--------------- Lang THESEUS  ----------------------}


@center{@graphic(Postscript="apps/lang1.ps", magnify=0.55, boundingbox=file)}
@center{@graphic(Postscript="apps/lang2.ps", magnify=0.4, boundingbox=file)}
@center{@graphic(Postscript="apps/lang3.ps", magnify=0.6, boundingbox=file)}
@begin(format)
@b(Raymond Lang)
Tulane University, Computer Science Department
@i(THESEUS)
@pr(lang@@rex.cs.tulane.edu)
@end(format)

These are images of windows from the
THESEUS application used by the Tulane University Computer Science
Department on guided tours of the department given to visiting high
school seniors and other interested parties.  THESEUS is intended to
be used as part of a presentation on what the study of computer
science entails.  It does this by showing graphically the progress and
results of common search methods applied to the problem of finding the
exit of a randomly created maze.  THESEUS was developed in CMU Common
Lisp version 16d and the Garnet X-Windows toolkit version 2.01.

@begin(description)
R. Raymond Lang, @i(THESEUS: Using Maze Search to
Introduce Computer Science).  Technical Report.  Computer Science
Department, Tulane University. 1992.
@end(description)

@newpage()

@comment{--------------- Berger Roentgen  ----------------------}


@center{@graphic(Postscript="apps/bergerdose.ps", magnify=1, boundingbox=file)}
@begin(format)
@b(Jeff Berger)
University of Chicago, Artificial Intelligence Laboratory
@i(Roentgen)
@pr(berger@@cs.uchicago.edu)
@end(format)
Roentgen is a case-based aid to radiation therapy planning.  It relies on
an archive of past therapy cases to suggest plans for new therapy
patients. Roentgen supports therapy planning by: 1) retrieving the case
which best matches the geometry and treatment constraints of the new
patient; 2) tailoring the plan to the specific details of the patient;
3) evaluating the results of applying the plan; 4) repairing the plan
to avoid any discovered faults in treatment results. This final plan
is the system's suggestion to the human planner. Roentgen breaks new
ground in solving problems in a domain dominated by spatial reasoning
and the satisficing of constraints.


@newpage()

@comment{--------------- lyleSurfHippo.ps  ----------------------}


@center{@graphic(Postscript="apps/lyleSurfHippo.ps", magnify=0.7, boundingbox=file)}
@begin(format)
@b(Lyle J. Borg-Graham)
MIT Dept. of Brain and Cognitive Sciences
@i(Surf-Hippo)
@pr(lyle@@ai.mit.edu)
@end(format)
The SURF-HIPPO Neuron Simulator is a circuit  simulation  package  for
investigating morphometrically and biophysically detailed models of single
neurons and small networks of neurons. SURF-HIPPO allows ready construction of
multiple cells from various file formats, which can describe complicated
dendritic trees in 3-space with distributed non-linearities and synaptic
contacts between cells. Cell geometries may also be traced from the histology
directly on the screen, using the mouse.   An extensive user interface is
provided, including menus, 3D graphics of dendritic trees, and data plotting.
Data files may also be saved for analysis with external tools.  A
research version of SURF-HIPPO
(available by anonymous ftp from ftp.ai.mit.edu [pub/surf-hippo]) is written in
LISP, and is configured to run using the public domain CMU Common Lisp and
Garnet packages. Our version is compiled for SPARC workstations, and should be
easily ported to other UNIX machines running X. LISP is a useful simulator
language because it has the benefits of a powerful interpreted script language,
but it may also be compiled. Thus it is convenient to integrate custom code
into SURF-HIPPO. The simulator may also be used with a minimum of programming
expertise, if desired.
@begin(description)
Borg-Graham, L. and Grzywacz, N. M. ``A Model of the Direction
Selectivity Circuit in Retina: Transformations by Neurons Singly and
in Concert,'' in @i{Single Neuron Computation}, edited by T.
McKenna, J. Davis, and S. F. Zornetzer. Academic Press, 1992.
@end(description)

@newpage()

@comment{--------------- Graphical MUD rjwilliams1.ps ----------------------}


@center{@graphic(Postscript="apps/rjwilliams1.ps", magnify=0.9, boundingbox=file)}
@begin(format)
@b(Roderick J. Williams)
The University of Leeds, Leeds, LS2 9JT, UK.
@i[GMD (Graphical Mud (Multi-User Domain) Designer)]
@pr(rodw@@cbl.leeds.ac.uk)
@end(format)
This application is aimed at supporting the creation of text-based multi-user
domains. Current techniques use text-based tools to create these environments,
but these tools have very little computer support so complexity and consistency
are sacrificed.
Our new application supports the graphical creation of MUD areas and enforces
topological constraints together with hierarchical grouping of features.
The graphical tool can be used in a number of modes which allow the information
to be filtered, zoomed and viewed in 2.5 D. Areas created can be printed and
additionally they can be saved as native code that can be executed.

@newpage()



@comment{--------------- sage.ps  ----------------------}


@Comment(l, bot, l->r,b->t)
@center{@graphic(Postscript="apps/sage1.ps",
        rotate 90, magnify=0.375,  
        boundingbox="1.0in, 1.8125in, 7.4375in,
        9.25in") @graphic(Postscript="apps/sage2.ps", magnify=0.45,
        boundingbox=file)}
@center{@graphic(Postscript="apps/sageNapolean.ps",
        rotate 90, magnify=0.6,  
        boundingbox="1.0in, 1.8125in, 7.4375in, 9.25in")}
@begin(format)
@b(Steven F. Roth)
Carnegie Mellon University, Robotics Institute
@i[SAGE]
@pr(roth@@isl1.ri.cmu.edu)
@end(format)
The SAGE project is developing systems which automate the process of
designing presentations of information.  An automatic presentation system is
an intelligent interface component which receives information from a user or
application program and designs a combination of graphics and text that
effectively conveys it.  It's  purpose is to assume as much responsibility
for designing displays as required by a user, from layout and color
decisions to broader decisions about the types of charts, tables and
networks that can be composed within a display.  The SAGE project is
developing an interactive data exploration environment which contains
automatic display design capabilities integrated with data navigation,
manipulation and modification tools. These tools are being used to
explore large amounts of diverse data from marketing, logistical, real
estate, census and other databases.
@begin(description)
Roth, S.F. & Mattis, J.A.  "Data Characterization for Intelligent
Graphics Presentation", In @i(CHI'90: Proceedings of the ACM/SIGCHI Conference
on Computer Human Interaction), Seattle, April, 1990. pages 193-200.
@end(description)
@newpage()


@comment{--------------- salisburgencl.ps  ----------------------}


@center{@graphic(Postscript="apps/salisburgencl.ps", magnify=0.9, boundingbox=file)}
@begin(format)
@b(Mike Salisbury)
University of Washington, Department of Computer Science
@i[Electronic Encyclopedia Exploratorium]
@pr(salisbur@@cs.washington.edu)
@end(format)
The Electronic Encyclopedia Exploratorium is an electronic
how-things-work book.  It allows the user to learn about devices 
by experimenting with the components of
those devices in a lab simulation setting.  A causal model simulator
lies beneath the user 
interface which simulates the current device and can provide
causal explanations of the results of that simulation.  Other
high-level tools are planned for future enhancement.
@begin(description)
F. G. Amador, D. Berman, A. Borning, T. DeRose, A. Finkelstein, D.
Neville, Norge, D. Notkin, D. Salesin, M. Salisbury, J. Sherman, Y.
Sun, D. S. Weld, and G. Winkenbach.  @i{Electronic "How Things Work"
Articles: A Preliminary Report}.  University of
Washington, Department of Computer Science and Engineering Technical
Report 92-04-08. June, 1992.
@end(description)

@newpage()



@comment{--------------- Soar  ----------------------}


@center{@graphic(Postscript="apps/soar.ps", magnify=1.0, boundingbox=file)}
@begin(format)
@b(Frank E. Ritter)
Department of Psychology, U. of Nottingham
@i[The Developmental Soar Interface]
@pr(Ritter@@psyc.nott.ac.uk)
@end(format)
The Developmental Soar Interface provides a graphical and
textual interface to observe and modify models (programs) for Soar, an
AI programming language that also realizes a unified theory of
cognition.  Garnet is used to graphically represent Soar's goal stack
and internal state, and to help users modify and observe structures in
Soar.
@begin(description)
Ritter, F. E. (1993) @i(TBPA: A methodology and software environment for
testing process models' sequential predictions with protocols), PhD
thesis, Department of Psychology, Carnegie-Mellon University.
Reprinted as techreport CMU-CS-93-101, Carnegie-Mellon University.
@end(description)

@newpage()


@comment{--------------- spackmancosma.ps  ----------------------}


@Comment(l, bot, l->r,b->t)
@center{@graphic(Postscript="apps/spackmancosma.ps",
        rotate 90, magnify=0.7,  
        boundingbox="1.6875in, 1.0625in, 6.75in, 10.0in")}
@begin(format)
@b(Stephen P. Spackman)
Projekt DISCO
Deutches Forschungszentrum fuer Kuenstliche Intelligenz GmbH
@i[COSMA, the CoOperative Scheduling Management Agent]
@pr(spackman@@dfki.uni-sb.de or stephen@@acm.org)
@end(format)
The calendar window shows the dark bar of the past sweeping, one pixel
each half hour of the day and night, across a horizontal line [not
visible in this Postscript image] summarising by its width and height
the user's working hours and appointments, tentative and firm.  The
marginal time tags can be dragged up and down, and it will eventually be
possible to type over the top of them to jump to a given time.  The
datebook window presents an expanded view of time as an infinite tape
from which appointment forms can be popped up by pointing or by sweeping
out free areas.  Most importantly, when arrangements involve several
people the system communicates with its peers and with meeting
participants by reading and writing email in German; the displays are
updated in real time.

The fields of the appointment form are semi-structured: they can
be filled in with the help of menus - such as that visible on the lower
window - that drop down from the small icons on the right; numeric, date
and time values within them can be incremented and decremented directly
with mouse buttons; and experienced users can type structured values
straight in.  Unconstrained German text (the graphic interface will soon
be English/French/German trilingual, but the natural language parser and
generator speak only German) can also be entered. It is routed to the
natural language system for analysis; planned improvements to the
pragmatics module will allow you to give up on the structured form
completely and type informal questions and instructions into the notes
field, as you might for a human secretary who had stepped out of the
room.
@begin(format, size 9)
The work underlying this picture was supported by a research grant, FKZ ITW
9002 0, from the German Bundesministerium fuer Forschung und
Technologie to the DFKI project DISCO.
@end(format)
@begin(description)
Elizabeth A. Hinkelman and Stephen P. Spackman,
``Abductive Speech Act Recognition, Corporate Agents and the COSMA System,''
in @i(Abduction, Beliefs and Context: Proceedings of the second ESPRIT
PLUS workshop in computational pragmatics).  W. J. Black and G. Sabah
and T. J. Wachtel, eds. Academic Press, 1992.
@end(description)

