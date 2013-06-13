$Id$

Instructions for building Garnet.

1. Location

The Garnet directory tree (the directory containing this file) should be placed where you want
it to live. For example, if you want to keep it with your SBCL distribution, you can place this entire tree in /usr/local/lib/sbcl/garnet (assuming you use the default location for SBCL).

The tree is self-contained and you can basically put it anywhere.

2. Install quicklisp (www.quicklisp.org). I have made this a requirement because it will be needed for the truetype support that is the whole point of my current work on Garnet. It also makes installing CLX (step 3) more convenient.

3. CLX

Garnet requires CLX. This setup assumes that you can do the following:

<start your lisp>
nova:~/bin > sbcl

<ask your lisp to load CLX>
CL-USER(1): (require :clx)

You should not get an error. If you do, you must install CLX. I recommend using Quicklisp (www.quicklisp.org) as a convenient way to do this (which you should have installed in step 2).

CL-USER(1): (ql:quickload :clx)
To load "clx":
  Load 1 ASDF system:
    clx
; Loading "clx"

(:CLX)
CL-USER(2): 

If CLX isn't already on your system it will download it and compile it.


4. Configure the build-garnet script.

Simply add the path to your lisp executable (or executables) to the beginning of the build-garnet script where indicated:

----------------------------------------
#
# Configure these paths for your system.
#
SBCL=/usr/local/bin/sbcl
CMUCL=/usr/local/bin/lisp
ACL=/usr/local/allegro/8.1/alisp

----------------------------------------

I have tested this setup with the above three Lisp implementations and they all work (modulo some Allegro weirdness with pixmaps that has been there forever).

5. Building Garnet

To build garnet all the way to a lisp image with a single command do the following:

Example:  ./build-garnet sbcl

Result: Garnet will be compiled, the fasl files will be moved into their target
directory and an image will be produced in this directory. In the case of SBCL a full executable image called "garnet" will be created.
