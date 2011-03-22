
Spectacle, an image-viewing application written in Common Lisp.

Spectacle relies heavy on a number of Common Lisp libraries, including
the image processing library opticl, and the McCLIM GUI toolkit, an
implementation of CLIM, the Common Lisp Interface Manager.

Spectacle is released under a BSD-style license; please consult the
COPYRIGHT file for more details

Spectacle depends on the opticl and mcclim libraries (and,
transitively, their dependencies). The easiest way to install these
libraries is to use Zach Beane's Quicklisp:

    (ql:quickload '(opticl mcclim))

To load spectacle, make sure that either asdf:*central-registry*
contains the directory in which the spectacle.asd file lives or that a
link to the spectacle.asd file is placed in one of the directories
listed in asdf:*central-registry*. For SBCL the easiest way to do this
is something like:

    ln -s [path to the spectacle directory]/spectacle.asd /usr/local/lib/sbcl/site-systems/

The to launch spectacle do:

    (asdf:load-system 'spectacle)
    (spectacle:spectacle)

The program will look a bit nicer if you load mcclim-truetype (or
another suitable mcclim backend) prior to launching spectacle, using a
command such as the following:

    (asdf:load-system 'mcclim-truetype)

which requires the following libraries: zpb-ttf, cl-vectors,
cl-paths-ttf, and cl-aa. These can be installed thusly:

    (ql:quickload '(zpb-ttf cl-vectors cl-paths-ttf cl-aa))

Then launch spectacle as above.

Spectacle supports reading and writing images in the file formats
supported by the opticl image processing library, currently JPEG, PNG,
TIFF, PPM/PBM/PGM, and GIF.

To read an image select the Load Image menu item or type Load Image in
the interactor pane in the lower portion of the window. To save an
image, use the Save Image command. Currently Save Image saves the
untransformed image, that is the data as they appeared in the original
image, although the image will be saved as the filetype specified by
the extension in the output pathname.
