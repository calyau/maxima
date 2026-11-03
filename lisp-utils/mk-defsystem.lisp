;;; -*- Mode: Lisp; Package: MAKE; Syntax: Common-Lisp -*-

;;; DEFSYSTEM 3.9.4.1 (More Modern)

;;; defsystem.lisp --

;;; MAKE -- A Portable Defsystem Implementation
;;; ===========================================

;;; This is a portable system definition facility for Common Lisp.
;;; Though home-grown, the syntax was inspired by fond memories of the
;;; defsystem facility on Symbolics 3600's. The exhaustive lists of
;;; filename extensions for various lisps and the idea to have one
;;; "operate-on-system" function instead of separate "compile-system"
;;; and "load-system" functions were taken from Xerox Corp.'s PCL
;;; system.

;;; This system improves on both PCL and Symbolics defsystem utilities
;;; by performing a topological sort of the graph of file-dependency
;;; constraints. Thus, the components of the system need not be listed
;;; in any special order, because the defsystem command reorganizes them
;;; based on their constraints. It includes all the standard bells and
;;; whistles, such as not recompiling a binary file that is up to date
;;; (unless the user specifies that all files should be recompiled).

;;; Originally written by Mark Kantrowitz, School of Computer Science,
;;; Carnegie Mellon University, October 1989.

;;; Copyright (c) 1989 - 1999 Mark Kantrowitz. All rights reserved.
;;;               1999 - 2005 Mark Kantrowitz and Marco Antoniotti. All
;;;                           rights reserved.
;;;               2005 - 2008 Marco Antoniotti. All rights reserved.
;;;               2008 - 2026 Marco Antoniotti and Madhu. All rights reserved.

;;; Emails:
;;; Marco Antoniotti - mantoniotti@common.lisp.net
;;; Madhu            - enometh@meer.net

;;; Use, copying, modification, merging, publishing, distribution
;;; and/or sale of this software, source and/or binary files and
;;; associated documentation files (the "Software") and of derivative
;;; works based upon this Software are permitted, as long as the
;;; following conditions are met:

;;;    o this copyright notice is included intact and is prominently
;;;      visible in the Software
;;;    o if modifications have been made to the source code of the
;;;      this package that have not been adopted for inclusion in the
;;;      official version of the Software as maintained by the Copyright
;;;      holders, then the modified package MUST CLEARLY identify that
;;;      such package is a non-standard and non-official version of
;;;      the Software.  Furthermore, it is strongly encouraged that any
;;;      modifications made to the Software be sent via e-mail to the
;;;      MK-DEFSYSTEM maintainers for consideration of inclusion in the
;;;      official MK-DEFSYSTEM package.

;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESSED OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT.
;;; IN NO EVENT SHALL M. KANTROWITZ AND M. ANTONIOTTI BE LIABLE FOR ANY
;;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Except as contained in this notice, the names of M. Kantrowitz and
;;; M. Antoniotti shall not be used in advertising or otherwise to promote
;;; the sale, use or other dealings in this Software without prior written
;;; authorization from M. Kantrowitz and M. Antoniotti.


;;; Please send bug reports, comments and suggestions to <mantoniotti@common-lisp.net>.


;;; MK-DEFSYSTEM quick history as of March 2026
;;; ===========================================
#|

MK-DEFSYSTEM (or MK:DEFSYSTEM, or MAKE:DEFSYSTEM) was originally
written by Mark Kantrowitz as part of the original "CMU Lisp
Utilities" collection; an early "public" set of Common Lisp code and
utilities that, in the writer's opinion form one of the basis of most
Common Lisp writing to date.

As stated (by M. Kantrowitz himself) in this file header, the original
version of MK-DEFSYSTEM was inspired by the Symbolics DEFSYSTEM (or
DEFSYS) tool.  Yet, MK-DEFSYSTEM differs significantly from it.

In its original form, MK-DEFSYSTEM was built in the CLtL1 era,
accommodated a lot of variance among filesystems and CL
implementations and it still bears those idiosycrasies. CLtL2 (1992)
first and ANSI (1994) next, started reshaping the code base then.

MK-DEFSYSTEM was originally distributed under a license agreement that
made redistribution tricky.  In 1999, the writer - that'd be me, Marco
Antoniotti - contacted Mark Kantrowitz offering to become a maintainer
while reworking the distribution license to hammer some FOSS into it.
Mark Kantrowitz graciously agreed and, after that, the writer got
literally and physically hugged by a few Common Lisp developers
because they could use MK-DEFSYSTEM more freely.

Of course, ASDF came along and it solved the same problems that
Symbolics (and Kent Pitman's) DEFSYS and MK-DEFSYSTEM solve, plus much
more.

Yet, MK-DEFSYSTEM has some nice features (in the eye of the beholder).

MK-DEFSYSTEM still ships in one file - `defsystem.lisp` - that you can
LOAD in your Common Lisp init file.  Of course, a big chunk of its
current code base is "backward compatibility" and new
ok-we-miss-UIOP-and-or-at-least-CL-FAD functionality, plus an ever
growing ongoing commentary like this one.

Given this background, the writer has been maintaining MK-DEFSYSTEM
for a long time, and more recently, Madhu has made significant
changes (and maintains himself a fork with some bells and whistles of
his own) since 2008.

Of course, many other contributors helped over the years, and are
acknowledged in the early Change Log and in comments in the code. 

In early 2026, the writer cleaned up the code and reworked some of
the logic, by factoring out some code from main functions.  In
particular, the CREATE-COMPONENT-PATHNAMES,
GENERATE-COMPONENT-PATHNAMES, COMPONENT-FULL-PATHNAME,
COMPONENT-FULL-NAMESTRING interplay is better organized; plus new
structures, leveraging DEFSTRUCT :INCLUDE feature have been
introduced, rendering the code TYPECASE-able.

MK-DEFSYSTEM is old, but it works.  It is quirky but it works (at
least for the two or three known users - which, in 2026, is already a
big chunk of the Common Lisp users' community).  Moreover, it does
have, at least in the eye of the beholder, some more user friendly
user API, for most use case, especially for plain Common Lisp code.

The current MK-DEFSYSTEM repository is at
https://gitlab.common-lisp.net/mantoniotti/mk-defsystem

(*) It is assumed that the reader knows about all the acronyms, tools
    and systems referred to in the text.

|#


;;; ********************************
;;; Change Log *********************
;;; ********************************
;;;
;;; Note: Several of the fixes from 30-JAN-91 and 31-JAN-91 were done in
;;; September and October 1990, but not documented until January 1991.
;;;
;;; akd  = Abdel Kader Diagne <diagne@dfki.uni-sb.de>
;;; as   = Andreas Stolcke <stolcke@ICSI.Berkeley.EDU>
;;; bha  = Brian Anderson <bha@atc.boeing.com>
;;; brad = Brad Miller <miller@cs.rochester.edu>
;;; bw   = Robert Wilhelm <wilhelm@rpal.rockwell.com>
;;; djc  = Daniel J. Clancy <clancy@cs.utexas.edu>
;;; fdmm = Fernando D. Mato Mira <matomira@di.epfl.ch>
;;; gc   = Guillaume Cartier <cartier@math.uqam.ca>
;;; gi   = Gabriel Inaebnit <inaebnit@research.abb.ch>
;;; gpw  = George Williams <george@hsvaic.boeing.com>
;;; hkt  = Rick Taube <hkt@cm-next-8.stanford.edu>
;;; ik   = Ik Su Yoo <ik@ctt.bellcore.com>
;;; jk   = John_Kolojejchick@MORK.CIMDS.RI.CMU.EDU
;;; kt   = Kevin Thompson <kthompso@ptolemy.arc.nasa.gov>
;;; kc   = Kaelin Colclasure <kaelin@bridge.com>
;;; kmr  = Kevin M. Rosenberg <kevin@rosenberg.net>
;;; lmh  = Liam M. Healy <Liam.Healy@nrl.navy.mil>
;;; mc   = Matthew Cornell <cornell@unix1.cs.umass.edu>
;;; oc   = Oliver Christ <oli@adler.ims.uni-stuttgart.de>
;;; rs   = Ralph P. Sobek <ralph@vega.laas.fr>
;;; rs2  = Richard Segal <segal@cs.washington.edu>
;;; sb   = Sean Boisen <sboisen@bbn.com>
;;; ss   = Steve Strassman <straz@cambridge.apple.com>
;;; tar  = Thomas A. Russ <tar@isi.edu>
;;; toni = Anton Beschta <toni%l4@ztivax.siemens.com>
;;; yc   = Yang Chen <yangchen%iris.usc.edu@usc.edu>
;;;
;;; Thanks to Steve Strassmann <straz@media-lab.media.mit.edu> and
;;; Sean Boisen <sboisen@BBN.COM> for detailed bug reports and
;;; miscellaneous assistance. Thanks also to Gabriel Inaebnit
;;; <inaebnit@research.abb.ch> for help with VAXLisp bugs.
;;;
;;; 05-NOV-90  hkt  Changed canonicalize-system-name to make system
;;;                 names package independent. Interns them in the
;;;                 keyword package. Thus either strings or symbols may
;;;                 be used to name systems from the user's point of view.
;;; 05-NOV-90  hkt  Added definition FIND-SYSTEM to allow OOS to
;;;                 work on systems whose definition hasn't been loaded yet.
;;; 05-NOV-90  hkt  Added definitions COMPILE-SYSTEM and LOAD-SYSTEM
;;;                 as alternates to OOS for naive users.
;;; 05-NOV-90  hkt  Shadowing-import of 'defsystem in Allegro CL 3.1 [NeXT]
;;;                 into USER package instead of import.
;;; 15-NOV-90  mk   Changed package name to "MAKE", eliminating "DEFSYSTEM"
;;;                 to avoid conflicts with allegro, symbolics packages
;;;                 named "DEFSYSTEM".
;;; 30-JAN-91  mk   Modified append-directories to work with the
;;;                 logical-pathnames system.
;;; 30-JAN-91  mk   Append-directories now works with Sun CL4.0. Also, fixed
;;;                 bug wrt Lucid 4.0's pathnames (which changed from lcl3.0
;;;                 -- 4.0 uses a list for the directory slot, whereas
;;;                 3.0 required a string). Possible fix to symbolics bug.
;;; 30-JAN-91  mk   Defined NEW-REQUIRE to make redefinition of REQUIRE
;;;                 cleaner. Replaced all calls to REQUIRE in this file with
;;;                 calls to NEW-REQUIRE, which should avoid compiler warnings.
;;; 30-JAN-91  mk   In VAXLisp, when we redefine lisp:require, the compiler
;;;                 no longer automatically executes require forms when it
;;;                 encounters them in a file. The user can always wrap an
;;;                 (eval-when (compile load eval) ...) around the require
;;;                 form. Alternately, see commented out code near the
;;;                 redefinition of lisp:require which redefines it as a
;;;                 macro instead.
;;; 30-JAN-91  mk   Added parameter :version to operate-on-system. If it is
;;;                 a number, that number is used as part of the binary
;;;                 directory name as the place to store and load files.
;;;                 If NIL (the default), uses regular binary directory.
;;;                 If T, tries to find the most recent version of the
;;;                 binary directory.
;;; 30-JAN-91  mk   Added global variable *use-timeouts* (default: t), which
;;;                 specifies whether timeouts should be used in
;;;                 Y-OR-N-P-WAIT. This is provided for users whose lisps
;;;                 don't handle read-char-no-hang properly, so that they
;;;                 can set it to NIL to disable the timeouts. Usually the
;;;                 reason for this is the lisp is run on top of UNIX,
;;;                 which buffers input LINES (and provides input editing).
;;;                 To get around this we could always turn CBREAK mode
;;;                 on and off, but there's no way to do this in a portable
;;;                 manner.
;;; 30-JAN-91  mk   Fixed bug where in :test t mode it was actually providing
;;;                 the system, instead of faking it.
;;; 30-JAN-91  mk   Changed storage of system definitions to a hash table.
;;;                 Changed canonicalize-system-name to coerce the system
;;;                 names to uppercase strings. Since we're no longer using
;;;                 get, there's no need to intern the names as symbols,
;;;                 and strings don't have packages to cause problems.
;;;                 Added UNDEFSYSTEM, DEFINED-SYSTEMS, and DESCRIBE-SYSTEM.
;;;                 Added :delete-binaries command.
;;; 31-JAN-91  mk   Franz Allegro CL has a defsystem in the USER package,
;;;                 so we need to do a shadowing import to avoid name
;;;                 conflicts.
;;; 31-JAN-91  mk   Fixed bug in compile-and-load-operation where it was
;;;                 only loading newly compiled files.
;;; 31-JAN-91  mk   Added :load-time slot to components to record the
;;;                 file-write-date of the binary/source file that was loaded.
;;;                 Now knows "when" (which date version) the file was loaded.
;;;                 Added keyword :minimal-load and global *minimal-load*
;;;                 to enable defsystem to avoid reloading unmodified files.
;;;                 Note that if B depends on A, but A is up to date and
;;;                 loaded and the user specified :minimal-load T, then A
;;;                 will not be loaded even if B needs to be compiled. So
;;;                 if A is an initializations file, say, then the user should
;;;                 not specify :minimal-load T.
;;; 31-JAN-91  mk   Added :load-only slot to components. If this slot is
;;;                 specified as non-NIL, skips over any attempts to compile
;;;                 the files in the component. (Loading the file satisfies
;;;                 the need to recompile.)
;;; 31-JAN-91  mk   Eliminated use of set-alist-lookup and alist-lookup,
;;;                 replacing it with hash tables. It was too much bother,
;;;                 and rather brittle too.
;;; 31-JAN-91  mk   Defined #@ macro character for use with AFS @sys
;;;                 feature simulator. #@"directory" is then synonymous
;;;                 with (afs-binary-directory "directory").
;;; 31-JAN-91  mk   Added :private-file type of module. It is similar to
;;;                 :file, but has an absolute pathname. This allows you
;;;                 to specify a different version of a file in a system
;;;                 (e.g., if you're working on the file in your home
;;;                 directory) without completely rewriting the system
;;;                 definition.
;;; 31-JAN-91  mk   Operations on systems, such as :compile and :load,
;;;                 now propagate to subsystems the system depends on
;;;                 if *operations-propagate-to-subsystems* is T (the default)
;;;                 and the systems were defined using either defsystem
;;;                 or as a :system component of another system. Thus if
;;;                 a system depends on another, it can now recompile the
;;;                 other.
;;; 01-FEB-91  mk   Added default definitions of PROVIDE/REQUIRE/*MODULES*
;;;                 for lisps that have thrown away these definitions in
;;;                 accordance with CLtL2.
;;; 01-FEB-91  mk   Added :compile-only slot to components. Analogous to
;;;                 :load-only. If :compile-only is T, will not load the
;;;                 file on operation :compile. Either compiles or loads
;;;                 the file, but not both. In other words, compiling the
;;;                 file satisfies the demand to load it. This is useful
;;;                 for PCL defmethod and defclass definitions, which wrap
;;;                 an (eval-when (compile load eval) ...) around the body
;;;                 of the definition -- we save time by not loading the
;;;                 compiled code, since the eval-when forces it to be
;;;                 loaded. Note that this may not be entirely safe, since
;;;                 CLtL2 has added a :load keyword to compile-file, and
;;;                 some lisps may maintain a separate environment for
;;;                 the compiler. This feature is for the person who asked
;;;                 that a :COMPILE-SATISFIES-LOAD keyword be added to
;;;                 modules. It's named :COMPILE-ONLY instead to match
;;;                 :LOAD-ONLY.
;;; 11-FEB-91  mk   Now adds :mk-defsystem to features list, to allow
;;;                 special cased loading of defsystem if not already
;;;                 present.
;;; 19-FEB-91  duff Added filename extension for hp9000/300's running Lucid.
;;; 26-FEB-91  mk   Distinguish between toplevel systems (defined with
;;;                 defsystem) and systems defined as a :system module
;;;                 of a defsystem. The former can depend only on systems,
;;;                 while the latter can depend on anything at the same
;;;                 level.
;;; 12-MAR-91  mk   Added :subsystem component type to be a system with
;;;                 pathnames relative to its parent component.
;;; 12-MAR-91  mk   Uncommented :device :absolute for CMU pathnames, so
;;;                 that the leading slash is included.
;;; 12-MAR-91  brad Patches for Allegro 4.0.1 on Sparc.
;;; 12-MAR-91  mk   Changed definition of format-justified-string so that
;;;                 it no longer depends on the ~<~> format directives,
;;;                 because Allegro 4.0.1 has a bug which doesn't support
;;;                 them. Anyway, the new definition is twice as fast
;;;                 and conses half as much as FORMAT.
;;; 12-MAR-91 toni  Remove nils from list in expand-component-components.
;;; 12-MAR-91 bw    If the default-package and system have the same name,
;;;                 and the package is not loaded, this could lead to
;;;                 infinite loops, so we bomb out with an error.
;;;                 Fixed bug in default packages.
;;; 13-MAR-91 mk    Added global *providing-blocks-load-propagation* to
;;;                 control whether system dependencies are loaded if they
;;;                 have already been provided.
;;; 13-MAR-91 brad  In-package is a macro in CLtL2 lisps, so we change
;;;                 the package manually in operate-on-component.
;;; 15-MAR-91 mk    Modified *central-registry* to be either a single
;;;                 directory pathname, or a list of directory pathnames
;;;                 to be checked in order.
;;; 15-MAR-91 rs    Added afs-source-directory to handle versions when
;;;                 compiling C code under lisp. Other minor changes to
;;;                 translate-version and operate-on-system.
;;; 21-MAR-91 gi    Fixed bug in defined-systems.
;;; 22-MAR-91 mk    Replaced append-directories with new version that works
;;;                 by actually appending the directories, after massaging
;;;                 them into the proper format. This should work for all
;;;                 CLtL2-compliant lisps.
;;; 09-APR-91 djc   Missing package prefix for lp:pathname-host-type.
;;;                 Modified component-full-pathname to work for logical
;;;                 pathnames.
;;; 09-APR-91 mk    Added *dont-redefine-require* to control whether
;;;                 REQUIRE is redefined. Fixed minor bugs in redefinition
;;;                 of require.
;;; 12-APR-91 mk    (pathname-host nil) causes an error in MCL 2.0b1
;;; 12-APR-91 mc    Ported to MCL2.0b1.
;;; 16-APR-91 mk    Fixed bug in needs-loading where load-time and
;;;                 file-write-date got swapped.
;;; 16-APR-91 mk    If the component is load-only, defsystem shouldn't
;;;                 tell you that there is no binary and ask you if you
;;;                 want to load the source.
;;; 17-APR-91 mc    Two additional operations for MCL.
;;; 21-APR-91 mk    Added feature requested by ik. *files-missing-is-an-error*
;;;                 new global variable which controls whether files (source
;;;                 and binary) missing cause a continuable error or just a
;;;                 warning.
;;; 21-APR-91 mk    Modified load-file-operation to allow compilation of source
;;;                 files during load if the binary files are old or
;;;                 non-existent. This adds a :compile-during-load keyword to
;;;                 oos, and load-system. Global *compile-during-load* sets
;;;                 the default (currently :query).
;;; 21-APR-91 mk    Modified find-system so that there is a preference for
;;;                 loading system files from disk, even if the system is
;;;                 already defined in the environment.
;;; 25-APR-91 mk    Removed load-time slot from component defstruct and added
;;;                 function COMPONENT-LOAD-TIME to store the load times in a
;;;                 hash table. This is safer than the old definition because
;;;                 it doesn't wipe out load times every time the system is
;;;                 redefined.
;;; 25-APR-91 mk    Completely rewrote load-file-operation. Fixed some bugs
;;;                 in :compile-during-load and in the behavior of defsystem
;;;                 when multiple users are compiling and loading a system
;;;                 instead of just a single user.
;;; 16-MAY-91 mk    Modified FIND-SYSTEM to do the right thing if the system
;;;                 definition file cannot be found.
;;; 16-MAY-91 mk    Added globals *source-pathname-default* and
;;;                 *binary-pathname-default* to contain default values for
;;;                 :source-pathname and :binary-pathname. For example, set
;;;                 *source-pathname-default* to "" to avoid having to type
;;;                 :source-pathname "" all the time.
;;; 27-MAY-91 mk    Fixed bug in new-append-directories where directory
;;;                 components of the form "foo4.0" would appear as "foo4",
;;;                 since pathname-name truncates the type. Changed
;;;                 pathname-name to file-namestring.
;;;  3-JUN-91 gc    Small bug in new-append-directories; replace (when
;;;                 abs-name) with (when (not (null-string abs-name)))
;;;  4-JUN-91 mk    Additional small change to new-append-directories for
;;;                 getting the device from the relative pname if the abs
;;;                 pname is "". This is to fix a small behavior in CMU CL old
;;;                 compiler. Also changed (when (not (null-string abs-name)))
;;;                 to have an (and abs-name) in there.
;;;  8-JAN-92 sb    Added filename extension for defsystem under Lucid Common
;;;                 Lisp/SGO 3.0.1+.
;;;  8-JAN-92 mk    Changed the definition of prompt-string to work around an
;;;                 AKCL bug. Essentially, AKCL doesn't default the colinc to
;;;                 1 if the colnum is provided, so we hard code it.
;;;  8-JAN-92 rs    (pathname-directory (pathname "")) returns '(:relative) in
;;;                 Lucid, instead of NIL. Changed new-append-directories and
;;;                 test-new-append-directories to reflect this.
;;;  8-JAN-92 mk    Fixed problem related to *load-source-if-no-binary*.
;;;                 compile-and-load-source-if-no-binary wasn't checking for
;;;                 the existence of the binary if this variable was true,
;;;                 causing the file to not be compiled.
;;;  8-JAN-92 mk    Fixed problem with null-string being called on a pathname
;;;                 by returning NIL if the argument isn't a string.
;;;  3-NOV-93 mk    In Allegro 4.2, pathname device is :unspecific by default.
;;; 11-NOV-93 fdmm  Fixed package definition lock problem when redefining
;;;                 REQUIRE on ACL.
;;; 11-NOV-93 fdmm  Added machine and software types for SGI and IRIX. It is
;;;                 important to distinguish the OS version and CPU type in
;;;                 SGI+ACL, since ACL 4.1 on IRIX 4.x and ACL 4.2 on IRIX 5.x
;;;                 have incompatible .fasl files.
;;; 01-APR-94 fdmm  Fixed warning problem when redefining REQUIRE on LispWorks.
;;; 01-NOV-94 fdmm  Replaced (software-type) call in ACL by code extracting
;;;                 the interesting parts from (software-version) [deleted
;;;                 machine name and id].
;;; 03-NOV-94 fdmm  Added a hook (*compile-file-function*), that is funcalled
;;;                 by compile-file-operation, so as to support other languages
;;;                 running on top of Common Lisp.
;;;                 The default is to compile  Common Lisp.
;;; 03-NOV-94 fdmm  Added SCHEME-COMPILE-FILE, so that defsystem can now
;;;                 compile Pseudoscheme files.
;;; 04-NOV-94 fdmm  Added the exported generic function SET-LANGUAGE, to
;;;                 have a clean, easy to extend  interface for telling
;;;                 defsystem which language to assume for compilation.
;;;                 Currently supported arguments: :common-lisp, :scheme.
;;; 11-NOV-94 kc    Ported to Allegro CL for Windows 2.0 (ACLPC) and CLISP.
;;; 18-NOV-94 fdmm  Changed the entry *filename-extensions* for LispWorks
;;;                 to support any platform.
;;;                 Added entries for :mcl and :clisp too.
;;; 16-DEC-94 fdmm  Added and entry for CMU CL on SGI to *filename-extensions*.
;;; 16-DEC-94 fdmm  Added OS version identification for CMU CL on SGI.
;;; 16-DEC-94 fdmm  For CMU CL 17 : Bypassed make-pathnames call fix
;;;                 in NEW-APPEND-DIRECTORIES.
;;; 16-DEC-94 fdmm  Added HOME-SUBDIRECTORY to fix CMU's ignorance about `~'
;;;                 when specifying registries.
;;; 16-DEC-94 fdmm  For CMU CL 17 : Bypassed :device fix in make-pathnames call
;;;                 in COMPONENT-FULL-PATHNAME. This fix was also reported
;;;                 by kc on 12-NOV-94. CMU CL 17 now supports CLtL2 pathnames.
;;; 16-DEC-94 fdmm  Removed a quote before the call to read in the readmacro
;;;                 #@. This fixes a really annoying misfeature (couldn't do
;;;                 #@(concatenate 'string "foo/" "bar"), for example).
;;; 03-JAN-95 fdmm  Do not include :pcl in *features* if :clos is there.
;;;  2-MAR-95 mk    Modified fdmm's *central-registry* change to use
;;;                 user-homedir-pathname and to be a bit more generic in the
;;;                 pathnames.
;;;  2-MAR-95 mk    Modified fdmm's updates to *filename-extensions* to handle
;;;                 any CMU CL binary extensions.
;;;  2-MAR-95 mk    Make kc's port to ACLPC a little more generic.
;;;  2-MAR-95 mk    djc reported a bug, in which GET-SYSTEM was not returning
;;;                 a system despite the system's just having been loaded.
;;;                 The system name specified in the :depends-on was a
;;;                 lowercase string. I am assuming that the system name
;;;                 in the defsystem form was a symbol (I haven't verified
;;;                 that this was the case with djc, but it is the only
;;;                 reasonable conclusion). So, CANONICALIZE-SYSTEM-NAME
;;;                 was storing the system in the hash table as an
;;;                 uppercase string, but attempting to retrieve it as a
;;;                 lowercase string. This behavior actually isn't a bug,
;;;                 but a user error. It was intended as a feature to
;;;                 allow users to use strings for system names when
;;;                 they wanted to distinguish between two different systems
;;;                 named "foo.system" and "Foo.system". However, this
;;;                 user error indicates that this was a bad design decision.
;;;                 Accordingly, CANONICALIZE-SYSTEM-NAME now uppercases
;;;                 even strings for retrieving systems, and the comparison
;;;                 in *modules* is now case-insensitive. The result of
;;;                 this change is if the user cannot have distinct
;;;                 systems in "Foo.system" and "foo.system" named "Foo" and
;;;                 "foo", because they will clobber each other. There is
;;;                 still case-sensitivity on the filenames (i.e., if the
;;;                 system file is named "Foo.system" and you use "foo" in
;;;                 the :depends-on, it won't find it). We didn't take the
;;;                 further step of requiring system filenames to be lowercase
;;;                 because we actually find this kind of case-sensitivity
;;;                 to be useful, when maintaining two different versions
;;;                 of the same system.
;;;  7-MAR-95 mk    Added simplistic handling of logical pathnames. Also
;;;                 modified new-append-directories so that it'll try to
;;;                 split up pathname directories that are strings into a
;;;                 list of the directory components. Such directories aren't
;;;                 ANSI CL, but some non-conforming implementations do it.
;;;  7-MAR-95 mk    Added :proclamations to defsystem form, which can be used
;;;                 to set the compiler optimization level before compilation.
;;;                 For example,
;;;                  :proclamations '(optimize (safety 3) (speed 3) (space 0))
;;;  7-MAR-95 mk    Defsystem now tells the user when it reloads the system
;;;                 definition.
;;;  7-MAR-95 mk    Fixed problem pointed out by yc. If
;;;                 *source-pathname-default* is "" and there is no explicit
;;;                 :source-pathname specified for a file, the file could
;;;                 wind up with an empty file name. In other words, this
;;;                 global default shouldn't apply to :file components. Added
;;;                 explicit test for null strings, and when present replaced
;;;                 them with NIL (for binary as well as source, and also for
;;;                 :private-file components).
;;;  7-MAR-95 tar   Fixed defsystem to work on TI Explorers (TI CL).
;;;  7-MAR-95 jk    Added machine-type-translation for Decstation 5000/200
;;;                 under Allegro 3.1
;;;  7-MAR-95 as    Fixed bug in AKCL-1-615 in which defsystem added a
;;;                 subdirectory "RELATIVE" to all filenames.
;;;  7-MAR-95 mk    Added new test to test-new-append-directories to catch the
;;;                 error fixed by as. Essentially, this error occurs when the
;;;                 absolute-pathname has no directory (i.e., it has a single
;;;                 pathname component as in "foo" and not "foo/bar"). If
;;;                 RELATIVE ever shows up in the Result, we now know to
;;;                 add an extra conditionalization to prevent abs-keyword
;;;                 from being set to :relative.
;;;  7-MAR-95 ss    Miscellaneous fixes for MCL 2.0 final.
;;;                 *compile-file-verbose* not in MCL, *version variables
;;;                 need to occur before AFS-SOURCE-DIRECTORY definition,
;;;                 and certain code needed to be in the CCL: package.
;;;  8-MAR-95 mk    Y-OR-N-P-WAIT uses a busy-waiting. On Lisp systems where
;;;                 the time functions cons, such as CMU CL, this can cause a
;;;                 lot of ugly garbage collection messages. Modified the
;;;                 waiting to include calls to SLEEP, which should reduce
;;;                 some of the consing.
;;;  8-MAR-95 mk    Replaced fdmm's SET-LANGUAGE enhancement with a more
;;;                 general extension, along the lines suggested by akd.
;;;                 Defsystem now allows components to specify a :language
;;;                 slot, such as :language :lisp, :language :scheme. This
;;;                 slot is inherited (with the default being :lisp), and is
;;;                 used to obtain compilation and loading functions for
;;;                 components, as well as source and binary extensions. The
;;;                 compilation and loading functions can be overridden by
;;;                 specifying a :compiler or :loader in the system
;;;                 definition. Also added :documentation slot to the system
;;;                 definition.
;;;                    Where this comes in real handy is if one has a
;;;                 compiler-compiler implemented in Lisp, and wants the
;;;                 system to use the compiler-compiler to create a parser
;;;                 from a grammar and then compile parser. To do this one
;;;                 would create a module with components that looked
;;;                 something like this:
;;;		    ((:module cc :components ("compiler-compiler"))
;;;		     (:module gr :compiler 'cc :loader #'ignore
;;;			      :source-extension "gra"
;;;			      :binary-extension "lisp"
;;;			      :depends-on (cc)
;;;			      :components ("sample-grammar"))
;;;		     (:module parser :depends-on (gr)
;;;			      :components ("sample-grammar")))
;;;                 Defsystem would then compile and load the compiler, use
;;;                 it (the function cc) to compile the grammar into a parser,
;;;                 and then compile the parser. The only tricky part is
;;;                 cc is defined by the system, and one can't include #'cc
;;;                 in the system definition. However, one could include
;;;                 a call to mk:define-language in the compiler-compiler file,
;;;                 and define :cc as a language. This is the prefered method.
;;;  8-MAR-95 mk    New definition of topological-sort suggested by rs2. This
;;;                 version avoids the call to SORT, but in practice isn't
;;;                 much faster. However, it avoids the need to maintain a
;;;                 TIME slot in the topsort-node structure.
;;;  8-MAR-95 mk    rs2 also pointed out that the calls to MAKE-PATHNAME and
;;;                 NAMESTRING in COMPONENT-FULL-PATHNAME are a major reason
;;;                 why defsystem is slow. Accordingly, I've changed
;;;                 COMPONENT-FULL-PATHNAME to include a call to NAMESTRING
;;;                 (and removed all other calls to NAMESTRING), and also made
;;;                 a few changes to minimize the number of calls to
;;;                 COMPONENT-FULL-PATHNAME, such as memoizing it. See To Do
;;;                 below for other related comments.
;;;  8-MAR-95 mk    Added special hack requested by Steve Strassman, which
;;;                 allows one to specify absolute pathnames in the shorthand
;;;                 for a list of components, and have defsystem recognize
;;;                 which are absolute and which are relative.
;;;                 I actually think this would be a good idea, but I haven't
;;;                 tested it, so it is disabled by default. Search for
;;;                 *enable-straz-absolute-string-hack* to enable it.
;;;  8-MAR-95 kt    Fixed problem with EXPORT in AKCL 1.603, in which it wasn't
;;;                 properly exporting the value of the global export
;;;                 variables.
;;;  8-MAR-95 mk    Added UNMUNGE-LUCID to fix nasty problem with COMPILE-FILE
;;;                 in Lucid. Lucid apparently tries to merge the :output-file
;;;                 with the source file when the :output-file is a relative
;;;                 pathname. Wierd, and definitely non-standard.
;;;  9-MAR-95 mk    Changed ALLEGRO-MAKE-SYSTEM-FASL to also include the files
;;;                 in any systems the system depends on, as per a
;;;                 request of oc.
;;;  9-MAR-95 mk    Some version of CMU CL couldn't hack a call to
;;;                 MAKE-PATHNAME with :host NIL. I'm not sure which version
;;;                 it is, but the current version doesn't have this problem.
;;;                 If given :host nil, it defaults the host to
;;;                 COMMON-LISP::*UNIX-HOST*. So I haven't "fixed" this
;;;                 problem.
;;;  9-MAR-95 mk    Integrated top-level commands for Allegro designed by bha
;;;                 into the code, with slight modifications.
;;;  9-MAR-95 mk    Instead of having COMPUTE-SYSTEM-PATH check the current
;;;                 directory in a hard-coded fashion, include the current
;;;                 directory in the *central-registry*, as suggested by
;;;                 bha and others.
;;;  9-MAR-95 bha   Support for Logical Pathnames in Allegro.
;;;  9-MAR-95 mk    Added modified version of bha's DEFSYSPATH idea.
;;; 13-MAR-95 mk    Added a macro for the simple serial case, where a system
;;;                 (or module) is simple a list of files, each of which
;;;                 depends on the previous one. If the value of :components
;;;                 is a list beginning with :serial, it expands each
;;;                 component and makes it depend on the previous component.
;;;                 For example, (:serial "foo" "bar" "baz") would create a
;;;                 set of components where "baz" depended on "bar" and "bar"
;;;                 on "foo".
;;; 13-MAR-95 mk    *** Now version 3.0. This version is a interim bug-fix and
;;;                 update, since I do not have the time right now to complete
;;;                 the complete overhaul and redesign.
;;;                 Major changes in 3.0 include CMU CL 17, CLISP, ACLPC, TI,
;;;                 LispWorks and ACL(SGI) support, bug fixes for ACL 4.1/4.2.
;;; 14-MAR-95 fdmm  Finally added the bit of code to discriminate cleanly
;;;                 among different lisps without relying on (software-version)
;;;                 idiosyncracies.
;;;                 You can now customize COMPILER-TYPE-TRANSLATION so that
;;;                 AFS-BINARY-DIRECTORY can return a different value for
;;;                 different lisps on the same platform.
;;;                 If you use only one compiler, do not care about supporting
;;;                 code for multiple versions of it, and want less verbose
;;;                 directory names, just set *MULTIPLE-LISP-SUPPORT* to nil.
;;; 17-MAR-95 lmh   Added EVAL-WHEN for one of the MAKE-PACKAGE calls.
;;;                 CMU CL's RUN-PROGRAM is in the extensions package.
;;;                 ABSOLUTE-FILE-NAMESTRING-P was missing :test keyword
;;;                 Rearranged conditionalization in DIRECTORY-TO-LIST to
;;;                 suppress compiler warnings in CMU CL.
;;; 17-MAR-95 mk    Added conditionalizations to avoid certain CMU CL compiler
;;;                 warnings reported by lmh.
;;; 1999-06-10 ma   Added shadowing of 'HARDCOPY-SYSTEM' for LW Personal Ed.
;;;
;;; 1999-12-11 ma   NEW VERSION 4.0 started.
;;; 1999-12-11 ma   Merged in changes requested by T. Russ of
;;;                 ISI. Please refer to the special "ISI" comments to
;;;                 understand these changes
;;; 2000-02-28 ma   The symbols FIND-SYSTEM, LOAD-SYSTEM, DEFSYSTEM,
;;;                 COMPILE-SYSTEM and HARDCOPY-SYSTEM are no longer
;;;                 imported in the COMMON-LISP-USER package.
;;;                 Cfr. the definitions of *EXPORTS* and
;;;                 *SPECIAL-EXPORTS*.
;;; 2000-07-21 rlt  Add COMPILER-OPTIONS to defstruct to allow user to
;;;                 specify special compiler options for a particular
;;;                 component.
;;; 2002-01-08 kmr  Changed allegro symbols to lowercase to support
;;;                 case-sensitive images
;;; 2024-06-28 ma   Bullet bitten: clean up all the REQUIRE/PROVIDE
;;;                 cruft and brought (mostly) up to speed to 2024 CL
;;;                 implementations.  Note that the version is 3.9.2
;;;                 and not 4.0 as in the note above.
;;; 2025-12-14 ma   Made a blunt decision about propagating operations
;;;                 on systems' dependendecies.  Bumped version to 3.9.3.
;;;
;;;
;;; 2026-12-20 ma   Modernize!  Bumped version to 3.9.4 (on a different
;;;                 git branch).  Component hierarchy and fixing
;;;                 (path)naming as Lispworks is too liberal with truenames.
;;;
;;; 2026-03-03 ma   Switched to truenames and namestrings and changed
;;;                 pathnames generation.
;;; 2026-03-12 ma   This version has "eager" pathname generation.
;;;                 That is, COMPONENT-FULL-PATHANME-I does very little
;;;                 as the COMPONENT-PATHNAMEs slots already contain
;;;                 pre-computed paths.
;;; 2026-04-02 ma   Bumped version to 3.9.4.1, eager pathnames working.


;;;---------------------------------------------------------------------------
;;; ISI Comments
;;;
;;; 1999-12-11 Marco Antoniotti
;;; These comments come from the "ISI Branch".  I believe I did
;;; include the :load-always extension correctly.  The other comments
;;; seem superseded by other changes made to the system in the
;;; following years.  Some others are now useless with newer systems
;;; (e.g. filename truncation for new Windows based CL
;;; implementations.)

;;;  1-OCT-92 tar   Fixed problem with TI Lisp machines and append-directory.
;;;  1-OCT-92 tar   Made major modifications to compile-file-operation and
;;;                 load-file-operation to reduce the number of probe-file
;;;                 and write-date inquiries.  This makes the system run much
;;;                 faster through slow network connections.
;;; 13-OCT-92 tar   Added :load-always slot to components. If this slot is
;;;                 specified as non-NIL, always loads the component.
;;;                 This does not trigger dependent compilation.
;;;                 (This can be useful when macro definitions needed
;;;                 during compilation are changed by later files.  In
;;;                 this case, not reloading up-to-date files can
;;;                 cause different results.)
;;; 28-OCT-93 tar   Allegro 4.2 causes an error on (pathname-device nil)
;;; 14-SEP-94 tar   Disable importing of symbols into (CL-)USER package
;;;                 to minimize conflicts with other defsystem utilities.
;;; 10-NOV-94 tar   Added filename truncation code to support Franz Allegro
;;;                 PC with it's 8 character filename limitation.
;;; 15-MAY-98 tar   Changed host attribute for pathnames to support LispWorks
;;;                 (Windows) pathnames which reference other Drives.  Also
;;;                 updated file name convention.
;;;  9-NOV-98 tar   Updated new-append-directories for Lucid 5.0
;;;


;;; Ports
;;; -----
;;;
;;;    MK:DEFSYSTEM has been tested (succesfully) in the following CL
;;;    implementations:
;;;       Allegro CL 11.0 (2024)
;;;       Clozure Common Lisp 1.12.2 (2024)
;;;       ECL 24.5.10 (2024)
;;;       Lispworks 8.0.1 (2024)
;;;       SBCL 2.2.9 (2024)

;;;    DEFSYSTEM had been previously tested (successfully) in the
;;;    following Common Lisp's:
;;;       CMU Common Lisp (M2.9 15-Aug-90, Compiler M1.8 15-Aug-90)
;;;       CMU Common Lisp (14-Dec-90 beta, Python Compiler 0.0 PMAX/Mach)
;;;       CMU Common Lisp 17f (Python 1.0)
;;;       SBCL
;;;       Franz Allegro Common Lisp 3.1.12 (ExCL 3/30/90)
;;;       Franz Allegro Common Lisp 4.0/4.1/4.2
;;;       Franz Allegro Common Lisp for Windows (2.0)
;;;       Lucid Common Lisp (Version 2.1 6-DEC-87)
;;;       Lucid Common Lisp (3.0 [SPARC,SUN3])
;;;       Lucid Common Lisp (4.0 [SPARC,SUN3])
;;;       VAXLisp (v2.2) [VAX/VMS]
;;;       VAXLisp (v3.1)
;;;       LispWorks (up to version 8.1.2)
;;;       CLISP (CLISP3 [SPARC])
;;;       Symbolics XL12000 (Genera 8.3)
;;;       Scieneer Common Lisp (SCL) 1.1
;;;       CCL, OPenMCL and Macintosh Common Lisp
;;;       ECL
;;;       ABCL
;;;
;;;    DEFSYSTEM needs to be tested in the following Common Lisp's:
;;;       CLASP
;;;
;;;    Abandoned ports (due to deadness of implementations)
;;;       Symbolics Common Lisp (8.0)
;;;       KCL (June 3, 1987 or later)
;;;       AKCL (1.86, June 30, 1987 or later)
;;;       GCL
;;;       TI (Release 4.1 or later)
;;;       Ibuki Common Lisp (01/01, October 15, 1987)
;;;       Golden Common Lisp (3.1 IBM-PC)
;;;       HP Common Lisp (same as Lucid?)
;;;       Procyon Common Lisp


;;; To Do
;;; -----
;;;
;;; COMPONENT-FULL-PATHNAME is a major source of slowness in the system
;;; because of all the calls to the expensive operations MAKE-PATHNAME
;;; and NAMESTRING. To improve performance, DEFSYSTEM should be reworked
;;; to avoid any need to call MAKE-PATHNAME and NAMESTRING, as the logical
;;; pathnames package does. Unfortunately, I don't have the time to do this
;;; right now. Instead, I installed a temporary improvement by memoizing
;;; COMPONENT-FULL-PATHNAME to cache previous calls to the function on
;;; a component by component and type by type basis. The cache is
;;; cleared before each call to OOS, in case filename extensions change.
;;; But DEFSYSTEM should really be reworked to avoid this problem and
;;; ensure greater portability and to also handle logical pathnames.
;;;
;;; Also, PROBE-FILE and FILE-WRITE-DATE are other sources of slowness.
;;; Perhaps by also memoizing FILE-WRITE-DATE and reimplementing PROBE-FILE
;;; in terms of FILE-WRITE-DATE, can achieve a further speed-up. This was
;;; suggested by Steven Feist (feist@ils.nwu.edu).
;;;
;;; True CLtL2 logical pathnames support -- can't do it, because CLtL2
;;; doesn't have all the necessary primitives, and even in Allegro CL 4.2
;;;   (namestring #l"foo:bar;baz.lisp")
;;; does not work properly.
;;;
;;; Create separate stand-alone documentation for defsystem, and also
;;; a test suite.
;;;
;;; Change SYSTEM to be a class instead of a struct, and make it a little
;;; more generic, so that it permits alternate system definitions.
;;; Replace OPERATE-ON-SYSTEM with MAP-SYSTEM (args: function, system-name,
;;; &rest options)
;;;
;;; Add a patch directory mechanism. Perhaps have several directories
;;; with code in them, and the first one with the specified file wins?
;;; LOAD-PATCHES function.
;;;
;;; Need way to load old binaries even if source is newer.
;;;
;;; Allow defpackage forms/package definitions in the defsystem? If
;;; a package not defined, look for and load a file named package.pkg?
;;;
;;; need to port for GNU CL (ala kcl)?
;;;
;;; Someone asked whether one can have :file components at top-level. I believe
;;; this is the case, but should double-check that it is possible (and if
;;; not, make it so).
;;;
;;; A common error/misconception seems to involve assuming that :system
;;; components should include the name of the system file, and that
;;; defsystem will automatically load the file containing the system
;;; definition and propagate operations to it. Perhaps this would be a
;;; nice feature to add.
;;;
;;; If a module is :load-only t, then it should not execute its :finally-do
;;; and :initially-do clauses during compilation operations, unless the
;;; module's files happen to be loaded during the operation.
;;;
;;; System Class. Customizable delimiters.
;;;
;;; Load a system (while not loading anything already loaded)
;;; and inform the user of out of date fasls with the choice
;;; to load the old fasl or recompile and then load the new
;;; fasl?
;;;
;;; Modify compile-file-operation to handle a query keyword....
;;;
;;; Perhaps systems should keep around the file-write-date of the system
;;; definition file, to prevent excessive reloading of the system definition?
;;;
;;; load-file-operation needs to be completely reworked to simplify the
;;; logic of when files get loaded or not.
;;;
;;; Need to revamp output: Nesting and indenting verbose output doesn't
;;; seem cool, especially when output overflows the 80-column margins.
;;;
;;; Document various ways of writing a system. simple (short) form
;;; (where :components is just a list of filenames) in addition to verbose.
;;; Put documentation strings in code.
;;;
;;; :load-time for modules and systems -- maybe record the time the system
;;; was loaded/compiled here and print it in describe-system?
;;;
;;; Make it easy to define new functions that operate on a system. For
;;; example, a function that prints out a list of files that have changed,
;;; hardcopy-system, edit-system, etc.
;;;
;;; If a user wants to have identical systems for different lisps, do we
;;; force the user to use logical pathnames? Or maybe we should write a
;;; generic-pathnames package that parses any pathname format into a
;;; uniform underlying format (i.e., pull the relevant code out of
;;; logical-pathnames.lisp and clean it up a bit).
;;;
;;;    Verify that Mac pathnames now work with append-directories.
;;;
;;; A common human error is to violate the modularization by making a file
;;; in one module depend on a file in another module, instead of making
;;; one module depend on the other. This is caught because the dependency
;;; isn't found. However, is there any way to provide a more informative
;;; error message? Probably not, especially if the system has multiple
;;; files of the same name.
;;;
;;; For a module none of whose files needed to be compiled, have it print out
;;; "no files need recompilation".
;;;
;;; Write a system date/time to a file? (version information) I.e., if the
;;; filesystem supports file version numbers, write an auxiliary file to
;;; the system definition file that specifies versions of the system and
;;; the version numbers of the associated files.
;;;
;;; Add idea of a patch directory.
;;;
;;; In verbose printout, have it log a date/time at start and end of
;;; compilation:
;;;     Compiling system "test" on 31-Jan-91 21:46:47
;;;     by Defsystem version v2.0 01-FEB-91.
;;;
;;; Define other :force options:
;;;    :query    allows user to specify that a file not normally compiled
;;;              should be. OR
;;;    :confirm  allows user to specify that a file normally compiled
;;;              shouldn't be. AND
;;;
;;; We currently assume that compilation-load dependencies and if-changed
;;; dependencies are identical. However, in some cases this might not be
;;; true. For example, if we change a macro we have to recompile functions
;;; that depend on it (except in lisps that automatically do this, such
;;; as the new CMU Common Lisp), but not if we change a function. Splitting
;;; these apart (with appropriate defaulting) would be nice, but not worth
;;; doing immediately since it may save only a couple of file recompilations,
;;; while making defsystem much more complex than it already is.
;;;
;;; Current dependencies are limited to siblings. Maybe we should allow
;;; nephews and uncles? So long as it is still a DAG, we can sort it.
;;; Answer: No. The current setup enforces a structure on the modularity.
;;; Otherwise, why should we have modules if we're going to ignore it?
;;;
;;; Currently a file is recompiled more or less if the source is newer
;;; than the binary or if the file depends on a file that has changed
;;; (i.e., was recompiled in this session of a system operation).
;;; Neil Goldman <goldman@isi.edu> has pointed out that whether a file
;;; needs recompilation is really independent of the current session of
;;; a system operation, and depends only on the file-write-dates of the
;;; source and binary files for a system. Thus a file should require
;;; recompilation in the following circumstances:
;;;   1. If a file's source is newer than its binary, or
;;;   2. If a file's source is not newer than its binary, but the file
;;;      depends directly or indirectly on a module (or file) that is newer.
;;;      For a regular file use the file-write-date (FWD) of the source or
;;;      binary, whichever is more recent. For a load-only file, use the only
;;;      available FWD. For a module, use the most recent (max) FWD of any of
;;;      its components.
;;; The impact of this is that instead of using a boolean CHANGED variable
;;; throughout the code, we need to allow CHANGED to be NIL/T/<FWD> or
;;; maybe just the FWD timestamp, and to use the value of CHANGED in
;;; needs-compilation decisions. (Use of NIL/T as values is an optimization.
;;; The FWD timestamp which indicates the most recent time of any changes
;;; should be sufficient.) This will affect not just the
;;; compile-file-operation, but also the load-file-operation because of
;;; compilation during load. Also, since FWDs will be used more prevalently,
;;; we probably should couple this change with the inclusion of load-times
;;; in the component defstruct. This is a tricky and involved change, and
;;; requires more thought, since there are subtle cases where it might not
;;; be correct. For now, the change will have to wait until the DEFSYSTEM
;;; redesign.
;;;
;;; ## 2026-03-11: ma: Handle HOST, DEVICE and VERSION pathname slots.
;;; 
;;; Currently the COMPONENT slots for HOST, DEVICE and VERSION are not
;;; really handled.  They probably should, even though that may create
;;; problems with logical pathnames.


;;; How to Use this System
;;; ----------------------

;;; To use this system,
;;; 1. If you want to have a central registry of system definitions,
;;;    modify the value of the variable *central-registry* below.
;;; 2. Load this file (defsystem.lisp) in either source or compiled form,
;;; 3. Load the file containing the "defsystem" definition of your system,
;;; 4. Use the function "operate-on-system" to do things to your system.

;;; For more information, see the documentation and examples in
;;; lisp-utilities.ps.


;;; Usage Comments
;;; --------------

;;; If you use symbols in the system definition file, they get interned in
;;; the COMMON-LISP-USER package, which can lead to name conflicts when
;;; the system itself seeks to export the same symbol to the COMMON-LISP-USER
;;; package. The workaround is to use strings instead of symbols for the
;;; names of components in the system definition file. In the major overhaul,
;;; perhaps the user should be precluded from using symbols for such
;;; identifiers.
;;;
;;; If you include a tilde in the :source-pathname in Allegro, as in "~/lisp",
;;; file name expansion is much slower than if you use the full pathname,
;;; as in "/user/USERID/lisp".


;;; Common Lisp Code
;;; ----------------

;;; Massage CLtL2 onto *features*
;;; -----------------------------
;;;
;;; Let's be smart about CLtL2 compatible Lisps:

(eval-when (:compile-toplevel :load-toplevel :execute) ; Sorry LUCID!
  ;; This ANSI EVAL-WHEN is already a show stopper for older CLs.
  ;; You can downgrade, but it would break other things.
  (pushnew :cltl2 *features*)
  )


;;; Set up the MAKE Package
;;; -----------------------

;;; Unfortunately, lots of lisps have their own defsystems, some more
;;; primitive than others, all uncompatible, and all in the DEFSYSTEM
;;; package. To avoid name conflicts, we've decided to name this the
;;; MAKE package. A nice side-effect is that the short nickname
;;; MK is my initials.

;;; Notes:
;;; 20220212 MA: Given the date, I am leaving the comment above, but I
;;; am removing the old cruft about DEFPACKAGE and IN-PACKAGE.
;;; If your CL implementation generates and error, you should upgrade
;;; or move to a more modern one.
;;; 
;;; Bottom line: no DEFPACKAGE.

(eval-when (:load-toplevel :compile-toplevel :execute) ; Sorry LUCID!
  (unless (find-package "MAKE")
    (let ((mk-pkg (make-package "MAKE"
				:nicknames '("MK")
				:use '("COMMON-LISP")))
	  ) 
      (setf (documentation mk-pkg t)
            "The MAKE (MK-DEFSYSTEM) Package.

This package contains the implementation of the venerable MK:DEFSYTEM
tool.

The main entry points are:
- DEFSYSTEM
- LOAD-SYSTEM
- COMPILE-SYSTEM
- FIND-SYSTEM
- ADD-REGISTRY-LOCATIONS
- LIST-CENTRAL-REGISTRY-DIRECTORIES
- PRINT-CENTRAL-REGISTRY-DIRECTORIES
"
            ))))

(in-package "MAKE")


;;; Defsystem Version
;;; -----------------

(eval-when (:load-toplevel :compile-toplevel :execute) ; Sorry LUCID!

  (defparameter *defsystem-version* "3.9.4.2 (More Modern), 2026-04-26"
    "Current version number/date for MK:DEFSYSTEM.")


  #+(or :unix :linux :dos :msdos :windows :mswindows :w32 :win32 :vms)
  (format t "~&;;; MK: loading MK-DEFSYSTEM ~A.~%"
	  *defsystem-version*)
  
  #-(or :unix :linux :dos :msdos :windows :mswindows :w32 :win32 :vms)
  (warn "~&;;; MK: loading MK-DEFSYSTEM ~A on a not-so-supported platform.~%"
	*defsystem-version*)
  )


;;; Compatibility
;;; =============
;;;
;;; Notes:
;;; 2024-06-30 MA: added warning valid at this date.


;;; Some compatibility issues.  Mostly for CormanLisp.
;;; 2002-02-20 Marco Antoniotti

#+cormanlisp
(defun compile-file-pathname (pathname-designator)
 (merge-pathnames (make-pathname :type "fasl")
		  (etypecase pathname-designator
		    (pathname pathname-designator)
		    (string (parse-namestring pathname-designator))
		    ;; We need FILE-STREAM here as well.
		    )))


#+cormanlisp
(defun file-namestring (pathname-designator)
  (let ((p (etypecase pathname-designator
	     (pathname pathname-designator)
	     (string (parse-namestring pathname-designator))
	     ;; We need FILE-STREAM here as well.
	     )))
    (namestring (make-pathname :directory ()
			       :name (pathname-name p)
			       :type (pathname-type p)
			       :version (pathname-version p)))))


;;; The external interface consists of *exports* and *other-exports*.

;;; AKCL (at least 1.603) grabs all the (export) forms and puts them up top in
;;; the compile form, so that you can't use a defvar with a default value and
;;; then a succeeding export as well.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *special-exports* nil)
  (defvar *exports* nil)
  (defvar *other-exports* nil)

  (export (setq *exports*
                '(add-registry-location
                  list-central-registry-directories
                  print-central-registry-directories

                  file
                  static-file
                  private-file

                  module
                  system
                  subsystem

                  list-component-components

                  find-system
                  find-component
                  find-component*

                  compile-system
                  load-system
                  hardcopy-system
                  describe-system
                  clean-system
                  edit-system

                  defsystem
                  undefsystem
                  defined-systems

                  operate-on-system
                  oos

                  files-in-system
                  )))
  
  (export (setq *special-exports*
                '()))
  
  (export (setq *other-exports*
                '(*central-registry*
                  *bin-subdir*
		  *mk-output*

		  filename-extensions
		  set-default-source-extension

                  system-definition-pathname
                  
                  missing-component
                  missing-component-name
                  missing-component-component
                  missing-module
                  missing-system

                  register-foreign-system

                  machine-type-translation
                  software-type-translation
                  compiler-type-translation

                  ;; require

                  allegro-make-system-fasl
                  files-which-need-compilation

                  afs-binary-directory afs-source-directory

                  system-description
                  system-documentation

                  system-source-size
                  make-system-tag-table

                  *defsystem-version*
                  *compile-during-load*
                  *minimal-load*
                  *dont-redefine-require*
                  *files-missing-is-an-error*
                  *reload-systems-from-disk*
                  *source-pathname-default*
                  *binary-pathname-default*
                  *multiple-lisp-support*

                  run-unix-program
                  *default-shell*
                  run-shell-command

                  define-language
                  find-language
                  )))
  )


;;; Is this needed?

#-(or :PCL :CLOS :scl)
(when (find-package "PCL")
  (pushnew :pcl *modules*)
  (pushnew :pcl *features*))


;;; Fascist SBCL section
;;; --------------------
;;;
;;; SBCL breaks your you-know-what even for "defined later
;;; functions".  The workaround, which is correct, but which is a
;;; drag, is to define all "forward declarations" as needed.

(declaim (ftype (function (t t t) t) print-component))
(declaim (ftype (function (component &optional keyword t) t)
                component-full-pathname))
(declaim (ftype (function (t &optional t) t)
                compute-system-path)) ; Plus; SBCL is kinda stupid here.
(declaim (ftype (function (t &optional keyword t) t) find-system))
(declaim (ftype (function (string &rest t) t) tell-user-generic))
(declaim (ftype (function (string &rest t) t) tell-user-info))
(declaim (ftype (function (string &rest t) t) tell-user-debug))
(declaim (ftype (function (string &rest t) t) tell-user-warn))
(declaim (ftype (function (string &rest t) t) tell-user-error))
(declaim (ftype (function (&optional t t string &rest t) t) y-or-n-p-wait))
(declaim (ftype (function (t #| Component |# keyword &key (:local t)) t)
                component-extension))
(declaim (ftype (function (t #| Component |# &optional keyword)
			  (or string pathname null))
		component-pathname
		component-root-dir
		))
(declaim (ftype (function (t #| Component |#) (or string pathname null))
		component-error-pathname))
(declaim (ftype (function (t #| Component |# &optional keyword t)
			  (or string pathname))
		component-full-pathname-i))
(declaim (ftype (function (t) t)
		default-source-extension
		default-binary-extension))
(declaim (ftype (function (t t) t) init-component-language))
(declaim (ftype (function (t t) (values t t)) init-component-operators))
(declaim (ftype (function (t t) t) init-component-pathnames))
(declaim (ftype (function (t &optional t) t) expand-component-components))
(declaim (ftype (function (list) t) link-component-depends-on))
(declaim (ftype (function (list) t) topological-sort))

(declaim (ftype (function (t t symbol) t) finalize-component-pathnames))

(declaim (ftype (function (list t &optional fixnum) t) expand-component-definitions))
(declaim (ftype (function (t t &optional fixnum) t) expand-component-definition))
(declaim (ftype (function (list t &optional fixnum) t) expand-serial-component-chain))
(declaim (ftype (function (t t t) t) operate-on-component))
(declaim (ftype (function (t t t t) t) operate-on-components))
(declaim (ftype (function (t t &optional t) t) operate-on-system-dependencies))
(declaim (ftype (function (t &optional t t t t) t) new-require))
(declaim (ftype (function (language t fixnum) t) print-language))
(declaim (ftype (function (t t) t)
		compile-file-operation
		load-file-operation))
(declaim (ftype (function (t t) t) needs-compilation))
(declaim (ftype (function (t) t)
		compile-and-load-source-if-no-binary
		load-source-if-no-binary))
(declaim (ftype (function (t &optional t) t) file-components-in-component))
(declaim (ftype (function (t t &optional t) t) file-pathnames-in-component))
(declaim (ftype (function (list) t) file-list-size))
(declaim (ftype (function (component list t &optional t) t)
                operate-on-system-structured-dependency))
(declaim (ftype (function (t) t) component-load-time))
#|
(declaim (ftype (function ((or string
			       pathname
			       foreign-system
			       component))
			  t)
                component-load-time))
|#
(declaim (ftype (function (t t) t) (setf component-load-time)))
#|
(declaim (ftype (function (t component) t) (setf component-load-time)))
|#
(declaim (ftype (function ((or string pathname)) t) ensure-directory-pathname))
(declaim (ftype (function ((or string pathname)) t) ensure-directory-pathname))
(declaim (ftype (function ((or string pathname file-stream)) pathname)
		pathname-directory-pathname))


;;; Customizable System Parameters
;;; ------------------------------

#+pre-cleanup
(defvar *dont-redefine-require*
  #+cmu (if (find-symbol "*MODULE-PROVIDER-FUNCTIONS*" "EXT") t nil)
  #+(or clisp sbcl) t
  #+allegro t
  #+clozure-common-lisp t
  #+ecl t
  #+lispworks t
  #-(or cmu sbcl clisp allegro clozure-common-lisp) nil
  "If T, prevents the redefinition of REQUIRE.

This is useful for lisps that treat REQUIRE specially in the compiler.")


(defvar *dont-redefine-require* t
  "If T, prevents the redefinition of REQUIRE.

This is useful for lisps that treat REQUIRE specially in the compiler.

Notes:

This should always be T after 2024.")


(defvar *multiple-lisp-support* t
  "If T, afs-binary-directory will try to return a name dependent
on the particular lisp compiler version being used.")


;;; home-subdirectory --
;;;
;;; HOME-SUBDIRECTORY is used only in *central-registry* below.
;;; Note that CMU CL 17e does not understand the ~/ shorthand for home
;;; directories.
;;;
;;; Notes:
;;; 20020220 Marco Antoniotti
;;; The #-cormanlisp version is the original one, which is broken anyway, since
;;; it is UNIX dependent.
;;; I added the kludgy #+cormalisp (v 1.5) one, since it is missing
;;; the ANSI USER-HOMEDIR-PATHNAME function.

#-cormanlisp
(defun home-subdirectory (directory)
  (concatenate 'string
	#+(or :sbcl :cmu :scl)
	"home:"
	#-(or :sbcl :cmu :scl)
	(let ((homedir (user-homedir-pathname)))
	  (or (and homedir (namestring homedir))
	      "~/"))
	directory))


#+cormanlisp
(defun home-subdirectory (directory)
  (declare (type string directory))
  (concatenate 'string "C:\\" directory))


;;; Central Registry
;;; ----------------

;;; *central-registry* --
;;;
;;; Change this variable to set up the location of a central
;;; repository for system definitions if you want one.
;;; This is a defvar to allow users to change the value in their
;;; lisp init files without worrying about it reverting if they
;;; reload defsystem for some reason.

;;; Note that if a form is included in the registry list, it will be evaluated
;;; in COMPUTE-SYSTEM-PATH to return the appropriate directory to check.

(defvar *central-registry*
  `(;; Current directory
    "./"
    #+:LUCID     (working-directory)
    #+ACLPC      (current-directory)
    #+:allegro   (excl:current-directory)
    #+:clisp     (ext:default-directory)
    #+:sbcl      (progn *default-pathname-defaults*)
    #+(or :cmu :scl)       (ext:default-directory)
    ;; *** Marco Antoniotti <marcoxa@icsi.berkeley.edu>
    ;; Somehow it is better to qualify default-directory in CMU with
    ;; the appropriate package (i.e. "EXTENSIONS".)
    ;; Same for Allegro.
    #+(and :lispworks
           (not :lispworks4)
           (not :lispworks5)
           (not :lispworks6)
           (not :lispworks7)
           (not :lispworks8)
           )
    ,(multiple-value-bind (major minor)
			  #-:lispworks-personal-edition
			  (system::lispworks-version)
			  #+:lispworks-personal-edition
			  (values system::*major-version-number*
				  system::*minor-version-number*)
       (if (or (> major 3)
	       (and (= major 3) (> minor 2))
	       (and (= major 3) (= minor 2)
		    (equal (lisp-implementation-version) "3.2.1")))
	   `(make-pathname :directory
			   ,(find-symbol "*CURRENT-WORKING-DIRECTORY*"
					 (find-package "SYSTEM")))
           (find-symbol "*CURRENT-WORKING-DIRECTORY*"
                        (find-package "LW"))))
    
    #+(or :lispworks4 :lispworks5 :lispworks6 :lispworks7 :lispworks8)
    ,(hcl:get-working-directory)
    
    ;; Home directory
    #-sbcl
    ,(mk::home-subdirectory "lisp/systems/")

    ;; Global registry
    #+unix
    ,@(and (probe-file (pathname "/usr/local/lisp/mkdefsystem/Registry/"))
	   (list (pathname "/usr/local/lisp/mkdefsystem/Registry/")))
    )
  "Central directory of system definitions.

May be either a single directory pathname, or a list of directory
pathnames to be checked after the local directory.")


(defun add-registry-location (pathname &optional (first-last :first)
                                       &aux (p (pathname pathname)))
  "Adds a path to the central registry.

Addition can be 'first' or 'last'.  Each location appears only once in
the registry."

  ;; Maybe add extra checks based on pathname 'last' folder name.

  (ecase first-last

    ((:first first)
     (pushnew p *central-registry* :test #'equalp))

    ((:last last)
     ;; Slow but hey! KISS.  After all this should not be called that often.
     (if (find p *central-registry* :test #'equalp)
         *central-registry*
         (setf *central-registry*
               (append *central-registry* (list p)))))
    ))


(defun registry-pathname (registry)
  "Return the pathname represented by the element of *CENTRAL-REGISTRY*."
  (typecase registry
    (string (pathname registry))
    (pathname registry)
    (otherwise (pathname (eval registry)))))


(defun print-central-registry-directories (&optional
					     (stream *standard-output*))
  "Print to STREAM each pathname in *CENTRAL-REGISTRY* in search order."

  (format stream "~%MK: CENTRAL-REGISTRY locations in search order:~%")
  (dolist (registry *central-registry* (terpri stream))
    (print (registry-pathname registry) stream)))


(defun list-central-registry-directories ()
  "Returns a list of the pathnames in *CENTRAL-REGISTRY* in search order."
  (mapcar #'registry-pathname *central-registry*))


(defvar *bin-subdir* ".bin/"
  "The subdirectory of an AFS directory where the binaries are really kept.")


;;; MK:DEFSYSTEM operation variables
;;; --------------------------------

;;; These variables set up defaults for operate-on-system, and are used
;;; for communication in lieu of parameter passing. Yes, this is bad,
;;; but it keeps the interface small. Also, in the case of the -if-no-binary
;;; variables, parameter passing would require multiple value returns
;;; from some functions. Why make life complicated?

(defvar *tell-user-when-done* nil
  "If T, system will print ...DONE at the end of an operation")

(defvar *oos-verbose* nil
  "Operate on System Verbose Mode")

(defvar *oos-test* nil
  "Operate on System Test Mode")

(defvar *defsystem-oos-debug* nil
  "Defsystem operations and Operate on System Debug Mode")

(defvar *load-source-if-no-binary* nil
  "If T, system will try loading the source if the binary is missing")

(defvar *bother-user-if-no-binary* t
  "If T, the system will ask the user whether to load the source if
   the binary is missing")

(defvar *load-source-instead-of-binary* nil
  "If T, the system will load the source file instead of the binary.")

(defvar *compile-during-load* :query
  "If T, the system will compile source files during load if the
binary file is missing. If :query, it will ask the user for
permission first.")


(defvar *compile-file-verbose* nil
  "Controls, in some CLs how verbose should be COMPILE-FILE.")

(defvar *minimal-load* nil
  "If T, the system tries to avoid reloading files that were already loaded
and up to date.")

(defvar *files-missing-is-an-error* t
  "If both the source and binary files are missing, signal a continuable
   error instead of just a warning.")

(defvar *operations-propagate-to-subsystems* t
  "If T, operations like :COMPILE and :LOAD propagate to subsystems
   of a system that are defined either using a component-type of :system
   or by another defsystem form.")

(defvar *operations-propagate-to-systems-depending-on* t
  "If T, operations like :COMPILE and :LOAD propagate to systems
   a system depends on.  Operations like :DELETE-BINARIES set this to NIL.")

;;; Particular to CMULisp

(defvar *compile-error-file-type* "err"
  "File type of compilation error file in cmulisp")

(defvar *cmu-errors-to-terminal* t
  "Argument to :errors-to-terminal in compile-file in cmulisp")

(defvar *cmu-errors-to-file* t
  "If T, cmulisp will write an error file during compilation")


;;; Global Variables
;;; ----------------

;;; Massage people's *features* into better shape.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (dolist (feature *features*)
    (when (and (symbolp feature)   ; 3600
               (equal (symbol-name feature) "CMU"))
      (pushnew :CMU *features*)))

  #+Lucid
  (when (search "IBM RT PC" (machine-type))
    (pushnew :ibm-rt-pc *features*))
  )


;;; *filename-extensions*
;;; A cons of the source and binary extensions for various CL
;;; implementations.

#+pre-ansi
(defvar *filename-extensions*
  (car `(#+(and Symbolics Lispm)              ("lisp" . "bin")
         #+(and dec common vax (not ultrix))  ("LSP"  . "FAS")
         #+(and dec common vax ultrix)        ("lsp"  . "fas")
 	 #+ACLPC                              ("lsp"  . "fsl")
 	 #+CLISP                              ("lisp" . "fas")
         #+KCL                                ("lsp"  . "o")
         ;; #+ECL                                ("lsp"  . "so")
         #+IBCL                               ("lsp"  . "o")
         #+Xerox                              ("lisp" . "dfasl")
	 ;; Lucid on Silicon Graphics
	 #+(and Lucid MIPS)                   ("lisp" . "mbin")
	 ;; the entry for (and lucid hp300) must precede
	 ;; that of (and lucid mc68000) for hp9000/300's running lucid,
	 ;; since *features* on hp9000/300's also include the :mc68000
	 ;; feature.
	 #+(and lucid hp300)                  ("lisp" . "6bin")
         #+(and Lucid MC68000)                ("lisp" . "lbin")
         #+(and Lucid Vax)                    ("lisp" . "vbin")
         #+(and Lucid Prime)                  ("lisp" . "pbin")
         #+(and Lucid SUNRise)                ("lisp" . "sbin")
         #+(and Lucid SPARC)                  ("lisp" . "sbin")
         #+(and Lucid :IBM-RT-PC)             ("lisp" . "bbin")
	 ;; PA is Precision Architecture, HP's 9000/800 RISC cpu
	 #+(and Lucid PA)		      ("lisp" . "hbin")
         #+excl ("cl"   . ,(pathname-type (compile-file-pathname "foo.cl")))
         #+(or :cmu :scl)  ("lisp" . ,(or (c:backend-fasl-file-type c:*backend*) "fasl"))
	 ;; #+(and :CMU (not (or :sgi :sparc)))  ("lisp" . "fasl")
	 ;; #+(and :CMU :sgi)                    ("lisp" . "sgif")
	 ;; #+(and :CMU :sparc)                  ("lisp" . "sparcf")
	 #+PRIME                              ("lisp" . "pbin")
         #+HP                                 ("l"    . "b")
         #+TI ("lisp" . #.(string (si::local-binary-file-type)))
         #+:gclisp                            ("LSP"  . "F2S")
         #+pyramid                            ("clisp" . "o")

	 ;; Harlequin LispWorks
	 #+:lispworks 	      ("lisp" . ,COMPILER:*FASL-EXTENSION-STRING*)
	 ;; #+(and :sun4 :lispworks)             ("lisp" . "wfasl")
	 ;; #+(and :mips :lispworks)             ("lisp" . "mfasl")
         #+:mcl		("lisp" . ,(pathname-type (compile-file-pathname "foo.lisp")))
         #+:coral	("lisp" . "fasl")

         ;; Otherwise,
         ("lisp" . ,(pathname-type (compile-file-pathname "foo.lisp")))))
  "Filename extensions for Common Lisp.

A cons of the form (Source-Extension . Binary-Extension). If the
system is unknown (as in *features* not known), defaults to lisp and
fasl.

Notes:

Most of the functionality of this variable is superseded by COMPILE-FILE-PATHANME.
")


(defvar *filename-extensions*
  #-:allegro
  (cons "lisp" (pathname-type (compile-file-pathname "foo.lisp")))
  
  #+:allegro
  (cons "cl" (pathname-type (compile-file-pathname "foo.lisp")))
  "Filename extensions for Common Lisp.

A cons of the form (Source-Extension . Binary-Extension). If the
system is unknown (as in *features* not known), defaults to lisp and
fasl.

Notes:

Most of the functionality of this variable is superseded by
COMPILE-FILE-PATHANME.
")


(defun filename-extensions ()
  "Return a cons with the default source and compiled extensions."
  *filename-extensions*)


(defun set-default-source-extension (source-extension)
  "Set the default source extension.

Useful for initialization purposes."
  
  (setq *filename-extensions*
	(cons source-extension
	      (pathname-type (compile-file-pathname "foo.lisp")))))
	      

(defvar *system-extension*
  ;; MS-DOS systems can only handle three character extensions.
  #-ACLPC "system"
  #+ACLPC "sys"

  "The filename extension to use with systems.

Notes:

This variable may not be relevant anymore, in the time of Windows 11.")


;;; The above variables and code should be extended to allow a list of
;;; valid extensions for each lisp implementation, instead of a single
;;; extension. When writing a file, the first extension should be used.
;;; But when searching for a file, every extension in the list should
;;; be used. For example, CMU Common Lisp recognizes "lisp" "l" "cl" and
;;; "lsp" (*load-source-types*) as source code extensions, and
;;; (c:backend-fasl-file-type c:*backend*)
;;; (c:backend-byte-fasl-file-type c:*backend*)
;;; and "fasl" as binary (object) file extensions (*load-object-types*).

;;; Note that the above code is used below in the LANGUAGE defstruct.


;;; *system-dependencies-delayed* --
;;;
;;; There is no real support for this variable being nil, so don't change it.
;;; Note that in any event, the toplevel system (defined with defsystem)
;;; will have its dependencies delayed. Not having dependencies delayed
;;; might be useful if we define several systems within one defsystem.

(defvar *system-dependencies-delayed* t
  "If T, system dependencies are expanded at run time")


;;; Utilities
;;; ---------

;;; Replace this with consp, dammit!

(defun non-empty-listp (list)
  (and list (listp list)))


(defun split-string (string &key (item #\space) (test #'char=))
  ;; Splits the string into substrings at spaces.
  (let ((len (length string))
	(index 0) result)
    (dotimes (i len
		(progn (unless (= index len)
			 (push (subseq string index) result))
		       (reverse result)))
      (when (funcall test (char string i) item)
	(unless (= index i);; two spaces in a row
	  (push (subseq string index i) result))
	(setf index (1+ i))))))


(defun null-string (s)
  (and (stringp s) (string= s "")))


(defun ensure-trailing-slash (dir)
  (if (and dir
           (stringp dir)
	   (not (null-string dir))
           (not (member (char dir (1- (length dir)))
                        '(#\/ #\: #\\)
                        :test #'char=))
	   )
      (concatenate 'string
                   dir
                   "/" ; Not very smart.
                   )
      dir))


;;; Useful Conditions
;;; -----------------

(define-condition missing-component (error) #|(simple-condition)|#
  ((name :reader missing-component-name
         :initarg :name)
   (component :reader missing-component-component
              :initarg :component)
   )
  #-gcl (:default-initargs :component nil)
  (:report (lambda (mmc stream)
	     (format stream "MK: Error: missing component ~S for ~S."
                     (missing-component-name mmc)
                     (missing-component-component mmc))))
  )


(define-condition missing-module (missing-component)
  ()
  (:report (lambda (mmc stream)
	     (format stream "MK: Error: missing module ~S for ~S."
                     (missing-component-name mmc)
                     (missing-component-component mmc))))
  )


(define-condition missing-system (missing-module)
  ()
  (:report (lambda (msc stream)
	     (format stream "MK: Error: missing system ~S~@[ for S~]."
                     (missing-component-name msc)
                     (missing-component-component msc))))
  )


(define-condition missing-foreign-system (missing-system)
  ((kind :reader missing-foreign-system-kind
         :initarg :kind)
   )
  (:default-initargs :kind :asdf)
  (:report (lambda (msc stream)
	     (format stream "MK: Error: missing foreign ~A system ~S~@[ for S~]."
                     (missing-foreign-system-kind msc)
                     (missing-component-name msc)
                     (missing-component-component msc))))
  )


;;; Component Operation Definition
;;; ------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)

(defvar *version-dir* nil
  "The version subdir. bound in operate-on-system.")

(defvar *version-replace* nil
  "The version replace. bound in operate-on-system.")

(defvar *version* nil
  "Default version.")
) ; eval-when


(defvar *mk-output* *standard-output*
  "Default MK:DEFSYSTEM output stream.")


(defvar *component-operations* (make-hash-table :test #'equal)
  "Hash table of (operation-name function) pairs.")


(defun component-operation (name &optional operation)
  (if operation
      (setf (gethash name *component-operations*) operation)
      (gethash name *component-operations*)))


(defun component-operation-noop (component &rest args)
  (declare (ignore args))
  (format *mk-output* "MAKE: NOOP operation on component ~A.~%"
	  component))


(defun set-component-operation (name operation)
  (if operation
      (setf (gethash name *component-operations*) operation)
      (setf (gethash name *component-operations*) 'component-operation-noop)))


(defun (setf component-operation) (operation name)
  (if operation
      (setf (gethash name *component-operations*) operation)
      (setf (gethash name *component-operations*) 'component-operation-noop)))



;;; AFS @sys imitator
;;; -----------------

;;; 2024-06-28 MA: all of this, up to "END AFS",  may probably
;;; disappear without problems.

;;; mc 11-Apr-91: Bashes MCL's point reader, so commented out.

#-:mcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; Define #@"foo" as a shorthand for (afs-binary-directory "foo").
  ;; For example,
  ;;    <cl> #@"foo"
  ;;    "foo/.bin/rt_mach/"

  (defparameter *saved-readtable* *readtable*)
  (defparameter *mk-readtable* (copy-readtable))

  (set-dispatch-macro-character
   #\# #\@
   #'(lambda (stream char arg)
       (declare (ignore char arg))
       `(afs-binary-directory ,(read stream t nil t)))
   *mk-readtable*)
  (setf *readtable* *mk-readtable*)
  )


(defvar *find-irix-version-script*
    "\"1,4 d\\
s/^[^M]*IRIX Execution Environment 1, *[a-zA-Z]* *\\([^ ]*\\)/\\1/p\\
/./,$ d\\
\"")


;;; Runtime queries
;;; ---------------

(defun operating-system-version ()
  #+(and :sgi :excl)
  (let* ((full-version (software-version))
	 (blank-pos (search " " full-version))
	 (os (subseq full-version 0 blank-pos))
	 (version-rest (subseq full-version
			       (1+ blank-pos)))
	 os-version)
    (setq blank-pos (search " " version-rest))
    (setq version-rest (subseq version-rest
			       (1+ blank-pos)))
    (setq blank-pos (search " " version-rest))
    (setq os-version (subseq version-rest 0 blank-pos))
    (setq version-rest (subseq version-rest
			       (1+ blank-pos)))
    (setq blank-pos (search " " version-rest))
    (setq version-rest (subseq version-rest
			       (1+ blank-pos)))
    (concatenate 'string
		 os " " os-version))	; " " version-rest
  
  #+(and :sgi :cmu :sbcl)
  (concatenate 'string
    (software-type)
    (software-version))
  
  #+(and :lispworks :irix)
  (let ((soft-type (software-type)))
    (if (equalp soft-type "IRIX5")
        (progn
          (system:call-system
	   (format nil "versions ~A | sed -e ~A > ~A"
                   "eoe1"
                   *find-irix-version-script*
                   "irix-version")
	   "/bin/csh")
          (with-open-file (s "irix-version")
            (format nil "IRIX ~S"
		    (read s))))
	soft-type))
  
  #-(or (and :excl :sgi) (and :cmu :sgi) (and :lispworks :irix))
  (software-type))


(defun compiler-version ()
  #+:lispworks
  (concatenate 'string
	       "Lispworks" " " (lisp-implementation-version))
  #+excl
  (concatenate 'string
	       "Allegro" " " excl::*common-lisp-version-number*)
  #+sbcl
  (concatenate 'string
	       "SBCL" " " (lisp-implementation-version))
  #+cmu
  (concatenate 'string
	       "CMUCL" " " (lisp-implementation-version))
  #+scl
  (concatenate 'string
	       "SCL" " " (lisp-implementation-version))

  #+kcl       "KCL"
  #+IBCL      "IBCL"
  #+akcl      "AKCL"
  #+gcl       "GCL"
  #+ecl       "ECL"
  #+lucid     "Lucid"
  #+ACLPC     "ACLPC"
  #+CLISP     "CLISP"
  #+Xerox     "XEROX"
  #+symbolics "Symbolics"
  #+mcl       "MCL"
  #+ccl       "CCL"
  #+coral     "Coral"
  #+gclisp    "Gclisp"
  #+(or abcl armedbear)      "ABCL"
  )


(defvar *compiler-type-hash-table* (make-hash-table :test #'equal)
  "Hash table for retrieving the Common Lisp type")


(defun compiler-type-translation (name &optional translation)
  (if name
      (setf (gethash (string-upcase name) *compiler-type-hash-table*) translation)
      (gethash (string-upcase name) *compiler-type-hash-table*)))


(compiler-type-translation "lispworks 3.2.1"         "lispworks")
(compiler-type-translation "lispworks 3.2.60 beta 6" "lispworks")
(compiler-type-translation "lispworks 4.2.0"         "lispworks")
(compiler-type-translation "lispworks 8.0.0"         "lispworks")
(compiler-type-translation "lispworks 8.0.1"         "lispworks")
(compiler-type-translation "lispworks 8.1.1"         "lispworks")
(compiler-type-translation "lispworks 8.1.2"         "lispworks")


#+allegro
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (or (find :case-sensitive common-lisp:*features*)
	      (find :case-insensitive common-lisp:*features*))
    (if (or (eq excl:*current-case-mode* :case-sensitive-lower)
	    (eq excl:*current-case-mode* :case-sensitive-upper))
	(push :case-sensitive common-lisp:*features*)
      (push :case-insensitive common-lisp:*features*))))


#+(and allegro case-sensitive ics)
(compiler-type-translation "excl 6.1" "excl-m")
#+(and allegro case-sensitive (not ics))
(compiler-type-translation "excl 6.1" "excl-m8")

#+(and allegro case-insensitive ics)
(compiler-type-translation "excl 6.1" "excl-a")
#+(and allegro case-insensitive (not ics))
(compiler-type-translation "excl 6.1" "excl-a8")

(compiler-type-translation "excl 4.2" "excl")
(compiler-type-translation "excl 4.1" "excl")
(compiler-type-translation "cmu 17f" "cmu")
(compiler-type-translation "cmu 17e" "cmu")
(compiler-type-translation "cmu 17d" "cmu")
(compiler-type-translation "cmu 21e" "cmu")


(defun afs-component (machine software &optional lisp)
  (format nil "~@[~A~]~@[_~A~]~@[_~A~]"
	    machine
	    (or software "mach")
	    lisp))


(defvar *machine-type-alist* (make-hash-table :test #'equal)
  "Hash table for retrieving the machine-type")


(defun machine-type-translation (name &optional operation)
  (if operation
      (setf (gethash (string-upcase name) *machine-type-alist*) operation)
      (gethash (string-upcase name) *machine-type-alist*)))


(machine-type-translation "IBM RT PC"                        "rt")
(machine-type-translation "DEC 3100"                         "pmax")
(machine-type-translation "DEC VAX-11"                       "vax")
(machine-type-translation "DECstation"                       "pmax")
(machine-type-translation "Sun3"                             "sun3")
(machine-type-translation "Sun-4"                            "sun4")
(machine-type-translation "MIPS Risc"                        "mips")
(machine-type-translation "SGI"                              "sgi")
(machine-type-translation "Silicon Graphics Iris 4D"         "sgi")
(machine-type-translation "Silicon Graphics Iris 4D (R3000)" "sgi")
(machine-type-translation "Silicon Graphics Iris 4D (R4000)" "sgi")
(machine-type-translation "Silicon Graphics Iris 4D (R4400)" "sgi")
(machine-type-translation "IP22"                             "sgi")
;;; MIPS R4000 Processor Chip Revision: 3.0
;;; MIPS R4400 Processor Chip Revision: 5.0
;;; MIPS R4600 Processor Chip Revision: 1.0
(machine-type-translation "IP20"                             "sgi")
;;; MIPS R4000 Processor Chip Revision: 3.0
(machine-type-translation "IP17"                             "sgi")
;;; MIPS R4000 Processor Chip Revision: 2.2
(machine-type-translation "IP12"                             "sgi")
;;; MIPS R2000A/R3000 Processor Chip Revision: 3.0
(machine-type-translation "IP7"                              "sgi")
;;; MIPS R2000A/R3000 Processor Chip Revision: 3.0

(machine-type-translation "x86"                              "x86")
;;; ACL
(machine-type-translation "IBM PC Compatible"                "x86")
;;; LW
(machine-type-translation "I686"                             "x86")
;;; LW
(machine-type-translation "PC/386"                           "x86")
;;; CLisp Win32

#+(and :lucid :sun :mc68000)
(machine-type-translation "unknown"     "sun3")


(defvar *software-type-alist* (make-hash-table :test #'equal)
  "Hash table for retrieving the software-type")


(defun software-type-translation (name &optional operation)
  (if operation
      (setf (gethash (string-upcase name) *software-type-alist*) operation)
      (gethash (string-upcase name) *software-type-alist*)))


(software-type-translation "BSD UNIX"      "mach") ; "unix"
(software-type-translation "Ultrix"        "mach") ; "ultrix"
(software-type-translation "SunOS"         "SunOS")
(software-type-translation "MACH/4.3BSD"   "mach")
(software-type-translation "IRIX System V" "irix") ; (software-type)
(software-type-translation "IRIX5"         "irix5")
;;(software-type-translation "IRIX liasg5 5.2 02282016 IP22 mips" "irix5") ; (software-version)

(software-type-translation "IRIX 5.2" "irix5")
(software-type-translation "IRIX 5.3" "irix5")
(software-type-translation "IRIX5.2"  "irix5")
(software-type-translation "IRIX5.3"  "irix5")

(software-type-translation "Linux" "linux") ; Lispworks for Linux
(software-type-translation "Linux 2.x, Redhat 6.x and 7.x" "linux") ; ACL
(software-type-translation "Microsoft Windows 9x/Me and NT/2000/XP" "win32")
(software-type-translation "Windows NT" "win32") ; LW for Windows
(software-type-translation "ANSI C program" "ansi-c") ; CLISP
(software-type-translation "C compiler" "ansi-c") ; CLISP for Win32

(software-type-translation nil             "")

#+:lucid
(software-type-translation "Unix"
			   #+:lcl4.0 "4.0"
			   #+(and :lcl3.0 (not :lcl4.0)) "3.0")



;;; *empty-pathname*
;;; *empty-relative-pathname*
;;;
;;; Useful "placeholders"

(defvar *empty-pathname*
  (make-pathname :host nil
		 :device nil
		 :directory nil
		 :name nil
		 :type nil
		 :version nil))


(defvar *empty-relative-pathname*
  (make-pathname :host nil
		 :device nil
		 :directory (list :relative)
		 :name nil
		 :type nil
		 :version nil))


;;; Versions (as subdirectories).
;;; Also code for old AFS file systems.
;;;
;;; Versions are essentially subdirectories, 

;;; #-lispworks
#+nil
(defun translate-version (version)
  "Return a 'version directory' and a indicator about how to use it.

Values returned are the version directory and whether (1) it replaces
the entire root (t), or (2) is a subdirectory.

Version may be nil to signify no subdirectory, a symbol, such as
'alpha', 'beta', 'omega', ':alpha', 'mark', which specifies a
subdirectory of the root, or a string, which replaces the root."

  (cond ((null version)
	 (values "" nil))
	
	((symbolp version)
	 (values (let ((sversion (string version)))
		   (if (find-if #'lower-case-p sversion)
		       sversion
		       (string-downcase sversion)))
		 nil))
	
	((stringp version)
	 (values version t))
	
	(t
	 (tell-user-error "Illegal version ~S." version))))


;;; 20211018 MA: it looks like this LW bug went away, but let's not
;;; touch this code.
;;;
;;; Looks like LW has a bug in MERGE-PATHNAMES.
;;;
;;;  (merge-pathnames "" "LP:foo;bar;") ==> "LP:"
;;;
;;; Which is incorrect.
;;; The change here ensures that the result of TRANSLATE-VERSION is
;;; appropriate.


;;; #+lispworks
(defun translate-version (version)
  "Return a 'version directory' and a indicator about how to use it.

Values returned are the version directory and whether (1) it replaces
the entire root (t), or (2) is a subdirectory.

Version may be nil to signify no subdirectory, a symbol, such as
'alpha', 'beta', 'omega', ':alpha', 'mark', which specifies a
subdirectory of the root, or a string, which replaces the root."

  (cond ((null version)
	 ;; (values (pathname "") nil)
         (values *empty-relative-pathname* nil))
	
	((symbolp version)
	 (values (let ((sversion (string version)))
		   (if (find-if #'lower-case-p sversion)
		       (ensure-directory-pathname (pathname sversion))
                       (ensure-directory-pathname (pathname (string-downcase sversion)))))
		 nil))
	
	((stringp version)
	 (values (ensure-directory-pathname (pathname version)) t))
	
	(t
	 (tell-user-error "Illegal version ~S." version))))


(defun afs-binary-directory (root-directory)
  ;; Function for obtaining the directory AFS's @sys feature would have
  ;; chosen when we're not in AFS. This function is useful as the argument
  ;; to :binary-pathname in defsystem. For example,
  ;; :binary-pathname (afs-binary-directory "scanner/")
  (let ((machine (machine-type-translation
		  #-(and :sgi :allegro-version>= (version>= 4 2))
		  (machine-type)
		  #+(and :sgi :allegro-version>= (version>= 4 2))
		  (machine-version)))
	(software (software-type-translation
		   #-(and :sgi (or :cmu :sbcl :scl
				   (and :allegro-version>= (version>= 4 2))))
		   (software-type)
		   #+(and :sgi (or :cmu :sbcl :scl
				   (and :allegro-version>= (version>= 4 2))))
		   (operating-system-version)))
	(lisp (compiler-type-translation (compiler-version)))
	)
    ;; pmax_mach rt_mach sun3_35 sun3_mach vax_mach
    (setq root-directory (namestring root-directory))
    (setq root-directory (ensure-trailing-slash root-directory))
    (format nil "~A~@[~A~]~@[~A/~]"
	    root-directory
	    *bin-subdir*
	    (if *multiple-lisp-support*
		(afs-component machine software lisp)
	      (afs-component machine software)))))


(defun afs-source-directory (root-directory &optional version-flag)
  ;; Function for obtaining the directory AFS's @sys feature would have
  ;; chosen when we're not in AFS. This function is useful as the argument
  ;; to :source-pathname in defsystem.
  (setq root-directory (namestring root-directory))
  (setq root-directory (ensure-trailing-slash root-directory))
  (format nil "~A~@[~A/~]"
          root-directory
          (and version-flag (translate-version *version*))))

;;; END AFS


;;; System Names
;;; ------------

;;; If you use strings for system names, be sure to use the same case
;;; as it appears on disk, if the filesystem is case sensitive.

;;; Notes:
;;; 20211012 MA: We can have EQUALP hash tables that work with case
;;; insensitive string keys.  CANONICALIZE-SYSTEM-NAME can therefore
;;; just 'stringify' its argument.

(defvar *defined-systems* (make-hash-table :test #'equalp)
  "Hash table containing the definitions of all known systems.")


(defun canonicalize-system-name (name)
  (declare (type (or string symbol) name))
  (if (stringp name) name (string name)))


(defun get-system (name)
  "Returns the definition of the system named NAME."
  (gethash (canonicalize-system-name name) *defined-systems*))


(defsetf get-system (name) (value)
  `(setf (gethash (canonicalize-system-name ,name) *defined-systems*) ,value))


(defun undefsystem (name)
  "Removes the definition of the system named NAME."
  (remhash (canonicalize-system-name name) *defined-systems*))


(defun defined-systems ()
  "Returns a list of defined systems."
  (let ((result nil))
    (maphash #'(lambda (key value)
		 (declare (ignore key))
		 (push value result))
	     *defined-systems*)
    result))


(defun defined-names-and-systems ()
  "Returns a a-list of defined systems along with their names."
  (loop for sname being the hash-keys of *defined-systems*
        using (hash-value s)
        collect (cons sname s)))


;;; Directory Pathname Hacking
;;; --------------------------

;;; Several file system related definitions here.  Especially
;;; directory appending.

;;; Unix example: An absolute directory starts with / while a
;;; relative directory doesn't. A directory ends with /, while
;;; a file's pathname doesn't. This is important 'cause
;;; (pathname-directory "foo/bar") will return "foo" and not "foo/".

;;; I haven't been able to test the fix to the problem with symbolics
;;; hosts. Essentially, append-directories seems to have been tacking
;;; the default host onto the front of the pathname (e.g., mk::source-pathname
;;; gets a "B:" on front) and this overrides the :host specified in the
;;; component. The value of :host should override that specified in
;;; the :source-pathname and the default file server. If this doesn't
;;; fix things, specifying the host in the root pathname "F:>root-dir>"
;;; may be a good workaround.

;;; Need to verify that merging of pathnames where modules are located
;;; on different devices (in VMS-based VAXLisp) now works.

;;; Merge-pathnames works for VMS systems. In VMS systems, the directory
;;; part is enclosed in square brackets, e.g.,
;;; 	"[root.child.child_child]" or "[root.][child.][child_child]"
;;; To concatenate directories merge-pathnames works as follows:
;;; 	(merge-pathnames "" "[root]")               ==> "[root]"
;;; 	(merge-pathnames "[root.]" "[son]file.ext") ==> "[root.son]file.ext"
;;; 	(merge-pathnames "[root.]file.ext" "[son]") ==> "[root.son]file.ext"
;;; 	(merge-pathnames "[root]file.ext" "[son]")  ==> "[root]file.ext"
;;; Thus the problem with the #-VMS code was that it was merging x y into
;;; [[x]][y] instead of [x][y] or [x]y.

;;; Miscellaneous notes:
;;;   On GCLisp, the following are equivalent:
;;;       "\\root\\subdir\\BAZ"
;;;       "/root/subdir/BAZ"
;;;   On VAXLisp, the following are equivalent:
;;;       "[root.subdir]BAZ"
;;;       "[root.][subdir]BAZ"
;;; Use #+:vaxlisp for VAXLisp 3.0, #+(and vms dec common vax) for v2.2

;;; 2024-06-30 MA
;;; New code replacing NEW-APPEND-DIRECTORIES with
;;; STITCH-NAMES-AS-DIRECTORIES.


;;; pathname-designator

(deftype pathname-designator ()
  '(or string pathname file-stream #| stream |#)
  ;; FILE-STREAM may be too strict, and STREAM is surely too wide.
  )


;;; cwd, current-working-directory
;;;
;;; Current working directory.
;;; A portable function for known common lisps.

(defun current-working-directory ()
  #+:LUCID     (pathname (working-directory)) ; Still working?
  #+ACLPC      (pathname (current-directory)) ; Still working?
  #+:allegro   (excl:current-directory)
  #+:clisp     (ext:default-directory)
  #+:sbcl      *default-pathname-defaults* ; There are other possibilities.
  #+(or :cmu :scl)       (ext:default-directory)
  #+lispworks  (hcl:get-working-directory) ; Assuming from LW4 up.
  #+abcl       *default-pathname-defaults*
  #+ecl        *default-pathname-defaults*
  )

(defun cwd () (current-working-directory))


;;; pathname-as-directory

(defun pathname-as-directory (p)
  "Returns a pathname that is a 'directory' pathname.

UN*X and Windows people are used to treat pathnames at the string
(NAMESTRING) level.  This creates problems when people do not realize
that \"foo/bar.baz\" has a 'name' of \"bar\", a 'type' of \"baz\" and
a 'directory' of `(:RELATIVE \"foo\")`, which then breaks expectations
in a variety of cases.

This function takes a pathname P and ensures its transformation into a
'directory' pathname with :NAME and :TYPE set to NIL.  The example
above will return a pathname with :DIRECTORY
`(:RELATIVE \"foo\" \"bar.baz\")`; note how the last directory
component name is constructed.

Notes:

The example above may wreck havoc on a different file systems like VMS."
  
  (declare (type pathname p))

  (flet ((quote-filename-as-dirname (nd)
	   (declare (type pathname nd))
	   
	   #+(or :unix :linux :windows :mswindows :win32 :msdos :dos)
	   nd
	   #+(or :vms)			; Ok: we may not have VMS
					; laying around, and yet...
	   (concatenate 'string
			(pathname-name nd)
			"^."		; Quote the dot.
			(pathname-type nd))
	   #-(or :vms :unix :linux :windows :mswindows :win32 :msdos :dos)
	   (progn			; Let'pray, but
					; warn...
	     (warn "MK: using ~S as a directory name on a rare platform."
		   nd)
	     nd)
	   )
	 )
    (let* ((pd (pathname-directory p))
	   (pn (pathname-name p))
	   (pt (pathname-type p))
	   (nd (cond ((and pn pt)
		      (namestring
		       (quote-filename-as-dirname
			(make-pathname
			 :name pn
			 :type pt
			 :defaults *empty-pathname*))))
		     (pn pn)
		     (pt
		      (namestring
		       (quote-filename-as-dirname
			(make-pathname
			 :type pt
			 :defaults *empty-pathname*))))
		     (t ())
		     ))
	   (dir (cond ((and pd nd) (append pd (list nd)))
		      (pd pd)
		      (nd (list :relative nd))
		      (t nil)))
	   )
      (make-pathname
       :directory dir
       :name nil
       :type nil
       :defaults p))
    ))


;;; relativize-pathname
;;; Transform a pathname into a relative one (if it has a non null
;;; directory component)

(defun relativize-pathname (p)
  (declare (type pathname p))

  (let ((pd (pathname-directory p)))
    (cond ((null pd) (merge-pathnames *empty-pathname* p))
	  ((eq :relative (first pd))
	   (merge-pathnames *empty-pathname* p)) ; Copy the pathname.
	  ((eq :absolute (first pd))
	   (make-pathname :directory (list* :relative (rest pd))
			  :defaults p)
	   )
	  (t (error "MK: cannot handle pathname directory ~S."
		    pd))
	  )
    ))


;;; stitch-names-as-directories

(defun stitch-names-as-directories (d1 d2)
  ;; Version of NEW-APPEND-DIRECTORIES which should work in 2024 for
  ;; "modern" CLs.

  (declare (type (or null pathname-designator) d1 d2))

  (flet ((normalize-name (n)
	   (etypecase n
	     (null (pathname ""))
	     (string (pathname n))
	     (pathname n)
	     (file-stream (pathname n))
	     ))
	 )
    (let* ((nd1 (normalize-name d1))
	   (nd2 (normalize-name d2))
	   (nd1-as-dir (pathname-as-directory nd1))
	   (nd2-as-relative (relativize-pathname nd2))
	   )
      (declare (type pathname nd1 nd2 nd1-as-dir nd2-as-relative))
      (merge-pathnames nd2-as-relative nd1-as-dir)
      )
    ))


;;; Pre 2024-06-30 code.

(defun directory-to-list (directory)
  ;; The directory should be a list, but nonstandard implementations have
  ;; been known to use a vector or even a string.
  (cond ((listp directory)
	 directory)
	
	((stringp directory)
	 (cond ((find #\; directory)
		;; It's probably a logical pathname, so split at the
		;; semicolons:
		(split-string directory :item #\;))
	       
               #+MCL
	       ((and (find #\: directory)
		     (not (find #\/ directory)))
		;; It's probably a MCL pathname, so split at the colons.
		(split-string directory :item #\:))
	       
	       (t
		;; It's probably a unix pathname, so split at the slash.
		(split-string directory :item #\/))))
	
	(t
	 (coerce directory 'list))))


;;; append-logical-pnames

(defun append-logical-pnames (absolute relative)
  (declare (type (or null string pathname) absolute relative))
  (let ((abs (if absolute
                 (pathname absolute)
                 (make-pathname :directory (list :absolute)
                                :name nil
                                :type nil)
                 ))
	(rel (if relative
                 (pathname relative)
                 (make-pathname :directory (list :relative)
                                :name nil
                                :type nil)
                 ))
	)
    ;; The following is messed up because CMUCL and LW use different
    ;; defaults for host (in particular LW uses NIL).  Thus
    ;; MERGE-PATHNAMES has legitimate different behaviors on both
    ;; implementations. Of course this is disgusting, but that is the
    ;; way it is and the rest tries to circumvent this crap.

    (etypecase abs
      (logical-pathname
       (etypecase rel
	 (logical-pathname
	  (namestring (merge-pathnames rel abs)))
	 (pathname
	  ;; The following potentially translates the logical pathname
	  ;; very early, but we cannot avoid it.
	  (namestring (merge-pathnames rel (translate-logical-pathname abs))))
	 ))
      (pathname
       (namestring (merge-pathnames rel abs)))
      )))


;;; This affects only one thing.
;;; 19990707 Marco Antoniotti
;;; old version

(defun namestring-probably-logical (namestring)
  (and (stringp namestring)
       ;; unix pathnames don't have embedded semicolons
       (find #\; namestring)))

#||
;;; New version
(defun namestring-probably-logical (namestring)
  (and (stringp namestring)
       (typep (parse-namestring namestring) 'logical-pathname)))


;;; New new version
;;; 20000321 Marco Antoniotti
(defun namestring-probably-logical (namestring)
  (pathname-logical-p namestring))
||#


(defun append-directories (absolute-directory relative-directory)
  "Append RELATIVE-DIRECTORY to ABSOLUTE-DIRECTORY.

There is no CL primitive for tacking a subdirectory onto a directory.
We need such a function because defsystem has both absolute and
relative pathnames in the modules. This is a somewhat ugly hack which
seems to work most of the time. We assume that ABSOLUTE-DIRECTORY
is a directory, with no filename stuck on the end. Relative-directory,
however, may have a filename stuck on the end."

  (when (or absolute-directory relative-directory)
    (cond
      ;; KMR commented out because: when appending two logical pathnames,
      ;; using this code translates the first logical pathname then appends
      ;; the second logical pathname -- an error.

      ((namestring-probably-logical absolute-directory)
       ;; A simplistic stab at handling logical pathnames
       (append-logical-pnames absolute-directory relative-directory))

      (t
       ;; In VMS, merge-pathnames actually does what we want!!!
       #+:VMS
       (namestring (merge-pathnames (or absolute-directory "")
				    (or relative-directory "")))
       #+:macl1.3.2
       (namestring (make-pathname :directory absolute-directory
				  :name relative-directory))
       ;; Cross your fingers and pray.
       #-(or :VMS :macl1.3.2)
       ;; (new-append-directories absolute-directory relative-directory)
       (stitch-names-as-directories absolute-directory relative-directory)
       ))))


#+:logical-pathnames-mk
(defun append-logical-directories-mk (absolute-dir relative-dir)
  (lp:append-logical-directories absolute-dir relative-dir))


;;; append-logical-pathnames-mk --
;;; The following is probably still bogus and it does not solve the
;;; problem of appending two logical pathnames.
;;; Anyway, as per suggetsion by KMR, the function is not called
;;; anymore.
;;; Hopefully this will not cause problems for ACL.

#+(and (and allegro-version>= (version>= 4 1))
       (not :logical-pathnames-mk))
(defun append-logical-directories-mk (absolute-dir relative-dir)
  ;; We know absolute-dir and relative-dir are non nil.  Moreover
  ;; absolute-dir is a logical pathname.
  (setq absolute-dir (logical-pathname absolute-dir))
  (etypecase relative-dir
    (string (setq relative-dir (parse-namestring relative-dir)))
    (pathname #| do nothing |#))

  (translate-logical-pathname
   (merge-pathnames relative-dir absolute-dir)))


#| Old version 2002-03-02
#+(and (and allegro-version>= (version>= 4 1))
       (not :logical-pathnames-mk))
(defun append-logical-directories-mk (absolute-dir relative-dir)
  ;; We know absolute-dir and relative-dir are non nil.  Moreover
  ;; absolute-dir is a logical pathname.
  (setq absolute-dir (logical-pathname absolute-dir))
  (etypecase relative-dir
    (string (setq relative-dir (parse-namestring relative-dir)))
    (pathname #| do nothing |#))

  (translate-logical-pathname
   (make-pathname
    :host (or (pathname-host absolute-dir)
	      (pathname-host relative-dir))
    :directory (append (pathname-directory absolute-dir)
		       (cdr (pathname-directory relative-dir)))
    :name (or (pathname-name absolute-dir)
	      (pathname-name relative-dir))
    :type (or (pathname-type absolute-dir)
	      (pathname-type relative-dir))
    :version (or (pathname-version absolute-dir)
		 (pathname-version relative-dir)))))

;; Old version
#+(and (and allegro-version>= (version>= 4 1))
       (not :logical-pathnames-mk))
(defun append-logical-directories-mk (absolute-dir relative-dir)
  (when (or absolute-dir relative-dir)
    (setq absolute-dir (logical-pathname (or absolute-dir ""))
	  relative-dir (logical-pathname (or relative-dir "")))
    (translate-logical-pathname
     (make-pathname
      :host (or (pathname-host absolute-dir)
		(pathname-host relative-dir))
      :directory (append (pathname-directory absolute-dir)
			 (cdr (pathname-directory relative-dir)))
      :name (or (pathname-name absolute-dir)
		(pathname-name relative-dir))
      :type (or (pathname-type absolute-dir)
		(pathname-type relative-dir))
      :version (or (pathname-version absolute-dir)
		   (pathname-version relative-dir))))))
|#

;;; Determines if string or pathname object is logical.

#+:logical-pathnames-mk
(defun logical-pathname-p (thing)
  (eq (lp:pathname-host-type thing) :logical))


;;; From Kevin Layer for 4.1final.

#-:logical-pathnames-mk
(defun logical-pathname-p (thing)
  (typep (parse-namestring thing) 'logical-pathname))


;;; This should work on Allegro of 2024...

#+pointless
(defun pathname-logical-p (thing)
  (typecase thing
    (logical-pathname t)
    #+clisp ; CLisp has non conformant Logical Pathnames.
    (pathname (pathname-logical-p (namestring thing)))
    (string (ignore-errors
	     (not (null (translate-logical-pathname thing)))))
    (t nil))
  )


(defun pathname-logical-p (thing)
  (logical-pathname-p thing))


#||
;;; This was a try at appending a subdirectory onto a directory.
;;; It failed. We're keeping this around to prevent future mistakes
;;; of a similar sort.
(defun merge-directories (absolute-directory relative-directory)
  ;; replace concatenate with something more intelligent
  ;; i.e., concatenation won't work with some directories.
  ;; it should also behave well if the parent directory
  ;; has a filename at the end, or if the relative-directory ain't relative
  (when absolute-directory
    (setq absolute-directory (pathname-directory absolute-directory)))
  (concatenate 'string
	       (or absolute-directory "")
	       (or relative-directory "")))
||#

#||
<cl> (defun d (d n) (namestring (make-pathname :directory d :name n)))

D
<cl> (d "~/foo/" "baz/bar.lisp")
"/usr0/mkant/foo/baz/bar.lisp"

<cl> (d "~/foo" "baz/bar.lisp")
"/usr0/mkant/foo/baz/bar.lisp"

<cl> (d "/foo/bar/" "baz/barf.lisp")
"/foo/bar/baz/barf.lisp"

<cl> (d "foo/bar/" "baz/barf.lisp")
"foo/bar/baz/barf.lisp"

<cl> (d "foo/bar" "baz/barf.lisp")
"foo/bar/baz/barf.lisp"

<cl> (d "foo/bar" "/baz/barf.lisp")
"foo/bar//baz/barf.lisp"

<cl> (d "foo/bar" nil)
"foo/bar/"

<cl> (d nil "baz/barf.lisp")
"baz/barf.lisp"

<cl> (d nil nil)
""

||#

;;; The following is a change proposed by DTC for SCL.
;;; Maybe it could be used all the time.

#-scl
(defun new-file-type (pathname type)
  ;; why not (make-pathname :type type :defaults pathname)?
  (make-pathname
   :host (pathname-host pathname)
   :device (pathname-device pathname)
   :directory (pathname-directory pathname)
   :name (pathname-name pathname)
   :type type
   :version (pathname-version pathname)))


#+scl
(defun new-file-type (pathname type)
  ;; why not (make-pathname :type type :defaults pathname)?
  (make-pathname
   :host (pathname-host pathname :case :common)
   :device (pathname-device pathname :case :common)
   :directory (pathname-directory pathname :case :common)
   :name (pathname-name pathname :case :common)
   :type (string-upcase type)
   :version (pathname-version pathname :case :common)))


;;; Component Structure
;;; -------------------

(defvar *source-pathname-default* nil
  "Default value of :source-pathname keyword in DEFSYSTEM. Set this to
   \"\" to avoid having to type :source-pathname \"\" all the time.")

(defvar *binary-pathname-default* nil
  "Default value of :binary-pathname keyword in DEFSYSTEM.")


(defstruct (topological-sort-node (:conc-name topsort-))
  (color :white :type (member :gray :black :white))
  )


(defparameter *component-evaluated-slots*
  '(:source-root-dir :source-pathname :source-extension
    :binary-root-dir :binary-pathname :binary-extension))


(defparameter *component-form-slots*
  '(:initially-do :finally-do :compile-form :load-form))


(deftype standard-components ()
  '(member :defsystem
           :system
           :subsystem
           :foreign-system
           :module
           :file
           :private-file
           :static-file
           ))


(defstruct (component (:include topological-sort-node)
                      (:print-function print-component))
  (type :file     ; To pacify the CMUCL compiler (:type is alway supplied)
	:type (or standard-components symbol))

  (name nil :type (or symbol string))
  (indent 0 :type (mod 1024))		; Number of characters of indent in
					; verbose output to the user.

  ;; Pathname related slots.

  host					; The pathname host (i.e., "/../a").
  device				; The pathname device.

  (source-pathname *source-pathname-default*)
  source-root-dir			; Relative or absolute (starts
					; with "/"), directory or file
					; (ends with "/").
  source-extension			; A string, e.g., "lisp"
					; if NIL, inherit.

  (binary-pathname *binary-pathname-default*)
  binary-root-dir                       ; Relative or absolute (starts
					; with "/"), directory or file
					; (ends with "/").
  binary-extension			; A string, e.g., "fasl". If
					; NIL, uses default for
					; machine-type.

  ;; Package related slots.

  package				; Package for use-package.

  ;; Component structure slots.

  (components () :type list)		; A list of components
					; comprising this component's
					; definition.
  (depends-on () :type list)		; A list of the components
					; this one depends on. may
					; refer only to the components
					; at the same level as this
					; one.


  ;; Language specific and alternated load/compile operations slots.
  ;;
  ;; The following three slots are used to provide for alternate compilation
  ;; and loading functions for the files contained within a component. If
  ;; a component has a compiler or a loader specified, those functions are
  ;; used. Otherwise the functions are derived from the language. If no
  ;; language is specified, it defaults to Common Lisp (:lisp). Other current
  ;; possible languages include :scheme (PseudoScheme) and :c, but the user
  ;; can define additional language mappings. Compilation functions should
  ;; accept a pathname argument and a :output-file keyword; loading functions
  ;; just a pathname argument. The default functions are #'compile-file and
  ;; #'load. Unlike fdmm's SET-LANGUAGE macro, this allows a defsystem to
  ;; mix languages.

  (language nil :type (or null symbol))
  (compiler nil :type (or null symbol function))
  (loader   nil :type (or null symbol function))

  (compiler-options nil :type list)	; A list of compiler options to
                                        ; use for compiling this
                                        ; component.  These must be
                                        ; keyword options supported by
                                        ; the compiler.

  proclamations				; Compiler options, such as
					; '(optimize (safety 3)).

  (compile-form                         ; For foreign libraries and systems.
   #'(lambda (&optional cn)
      (declare (ignore cn)) nil))

  (load-form                            ; For foreign libraries and systems.
   #'(lambda (&optional cn)
       (declare (ignore cn)) nil))


  ;; Component management slots.

  (initially-do nil)                    ; Form to evaluate before the
					; operation (if not NIL).
  (finally-do nil)                      ; Form to evaluate after the
                                        ; operation (if not NIL).

  ;; If load-only is T, will not compile the file on operation :compile.
  ;; In other words, for files which are :load-only T, loading the file
  ;; satisfies any demand to recompile.

  load-only				; If T, will not compile this
					; file on operation :compile.

  ;; If compile-only is T, will not load the file on operation :compile.
  ;; Either compiles or loads the file, but not both. In other words,
  ;; compiling the file satisfies the demand to load it. This is useful
  ;; for PCL defmethod and defclass definitions, which wrap a
  ;; (eval-when (compile load eval) ...) around the body of the definition.
  ;; This saves time in some lisps.

  compile-only				; If T, will not load this
					; file on operation :compile.
  #|| ISI Extension ||#
  load-always				; If T, will force loading
					; even if file has not
					; changed.

  ;; (load-time 0)                      ; Handled separately.

  ;; PVE: add banner

  (banner nil :type (or null string))


  ;; Meta data slots.

  (documentation nil :type (or null string)) ; Optional documentation slot
  (long-documentation nil :type (or null string)) ; Optional long documentation slot

  ;; Added AUTHOR, MAINTAINER, VERSION and LICENCE slots.

  (author nil :type (or null string))
  (licence nil :type (or null string))
  (maintainer nil :type (or null string))

  ;; The version field must accommodate the pathname related things (otherwise
  ;; SBCL complains)
  
  (version nil :type (or null string integer (member :wild :unspecific :newest)))
  
  ;; (description nil :type (or null string))
  ;; (license nil :type (or null string))

  ;; Added NON-REQUIRED-P slot.  Useful for optional items.

  (non-required-p nil :type boolean)	; If T a missing file or
					; sub-directory will not cause
					; an error.

  (if-feature nil :type (or null keyword cons))
  )

#+old-version
(defun print-component (component stream depth)
  (declare (ignore depth))
  (format stream "#<~:@(~A~): ~A>"
          (component-type component)
          (component-name component)))


(defun print-component (component stream depth)
  (declare (ignore depth))
  (print-unreadable-object (component stream :type t)
    (format stream "~S~:[~; with ~D sub-components~]"
            (component-name component)
            (component-components component)
            (length (component-components component)))))


;;; module

(defstruct (module (:include component (type :module))))


;;; systems

(defstruct (system (:include module (type :system))))

(defstruct (subsystem (:include system (type :subsystem))))

(defstruct (defsystem (:include system (type :defsystem))))

;;; See also definition of FOREIGN-SYSTEM below.


;;; files

(eval-when (:load-toplevel :compile-toplevel :execute)
  (shadow '(cl:file-author)))

(defstruct (file (:include component (type :file))))

(defstruct (private-file (:include file (type :private-file))))

(defstruct (static-file (:include file (type :static-file))))


;;; new-component
;;;
;;; The 'factory' costructor for components.
;;; It will be called by CREATE-COMPONENT.

(defun new-component (&rest keys &key (type :file) &allow-other-keys)
  "Create a COMPONENT given TYPE."
  (case type
    (:defsystem (apply #'make-defsystem keys))
    (:system (apply #'make-system keys))
    (:subsystem (apply #'make-subsystem keys))
    (:module (apply #'make-module keys))
    (:file (apply #'make-file keys))
    (:private-file (apply #'make-private-file keys))
    (:static-file (apply #'make-static-file keys))
    ;; No :foreign-system
    (t (apply #'make-component :type type keys))
    ))


;;; Foreign Systems
;;; ---------------

;;; To allow dependencies from "foreign systems" like ASDF or one of
;;; the proprietary ones like ACL or LW.

(deftype foreign-system-kinds ()
  '(member :asdf :lw :allegro :pcl))


(defparameter *foregn-systems-constructors* ())


(declaim (ftype (function (t t t) t) print-foreign-sys)) ; For SBCL benefit.

(defstruct (foreign-system
            (:include system)
            (:print-function print-foreign-sys)
            )
  kind ; This is a keyword: (member :asdf :lw :allegro :pcl ...)
  object ; The actual foreign system object.
  (load-time 0)
  )


(defun print-foreign-sys (component stream depth)

  (declare (ignore depth)
           (type foreign-system component))

  (format stream "#<FOREIGN ~:@(~A~): ~S loaded ~D (~:[~A~;~:*~A~])>"
          (component-type component)
          (component-name component)
          (foreign-system-load-time component)
          (foreign-system-object component)
          (foreign-system-kind component)
          )
  )


;;; ASDF Foreign System
;;; -------------------

;;; Declaims for fascist SBCL.

(declaim (ftype (function (component) t) asdf-compile-fun))
(declaim (ftype (function (component) t) asdf-load-fun))


;;; asdf-foreign-system

(defstruct (asdf-foreign-system
            (:include foreign-system
             (kind :asdf)
             (compile-form #'asdf-compile-fun)
             (load-form #'asdf-load-fun)
             ))
  "The MK DEFSYSTEM ASDF Foreign System Structure."
  )


;;; Foreign systems functions.

(defun build-foreign-system (&key name
                                  (kind :asdf)
                                  object
                                  &allow-other-keys)
  ;; Hardcoded FTTB

  (ecase kind
    (:asdf ; See below.
     (make-asdf-foreign-system :name name
                               :object object))
    ((:lw :allegro :pcl)
     (error "MK: foreign ~A systems are not handled yet (system ~S)."
            kind
            name))
    ((nil)
     (error "MK: cannot determine foreign system kind for system ~S."
            name))
    ))


(defun foreign-system-known-p (foreign-sys)
  (declare (type foreign-system foreign-sys))
  (not (null (foreign-system-object foreign-sys))))


(defun register-foreign-system (sys-designator &key representation kind)
  (declare (type (or symbol string foreign-system) sys-designator))
  (etypecase sys-designator
    ((or symbol string)
     (let ((fs (build-foreign-system :name sys-designator
                                     :kind kind
                                     :object representation)))
       (setf (get-system sys-designator) fs))
     )
    (foreign-system
     (setf (get-system (foreign-system-name sys-designator))
           sys-designator))
    ))


(defun find-foreign-system (sys-designator
                            &key
                            (errorp t)
                            (kind :asdf)
                            (where #P"."))
  (declare (type (or symbol
                     string
                     foreign-system)
                 sys-designator))

  (labels ((find-foreign-sys-name (sys-designator kind where)
             (declare (ignore where)
                      (type (or symbol string) sys-designator)
                      (type foreign-system-kinds kind)
                      )

             (ecase kind
               (:asdf
                (if (find-package "ASDF")
                    (funcall (find-symbol "FIND-SYSTEM" "ASDF")
                             (string-downcase sys-designator)
                             t)
                    (when errorp
                      (error 'missing-foreign-system
                             :name sys-designator)))
                )
               ((:lispworks :allegro)
                (when errorp
                  (error 'missing-foreign-system
                         :kind kind
                         :name sys-designator)))
               )
             )

           (find-foreign-sys-object (sys-designator)
             (declare (type foreign-system sys-designator))
             (let ((foreign-sys
                    (foreign-system-object sys-designator))
                   )
               (if (not (null foreign-sys))
                   sys-designator
                   (let ((real-foreign-sys
                          (find-foreign-sys-name
                           (foreign-system-name sys-designator)
                           (foreign-system-kind sys-designator)
                           nil))
                         )
                     (setf (foreign-system-object sys-designator)
                           real-foreign-sys)
                     sys-designator)))
             )
           )
    (handler-case
        (etypecase sys-designator
          ((or symbol string)
           (let* ((real-foreign-sys
                   (find-foreign-sys-name sys-designator kind where))
                  (foreign-sys-component
                   (register-foreign-system sys-designator
                                            :representation real-foreign-sys
                                            :kind kind))
                  )
             ;; In this case I most likely just loaded it.
             ;; This is not so correct, but at least it is a proper
             ;; "time" value.
             
             (setf (component-load-time foreign-sys-component)
                   (get-universal-time))
             foreign-sys-component
             )
           )
          (foreign-system
           (register-foreign-system
            (find-foreign-sys-object sys-designator))
           )
          )
      (missing-foreign-system (mfs)
        (when errorp
          (error mfs)))
      )
    ))


;;; asdf-compile-fun

(defun asdf-compile-fun (asdf-sys-component
                         &aux
                         (asdf-sys-name (component-name
                                         asdf-sys-component))
                         (asdf-pkg (find-package "ASDF"))
                         (compile-sys-sym
                          (when asdf-pkg
                            (find-symbol "COMPILE-SYSTEM" asdf-pkg)))
                         (load-sys-sym
                          (when asdf-pkg
                            (find-symbol "LOAD-SYSTEM" asdf-pkg)))
                         )

  ;; In general in MK:DEFSYSTEM a compile is always followed by a load
  ;; (cfr. compile-and-load-operation).  Not so in ASDF.
  ;; I must follow the compile with a load operation.
  ;;
  ;; Notes:
  ;; Check whether the ASDF compile operation can load the system.

  (if asdf-pkg
      (let ((sysname (string-downcase asdf-sys-name)))
        (funcall compile-sys-sym sysname)
        (funcall load-sys-sym sysname)
        )
      (error "MK: Error: trying to compile the ~S ASDF system, but ASDF is not available."
             asdf-sys-name)))


;;; asdf-load-fun

(defun asdf-load-fun (asdf-sys-component
                      &aux (asdf-sys-name (component-name asdf-sys-component)))
  (if (find-package "ASDF")
      (funcall (find-symbol "LOAD-SYSTEM" "ASDF")
               (string-downcase asdf-sys-name))
      (error "MK: Error: trying to load the ~S ASDF system, but ASDF is not available."
             asdf-sys-name)))


;;; Add other types of systems...

;;; Lispworks Defsystem
;;; -------------------


;;; Allegro Defsystem
;;; -----------------


;;; KMP Defsystem
;;; -----------------


;;; Components' load time
;;; ---------------------

(defvar *file-load-time-table* (make-hash-table :test #'equalp)
  "Hash table of load times for the system definitions and files
in the system definitions.")


(defun component-load-time (component)
  (when component
    (etypecase component
      (string   (gethash component *file-load-time-table*))
      (pathname (gethash (namestring component) *file-load-time-table*))
      (foreign-system (foreign-system-load-time component))
      (component
       (ecase (component-type component)
	 (:defsystem
	  (let* ((name (component-name component))
		 (path (when name (compute-system-path name nil))))
	    (declare (type (or string pathname null) path))
	    (when path
	      (gethash (namestring path) *file-load-time-table*))))

	 ((:file :static-file :private-file)
          #| old (:file :private-file)|#
	  ;; Use only :source pathname to identify component's
	  ;; load time.
	  (let ((path (component-full-pathname component :source)))
	    (when path
	      (gethash path *file-load-time-table*)))))))))


#|
#-(or :cmu)
(defsetf component-load-time (component) (value)
  `(when ,component
    (etypecase ,component
      (string   (setf (gethash ,component *file-load-time-table*) ,value))
      (pathname (setf (gethash (namestring (the pathname ,component))
			       *file-load-time-table*)
		      ,value))
      (foreign-system (setf (foreign-system-load-time ,component) ,value))
      (component
       (ecase (component-type ,component)
	 (:defsystem
	  (let* ((name (component-name ,component))
		 (path (when name (compute-system-path name nil))))
	    (declare (type (or string pathname null) path))
	    (when path
	      (setf (gethash (namestring path) *file-load-time-table*)
		    ,value))))

	 ((:file :private-file)
	  ;; Use only :source pathname to identify file.
	  (let ((path (component-full-pathname ,component :source)))
	    (when path
	      (setf (gethash path *file-load-time-table*)
		    ,value)))))))
    ,value))
|#

(defun (setf component-load-time) (value component)
  (declare (type (or null string pathname component) component)
           (type (or unsigned-byte null) value))
  (when component
    (etypecase component
      (string   (setf (gethash component *file-load-time-table*) value))
      (pathname (setf (gethash (namestring (the pathname component))
			       *file-load-time-table*)
		      value))
      (foreign-system (setf (foreign-system-load-time component) value))
      (component
       (ecase (component-type component)
	 (:defsystem
	     (let* ((name (component-name component))
		    (path (when name (compute-system-path name nil))))
	       (declare (type (or string pathname null) path))
	       (when path
		 (setf (gethash (namestring path) *file-load-time-table*)
		       value))))
	 ((:file :private-file :static-file) #| old (:file :private-file) |#
	  ;; Use only :source pathname to identify file.
	  (let ((path (component-full-pathname component :source)))
	    (when path
	      (setf (gethash path *file-load-time-table*)
		    value)))))))
    value))


;;; Systems' Paths
;;; --------------

(defvar *fs-directory-separators* ; Ok, I am overdoing it.
  (list #+(or unix windows os-windows win32) #\/
        #+(or windows os-windows win32) #\\
        #+vms #\.
        ))


(defvar *fs-directory-terminators* ; Ok, I am overdoing it.
  (list  #+(or unix windows os-windows win32) #\/
         #+(or windows os-windows win32) #\\
         #+vms #\]
         ))


(defvar *fs-file-type-separator* ; Ok, I am overdoing it.
  (list #+(or windows os-windows win32) #\. ; This is very correct.
        #+vms #\.     ; This is very correct.
        #+unix #\.    ; This is semi-correct and conventional.
        ))


(defun fs-directory-separator-p (c)
  (declare (type character c))
  (member c *fs-directory-separators* :test #'char=))


(defun fs-directory-terminator-p (c)
  (declare (type character c))
  (member c *fs-directory-terminators* :test #'char=))


(defun fs-file-type-separator-p (c)
  (declare (type character c))
  (member c *fs-file-type-separator* :test #'char=))
  

(defun absolute-pathname-p (thing
                            &aux
                            (p (pathname thing))
                            (d (pathname-directory p)))
  (and d (listp d) (eq :absolute (first d))))


(defun relative-pathname-p (thing
                            &aux
                            (p (pathname thing))
                            (d (pathname-directory p)))
  (and d (listp d) (eq :relative (first d))))


;;; compute-system-path --
;;;
;;; 20200907: MA: Check madhu's version.
;;; 20211012: MA: Included madhu's changes, but with error.

(defun compute-system-path-internal (module-name &optional definition-pname)
  "Compute a system path using the registry.

The function returns an existing file or NIL.  The (non NIL) file returned
is either DEFINITION-PNAME, if it names an existing pathname, or in
the *CENTRAL-REGISTRY* or in the current directory. The namestring is
(e.g., on UNIX) 'MODULE-NAME.<sys extension>'
or 'MODULE-NAME/MODULE-NAME.<sys extension>'."

  ;; Note that MODULE-NAME should really be SYSTEM-NAME.

  ;; madhu 20230717 - handle retarded module-names "cffi/c2fi" ccl: blows
  ;; up on probe-file. clisp blows up on make-pathname :name

  (declare (type (or null string pathname) definition-pname))

  (let* ((module-string-name-0
          (etypecase module-name
            (symbol (string-downcase (string module-name)))
            (string module-name)))

         (module-string-name
          (let ((slash-pos (position-if #'fs-directory-separator-p
                                        module-string-name-0))
                ;; (slash-pos (position #\/ module-string-name-0))
                )
            (if slash-pos
                (error "MK: module-name contains potential folder separator ~S."
                       *fs-directory-separators*)
                module-string-name-0))
          )

         (file-pathname
	  (make-pathname :name module-string-name
			 :type *system-extension*))

         (lib-file-pathname
	  (make-pathname :directory (list :relative module-string-name)
                         :name module-string-name
			 :type *system-extension*))
         )
    
    (or (and definition-pname		; Pathname was given for system def.
	     (probe-file definition-pname))

	;; Then the central registry. Note that we also check the current
	;; directory in the registry, but the above check is hard-coded.

	(cond (*central-registry*
	       (if (listp *central-registry*)
		   (dolist (registry *central-registry*)
		     (let* ((reg-path (registry-pathname registry))
                            (file (or (probe-file
                                       (append-directories
                                        reg-path file-pathname))
                                      (probe-file
                                       (append-directories
                                        reg-path lib-file-pathname)))))
		       (when file
			 (return file))))
		   (or (probe-file (append-directories *central-registry*
						       file-pathname))
                       (probe-file (append-directories *central-registry*
						       lib-file-pathname))
                       ))
               )
	      (t
	       ;; No central registry. Assume current working directory.
	       ;; Maybe this should be an error?
	       (or (probe-file file-pathname)
                   (probe-file lib-file-pathname)))))
    ))


(defvar *defsystem-to-defsystem-pathname-map*
  ;; Madhu's machinery to remember and warn of changed defsystem origins.
  (make-hash-table :test #'equalp))


(defun compute-system-path (module-name &optional definition-pname)
  ;; Madhu's version with check that definition pathname has not changed.
  ;; The semantics is different as preference is given to the
  ;; "current" pathname.

  (declare (type (or null string pathname) definition-pname))

  (when (typep module-name 'component)
    (setq module-name (component-name module-name)))

  (let ((current-spname (compute-system-path-internal module-name definition-pname))
	(previous-spname (gethash (canonicalize-system-name module-name)
                                  *defsystem-to-defsystem-pathname-map*)))
    (if current-spname
        (if previous-spname
            (if (equalp current-spname (probe-file previous-spname))
                current-spname
                (progn
                  (warn "MK: COMPUTE-SYSTEM-PATH: system was last previously loaded ~
                         from ~A (not ~A): using currently loaded path."
                        previous-spname
                        current-spname)
                  current-spname))
            current-spname)
        ;; (if previous-spname previous-spname)
        previous-spname
        )
    ))


(defun system-definition-pathname (system-name)
  "Return the pathname where the system was loaded from.

Given SYSTEM-NAME (a string or symbol) this function returns a
pathname representing the system source and whether this pathname
actually exists (via PROBE-FILE)."

  (let ((system (ignore-errors (find-system system-name :error))))
    (if system
        (let ((system-def-pathname
               (make-pathname
		:type "system"
		:defaults (pathname (component-full-pathname system :source))))
              )
          (values system-def-pathname
                  (probe-file system-def-pathname)))
        (values nil nil))))


;;; Finding Systems
;;; ---------------

(defvar *reload-systems-from-disk* t
  "If T, always tries to reload newer system definitions from disk.

Otherwise first tries to find the system definition in the current
environment.")

#| Old version not so smart about reloading.
(defun find-system (system-name &optional (mode :ask) definition-pname)
  "Returns the system named SYSTEM-NAME.

If not already loaded, loads it, depending on the value of
*RELOAD-SYSTEMS-FROM-DISK* and of the value of MODE. MODE can be :ASK,
:ERROR, :LOAD-OR-NIL, or :LOAD. :ASK is the default.
This allows OPERATE-ON-SYSTEM to work on non-loaded as well as
loaded system definitions. DEFINITION-PNAME is the pathname for
the system definition, if provided."
  (ecase mode
    (:ask
     (or (get-system system-name)
	 (when (y-or-n-p-wait
		#\y 20
		"MK: system ~A not loaded. Shall I try loading it? "
		system-name)
	   (find-system system-name :load definition-pname))))

    (:error
     (or (get-system system-name)
	 (error 'missing-system :name system-name)))

    (:load-or-nil
     (let ((system (get-system system-name)))

       ;; (break "System ~S ~S." system-name system)

       (or (unless *reload-systems-from-disk* system)

	   ;; If SYSTEM-NAME is a symbol, it will lowercase the
	   ;; symbol's string.
	   ;; If SYSTEM-NAME is a string, it doesn't change the case of the
	   ;; string. So if case matters in the filename, use strings, not
	   ;; symbols, wherever the system is named.

           ;; Foreign system.
           ;; Directly return the result, especially whn NIL.

           (when (and system (foreign-system-p system))
             (warn "~&MK: Trying to find and (re)load foreign system~%MK: ~S."
		   system)
             (let ((real-foreign-sys (find-foreign-system system)))
               (if real-foreign-sys
                   (return-from find-system real-foreign-sys)
                   (return-from find-system nil))))

           ;; "Regular" system.

	   (let ((path (compute-system-path system-name definition-pname)))
	     (when (and path
			(or (null system)
			    (null (component-load-time path))
			    (< (component-load-time path)
			       (file-write-date path))))
	       (tell-user-generic "Loading system ~A from file ~A."
                                  system-name
                                  path)
	       (load path)
	       (setf system (get-system system-name))
	       (when system
		 (setf (component-load-time path)
                       (file-write-date path)
                       ;; (get-universal-time)
                       )))
	     system)
	   ;; system ; SBCL correctly complains that this is
           ;; unreachable. Remove this later (20220212 MA)
           )))

    (:load
     (let ((sys (get-system system-name)))
       (if sys
           (or (unless *reload-systems-from-disk* sys)

               (when (foreign-system-p sys)
                 (if (foreign-system-known-p sys)
                     sys
                     (find-foreign-system sys :errorp nil)))

               (or (find-system system-name :load-or-nil definition-pname)
                   (error 'missing-system :name system-name)))

           ;; Unknown system. Try to load no matter what.  If unlucky ERROR.
           (or (find-system system-name :load-or-nil definition-pname)
               (error 'missing-system :name system-name))
           )))
    ))
|#


(defun find-system (system-name
                    &optional
                    (mode :ask)
                    definition-pname
                    &aux
                    (sys (get-system system-name))
                    (sys-load-time (if sys
                                       (component-load-time sys)
                                       0)))

  "Return the system named SYSTEM-NAME.

If not already loaded, loads it, depending on the value of
*RELOAD-SYSTEMS-FROM-DISK* and of the value of MODE. MODE can be :ASK,
:ERROR, :LOAD-OR-NIL, or :LOAD. :ASK is the default.
This allows OPERATE-ON-SYSTEM to work on non-loaded as well as
loaded system definitions. DEFINITION-PNAME is the pathname for
the system definition, if provided."

  (declare (type (or symbol string) system-name)
           (type (or null system) sys)
           (ignore sys-load-time))

  (ecase mode
    (:ask
     (or sys
	 (when (y-or-n-p-wait
		#\y 20
		"MK: system ~A not loaded. Shall I try loading it? "
		system-name)
	   (find-system system-name :load definition-pname))))

    (:error
     (or sys
	 (error 'missing-system :name system-name)))

    (:load-or-nil

     ;; (break "System ~S ~S." system-name system)

     (or (unless *reload-systems-from-disk* sys)

         ;; If SYSTEM-NAME is a symbol, it will lowercase the
         ;; symbol's string.
         ;; If SYSTEM-NAME is a string, it doesn't change the case of the
         ;; string. So if case matters in the filename, use strings, not
         ;; symbols, wherever the system is named.

         ;; Foreign system.
         ;; Directly return the result, especially when NIL.

         (when (and sys (foreign-system-p sys))
           (tell-user-warn "Trying to find and (re)load foreign system~%MK: ~S."
                           sys)
           (let ((real-foreign-sys (find-foreign-system sys)))
             (if real-foreign-sys
                 (return-from find-system real-foreign-sys)
                 (return-from find-system nil))))

         ;; "Regular" system.
         ;; We get here is SYSTEM is NIL or when
         ;; *reload-systems-from-disk* is T (the default).

         (let ((path (compute-system-path system-name definition-pname)))
           (when (and path
                      (or (null sys)
                          (null (component-load-time sys))
                          (< (component-load-time sys) ; SYS not PATH.
                             (file-write-date path))))

             ;; We really need to reload.

             (tell-user-generic "Loading system ~A from file ~A."
                                system-name
                                path)
             (load path)
             (setf sys (get-system system-name))
             (when sys
               (setf (component-load-time sys) ; SYS not PATH.
                     (file-write-date path)
                     ;; (get-universal-time)
                     )))
           sys)
         ))

    (:load
     (if sys
         (or (unless *reload-systems-from-disk* sys)

             (when (foreign-system-p sys)
               (if (foreign-system-known-p sys)
                   sys
                   (find-foreign-system sys :errorp nil)))

             (or (find-system system-name :load-or-nil definition-pname)
                 (error 'missing-system :name system-name)))

         ;; Unknown system. Try to load no matter what.  If unlucky ERROR.
         (or (find-system system-name :load-or-nil definition-pname)
             (error 'missing-system :name system-name))
         ))
    ))


;;; Describing Systems and Components
;;; ---------------------------------

(defun describe-system (name &optional (stream *standard-output*))
  "Prints a description of the system to the stream.

 If NAME is the name of a system, gets it and prints a description of
the system. If NAME is a component, prints a description of the
component."
  (let ((system (if (typep name 'component) name (find-system name :load))))
    (format stream "~&~A ~A: ~
                    ~@[~&~A~2%~]~
                    ~@[~&   Host: ~A~]~
                    ~@[~&   Device: ~A~]~
                    ~@[~&   Package: ~A~]~
                    ~&   Source: ~@['~A'~] ~@['~A'~] ~@['~A'~]~
                    ~&   Binary: ~@['~A'~] ~@['~A'~] ~@['~A'~]~
                    ~@[~&   Depends On: ~A ~]~&   Components:~{~15T~A~&~}"
	    (component-type system)
	    (component-name system)
            (component-documentation system)
	    (component-host system)
	    (component-device system)
	    (component-package system)

	    (component-root-dir system  :source)
	    (component-pathname system  :source)
	    (component-extension system :source)

	    (component-root-dir system  :binary)
	    (component-pathname system  :binary)
	    (component-extension system :binary)

	    (component-depends-on system)
	    (component-components system))
    system))


(defun system-description (name)
  (let ((system (if (typep name 'component) name (find-system name :load))))
    (when system
      (component-documentation system))))


#|
(defun system-documentation (name)
  (system-description name))
|#

(eval-when (:load-toplevel :execute)
  (setf (fdefinition 'system-documentation) #'system-description))


(defun component-description (c)
  ;; Necessary to avoid slot duplication.
  (component-documentation c))


(defun (setf component-description) (d c)
  ;; Necessary to avoid slot duplication.
  (setf (component-documentation c) d)
  )


(defun component-license (c)
  ;; Necessary to avoid slot duplication.
  (component-licence c)
  )


(defun (setf component-license) (d c)
  ;; Necessary to avoid slot duplication.
  (setf (component-licence c) d)
  )


;;; Components Names and Pathnames
;;; ------------------------------
;;;
;;; Notes:
;;;
;;; 2026-03-05: ma:
;;; These functions are the resuly of historical stratification.
;;; The interplay between the component's name, the underlying
;;; pathname's name and the 'full' pathname (which is what is used by
;;; the operations) is complicated, given the possible (actually
;;; certain) misuse by the user.
;;; 
;;; COMPONENT-FULL-PATHNAME does several things at "operation" time,
;;; plus it returns a namestring (hence it is now, who remembers since
;;; when, misnamed).  Now fixed to return a pathname.
;;;
;;; Some extra functionalities -- e.g., handling of component's names
;;; that encode filesystem conventions -- should be factored out.

(defun canonicalize-component-name (component
                                    &aux (cn (component-name component)))

  (declare (type component component))

  ;; Within the component, the name is a string.

  (etypecase cn
    (string cn)
    ;; Unnecessary to change it, so just return it, same case.

    (symbol
     (setf (component-name component)
           (string-downcase (string (component-name component)))))

     ;; Otherwise, make it a downcase string -- important since file
     ;; names are often constructed from component names, and unix
     ;; prefers lowercase as a default.
     ;;
     ;; Maybe something hairier with MAKE-PATHNAME and :CASE could be
     ;; used. 
     ))


(defun component-pathname (component &optional (type :source))
  (when component
    (ecase type
      (:source (component-source-pathname component))
      (:binary (component-binary-pathname component))
      (:error  (component-error-pathname component)))))


(defun component-error-pathname (component)
  (let ((binary (component-pathname component :binary)))
    (new-file-type binary *compile-error-file-type*)))

(defsetf component-pathname (component &optional (type :source)) (value)
  `(when ,component
     (ecase ,type
       (:source (setf (component-source-pathname ,component) ,value))
       (:binary (setf (component-binary-pathname ,component) ,value)))))


(defun component-root-dir (component &optional (type :source))
  (when component
    (ecase type
      (:source (component-source-root-dir component))
      ((:binary :error) (component-binary-root-dir component))
      )))

(defsetf component-root-dir (component &optional (type :source)) (value)
  `(when ,component
     (ecase ,type
       (:source (setf (component-source-root-dir ,component) ,value))
       (:binary (setf (component-binary-root-dir ,component) ,value)))))


(defvar *source-pathnames-table* (make-hash-table :test #'equalp)
  "Table which maps from components to full source pathnames.")


(defvar *binary-pathnames-table* (make-hash-table :test #'equalp)
  "Table which maps from components to full binary pathnames.")


(defparameter *reset-full-pathname-table* t
  "If T, clears the full-pathname tables before each call to OPERATE-ON-SYSTEM.

Setting this to NIL may yield faster performance after multiple calls
to LOAD-SYSTEM and COMPILE-SYSTEM, but could result in changes to
system and language definitions to not take effect, and so should be
used with caution.")


(defun clear-full-pathname-tables ()
  (clrhash *source-pathnames-table*)
  (clrhash *binary-pathnames-table*))


;;; make-component-name-pathname

(defun make-component-name-pathname (c)
  "Make a pathname starting only from C component's name.

The pathname created is dependent on the type of the compont.  MODULEs
have the pathname with :NAME and :TYPE set to NIL, essentially a
directory.  Otherwise, the COMPONENT-NAME is 'parsed' as is."
  (declare (type component c))
  
  ;; Order important.

  (etypecase c
    ;; Add (OR SYSTEM DEFSYSTEM) first?

    (module
     (ensure-directory-pathname (component-name c)))

    (component
     (parse-namestring (string (component-name c))))
    ))


;;; component-full-pathname, component-full-pathname-i
;;;
;;; These functions are used at "operation time", i.e., they are
;;; subject to variable bindings set by the OPERATE- functions.

(defun component-full-pathname (component
                                &optional
                                (type :source)
                                (version *version*))
  "Return the pathname corresponding to COMPONENT.

The TYPE of the pathname is either :SOURCE or :BINARY.  The resulting
pathname is built from the the COMPONENT's slot values.  The function
caches the pre-computed values to speed up the computation.

Notes:

The actual values are computed by the auxiliary function
COMPONENT-FULL-PATHNAME-I."

  (when component
    (case type
      (:source
       (let ((old (gethash component *source-pathnames-table*)))
	 (or old
	     (let ((new (component-full-pathname-i component type version)))
	       (setf (gethash component *source-pathnames-table*) new)
	       new))))

      (:binary
        (let ((old (gethash component *binary-pathnames-table*)))
	 (or old
	     (let ((new (component-full-pathname-i component type version)))
	       (setf (gethash component *binary-pathnames-table*) new)
	       new))))

      (otherwise
       (component-full-pathname-i component type version)))))


;;; Madhu's fix for clozure.
;;; 20200907: MA: Check madhu's fix for clozure.
;;;
;;; madhu 200229  long standing userspace kludge.
;;; clozure escapes the final dot in pathname-name or pathname-type
;;; (namestring (make-pathname :name "foo.lisp" :type "bar.lisp"))
;;;  "foo.lisp.bar\\.lisp"
;;;  (namestring (make-pathname :name "foo.lisp" :type nil))
;;; "foo\\.lisp"
;;;  (namestring (make-pathname :name "foo.lisp" :type "bar\\."))
;;;
;;; (pathname  "foo.lisp.bar\\.")
;;; (fix-clozure-namestring (namestring (make-pathname :name "/a.b/c" :type "d.d/foo.bar.lisp")
;;; (fix-clozure-namestring (namestring (probe-file "/dev/shm/\\\\.")))
;;; (elt  (directory "/dev/shm/*" :follow-links nil) 5)

#| Test
(fix-clozure-namestring  "foo.lisp.bar\\.\\.lisp\\.var")
|#

#+clozure
(defun fix-clozure-namestring (string)
  (declare (type string string))
  (let ((p (search "\\." string :from-end t)))
    (if p
	(let ((end (length string)) ret)
	  (loop
           (push (subseq string (1+ p) end) ret)
           (setq end p)
           (unless (setq p (search "\\." string :from-end t :end2 p))
             (push (subseq string 0 end) ret)
             (return (apply #'concatenate 'string ret)))))
	string)))


#-clozure
(defun fix-clozure-namestring (string) string)


#| #+old-version
(defun component-full-pathname-i (component &optional
                                            (type :source)
                                            (version *version*)
					    &aux
                                            version-dir
                                            version-replace)

  ;; If the pathname-type is :binary and the root pathname is null,
  ;; distribute the binaries among the sources (= use :source pathname).
  ;; This assumes that the component's :source pathname has been set
  ;; before the :binary one.

  (if version
      (multiple-value-setq (version-dir version-replace)
	(translate-version version))
      (setq version-dir *version-dir*
            version-replace *version-replace*))

  ;; (format *trace-output* "~&>>>> VERSION COMPUTED ~S ~S~%" version-dir version-replace)

  (let ((pathname
	 (append-directories
	  (if version-replace
	      version-dir
	      (append-directories (component-root-dir component type)
				  version-dir))
	  (component-pathname component type))))

    ;; When a logical pathname is used, it must first be translated to
    ;; a physical pathname. This isn't strictly correct. What should happen
    ;; is we fill in the appropriate slots of the logical pathname, and
    ;; then return the logical pathname for use by compile-file & friends.
    ;; But calling translate-logical-pathname to return the actual pathname
    ;; should do for now.

    ;; (format t "pathname = ~A~%" pathname)
    ;; (format t "type = ~S~%" (component-extension component type))

    ;; 20000303 Marco Antoniotti
    ;; Changed the following according to suggestion by Ray Toy.  I
    ;; just collapsed the tests for "logical-pathname-ness" into a
    ;; single test (heavy, but probably very portable) and added the
    ;; :name argument to the MAKE-PATHNAME in the MERGE-PATHNAMES
    ;; beacuse of possible null names (e.g. :defsystem components)
    ;; causing problems with the subsequenct call to NAMESTRING.
    ;; (format *trace-output* "~&>>>> PATHNAME is ~S~%" pathname)

    ;; 20050309 Marco Antoniotti
    ;; The treatment of PATHNAME-HOST and PATHNAME-DEVICE in the call
    ;; to MAKE-PATHNAME in the T branch is bogus.   COMPONENT-DEVICE
    ;; and COMPONENT-HOST must respect the ANSI definition, hence,
    ;; they cannot be PATHNAMEs.  The simplification of the code is
    ;; useful.  SCL compatibility may be broken, but I doubt it will.

    ;; 20050310 Marco Antoniotti
    ;; After a suggestion by David Tolpin, the code is simplified even
    ;; more, and the logic should be now more clear: use the user
    ;; supplied pieces of the pathname if non nil.

    ;; 20050613 Marco Antoniotti
    ;; Added COMPONENT-NAME extraction to :NAME part, in case the
    ;; PATHNAME-NAME is NIL.

    ;; 20200907: MA: check madhu's 200060520 comment

    (cond ((pathname-logical-p pathname) ; See definition of test above.
	   (setf pathname
		 (merge-pathnames pathname
				  (make-pathname ; madhu 20080208 (LW)
				   :name (or (pathname-name pathname)
                                             (component-name component))
				   :type (component-extension component
							      type))))
	   (namestring (translate-logical-pathname pathname)))

	  (t
	   (let ((ns
                  (namestring
                   (make-pathname
                    :host (or (component-host component)
                              (pathname-host pathname))

                    :directory (pathname-directory pathname
                                                   #+scl :case
                                                   #+scl :common
                                                   )

                    :name (or ; Check madhu's name hacking.
                              ; Handle pathological cases like the following.
                              ; (:file "src/package")
			      ; (:file "glib.package")
			      ; (:file "src/glib.package")

                           (cond ((find #\. (component-name component))
				   (cond ((find #\/ (component-name component))
					  (file-namestring
					   (component-name component)))
					 (t (component-name component))))
				  (t
				   (cond ((find #\/ (component-name component))
					  nil)
					 (t nil))))

                               (pathname-name pathname
                                             #+scl :case
                                             #+scl :common
                                             )
                              (component-name component))

                    :type
                    #-scl (component-extension component type)
                    #+scl (string-upcase
                           (component-extension component type))

                    :device
                    #+sbcl
                    :unspecific
                    #-(or :sbcl)
                    (or (component-device component)
                        (pathname-device pathname
                                         #+scl :case
                                         #+scl :common
                                         ))
                    ;; :version :newest
                    )))
                  )
             (fix-clozure-namestring ns))
           ))))
|#


(defun component-pathname-version (c)
  "Return a 'version' usable by MAKE-PATHNAME.

Notes:

This is to placate, as usual, SBCL."

  (declare (type component c))
  
  (let ((cv (component-version c)))
    (typecase cv
      (string :newest)
      (t cv))
    ))


(defun sanitize-component-name (component pathname)
  ;; Check madhu's name hacking in old COMPUTE-FULL-PATHNAME-I
  ;; function.  THe logic should be the same.

  ;; Handle pathological cases like the following.
  ;; (:file "src/package")
  ;; (:file "glib.package")
  ;; (:file "src/glib.package")

  (or (and (find-if #'fs-file-type-separator-p (component-name component))
           (if (find-if #'fs-directory-separator-p (component-name component))
               (file-namestring (component-name component))
               (component-name component)))
      
      (pathname-name pathname #+scl :case #+scl :common)
      (component-name component)
      ))


;;; component-full-pathname-i
;;;
;;; Notes:
;;;
;;; The following are the historical comments pertaining the handling
;;; of logical-pathnames.
;;;
;;; 20000303 Marco Antoniotti
;;; Changed the following according to suggestion by Ray Toy.  I
;;; just collapsed the tests for "logical-pathname-ness" into a
;;; single test (heavy, but probably very portable) and added the
;;; :name argument to the MAKE-PATHNAME in the MERGE-PATHNAMES
;;; beacuse of possible null names (e.g. :defsystem components)
;;; causing problems with the subsequenct call to NAMESTRING.
;;;
;;; 20050309 Marco Antoniotti
;;; The treatment of PATHNAME-HOST and PATHNAME-DEVICE in the call
;;; to MAKE-PATHNAME in the T branch is bogus.   COMPONENT-DEVICE
;;; and COMPONENT-HOST must respect the ANSI definition, hence,
;;; they cannot be PATHNAMEs.  The simplification of the code is
;;; useful.  SCL compatibility may be broken, but I doubt it will.
;;;
;;; 20050310 Marco Antoniotti
;;; After a suggestion by David Tolpin, the code is simplified even
;;; more, and the logic should be now more clear: use the user
;;; supplied pieces of the pathname if non nil.
;;;
;;; 20050613 Marco Antoniotti
;;; Added COMPONENT-NAME extraction to :NAME part, in case the
;;; PATHNAME-NAME is NIL.
;;;
;;; 20200907: MA: check madhu's 200060520 comment.

#| #+old-version-pre-eager-pathnamea
(defun component-full-pathname-i (component &optional
                                            (type :source)
                                            (version *version*)
					    &aux
                                            version-dir
                                            version-replace)
  "Build the actual COMPONENT's pathname using its slot values.

The TYPE can be :SOURCE, :BINARY or :ERROR.  VERSION is either NIL, a
symbol or a string (assuming it is completely alphanumeric).

VERSION is used as an extra directory 'level' where the versioned
component is found."

  (declare (type component component)
           (type (member :source :binary :error) type))

  ;; If the pathname-type is :binary and the root pathname is null,
  ;; distribute the binaries among the sources (i.e., use :source pathname).
  ;; This assumes that the component's :source pathname has been set
  ;; before the :binary one.

  (if version
      (multiple-value-setq (version-dir version-replace)
          (translate-version version))
      (setq version-dir *version-dir*
            version-replace *version-replace*))

  (let ((pathname
	 (append-directories
	  (if version-replace
	      version-dir
	      (append-directories (component-root-dir component type)
				  version-dir))
	  (component-pathname component type))))

    ;; When a logical pathname is used, it must first be translated to
    ;; a physical pathname. This isn't strictly correct. What should happen
    ;; is we fill in the appropriate slots of the logical pathname, and
    ;; then return the logical pathname for use by compile-file & friends.
    ;; But calling translate-logical-pathname to return the actual pathname
    ;; should do for now.

    (if (pathname-logical-p pathname) ; See definition of test above.
        (translate-logical-pathname
         (merge-pathnames pathname
                          (make-pathname ; madhu 20080208 (LW)
                           :name (or (pathname-name pathname)
                                     (component-name component))
                           :type (component-extension component type))))
        ;; else
        (make-pathname
         :host (or (component-host component)
                   (pathname-host pathname))

         :directory (pathname-directory pathname #+scl :case #+scl :common)

         :name (sanitize-component-name component pathname)

         :type
         #-scl (component-extension component type)
         #+scl (string-upcase
                (component-extension component type))

         :device (or (component-device component)
                     (pathname-device pathname #+scl :case #+scl :common))

         :version (or (component-version component)
                      (pathname-version pathname)
                      :newest)
         ))))
|#


;;; component-full-pathname-i
;;; New version that aacounts for the fact that at "operation time"
;;; the components have their pathnames fully constructed.

(defun component-full-pathname-i (component &optional
                                            (type :source)
                                            (version *version*)
					    &aux
                                            (version-dir *version-dir*)
                                            (version-replace *version-replace*))
  "Build the actual COMPONENT's pathname using its slot values.

The TYPE can be :SOURCE, :BINARY or :ERROR.  VERSION is either NIL, a
symbol or a string (assuming it is completely alphanumeric).

VERSION is used as an extra directory 'level' where the versioned
component is found."

  (declare (type component component)
           (type (member :source :binary :error) type))

  ;; If the pathname-type is :binary and the root pathname is null,
  ;; distribute the binaries among the sources (i.e., use :source pathname).
  ;; This assumes that the component's :source pathname has been set
  ;; before the :binary one.

  (when version
    (multiple-value-setq (version-dir version-replace)
        (translate-version version)))

  (let* ((cp (component-pathname component type))
         (cpn (pathname-name cp))
         (cpt (pathname-type cp))
         (cpdir (pathname-directory-pathname cp))

         (pathname
          (cond ((and version-replace version-dir)
                 (make-pathname :name cpn
                                :type cpt
                                :defaults version-dir))
                (version-dir (merge-pathnames
                              (make-pathname :name cpn
                                             :type cpt
                                             :defaults version-dir)
                              cpdir))
                (t cp)))
         )

    (declare (type pathname cp))

    ;; When a logical pathname is used, it must first be translated to
    ;; a physical pathname. This isn't strictly correct. What should happen
    ;; is we fill in the appropriate slots of the logical pathname, and
    ;; then return the logical pathname for use by compile-file & friends.
    ;; But calling translate-logical-pathname to return the actual pathname
    ;; should do for now.

    (if (pathname-logical-p pathname) ; See definition of test above.
        (translate-logical-pathname
         (merge-pathnames pathname
                          (make-pathname ; madhu 20080208 (LW)
                           :name (or (pathname-name pathname)
                                     (component-name component))
                           :type (component-extension component type))))
        ;; else
        (make-pathname
         :host (or (component-host component)
                   (pathname-host pathname))

         :directory (pathname-directory pathname #+scl :case #+scl :common)

         :name (sanitize-component-name component pathname)

         :type
         #-scl (component-extension component type)
         #+scl (string-upcase
                (component-extension component type))

         :device (or (component-device component)
                     (pathname-device pathname #+scl :case #+scl :common))

         :version (or (component-pathname-version component)
                      (pathname-version pathname)
                      :newest)
         :defaults pathname
         ))))


(defun component-full-namestring (c)
  "Return the full namestring for component C.

Notes:

The result is also processed to 'fix' some idiosyncrasies of CCL."
  (declare (type component c))
  (fix-clozure-namestring (namestring (component-full-pathname c))))


;;; 20200907: MA: Check madhu's version.

(defun component-extension (component type &key local)
  (ecase type
    (:source (or (component-source-extension component)
		 (unless local
		   (default-source-extension component)) ; system default
                 ;; (and (component-language component))
                 ))
    (:binary (or (component-binary-extension component)
		 (unless local
		   (default-binary-extension component)) ; system default
                 ;; (and (component-language component))
                 ))
    (:error  *compile-error-file-type*)))


(defsetf component-extension (component type) (value)
  `(ecase ,type
     (:source (setf (component-source-extension ,component) ,value))
     (:binary (setf (component-binary-extension ,component) ,value))
     (:error  (setf *compile-error-file-type* ,value))))


;;; System and Component Definitions and Creations.
;;; -----------------------------------------------

;;; The logic of component creation (entry point: CREATE-COMPONENT) is
;;; now simplified.
;;;
;;; CREATE-COMPONENT makes the components (via NEW-COMPONENT, a
;;; wrapper around the various MAKE-(*-)COMPONENT constructors) and
;;; then processes the slots via the INIT-* functions.  Finally, it
;;; calls the FINALIZE-* functions (which used to be called FINALIZE-*).

(defun create-component (type name definition-body
                              &optional
                              parent
                              (indent 0))
  "Create a COMPONENT.

This is a 'factory' constructor.  TYPE is the COMPONENT's type, NAME
the COMPONENT's name, DEFINITION-BODY the collection of the other slot
definitions and of the COMPONENT's children.  Since components are
organized in a 'part-of' hierarchy, PARENT is the component of which the
one being created is a child of.  INDENT is the 'level' of the
COMPONENT."

  ;; In the following there is a code pattern that is repeated all
  ;; over.
  ;;
  ;; UNLESS some slot is in the DEFINITION-BODY, set it to the
  ;; PARENT's slot value (if there is a PARENT).  The resulting value
  ;; can be NIL.

  (let ((component (apply #'new-component
                          ;; #'make-component
			  :type type
			  :name name
			  :indent indent
			  definition-body)))

    ;; Initializations/after makes
    (canonicalize-component-name component)

    ;; Set up :load-only attribute
    (unless (find :load-only definition-body)
      (setf (component-load-only component)
	    (when parent
	      (component-load-only parent))))

    ;; Set up :compile-only attribute
    (unless (find :compile-only definition-body)
      (setf (component-compile-only component)
	    (when parent
	      (component-compile-only parent))))

    ;; Set up :compiler-options attribute
    (unless (find :compiler-options definition-body)
      (setf (component-compiler-options component)
	    (when parent
	      (component-compiler-options parent))))

    #|| ISI Extension ||#
    ;; Set up :load-always attribute
    (unless (find :load-always definition-body)
      (setf (component-load-always component)
	    (when parent
	      (component-load-always parent))))

    ;; Inherit package from parent if not specified.
    (setf (component-package component)
	  (or (component-package component)
	      (when parent (component-package parent))))

    ;; Type specific setup:
    (when (or (typep component 'system) ; Now I can do this (2026-02-20).
              (eq type :defsystem) (eq type :system) (eq type :subsystem))
      (setf (get-system name) component)
      #|(unless (component-language component)
	(setf (component-language component) :lisp))|#
      )

    ;; Set up the component's language.
    (init-component-language component parent)

    ;; Set up the component's compiler and loader.
    (init-component-operators component parent)

    ;; Set up the component's pathname
    (init-component-pathnames component parent)

    ;; At this point the pathname related slots are all filled up with
    ;; either NIL, or a value which was provided by the user.
    ;; Now we can set up all the extra bits and pieces for the component
    ;; pathnames. 
    
    ;; Set up pathname defaults -- expand with parent
    ;; We must set up the source pathname before the binary pathname
    ;; to allow distribution of binaries among the sources to work.
    
    (tell-user-info "ICP: done initializing.~2%")

    (finalize-component-pathnames component parent :source)
    (tell-user-info "FCP: source-pathname ~S~%" (component-pathname component :source))

    (finalize-component-pathnames component parent :binary)
    (tell-user-info "FCP: binary-pathname ~S~%" (component-pathname component :binary))
    (tell-user-info "FCP: done finalizing.~2%")

    ;; If there are any components of the component, expand them too.
    (expand-component-components component (+ indent 2))

    ;; Make depends-on refer to structs instead of names.
    (link-component-depends-on (component-components component))

    ;; Design Decision: Topologically sort the dependency graph at
    ;; time of definition instead of at time of use. Probably saves a
    ;; little bit of time for the user.

    ;; Topological Sort the components at this level.
    (setf (component-components component)
          (topological-sort (component-components component)))

    ;; Return the component.
    component))


;;; preprocess-component-definition --
;;; New function introduced to manipulate the "evaluated" slots as per
;;; SDS' suggestions.
;;; 20050824 MA
;;;
;;; 20220204 MA; This is buggy as it ovverides the defaults if the
;;; slots are not given in the DEFSYSTEM form, need to fix it!

(defun preprocess-component-definition (definition-body)
  `(list* ,@(loop for slot in *component-evaluated-slots*
	          for value = (getf definition-body slot)
	          when value
                    do (remf definition-body slot)
                    and nconc `(,slot ,value))
	  ,@(loop for slot in *component-form-slots*
		  for form = (getf definition-body slot)
                  when form
                    do (remf definition-body slot)
                    and nconc `(,slot (lambda () ,form)))
	  ',definition-body))


;;; defsystem --
;;;
;;; The main macro.
;;;
;;; 2002-11-22 Marco Antoniotti
;;; Added code to achieve a first cut "pathname less" operation,
;;; following the ideas in ASDF.  If the DEFSYSTEM form is loaded from
;;; a file, then the location of the file (intended as a directory) is
;;; computed from *LOAD-PATHNAME* and stored as the :SOURCE-PATHNAME
;;; of the system.

;;; 2020-09-07: ma: Check madhu's version with defsystem definition
;;;                 recording.
;;; 2026-03-03: ma: Switched from *load-pathname* to *load-truename*.
;;; 2026-03-04: ma: Maybe keep the *load-truename*?
;;; 2026-03-09: ma: Reworked.

#+old
(defmacro defsystem (name &rest definition-body)
  (unless (find :source-pathname definition-body)
    (setf definition-body
	  (list* :source-pathname
		 '(when *load-truename* ; Was *load-pathname*
                    (make-pathname :name nil
                                   :type nil
                                   :defaults *load-truename*
                                   ))
		 definition-body)))

  `(progn
     (tell-user-info "DEFSYSTEM: *load-pathname* is ~S~%" *load-pathname*)
     (tell-user-info "DEFSYSTEM: *load-truename* is ~S~%" *load-truename*)
     (tell-user-info "DEFSYSTEM: *d-p-d* is ~S~2%" *default-pathname-defaults*)
     (create-component :defsystem ',name
                       ,(preprocess-component-definition definition-body)
                       nil
                       0)))


(defmacro defsystem (name &rest definition-body)
  "Define a system.

This macro defines a system and it is usually present in a separate
file, mostly by itself.

It is essentially a wrapper around the 'factory' CREATE-COMPONENT.

See Also:

CREATE-COMPONENT."

  `(progn

     (tell-user-info "DEFSYSTEM: *load-pathname* is ~S~%" *load-pathname*)
     (tell-user-info "DEFSYSTEM: *load-truename* is ~S~%" *load-truename*)
     (tell-user-info "DEFSYSTEM: *d-p-d* is ~S~2%" *default-pathname-defaults*)

     (create-component :defsystem ',name
                       ,(preprocess-component-definition definition-body)
                       nil
                       0)))


(defun init-component-language (component parent)
  ;; Set up language-specific defaults.

  (setf (component-language component)
	(or (component-language component) ; for local defaulting
	    (when parent		   ; parent's default
	      (component-language parent))))

  (tell-user-info "ICL: language ~S~%" (component-language component))

  (component-language component))


(defun init-component-operators (component parent)
  ;; Init the 'compiler' and 'loader' operations.

 (setf (component-compiler component)
	(or (component-compiler component) ; for local defaulting
	    (when parent		   ; parent's default
	      (component-compiler parent))))

  (tell-user-info "ICO: compiler ~S~%" (component-compiler component))

  (setf (component-loader component)
	(or (component-loader component) ; for local defaulting
	    (when parent		 ; parent's default
	      (component-loader parent))))

  (tell-user-info "ICO: loader ~S~%" (component-loader component))

  (values (component-compiler component)
          (component-loader component))
  )


(defun init-component-pathnames (component parent)
  ;; Set up the creation-time component pathnames.
  ;; This step essentially deals with source level slot
  ;; initializations, plus the special treatment of :defsystem
  ;; definitions assumed to be loaded from a file.

  (tell-user-info "ICP: component ~S, type ~S, parent ~S~%"
                  (component-name component)
                  (component-type component)
                  parent)
  (tell-user-info "ICP: *load-truename* ~S~%" *load-truename*)
  (tell-user-info "ICP: *load-pathname* ~S~%" *load-pathname*)

  (when (eq (component-type component) :defsystem)
    (unless (component-source-pathname component)
      (setf (component-source-pathname component)
            *load-truename* ; NIL if not loading from a file.
            )))

  (tell-user-info "ICP: before: source-root-dir ~S~%" (component-root-dir component :source))

  ;; Evaluate the root dir arg
  (setf (component-root-dir component :source)
	(eval (component-root-dir component :source)))

  (tell-user-info "ICP: after:  source-root-dir ~S~%" (component-root-dir component :source))

  (tell-user-info "ICP: before: binary-root-dir ~S~%" (component-root-dir component :binary))

  (setf (component-root-dir component :binary)
	(eval (component-root-dir component :binary)))

  (tell-user-info "ICP: after:  binary-root-dir ~S~%" (component-root-dir component :binary))


  (tell-user-info "ICP: before: source-pathname ~S~%" (component-pathname component :source))
  ;; Evaluate the pathname arg
  (setf (component-pathname component :source)
	(eval (component-pathname component :source)))

  (tell-user-info "ICP: after:  source-pathname ~S~%" (component-pathname component :source))

  (tell-user-info "ICP: before: binary-pathname ~S~%" (component-pathname component :binary))

  (setf (component-pathname component :binary)
	(eval (component-pathname component :binary)))

  (tell-user-info "ICP: binary-pathname ~S~%" (component-pathname component :binary))

  ;; Pass along the host and devices
  (setf (component-host component)
	(or (component-host component)
	    (when parent (component-host parent))
	    (pathname-host *default-pathname-defaults*)))

  (setf (component-device component)
	(or (component-device component)
	    (when parent (component-device parent))
            (pathname-device *default-pathname-defaults*)))

  ;; Set up extension defaults
  (setf (component-extension component :source)
	(or (component-extension component :source
                                 :local #| (component-language component) |#
                                 t
                                 ) ; local default
            (when (component-language component)
              (default-source-extension component))
	    (when parent		; parent's default
	      (component-extension parent :source))))

  (setf (component-extension component :binary)
	(or (component-extension component :binary
                                 :local #| (component-language component) |#
                                 t
                                 ) ; local default
            (when (component-language component)
              (default-binary-extension component))
	    (when parent		; parent's default
	      (component-extension parent :binary))))
  )


(defun pathname-directory-pathname (cp)
  "Return a pathname that represents the directory of CP.

CP can be a PATHNAME, a STRING or a FILE-STREAM." 
  (make-pathname :name nil
                 :type nil
                 :defaults cp))


(defun name-type-as-dirname (name type)
  ;; Here :unspecific is treated as if we were printing stuff.  Cf.,
  ;; CLHS 19.2.2.2.3.1.

  (let ((name (if (eq name :unspecific) "" (or name "")))
        (type (unless (eq type :unspecific) type))
        )
    (declare (type string name)
             (type (or null string) type))

    (unless (and (null-string name)
                 (or (null type) (null-string type)))

      ;; Return NIL is effectively no name and no type.

      #+vms (format nil "~A~@[^.~A~]" name type) ; Quote the dot.
      #-vms (format nil "~A~@[.~A~]" name type)
      )))


(defun ensure-directory-pathname (cp)
  "Ensure that CP becomes a directory pathname."

  ;; This is just cl-fad:pathname-as-directory, always returning a
  ;; non-null pathname directory slot.

  (declare (type (or string pathname) cp))

  (etypecase cp
    (pathname
     (let ((cpd (pathname-directory cp))
           (cpn (pathname-name cp))
           (cpt (pathname-type cp))
           )
       (let ((name-as-dirname (name-type-as-dirname cpn cpt)))
         (make-pathname :name nil
                        :type nil
                        :directory (append (or cpd (list :relative))
                                           (and name-as-dirname
                                                (list name-as-dirname)))
                        :defaults cp))
       ))

    (string
     (cond ((null-string cp) *empty-relative-pathname*)

           ((logical-pathname-p cp)
            ;; Let's work on the translation; see above.
            (ensure-directory-pathname (translate-logical-pathname cp)))

           (t
            (let ((last-char (char cp (1- (length cp)))))

              (if (and last-char
                       (member last-char *fs-directory-terminators* :test #'char=))

                  ;; A string ending, e.g., on Windows with a #\\.

                  (pathname cp)

                  ;; Otherwise we have a string with a name (and maybe a
                  ;; type and a version).
                  ;; Let's unpack and work around a few annyoing
                  ;; "implementation dependent" stuff.

                  (flet ((is-unusable (pname-item)
                           (or (null pname-item)
                               (null-string pname-item)
                               (eq :unspecific pname-item)))
                         )
                    (let* ((cpp (pathname cp))
                           (cppn (pathname-name cpp))
                           (cppt (pathname-type cpp))
                           (cppd (pathname-directory cpp))
                           (nd
                            (let ((rcppd (or cppd (list :relative))))

                              ;; Assume CPPD is a proper directory list.

                              ;; Do we have a UNIX style hidden name? I.e.,
                              ;; something like .bashrc?

                              (if (is-unusable cppn) ; No usable name.

                                  (if (is-unusable cppt) ; No usable type.

                                      rcppd

                                      ;; Otherwise, attach the type as
                                      ;; the last directory element.

                                      (append rcppd
                                              (list (concatenate
                                                     'string
                                                     #+unix "."
                                                     #+vms "^." ; Need quoting.
                                                     #+windows "_" ; Kludge!!!!
                                                     cppt))))

                                  ;; We have a name.

                                  (if (is-unusable cppt) ; No usable type.

                                      (append rcppd (list cppn))

                                      ;; Otherwise, attach the
                                      ;; "name.type" as the last
                                      ;; directory element.

                                      (append rcppd
                                              (list (concatenate
                                                     'string
                                                     cppn
                                                     #+unix "."
                                                     #+vms "^." ; Need quoting.
                                                     #+windows "_" ; Kludge!!!!
                                                     cppt))))
                                  )))
                           )
                      (make-pathname :name nil
                                     :type nil
                                     :directory nd
                                     :defaults cpp)))))
            )))
    ))


;;; finalize-component-pathnames --
;;;
;;; This function is incredibly hairy, but it must take care of
;;; countless corner cases that result from the combination of File System
;;; conventions, implementation dependencies and, above all, luser
;;; inventiveness.
;;;
;;; Maybe file's inheriting of pathnames should be moved elsewhere?

#| #+old-version
(defun generate-component-pathnames (component parent pathname-type)

  ;; Pieces together a pathname for the component based on its component-type.
  ;; Assumes source pathname is defined first.
  ;; Null binary pathnames inherit from source instead of the component's
  ;; name. This allows binaries to be distributed among the source if
  ;; binary pathnames are not specified. Or, if the root directory is
  ;; specified for binaries, but no module directories, it inherits
  ;; parallel directory structure.

  (case (component-type component)
    ((:defsystem :system)		; Absolute Pathname

     ;; Set the root-dir to be an "absolute" pathname

     #| old-pre-20181028
     (setf (component-root-dir component pathname-type)
	   (or (component-pathname component pathname-type)
	       (when (eq pathname-type :binary)
		 ;; When the binary root is nil, use source.
		 (component-root-dir component :source))) )
     |#

     (let ((cp (component-pathname component pathname-type)))
       (tell-user-info "GPC: :defsystem or :system cp ~S~%" cp)
       (ccase pathname-type
         (:source ; :source has been pre-processed to be at least *load-pathname*
          (setf (component-root-dir component :source) cp))

         (:binary
          (let ((cbp ; ensure it is a folder.
                 (etypecase cp
                   (pathname (let ((cpn (pathname-name cp))
                                   (cpd (pathname-directory cp))
                                   )
                               (if (and cpn
                                        (not (eq :unspecific cpn)))
                                   (make-pathname :name nil
                                                  :type nil
                                                  :directory (append (if (null cpd)
                                                                         '(:relative)
                                                                         cpd)
                                                                     (and (not (null-string cpn))
                                                                          (list cpn)))
                                                  :defaults cp)
                                   cp)))
                   (string
                    (if (null-string cp)
                        nil
                        (let ((last-char (and (string/= "" cp)
                                              (char cp (1- (length cp)))))
                              )
                          (if (and last-char
                                   (member last-char '(#\/ #\: #\\) :test #'char=))
                              (pathname cp)
                              (let* ((cpp (pathname cp))
                                     (cppn (pathname-name cpp))
                                     (cppt (pathname-type cpp))
                                     (cppd (pathname-directory cpp))
                                     (nd
                                      (let ((rcppd (if (null cppd)
                                                       (list :relative)
                                                       cppd))
                                            )

                                        ;; Do we have a UNIX style hidden name?
                                        (if (or (null cppn)
                                                (null-string cppn)
                                                (eq :unspecific cppn))
                                            (if (or (null cppt)
                                                    (null-string cppt)
                                                    (eq :unspecific cppt))
                                                rcppd
                                                (append rcppd
                                                        (list (concatenate 'string
                                                                           #+unix "."
                                                                           #+windows "_" ; Kludge!!!!
                                                                           cppt))))

                                            (if (or (null cppt)
                                                    (null-string cppt)
                                                    (eq :unspecific cppt))
                                                (append rcppd (list cppn))
                                                (append rcppd
                                                        (list (concatenate 'string
                                                                           cppn
                                                                           #+unix "."
                                                                           #+windows "_" ; Kludge!!!!
                                                                           cppt))))
                                            )))
                                     )
                                (make-pathname :name nil
                                               :type nil
                                               :directory nd
                                               :defaults cpp))))))
                    
                   (null cp)))
                )
                          
            (cond ((and cbp
                        (eq :relative (first (pathname-directory (pathname cbp)))))
                   (setf (component-root-dir component :binary)
                         (merge-pathnames cbp
                                          (pathname (component-root-dir component :source)))))
                  (cbp
                   ;; CP is an absolute pathname.
                   (setf (component-root-dir component :binary)
                         cbp))

                  (t
                   ;; CP is empty.
                   (setf (component-root-dir component :binary)
                         (component-root-dir component :source)))
                  )
            )))

 
       ;; Set the relative pathname to be nil.
       (setf (component-pathname component pathname-type)
             nil))) ; should this be "" instead?


    ;; If the name of the component-pathname is nil, it
    ;; defaults to the name of the component. Use "" to
    ;; avoid this defaulting.

    (:private-file                      ; Absolute Pathname
     ;; Root-dir is the directory part of the pathname
     (setf (component-root-dir component pathname-type)
	   ""
	   #+ignore
           (or (when (component-pathname component pathname-type)
                 (pathname-directory
                  (component-pathname component pathname-type)))
               (when (eq pathname-type :binary)
                 ;; When the binary root is nil, use source.
                 (component-root-dir component :source)))
	   )
     ;; If *SOURCE-PATHNAME-DEFAULT* or *BINARY-PATHNAME-DEFAULT* is "",
     ;; then COMPONENT-SOURCE-PATHNAME or COMPONENT-BINARY-PATHNAME could
     ;; wind up being "", which is wrong for :file components. So replace
     ;; them with NIL.

     (when (null-string (component-pathname component pathname-type))
       (setf (component-pathname component pathname-type) nil))

     ;; The relative pathname is the name part
     (setf (component-pathname component pathname-type)
	   (or (when (and (eq pathname-type :binary)
			  (null (component-pathname component :binary)))
		 ;; When the binary-pathname is nil use source.
		 (component-pathname component :source))
	       (or (when (component-pathname component pathname-type)
                     ;; (pathname-name )
		     (component-pathname component pathname-type))
		   (component-name component)))))


    ((:module :subsystem)			; Pathname relative to parent.
     ;; Inherit root-dir from parent
     (setf (component-root-dir component pathname-type)
	   (component-root-dir parent pathname-type))

     ;; Tack the relative-dir onto the pathname
     (setf (component-pathname component pathname-type)
	   (or (when (and (eq pathname-type :binary)
			  (null (component-pathname component :binary)))
		 ;; When the binary-pathname is nil use source.
		 (component-pathname component :source))
	       (append-directories
		(component-pathname parent pathname-type)
		(or (component-pathname component pathname-type)
		    (component-name component))))))


    (:file				; Pathname relative to parent.
     ;; Inherit root-dir from parent
     (setf (component-root-dir component pathname-type)
	   (component-root-dir parent pathname-type))

     ;; If *SOURCE-PATHNAME-DEFAULT* or *BINARY-PATHNAME-DEFAULT* is "",
     ;; then COMPONENT-SOURCE-PATHNAME or COMPONENT-BINARY-PATHNAME could
     ;; wind up being "", which is wrong for :file components. So replace
     ;; them with NIL.

     (when (null-string (component-pathname component pathname-type))
       (setf (component-pathname component pathname-type) nil))

     ;; Tack the relative-dir onto the pathname
     (setf (component-pathname component pathname-type)
	   (or (append-directories
		(component-pathname parent pathname-type)
		(or (component-pathname component pathname-type)
		    (component-name component)
		    (when (eq pathname-type :binary)
		      ;; When the binary-pathname is nil use source.
		      (component-pathname component :source)))))))
    ))
|#

;;; SBCL pre declarations.

(declaim (ftype (function (component t keyword) t #| component |# )
                ;; finalize-component-pathnames
                finalize-system-pathnames
                finalize-private-file-pathnames
                finalize-module-pathnames
                finalize-file-pathnames))


(defun finalize-system-pathnames (component
                                  parent
                                  pathname-type
                                  &aux
                                  (cp (component-pathname component
                                                          pathname-type))
                                  (loading-p (not (null *load-truename*))))
  
  ;; Auxiliary function for finalize-component-pathnames.

  (declare (ignore parent)
           (type keyword pathname-type))

  ;; These component types have absolute pathnames.
  (assert (member (component-type component)
                  '(:defsystem :system)
                  :test #'eq))

  (tell-user-info "FPC: :defsystem or :system ~S cp ~S~%"
                  pathname-type cp)

  (labels ((ensure-cp (cp)
             ;; CP may be NIL, something supplied by the user or
             ;; *load-truename* (for :defsystem :source)
             (etypecase cp
               (null
                (ecase pathname-type
                  (:source
                   (make-pathname
                      :name (component-name component)
                      :type *system-extension*
                      :defaults (mk::current-working-directory) ; Paranoid.
                      ))
                  (:binary
                   (make-pathname
                      :name (component-name component)
                      :type (component-extension component :binary)
                      :defaults (if (component-root-dir component :source)
                                    (component-root-dir component :source)
                                    (mk::current-working-directory)) ; Paranoid.
                      ))
                  )) ; NULL case.

               (pathname
                ;; Interesting case.  Is CP *LOAD-TRUENAME* or it is
                ;; something else?
                (cond ((and loading-p
                            (equalp (namestring cp)
                                    (namestring *load-truename*)))
                       ;; Yep, we just loaded the system from a file.
                       cp)

                      (loading-p
                       ;; CP is something user supplied.
                       ;; It should denote a directory.
                       ;; Maybe issue a warning here.
                       (ensure-directory-pathname cp))

                      (t
                       ;; Ok. Consequence of the COND form.
                       (ensure-directory-pathname cp)))
                ) ; PATHNAME

               (string
                (ensure-cp (pathname cp))
                ) ; STRING
               ))
           )
    (let ((cp (ensure-cp cp)))
      (declare (type pathname cp))

      ;; No check for pathological cases yet in the above.

      (tell-user-info "FPC: :defsystem or :system pathname ~S CP is ~S~%"
                      pathname-type cp)

      (ccase pathname-type
        (:source ; CP is the :source pathname.
       
         (let ((cspd (pathname-directory-pathname cp)))
           ;; CP may have been a string like, e.g., "syssrc".
           ;; For systems it must be forced to be a directory.
         
           ;; The code below is redundant and repetitive, but let me
           ;; write it this way.

           (if (absolute-pathname-p cspd)

               ;; Set the root-dir to be an "absolute" pathname
               (setf (component-root-dir component :source)
                     (make-pathname :name nil
                                    :type nil
                                    :defaults cspd))

               ;; else (relative-pathname-p cspd); yep, it will be so.

               ;; Set the root-dir to the "relative" pathname.
               ;; This should happen only with a user supplied
               ;; :source-pathname.
               (setf (component-root-dir component :source)
                     (make-pathname :name nil
                                    :type nil
                                    :defaults cspd))
               )))

        (:binary ; CP is the :binary pathname.

         (let ((cbpd (pathname-directory-pathname cp))) ; Ensure it is a folder.
           (declare (type pathname cbpd))

           (tell-user-info "FPC: directory pathname for :binary is ~S~%"
                           cbpd)
           (tell-user-info "FPC: :source root-dir is ~S~%"
                           (component-root-dir component))

           (cond ((absolute-pathname-p cp) ; CP is an absolute pathname.
                  (setf (component-root-dir component :binary) cbpd))
               
                 ((relative-pathname-p cp)
                  (setf (component-root-dir component :binary)
                        (merge-pathnames cbpd
                                         (pathname (component-root-dir component :source)))))

                 (t ; CP is "empty".
                  (setf (component-root-dir component :binary)
                        (component-root-dir component :source)))
                 )

           (tell-user-info "FPC: :binary root-dir is ~S~%"
                           (component-root-dir component :binary))
           ))
        ))
    component
    ))


(defun finalize-private-file-pathnames (component
                                        parent
                                        pathname-type
                                        &aux
                                        (cp (component-pathname component
                                                                pathname-type)))

  (declare (ignore parent)
           (type keyword pathname-type))

  (assert (eq (component-type component) :private-file))

  ;; The semantics of :private-file is that its pathnames must be
  ;; absolute.

  ;; CP is either a pathnname (:source or :binary) or it is either
  ;; *source-pathname-default*, *binary-pathname-default*, or NIL.
  ;; In the the last two cases it is as if it was not user
  ;; initialized, in which case I need to check the component name.

  (when (or (equal *source-pathname-default* cp) ; :source case
            (equal *binary-pathname-default* cp) ; :binary case
            (null cp)
            )
    ;; Nothing was specified right here by the user, if not to
    ;; indicate defaulting.
    
    ;; The component-pathname must be built from the component-name.

    (ccase pathname-type
      (:source
       (let ((cnp (pathname (component-name component))))
         (setf (component-pathname component :source)
               (make-pathname
                :type (component-extension component :source)
                :defaults cnp)

               cp
               (component-pathname component :source)
               )))

      (:binary
       ;; Binary is build *after* source.
       (setf (component-pathname component :binary)
             (make-pathname
              :type (component-extension component :binary)
              :defaults (component-pathname component :source))

             cp
             (component-pathname component :binary)
             ))
      ))

  ;; At this point either we have a user supplied component-pathname
  ;; or we built one from the the COMPONENT-NAME.  It is however, in
  ;; CP.

  ;; Check that it is an absolute pathname.

  (unless (absolute-pathname-p cp)
    (tell-user-error "pathname ~A for ~S :private-file ~S is not absolute~%"
                     cp
                     pathname-type
                     (component-name component)
                     ))
  
  ;; Root-dir is the directory part of the pathname and it is unused
  ;; for :private-file's.

  (setf (component-root-dir component pathname-type)
        *empty-pathname*)

  component)

#| #+original-code
(defun finalize-module-pathnames (component
                                  parent
                                  pathname-type
                                  &aux
                                  (cp (component-pathname component
                                                          pathname-type)))
  (declare (ignore cp))

  ;; Inherit root-dir from parent
  (setf (component-root-dir component pathname-type)
        (component-root-dir parent pathname-type))
  
  ;; Tack the relative-dir onto the pathname
  (setf (component-pathname component pathname-type)
        (or (when (and (eq pathname-type :binary)
                       (null (component-pathname component :binary)))
              ;; When the binary-pathname is nil use source.
              (component-pathname component :source))
            (append-directories
             (component-pathname parent pathname-type)
             (or (component-pathname component pathname-type)
                 (component-name component)))))
  component
  )
|#

;;; finalize-module-pathname
;;;
;;; New code patterned after FINALIZE-PRIVATE-FILE-PATHNAME.
;;;
;;; As usual, this function is called first with PATHNAME-TYPE :SOURCE
;;; and only afterward with PATHNAME-TYPE :BINARY.

(defun finalize-module-pathnames (component
                                  parent
                                  pathname-type
                                  &aux
                                  (cp (component-pathname component
                                                          pathname-type)))

  (assert (member (component-type component) '(:subsystem :module)
                  :test #'eq))

  (tell-user-info "FMP: root dir ~S before: ~S~%"
                  pathname-type
                  (component-root-dir component pathname-type))

  ;; Inherit root-dir from parent.
  (setf (component-root-dir component pathname-type)
        (component-root-dir parent pathname-type))

  (tell-user-info "FMP: root dir ~S after: ~S~%"
                  pathname-type
                  (component-root-dir component pathname-type))


  ;; CP is either a pathnname (:source or :binary) or it is either
  ;; *source-pathname-default*, *binary-pathname-default*, or NIL.
  ;; In the the last two cases it is as if it was not user
  ;; initialized, in which case I need to check the component name.

  (when (or (equal *source-pathname-default* cp) ; :source case.
            (equal *binary-pathname-default* cp) ; :binary case.
            (null cp)
            )

    ;; Nothing was specified by the user, if not to indicate
    ;; defaulting right here.
    ;; The component-pathname must be built from the component-name.

    (setf (component-pathname component pathname-type)
          (pathname (component-name component))

          cp
          (component-pathname component pathname-type)))

  (tell-user-info "FMP: CP ~S is initially ~S~%"
                  pathname-type
                  cp)

  ;; Now we have a usable CP for PATHNAME-TYPE.  We merge it with the
  ;; root dir, IF it is a relative pathname.  But first we must ensure
  ;; that it is shaped as a directory.

  (setf (component-pathname component pathname-type)
        (ensure-directory-pathname cp)

        cp
        (component-pathname component pathname-type))

  (tell-user-info "FMP: CP ~S is now ~S~%"
                  pathname-type
                  cp)

  (unless (relative-pathname-p cp)
    ;; If it was built just before, we are home free.
    (tell-user-error "FMP: component pathname for module ~S is not relative.~%"
                     (component-name component)))

  (setf (component-pathname component pathname-type)
        (merge-pathnames cp (component-root-dir component pathname-type)) 
        
        (component-root-dir component pathname-type)
        (component-pathname component pathname-type) ; Need to update this.
        )

  (tell-user-info "FMP: CP ~S is finally ~S~%"
                  pathname-type
                  (component-pathname component pathname-type))

  ;; And we are done...
  component
  )


#| #+original-code
(defun finalize-file-pathnames (component
                                parent
                                pathname-type
                                &aux
                                (cp (component-pathname component
                                                        pathname-type)))


  (assert (eq (component-type component) :file))

  ;; Inherit root-dir from parent
  (setf (component-root-dir component pathname-type)
        (component-root-dir parent pathname-type))

  ;; If *SOURCE-PATHNAME-DEFAULT* or *BINARY-PATHNAME-DEFAULT* is "",
  ;; then COMPONENT-SOURCE-PATHNAME or COMPONENT-BINARY-PATHNAME could
  ;; wind up being "", which is wrong for :file components. So replace
  ;; them with NIL.

  (when (null-string (component-pathname component pathname-type))
    (setf (component-pathname component pathname-type) nil))

  ;; Tack the relative-dir onto the pathname
  (setf (component-pathname component pathname-type)
        (or (append-directories
             (component-pathname parent pathname-type)
             (or (component-pathname component pathname-type)
                 (component-name component)
                 (when (eq pathname-type :binary)
                   ;; When the binary-pathname is nil use source.
                   (component-pathname component :source))))))
  component
  )
|#


;;; finalize-file-pathname
;;;
;;; New code patterned after FINALIZE-PRIVATE-FILE-PATHNAME.
;;;
;;; As usual, this function is called first with PATHNAME-TYPE :SOURCE
;;; and only afterward with PATHNAME-TYPE :BINARY.

(defun finalize-file-pathnames (component
                                parent
                                pathname-type
                                &aux
                                (cp (component-pathname component
                                                        pathname-type)))


  (assert (or (eq (component-type component) :file)
              (eq (component-type component) :static-file)))

  ;; CP is either a pathnname (:source or :binary) or it is either
  ;; *source-pathname-default*, *binary-pathname-default*, or NIL.
  ;; In the the last two cases it is as if it was not user
  ;; initialized, in which case I need to check the component name.

  (when (or (equal *source-pathname-default* cp) ; :source case.
            (equal *binary-pathname-default* cp) ; :binary case.
            (null cp)
            )

    ;; Nothing was specified by the user, if not to indicate
    ;; defaulting right here.
    ;; The component-pathname must be built from the component-name.

    (setf (component-pathname component pathname-type)
          (make-pathname :type (component-extension component pathname-type)
                         :defaults (component-name component))

          cp
          (component-pathname component pathname-type)))

  ;; Now we have a usable CP for PATHNAME-TYPE.  We merge it with the
  ;; root dir.

  ;; Inherit root-dir from parent; important.  Note that PARENT is a
  ;; MODULE.

  (setf (component-root-dir component pathname-type)
        (component-root-dir parent pathname-type)
        )

  (setf (component-pathname component pathname-type)
        (merge-pathnames cp
                         (component-root-dir component pathname-type)))

  (tell-user-info "FMP: CP ~S is finally ~S~%"
                  pathname-type
                  (component-pathname component pathname-type))
  ;; We are done.
  component
  )


(defun finalize-component-pathnames (component parent pathname-type)
  "Piece together a pathname for the COMPONENT, based on its component-type.
  
PARENT is the COMPONENT's parent and PATHNAME-TYPE is :source, :binary
or :error.

The function assumes that the source pathname has already been
defined.

Null binary pathnames inherit from source instead of the COMPONENT's
name. This allows binaries to be distributed among the source if
binary pathnames are not specified. Or, if the root directory is
specified for binaries, but no module directories, it inherits
parallel directory structure."

  (tell-user-info "FPC: ~S ~S ~S~%"
                  (component-name component)
                  parent
                  pathname-type)
  (tell-user-info "FPC: :source root-dir is ~S~%"
                  (component-root-dir component))
  (tell-user-info "FPC: :binary root-dir is ~S~%"
                  (component-root-dir component :binary))

  (let ((cp (component-pathname component pathname-type)))
    ;; CP may still be NIL here.
    (declare (type (or null string pathname) cp))
    (declare (ignore cp))

    (case (component-type component)
      ((:defsystem :system) ; These component types have absolute pathnames.

       (finalize-system-pathnames component parent pathname-type)
       )

      ;; If the name of the component-pathname is nil, it
      ;; defaults to the name of the component. Use "" to
      ;; avoid this defaulting.

      (:private-file ; :private-files have absolute pathnames.

       (finalize-private-file-pathnames component parent pathname-type)

       #|
       ;; Root-dir is the directory part of the pathname
       (setf (component-root-dir component pathname-type)
             *empty-pathname*)

       ;; If *SOURCE-PATHNAME-DEFAULT* or *BINARY-PATHNAME-DEFAULT* is "",
       ;; then COMPONENT-SOURCE-PATHNAME or COMPONENT-BINARY-PATHNAME could
       ;; wind up being "", which is wrong for :file components. So replace
       ;; them with NIL.

       (when (null-string (component-pathname component pathname-type))
         (setf (component-pathname component pathname-type) nil))

       ;; The relative pathname is the name part.

       (setf (component-pathname component pathname-type)
             (or (when (and (eq pathname-type :binary)
                            (null (component-pathname component :binary)))
                   ;; When the binary-pathname is nil use source.
                   (component-pathname component :source))
                 (or (when (component-pathname component pathname-type)
                       (component-pathname component pathname-type))
                     (component-name component))))
       |#
       )


      ((:module :subsystem) ; :module and :subsystem have pathnames
                            ; relative to parent.

       (finalize-module-pathnames component parent pathname-type)
       #|
       
       ;; Inherit root-dir from parent
       (setf (component-root-dir component pathname-type)
             (component-root-dir parent pathname-type))

       ;; Tack the relative-dir onto the pathname
       (setf (component-pathname component pathname-type)
             (or (when (and (eq pathname-type :binary)
                            (null (component-pathname component :binary)))
                   ;; When the binary-pathname is nil use source.
                   (component-pathname component :source))
                 (append-directories
                  (component-pathname parent pathname-type)
                  (or (component-pathname component pathname-type)
                      (component-name component)))))                           
        |#
       )


      ((:file :static-file) ; :file components have pathnames relative to parent.
       (finalize-file-pathnames component parent pathname-type)

       #|
       ;; Inherit root-dir from parent
       (setf (component-root-dir component pathname-type)
             (component-root-dir parent pathname-type))

       ;; If *SOURCE-PATHNAME-DEFAULT* or *BINARY-PATHNAME-DEFAULT* is "",
       ;; then COMPONENT-SOURCE-PATHNAME or COMPONENT-BINARY-PATHNAME could
       ;; wind up being "", which is wrong for :file components. So replace
       ;; them with NIL.

       (when (null-string (component-pathname component pathname-type))
         (setf (component-pathname component pathname-type) nil))

       ;; Tack the relative-dir onto the pathname
       (setf (component-pathname component pathname-type)
             (or (append-directories
                  (component-pathname parent pathname-type)
                  (or (component-pathname component pathname-type)
                      (component-name component)
                      (when (eq pathname-type :binary)
                        ;; When the binary-pathname is nil use source.
                        (component-pathname component :source))))))
       |#
       )
      )))



#|| ;; old version
(defun expand-component-components (component &optional (indent 0))
  (let ((definitions (component-components component)))
    (setf (component-components component)
	  (remove-if #'null
		     (mapcar #'(lambda (definition)
				 (expand-component-definition definition
							      component
							      indent))
			     definitions)))))
||#

;;; new version
(defun expand-component-components (component &optional (indent 0))
  (let ((definitions (component-components component)))
    (if (eq (car definitions) :serial)
	(setf (component-components component)
	      (expand-serial-component-chain (rest definitions)
					     component
                                             indent))
	(setf (component-components component)
	      (expand-component-definitions definitions
                                            component
                                            indent)))))


(defun expand-component-definitions (definitions parent &optional (indent 0))
  (let ((components nil))
    (dolist (definition definitions)
      (let ((new (expand-component-definition definition parent indent)))
	(when new (push new components))))
    (nreverse components)))


(defun expand-serial-component-chain (definitions parent &optional (indent 0))
  (let ((previous nil)
	(components nil))
    (dolist (definition definitions)
      (let ((new (expand-component-definition definition parent indent)))
	(when new
	  ;; Make this component depend on the previous one. Since
	  ;; we don't know the form of the definition, we have to
	  ;; expand it first.
	  (when previous (pushnew previous (component-depends-on new)))
	  ;; The dependencies will be linked later, so we use the name
	  ;; instead of the actual component.
	  (setq previous (component-name new))
	  ;; Save the new component.
	  (push new components))))
    ;; Return the list of expanded components, in appropriate order.
    (nreverse components)))


(defparameter *enable-straz-absolute-string-hack* nil
  "Special hack requested by Steve Strassman, where the shorthand
   that specifies a list of components as a list of strings also
   recognizes absolute pathnames and treats them as files of type
   :private-file instead of type :file. Defaults to NIL, because I
   haven't tested this.")


(defun absolute-file-namestring-p (string)

  ;; If a FILE namestring starts with a slash, or is a logical pathname
  ;; as implied by the existence of a colon in the filename, assume it
  ;; represents an absolute pathname.

  (or (find #\: string :test #'char=) ; Whould test the LP translation.
      (and (not (null-string string))
	   (fs-directory-separator-p (char string 0)))))


(defun expand-component-definition (definition parent &optional (indent 0))
  ;; Should do some checking for malformed definitions here.

  (cond ((null definition) nil)

        ((stringp definition)
         ;; Strings are assumed to be of type :file
	 (if (and *enable-straz-absolute-string-hack*
		  (absolute-file-namestring-p definition))
	     ;; Special hack for Straz
	     (create-component :private-file definition nil parent indent)
	     ;; Normal behavior
	     (create-component :file definition nil parent indent)))

        ((and (listp definition)
              (not (typep (first definition) 'standard-components)))

         ;; Lists whose first element is not a component type
         ;; are assumed to be of type :file.  The first element of the
         ;; list is considered the 'name' of the component.

         (create-component :file
			   (first definition)
			   ;; (preprocess-component-definition (rest definition)) ; Not working.
                           (rest definition)
			   parent
			   indent))

        ((listp definition)
         ;; Otherwise, it is (we hope) a normal form definition
         (create-component (first definition)   ; type
                           (second definition)  ; name

			   ;; definition body
                           ;; (preprocess-component-definition (cddr definition)) ; Not working.
                           (cddr definition)

                           parent             ; parent
			   indent)            ; indent
         )))


(defun link-component-depends-on (components)
  (dolist (component components)
    (unless (and *system-dependencies-delayed*
                 (eq (component-type component) :defsystem))
      (setf (component-depends-on component)
            (mapcar #'(lambda (dependency)
			(let ((parent (find (string dependency) components
					    :key #'component-name
					    :test #'string-equal)))
			  (cond (parent parent)
				;; make it more intelligent about the following
				(t
                                 ;; Used to be a WARN.
                                 (error "Dependency ~S of component ~S not found."
                                        dependency component)))))

                    (component-depends-on component))))))


;;; Topological Sort of the Dependency Graph
;;; ----------------------------------------

;;; New version of topological sort suggested by rs2. Even though
;;; this version avoids the call to sort, in practice it isn't faster. It
;;; does, however, eliminate the need to have a TIME slot in the
;;; topological-sort-node defstruct.

(defun topological-sort (list &aux (sorted-list nil))
  (labels ((dfs-visit (znode)
             (setf (topsort-color znode) :gray)
             (unless (and *system-dependencies-delayed*
                          (eq (component-type znode) :system))
               (dolist (child (component-depends-on znode))
                 (cond ((eq (topsort-color child) :white)
                        (dfs-visit child))
                       ((eq (topsort-color child) :gray)
                        (format t "~&Detected cycle containing ~A" child)))))
             (setf (topsort-color znode) :black)
             (push znode sorted-list)))
    (dolist (znode list)
      (setf (topsort-color znode) :white))
    (dolist (znode list)
      (when (eq (topsort-color znode) :white)
        (dfs-visit znode)))
    (nreverse sorted-list)))

#||
;;; Older version of topological sort.
(defun topological-sort (list &aux (time 0))
  ;; The algorithm works by calling depth-first-search to compute the
  ;; blackening times for each vertex, and then sorts the vertices into
  ;; reverse order by blackening time.
  (labels ((dfs-visit (node)
	      (setf (topsort-color node) 'gray)
	      (unless (and *system-dependencies-delayed*
			   (eq (component-type node) :defsystem))
		(dolist (child (component-depends-on node))
		  (cond ((eq (topsort-color child) 'white)
			 (dfs-visit child))
			((eq (topsort-color child) 'gray)
			 (format t "~&Detected cycle containing ~A" child)))))
		      (setf (topsort-color node) 'black)
		      (setf (topsort-time node) time)
		      (incf time)))
    (dolist (node list)
      (setf (topsort-color node) 'white))
    (dolist (node list)
      (when (eq (topsort-color node) 'white)
        (dfs-visit node)))
    (sort list #'< :key #'topsort-time)))
||#


;;; More component's inspection
;;; ---------------------------

(defun list-component-components (c)
  "Return an inorder list of components rooted at C."
  (declare (type component c))
  (cons c (mapcan #'list-component-components (component-components c))))


;;; Output to User
;;; --------------
;;;
;;; All output to the user is via the tell-user functions.

;;; prompt-string --
;;;
;;; Probably should remove the ",1" entirely. But AKCL 1.243 dies on it
;;; because of an AKCL bug.
;;; KGK suggests using an 8 instead, but 1 does nicely.

(defun prompt-string (component)
  (format nil "; MK:~:[~; TEST:~]~V,1@T "
	  *oos-test*
	  (component-indent component)))


#||
(defun format-justified-string (prompt contents)
  (format t (concatenate 'string
			 "~%"
			 prompt
			 "-~{~<~%" prompt " ~1,80:; ~A~>~^~}")
	  (split-string contents))
  (finish-output *standard-output*))
||#


(defun format-justified-string (prompt contents
                                       &optional
                                       (width 80)
				       (stream *standard-output*))
  (let ((prompt-length (+ 2 (length prompt))))
    (cond ((< (+ prompt-length (length contents)) width)
	   (format stream "~%~A- ~A." prompt contents))
	  (t
	   (format stream "~%~A-" prompt)
	   (do* ((cursor prompt-length)
		 (contents (split-string contents) (cdr contents))
		 (content (car contents) (car contents))
		 (content-length (1+ (length content)) (1+ (length content))))
	       ((null contents) (format stream "."))
	     (cond ((< (+ cursor content-length) width)
		    (incf cursor content-length)
		    (format stream " ~A" content))
		   (t
		    (setf cursor (+ prompt-length content-length))
		    (format stream "~%~A  ~A" prompt content)))))))
  (finish-output stream))


(defun tell-user (what component &optional type no-dots force)
  (when (or *oos-verbose* force)
    (format-justified-string (prompt-string component)
     (format nil "~A ~(~A~) ~@[\"~A\"~]~:[~; ..~]"

	     ;; To have better messages, wrap the following around the
	     ;; case statement:
	     ;;(if (find (component-type component)
	     ;;    '(:defsystem :system :subsystem :module))
	     ;;  "Checking"
	     ;;  (case ...))
	     ;; This gets around the problem of DEFSYSTEM reporting
	     ;; that it's loading a module, when it eventually never
	     ;; loads any of the files of the module.

	     (case what
	       ((compile :compile)
		(if (component-load-only component)
		    ;; If it is :load-only t, we're loading.
		    "Loading"
		    ;; Otherwise we're compiling.
		    "Compiling"))

	       ((load :load) "Loading")

	       (otherwise what))

	     (component-type component)

	     (or (when type
		   (component-full-pathname component type))
		 (component-name component))

	     (and *tell-user-when-done*
		  (not no-dots))))
  ))


(defun tell-user-done (component &optional force no-dots)
  ;; Test is no longer really used, but we're leaving it in.
  (when (and *tell-user-when-done*
	     (or *oos-verbose* force))
    (format t "~&; MK: ~A~:[~;...~] Done."
	    (prompt-string component) (not no-dots))
    (finish-output *standard-output*)))


(defmacro with-tell-user ((what component &optional type no-dots force) &body body)
  `(progn
     (tell-user ,what ,component ,type ,no-dots ,force)
     ,@body
     (tell-user-done ,component ,force ,no-dots)))


(defun tell-user-no-files (component &optional force)
  (when (or *oos-verbose* force)
    (format-justified-string (prompt-string component)
      (format nil "~&; MK: Source file ~A ~
                   ~:[and binary file ~A ~;~]not found, not loading."
	      (component-full-pathname component :source)
	      (or *load-source-if-no-binary* *load-source-instead-of-binary*)
	      (component-full-pathname component :binary)))))


(defun tell-user-require-system (name parent)
  (when *oos-verbose*
    (format t "~&; MK: ~:[~;TEST:~] System ~A requires ~S"
	    *oos-test* (component-name parent) name)
    (finish-output *standard-output*)))


(defun tell-user-generic (format-control &rest args)
  (when *oos-verbose*
    (format t "~&; MK:~:[ ~; TEST: ~]~?"
	    *oos-test*
            format-control
            args)
    (finish-output *standard-output*)))


(defun tell-user-info (format-control &rest args)
  (when *oos-verbose*
    (format t "~&; MK:~:[ ~; TEST: ~]INFO: ~?"
            *oos-test*
            format-control
            args)
    (finish-output *standard-output*)))


(defun tell-user-debug (format-control &rest args)
  (when *defsystem-oos-debug*
    (format t "~&; MK:~:[ ~; TEST: ~]DEBUG: ~?"
            *oos-test*
            format-control
            args)
    (finish-output *standard-output*)))


(defun tell-user-error (format-control &rest args)
  (error (format nil "~&; MK:~:[ ~; TEST: ~]ERROR: ~?"
                 *oos-test*
                 format-control
                 args)))


(defun tell-user-warn (format-control &rest args)
  (warn (format nil "~&; MK:~:[ ~; TEST: ~]WARN: ~?"
                *oos-test*
                format-control
                args)))


;;; y-or-n-p-wait --
;;;
;;; Y-OR-N-P-WAIT is like Y-OR-N-P, but will timeout after a specified
;;; number of seconds. I should really replace this with a call to
;;; the Y-OR-N-P-WAIT defined in the query.cl package and include that
;;; instead.

(defparameter *use-timeouts* t
  "If T, timeouts in Y-OR-N-P-WAIT are enabled.

 Otherwise it behaves like Y-OR-N-P. This is provided for users whose
lisps don't handle read-char-no-hang properly.")


(defparameter *clear-input-before-query* t
  "If T, y-or-n-p-wait will clear the input before printing the prompt
and asking the user for input.")


;;; The higher *sleep-amount* is, the less consing, but the lower the
;;; responsiveness.

(defparameter *sleep-amount* #-CMU 0.1 #+CMU 1.0
    "Amount of time to sleep between checking *QUERY-IO*.

In multiprocessing Lisps, this allows other processes to continue
while we busy-wait. If 0, skips call to SLEEP.")


(defun internal-real-time-in-seconds ()
  (get-universal-time))


(defun read-char-wait (&optional (timeout 20)
                                 input-stream
                                 (eof-error-p t)
                                 eof-value
                                 &aux
                                 peek)
  (do ((start (internal-real-time-in-seconds)))
      ((or (setq peek (listen input-stream))
           (< (+ start timeout) (internal-real-time-in-seconds)))
       (when peek
         ;; Back to READ-CHAR-NO-HANG
         (read-char-no-hang input-stream eof-error-p eof-value)))
    (unless (zerop *sleep-amount*)
      (sleep *sleep-amount*))))


;;; Lots of lisps, especially those that run on top of UNIX, do not get
;;; their input one character at a time, but a whole line at a time because
;;; of the buffering done by the UNIX system. This causes y-or-n-p-wait
;;; to not always work as expected.
;;;
;;; I wish lisp did all its own buffering (turning off UNIX input line
;;; buffering by putting the UNIX into CBREAK mode). Of course, this means
;;; that we lose input editing, but why can't the lisp implement this?

(defun y-or-n-p-wait (&optional (default #\y)
                                (timeout 20)
				format-string &rest args)
  "Y-OR-N-P-WAIT prints the message, if any.

Y-OR-N-P-WAIT then reads characters from *QUERY-IO* until the user
enters y, Y or space as an affirmative, or either n or N as a negative
answer, or the timeout occurs. It asks again if you enter any other
characters."

  (when *clear-input-before-query* (clear-input *query-io*))

  (when format-string
    (fresh-line *query-io*)
    (apply #'format *query-io* format-string args)
    ;; FINISH-OUTPUT needed for CMU and other places which don't handle
    ;; output streams nicely. This prevents it from continuing and
    ;; reading the query until the prompt has been printed.
    (finish-output *query-io*))

  (loop
   (let* ((read-char (if *use-timeouts*
			 (read-char-wait timeout *query-io* nil nil)
			 (read-char *query-io*)))
	  (char (or read-char default)))

     ;; We need to ignore #\newline because otherwise the bugs in
     ;; clear-input will cause y-or-n-p-wait to print the "Type ..."
     ;; message every time... *sigh*
     ;; Anyway, we might want to use this to ignore whitespace once
     ;; clear-input is fixed.

     (unless (find char '(#\tab #\newline #\return))
       (when (null read-char)
	 (format *query-io* "~@[~A~]" default)
	 (finish-output *query-io*))

       (cond ((null char) (return t))
	     ((find char '(#\y #\Y #\space) :test #'char=) (return t))
	     ((find char '(#\n #\N) :test #'char=) (return nil))
	     (t
	      (when *clear-input-before-query* (clear-input *query-io*))
	      (format *query-io* "~&Type \"y\" for yes or \"n\" for no. ")
	      (when format-string
		(fresh-line *query-io*)
		(apply #'format *query-io* format-string args))
	      (finish-output *query-io*)))))))


#||
(y-or-n-p-wait #\y 20 "What? ")
(progn (format t "~&hi") (finish-output)
       (y-or-n-p-wait #\y 10 "1? ")
       (y-or-n-p-wait #\n 10 "2? "))
||#

;;; Running the operations
;;; ======================

(defvar %%component%% nil)

(export '(%%component%%)) ; Just a placeholder. Move it to the export list.


(defmacro with-special-component-vars ((c) &body forms)
  `(let ((%%component%% ,c))
    (declare (special %%component%%))
    ,@forms))


;;; Operate on System
;;; -----------------

;;; operate-on-system --
;;;
;;; Arguments and Values:
;;;
;;; Operation is :compile, 'compile, :load or 'load
;;;
;;; Force is :all or :new-source or :new-source-and-dependents or a list of
;;;    specific modules.
;;;    :all (or T) forces a recompilation of every file in the system
;;;    :new-source-and-dependents compiles only those files whose
;;;          sources have changed or who depend on recompiled files.
;;;    :new-source compiles only those files whose sources have changed
;;;    A list of modules means that only those modules and their
;;;    dependents are recompiled.
;;;
;;; Test is T to print out what it would do without actually doing it.
;;;    Note: it automatically sets verbose to T if test is T.
;;;
;;; Verbose is T to print out what it is doing (compiling, loading of
;;;    modules and files) as it does it.
;;;
;;; Dribble should be the pathname of the dribble file if you want to
;;;    dribble the compilation.
;;;
;;; Load-source-instead-of-binary is T to load .lisp instead of binary
;;;    files.
;;;
;;; Version may be nil to signify no subdirectory,
;;;    a symbol, such as alpha, beta, omega, :alpha, mark, which
;;;    specifies a subdirectory of the root, or a string, which
;;;    replaces the root.

(defun operate-on-system (name operation
			       &key
			       force
			       (version *version*)
			       (test *oos-test*)
                               (debug *defsystem-oos-debug*)
                               (verbose *oos-verbose*)
                               (load-source-instead-of-binary
				*load-source-instead-of-binary*)
                               (load-source-if-no-binary
				*load-source-if-no-binary*)
			       (bother-user-if-no-binary
				*bother-user-if-no-binary*)
			       (compile-during-load *compile-during-load*)
			       dribble
			       (minimal-load *minimal-load*)
			       (override-compilation-unit t)
			       )

  (declare #-(or :cltl2 :ansi-cl) (ignore override-compilation-unit))

  (unwind-protect
      ;; Protect the undribble.
      (#+(and (or :cltl2 :ansi-cl) (not :gcl)) with-compilation-unit
	 #+(and (or :cltl2 :ansi-cl) (not :gcl)) (:override override-compilation-unit)
	 #-(and (or :cltl2 :ansi-cl) (not :gcl)) progn
	(when *reset-full-pathname-table* (clear-full-pathname-tables))
	(when dribble (dribble dribble))
	(when test (setq verbose t))
	(when (null force)		; defaults
	  (case operation
	    ((load :load) (setq force :all))
	    ((compile :compile) (setq force :new-source-and-dependents))
	    (t (setq force :all))))

	;; Some CL implementations have a variable called *compile-verbose*
	;; or *compile-file-verbose*.

	(multiple-value-bind (*version-dir* *version-replace*)
	    (translate-version version)
	  ;; CL implementations may uniformly default this to nil
	  (let ((*load-verbose* #-common-lisp-controller t
				#+common-lisp-controller nil) ; nil
		#-(or MCL CMU CLISP ECL :sbcl lispworks scl)
		(*compile-file-verbose* t) ; nil
		#+common-lisp-controller
		(*compile-print* nil)
		#+(and common-lisp-controller cmu)
		(ext:*compile-progress* nil)
		#+(and common-lisp-controller cmu)
		(ext:*require-verbose* nil)
		#+(and common-lisp-controller cmu)
		(ext:*gc-verbose* nil)

		(*compile-verbose* #-common-lisp-controller t
				   #+common-lisp-controller nil) ; nil
		(*version* version)
		(*oos-verbose* verbose)
		(*oos-test* test)
                (*defsystem-oos-debug* debug)
		(*load-source-if-no-binary* load-source-if-no-binary)
		(*compile-during-load* compile-during-load)
		(*bother-user-if-no-binary* bother-user-if-no-binary)
		(*load-source-instead-of-binary* load-source-instead-of-binary)
		(*minimal-load* minimal-load)
		(system (if (and (component-p name)
                                 (member (component-type name)
					 '(:system :defsystem :subsystem)))
                            name
                            (find-system name :load))))
	    #-(or :abcl :cmu CLISP :sbcl :lispworks :cormanlisp scl)
	    (declare (special *compile-verbose* #-MCL *compile-file-verbose*)
		     #-openmcl (ignore *compile-verbose*
				       #-MCL *compile-file-verbose*)
		     #-openmcl (optimize (inhibit-warnings 3)))

            ;; (declare (ignorable *compile-file-verbose*))
                     
	    (unless (component-operation operation)
	      (error "Operation ~A undefined." operation))

	    (operate-on-component system operation force))))
    (when dribble (dribble))))


(defun compile-system (name &key
                            force
			    (version *version*)
			    (test *oos-test*)
                            (verbose *oos-verbose*)
			    (load-source-instead-of-binary
			     *load-source-instead-of-binary*)
			    (load-source-if-no-binary
			     *load-source-if-no-binary*)
			    (bother-user-if-no-binary
			     *bother-user-if-no-binary*)
			    (compile-during-load *compile-during-load*)
			    dribble
			    (minimal-load *minimal-load*)
                            )
  "Compiles a system, not necessarily loading it in the CL environment."

  ;; For users who are confused by OOS.
  ;;
  ;; MA 20211111: That is, the vast majority of them, present company
  ;; included.

  (operate-on-system
   name :compile
   :force force
   :version version
   :test test
   :verbose verbose
   :load-source-instead-of-binary load-source-instead-of-binary
   :load-source-if-no-binary load-source-if-no-binary
   :bother-user-if-no-binary bother-user-if-no-binary
   :compile-during-load compile-during-load
   :dribble dribble
   :minimal-load minimal-load))


(defun load-system (name &key force
			 (version *version*)
			 (test *oos-test*) (verbose *oos-verbose*)
			 (load-source-instead-of-binary
			  *load-source-instead-of-binary*)
			 (load-source-if-no-binary *load-source-if-no-binary*)
			 (bother-user-if-no-binary *bother-user-if-no-binary*)
			 (compile-during-load *compile-during-load*)
			 dribble
			 (minimal-load *minimal-load*))
  "Loads a system in the CL environemnt, not necessarily compiling it."

  ;; For users who are confused by OOS.
  ;;
  ;; MA 20211111: That is, the vast majority of them, present company
  ;; included.

  (operate-on-system
   name :load
   :force force
   :version version
   :test test
   :verbose verbose
   :load-source-instead-of-binary load-source-instead-of-binary
   :load-source-if-no-binary load-source-if-no-binary
   :bother-user-if-no-binary bother-user-if-no-binary
   :compile-during-load compile-during-load
   :dribble dribble
   :minimal-load minimal-load))


(defun clean-system (name &key
                          (force :all)
                          ((:propagate-to-subsystems
                            *operations-propagate-to-subsystems*) nil)
                          ((:propagate-to-system-dependecies
                            *operations-propagate-to-systems-depending-on*) nil)
                          (version *version*)
                          (test *oos-test*)
                          (verbose *oos-verbose*)
                          dribble)
  "Deletes all the binaries in the system."

  ;; For users who are confused by OOS.
  ;;
  ;; MA 20211111: That is, the vast majority of them, present company
  ;; included.

  (operate-on-system
   name :delete-binaries
   :force force
   :version version
   :test test
   :verbose verbose
   :dribble dribble))


(defun edit-system
    (name &key force
	       (version *version*)
	       (test *oos-test*)
	       (verbose *oos-verbose*)
	       dribble)

  (operate-on-system
   name :edit
   :force force
   :version version
   :test test
   :verbose verbose
   :dribble dribble))


(defun hardcopy-system
    (name &key force
	       (version *version*)
	       (test *oos-test*)
	       (verbose *oos-verbose*)
	       dribble)

  (operate-on-system
   name :hardcopy
   :force force
   :version version
   :test test
   :verbose verbose
   :dribble dribble))


;;; ensure-external-system-def-loaded component --
;;; Let's treat definition clauses of the form
;;;
;;; 	(:system "name")
;;; i.e.
;;;
;;;	(:system "name" :components nil)
;;;
;;; in a special way.
;;; When encountered, MK:DEFSYSTEM tries to FIND-SYSTEM
;;; the system named "name" (by forcing a reload from disk).
;;; This may be more "natural".

(defun ensure-external-system-def-loaded (component)
  (assert (member (component-type component)
		  '(:subsystem :system)))
  (when (null (component-components component))
    (let* ((cname (component-name component)))
      (declare (ignorable cname))
      ;; First we ensure that we reload the system definition.
      (undefsystem cname)
      (let* ((*reload-systems-from-disk* t)
	     (system-component
              
              ;; (find-system (component-name component) :load)

              #| Marco Antoniotti 20090416: may break in strange ways
                 due to circularities.
              |#
              
              (if (foreign-system-p component)
                  (let ((foreign-sys (find-foreign-system component)))
                    foreign-sys
                    )
                  (ecase (component-type component)
                    (:system
                     (find-system (component-name component) :load))

                    (:subsystem
                     #|
                       (warn "Circular dependencies may happen with ~
                              :SUBSYSTEM components.")
                     |#
                     (find-system (component-name component) :load
                                  (merge-pathnames
                                   (make-pathname :name cname
                                                  :type "system"
                                                  :directory nil)
                                   (component-full-pathname component
                                                            :source))
                                  ))))
              )
             )
	;; Now we have a problem.
	;; We have just ensured that a system definition is
	;; loaded, however, the COMPONENT at hand is different
	;; from SYSTEM-COMPONENT.
	;; To fix this problem we just use the following
	;; kludge.  This should prevent re-entering in this
	;; code branch, while actually preparing the COMPONENT
	;; for operation.
        ;;
        ;; 20220204 MA: The kludge does not work for the new
        ;; FOREIGN-SYSTEMs as COMPONENT and SYSTEM-COMPONENT *are* now
        ;; the same. Hence the system would loop if we did not test as
        ;; below.

        (unless (eq component system-component)
          ;; ... and (not (foreign-system-p system-component))
          (setf (component-components component)
                (list system-component)))
	))))


(defun operate-on-component (component operation force &aux changed)
  ;; Returns T if something changed and had to be compiled.

  (let ((type (component-type component))
	(old-package (package-name *package*))
        )

    (unwind-protect
	;; Protect old-package.

	(progn
	  ;; Use the correct package.
	  (when (component-package component)
	    (tell-user-generic "Using package ~A."
                               (component-package component))
	    (unless *oos-test*
	      (unless (find-package (component-package component))

		;; If the package name is the same as the name of the system,
		;; and the package is not defined, this would lead to an
		;; infinite loop, so bomb out with an error.

		(when (string-equal (string (component-package component))
				    (component-name component))
		  (tell-user-generic "Component ~A not loaded."
                                     (component-name component))
		  (error "MK: Error: Package ~A is not defined."
			  (component-package component)))

		;; If package not found, try using REQUIRE to load it.

		(new-require (component-package component))
                )

	      ;; This was USE-PACKAGE, but should be IN-PACKAGE.
	      ;; Actually, CLtL2 lisps define in-package as a macro,
	      ;; so we'll set the package manually.
	      ;; (in-package (component-package component))

	      (let ((package (find-package (component-package component))))
		(when package
		  (setf *package* package)))))

	  ;; Marco Antoniotti 20040609
	  ;; New feature.  Try to FIND-SYSTEM :system components if
	  ;; they have no local :components definition.
	  ;; OPERATE-ON-SYSTEM-DEPENDENCIES should still work as
	  ;; advertised, given the small change made there.

	  (when (or (eq type :system) (eq type :subsystem))
	    (ensure-external-system-def-loaded component))

	  (when (or (eq type :defsystem) (eq type :system))
            (handler-case
                (operate-on-system-dependencies component
                                                operation
                                                force)
              (error (e)
                (tell-user-generic "Some system dependency for ~A failed."
                                   (component-name component))
                (if (component-non-required-p component)
                    (tell-user-generic "Dependency ignored as ~A is not required."
                                       (component-name component))
                    (error e))
              )))

	  ;; Do any compiler proclamations.
	  (when (component-proclamations component)
	    (tell-user-generic "Doing proclamations for ~A."
                               (component-name component))
	    (unless *oos-test*
              (proclaim (component-proclamations component))))

	  ;; Do any initial actions.
	  (when (component-initially-do component)
	    (tell-user-generic "Doing initializations for ~A."
                               (component-name component))
	    (unless *oos-test*
              (with-special-component-vars (component)
                 (let ((initially-do (component-initially-do component)))
                   (if (functionp initially-do)
                       (funcall initially-do)
                       (eval initially-do))))
              ))

	  ;; If operation is :compile and load-only is T, this would change
	  ;; the operation to load. Only, this would mean that a module would
	  ;; be considered to have changed if it was :load-only and had to be
	  ;; loaded, and then dependents would be recompiled -- this doesn't
	  ;; seem right. So instead, we propagate the :load-only attribute
	  ;; to the components, and modify compile-file-operation so that
	  ;; it won't compile the files (and modify tell-user to say "Loading"
	  ;; instead of "Compiling" for load-only modules).
	  #||
	  (when (and (find operation '(:compile compile))
		     (component-load-only component))
	    (setf operation :load))
	  ||#

	  ;; Do operation and set changed flag if necessary.
	  (setq changed
		(case type
		  ((:file :private-file)
		   (funcall (component-operation operation) component force))
		  ((:module :system :subsystem :defsystem)
		   (operate-on-components component operation force changed))))

	  ;; Do any final actions
	  (when (component-finally-do component)
	    (tell-user-generic "Doing finalizations for ~A."
                               (component-name component))
	    (unless *oos-test*
              (with-special-component-vars (component)
                 (let ((finally-do (component-finally-do component)))
                   (if (functionp finally-do)
                       (funcall finally-do)
                       (eval finally-do))))
                ))

	  ;; Add the banner if needed.
	  #+(or cmu scl)
	  (when (component-banner component)
	    (unless (stringp (component-banner component))
	      (error "The banner should be a string, it is: ~S"
	             (component-banner component)))
	    (setf (getf ext:*herald-items*
			(intern (string-upcase  (component-name component))
				(find-package :keyword)))
		  (list (component-banner component))))
          ) ; End of protected form.

      ;; Reset the package. (Cleanup form of unwind-protect.)

      (setf *package* (find-package old-package))
      ) ; End of UNWIND-PROTECT.


    ;; Provide the loaded system

    (when (or (eq type :defsystem) (eq type :system) (eq type :subsystem))
      (tell-user-generic "Providing system ~A.~%"
                         (component-name component))
      (or *oos-test*
	  (provide (canonicalize-system-name (component-name component))))))

  ;; Return non-NIL if something changed in this component and hence had
  ;; to be recompiled. This is only used as a boolean.
  changed)


(defvar *force* nil
  "If T, operations will be performed regarless of dependencies status.")


(defvar *providing-blocks-load-propagation* t
  "If T, if a system dependency exists on *modules*, it is not loaded.")


(defun operate-on-system-dependencies (component operation
                                                 &optional
                                                 (force *force*))
  (when (and *system-dependencies-delayed*
             *operations-propagate-to-systems-depending-on*)

    ;; Do nothing if we do not propagate to subsystems or systems this
    ;; one depends on.

    (let ((*force* force))

      (declare (special *force*))

      (dolist (system (component-depends-on component))

	;; For each system that this system depends on, if it is a
	;; defined system (either via defsystem or component type :system),
	;; and propagation is turned on, propagates the operation to the
	;; subsystem. Otherwise runs require (my version) on that system
	;; to load it (needed since we may be depending on a lisp
	;; dependent package).
	;; Explores the system tree in a DFS manner.

	;; Do not try to do anything with non system components.
        (cond ((and (not (listp system))
		    (or (stringp system) (symbolp system))
                    ;; The subsystem is a defined system.
                    (find-system system :load-or-nil))

               ;; Call OOS on it. Since *system-dependencies-delayed* is
               ;; T, the :depends-on slot is filled with the names of
               ;; systems, not defstructs.
               ;; Aside from system, operation, force, for everything else
               ;; we rely on the globals.

               (unless (and *providing-blocks-load-propagation*

                            ;; If *providing-blocks-load-propagation* is T,
                            ;; the system dependency must not exist in the
                            ;; *modules* for it to be loaded. Note that
                            ;; the dependencies are implicitly systems.

                            (find operation '(load :load) :test #'eq) ; Already loaded.
                            ;; (or (eq force :all) (eq force t))

                            (find (canonicalize-system-name system)
                                  *modules*
				  :test #'string-equal))

                 (operate-on-system system operation :force force))
               )

	      ;; Is the system "foreign"?  I.e., can it be found using
	      ;; FIND-FOREIGN-SYSTEM?

	      ((and (not (listp system))
		    (or (stringp system) (symbolp system))
                    ;; The subsystem is a foreign defined system.
                    (find-foreign-system (string-downcase system)
					 :errorp nil))

               ;; Call OOS on it. Since *system-dependencies-delayed* is
               ;; T, the :depends-on slot is filled with the names of
               ;; systems, not defstructs.
               ;; Aside from system, operation, force, for everything else
               ;; we rely on the globals.

               (unless (and *providing-blocks-load-propagation*

                            ;; If *providing-blocks-load-propagation* is T,
                            ;; the system dependency must not exist in the
                            ;; *modules* for it to be loaded. Note that
                            ;; the dependencies are implicitly systems.

                            (find operation '(load :load) :test #'eq) ; Already loaded.
                            ;; (or (eq force :all) (eq force t))

                            (find (canonicalize-system-name system)
                                  *modules*
				  :test #'string-equal))

                 (operate-on-system system operation :force force))
               )

              ((listp system)

               (operate-on-system-structured-dependency component 
                                                        system
                                                        operation)
               #| Clean this up later; after testing...
               ;; If the SYSTEM is a list then its contents are as follows.
               ;;
               ;;    (<name> &key <system-type>
               ;;                 <definition-pathname>
               ;;                 <action>
               ;;                 <version>
               ;;                 (<required-p> t))
               ;;

               (destructuring-bind (system-name &key
                                                system-type
                                                definition-pathname
                                                action
                                                version
                                                (required-p t))
                   system

                 (declare (ignorable system-type))

                 (tell-user-require-system
                  (if (and (null system-name)
                           (null definition-pathname))
                      action
                      system)
                  component)

                 (handler-case
                     (or *oos-test*
                         (new-require system-name
                                      nil
                                      (eval definition-pathname)
                                      action
                                      (or version *version*)))
                   (error (e)
                     (tell-user-generic
                      "System dependency ~S for ~A failed." 
                      system
                      (component-name component))
                     (if required-p
                         (error e)
                         (tell-user-generic
                          "Failed dependency ignored as non required."))
                     )
                   #|
                   (error (mce)
                     (when required-p (error mce)))|#
                   )
                 )
               |#
               )

              ((and (component-p system)
                    (not (member (component-type system)
                                 '(:defsystem :subsystem :system))))
               ;; Do nothing for non system components.
               )
              (t
               (tell-user-require-system system component)
               (or *oos-test* (new-require system))))
        ))))


;;; operate-on-system-structured-dependency
;;;
;;; New functionality
;;; 20220201 MA

(defun operate-on-system-structured-dependency (component
                                                system
                                                operation
                                                &optional
                                                (force *force*)
                                                )
  ;; COMPONENT is actually a 'system'.
  ;;
  ;; SYSTEM, i.e., the "other" system on which COMPONENT depends on,
  ;; is a list with syntax:
  ;;
  ;;     (<sys-keyword> <name>
  ;;                    &key
  ;;                    <kind>
  ;;                    <definition-pathname>
  ;;                    <action>
  ;;                    <version>
  ;;                    (<required-p> t))
  ;;
  ;; SYS-KEYWORD can be :FILE or it can be :SYSTEM, :DEFSYSTEM or
  ;; :FOREIGN-SYSTEM; the first two are synonims FTTB.
  ;;
  ;; When SYS-KEYWORD is :FOREIGN-SYSTEM then KIND is checked
  ;; and may be used to state that the system is a non MK-DEFSYSTEM
  ;; one (most notably :ASDF, which is the default).

  (declare (type list system)
           (ignorable operation)
           )

  (destructuring-bind (sys-kwd system-name
                               &key
                               (kind :asdf)
                               (definition-pathname
                                (pathname (string system-name))
                                dp-supplied-p)
                               action
                               version
                               (required-p t))
      system

    (declare (ignore dp-supplied-p))

    ;; 20220204 MA: Come back later to add :FILE handling.
    (ecase sys-kwd
      ((:system :defsystem)

       (tell-user-require-system
        (if (and (null system-name) (null definition-pathname))
            action
            system)
        system-name)

       (handler-case
           (unless *oos-test*
             (new-require system-name
                          nil
                          (eval definition-pathname)
                          action
                          (or version *version*)))
         (error (e)
           (tell-user-generic
            "System dependency ~S for ~A failed."
            system
            (component-name component))

           (if required-p
               (error e)
               (tell-user-generic
                "Failed dependency ignored as non required."))
           )
         #|
           (error (mce)
                  (when required-p (error mce)))
          |#
      ))

      (:foreign-system
       ;; (register-foreign-system system-name :kind system-type)
       (find-foreign-system system-name :kind kind :errorp t)
       (operate-on-system system-name operation :force force)
       )
      )
    ))


;;; Modules can depend only on siblings. If a module should depend
;;; on an uncle, then the parent module should depend on that uncle
;;; instead. Likewise a module should depend on a sibling, not a niece
;;; or nephew. Modules also cannot depend on cousins. Modules cannot
;;; depend on parents, since that is circular.

(defun module-depends-on-changed (module changed)
  (dolist (dependent (component-depends-on module))
    (when (member dependent changed)
      (return t))))


(defun operate-on-components (component operation force changed)
  (with-tell-user (operation component)
    (if (component-components component)
	(dolist (module (component-components component))
	  (when (operate-on-component module operation
		  (cond ((and (module-depends-on-changed module changed)
			      #||(some #'(lambda (dependent)
					(member dependent changed))
				    (component-depends-on module))||#
			      (or (non-empty-listp force)
				  (eq force :new-source-and-dependents)))
			 ;; The component depends on a changed file
			 ;; and force agrees.
			 (if (eq force :new-source-and-dependents)
			     :new-source-all
			     :all))
			((and (non-empty-listp force)
			      (member (component-name module) force
				      :test #'string-equal :key #'string))
			 ;; Force is a list of modules
			 ;; and the component is one of them.
			 :all)
			(t force)))
	    (push module changed)))

        ;; ... else no (sub)components.
	(case operation
	  ((compile :compile)
	   (with-special-component-vars (component)
             (let ((compile-form (component-compile-form component)))
               (if (functionp compile-form)
	           (funcall compile-form component)
	           (eval compile-form)))))
          
	  ((load :load)
	   (with-special-component-vars (component)
             (let ((load-form (component-load-form component)))
               (if (functionp load-form)
	           (funcall load-form component)
                   (eval load-form)))
	     )))))
  ;; This is only used as a boolean.
  changed)


;;; New Require
;;; -----------

;;; This needs cleaning.  Obviously the code is a left over from the
;;; time people did not know how to use packages in a proper way or
;;; CLs were shaky in their implementation.

;;; First of all we need this. (Commented out for the time being)
;;; (shadow '(cl:require))


(defvar *old-require* nil)

;;; All calls to require in this file have been replaced with calls
;;; to new-require to avoid compiler warnings and make this less of
;;; a tangled mess.

(defun new-require (module-name
		    &optional
		    pathname
		    definition-pname
		    default-action
		    (version *version*))
  ;; If the pathname is present, this behaves like the old require.

  (unless (and module-name

	       ;; madhu: Allegro cannot coerce pathnames to strings
	       ;; via (string #p"foo") and module-name turns out to be
	       ;; a pathname when REQUIRE is used internally to load
	       ;; internal modules.

	       #+allegro
	       (and (pathnamep module-name)
		    (setq module-name (namestring module-name)))

	       (find (string module-name)
		     *modules* :test #'string=))
    (cond (pathname
           (funcall *old-require* module-name pathname))

          ;; If the system is defined, load it.
          ((find-system module-name :load-or-nil definition-pname)
           (operate-on-system
            module-name :load
            :force *force*
            :version version
            :test *oos-test*
            :debug *defsystem-oos-debug*
            :verbose *oos-verbose*
            :load-source-if-no-binary *load-source-if-no-binary*
            :bother-user-if-no-binary *bother-user-if-no-binary*
            :compile-during-load *compile-during-load*
            :load-source-instead-of-binary *load-source-instead-of-binary*
            :minimal-load *minimal-load*))

          ;; If there's a default action, do it. This could be a progn which
          ;; loads a file that does everything.
          ((and default-action
                (eval default-action)))

          ;; If no system definition file, try regular require.
          ((funcall *old-require* module-name))

          ;; If no default action, print a warning or error message.
          (t
           #||
	       (format t "~&Warning: System ~A doesn't seem to be defined..."
	               module-name)
	       ||#
           (error 'missing-system :name module-name)))
    ))


;;; Note that in some lisps, when the compiler sees a REQUIRE form at
;;; top level it immediately executes it. This is as if an
;;; (eval-when (compile load eval) ...) were wrapped around the REQUIRE
;;; form. I don't see any easy way to do this without making REQUIRE
;;; a macro.
;;;
;;; For example, in VAXLisp, if a (require 'streams) form is at the top of
;;; a file in the system, compiling the system doesn't wind up loading the
;;; streams module. If the (require 'streams) form is included within an
;;; (eval-when (compile load eval) ...) then everything is OK.
;;;
;;; So perhaps we should replace the redefinition of lisp:require
;;; with the following macro definition:
#||
(unless *old-require*
  (setf *old-require*
	(symbol-function #-(or :lispworks
			       :sbcl
			       (and :excl :allegro-v4.0)) 'lisp:require
			 #+:sbcl 'cl:require
			 #+:lispworks 'system:::require
			 #+(and :excl :allegro-v4.0) 'cltl1:require))

  (let (#+:CCL (ccl:*warn-if-redefine-kernel* nil))
    ;; Note that lots of lisps barf if we redefine a function from
    ;; the LISP package. So what we do is define a macro with an
    ;; unused name, and use (setf macro-function) to redefine
    ;; lisp:require without compiler warnings. If the lisp doesn't
    ;; do the right thing, try just replacing require-as-macro
    ;; with lisp:require.
    (defmacro require-as-macro (module-name
				&optional pathname definition-pname
				default-action (version '*version*))
      `(eval-when (:compile-toplevel :load-toplevel :execute)
	 (new-require ,module-name ,pathname ,definition-pname
		      ,default-action ,version)))
    (setf (macro-function #-(and :excl :sbcl :allegro-v4.0) 'lisp:require
			  #+:sbcl 'cl:require
			  #+(and :excl :allegro-v4.0) 'cltl1:require)
	  (macro-function 'require-as-macro))))
||#
;;; This will almost certainly fix the problem, but will cause problems
;;; if anybody does a funcall on #'require.

;;; Redefine old require to call the new require.

(eval-when #-(or :lucid) (:load-toplevel :execute)
	   #+(or :lucid) (load eval)
(unless *old-require*
  (setf *old-require*
	(symbol-function
	 #-(or (and :excl :allegro-v4.0)
               :mcl
	       :ecl
               :sbcl
               :lispworks
               :clozure-common-lisp
	       :abcl) 'lisp:require
	 #+(and :excl :allegro-v4.0) 'cltl1:require
	 #+:sbcl 'cl:require
	 #+:ecl 'cl:require
	 #+:lispworks3.1 'common-lisp::require
	 #+(and :lispworks (not :lispworks3.1)) 'system::require
	 #+(or :openmcl :clozure-common-lisp)'cl:require
	 #+(and :mcl (not :openmcl)) 'ccl:require
	 #+(or :abcl :armedbear) 'cl:require
	 ))

  (unless *dont-redefine-require*
    (let (#+(or :mcl (and :CCL (not :lispworks)))
	  (ccl:*warn-if-redefine-kernel* nil))
      #-(or (and allegro-version>= (version>= 4 1)) :lispworks)
      (setf (symbol-function
	     #-(or (and :excl :allegro-v4.0)
                   :mcl
		   :ecl
                   :sbcl
                   :lispworks
                   :clozure-common-lisp
		   :abcl) 'lisp:require
	     #+(and :excl :allegro-v4.0) 'cltl1:require
	     #+:lispworks3.1 'common-lisp::require
	     #+:ecl 'cl:require
	     #+:sbcl 'cl:require
	     #+(and :lispworks (not :lispworks3.1)) 'system::require
	     #+:openmcl 'cl:require
	     #+(and :mcl (not :openmcl)) 'ccl:require
	     #+(or :abcl :armedbear) 'cl:require
	     )
	    (symbol-function 'new-require))
      #+:lispworks
      (let ((warn-packs system::*packages-for-warn-on-redefinition*))
	(declare (special system::*packages-for-warn-on-redefinition*))
	(setq system::*packages-for-warn-on-redefinition* nil)
	(setf (symbol-function
	       #+:lispworks3.1 'common-lisp::require
	       #-:lispworks3.1 'system::require
	       )
	      (symbol-function 'new-require))
	(setq system::*packages-for-warn-on-redefinition* warn-packs))
      #+(and allegro-version>= (version>= 4 1))
      (excl:without-package-locks
       (setf (symbol-function 'lisp:require)
	 (symbol-function 'new-require))))))
)


;;; Well, let's add some more REQUIRE hacking; specifically for SBCL,
;;; and, eventually, for CMUCL.

#+sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)

(defun sbcl-mk-defsystem-module-provider (name)
  ;; Let's hope things go smoothly.
    (let ((module-name (string-downcase (string name))))
      (when (mk:find-system module-name :load-or-nil)
	(mk:load-system module-name
			:compile-during-load t
			:verbose nil))))

(pushnew 'sbcl-mk-defsystem-module-provider
         sb-ext:*module-provider-functions*)
)


#+clozure-common-lisp
(eval-when (:compile-toplevel :load-toplevel :execute)

(defun ccl-mk-defsystem-module-provider (name)
  ;; Let's hope things go smoothly.
  ;; (break "Now now now...")
  (let ((module-name (string-downcase (string name))))
    (when (mk:find-system module-name :load-or-nil)
      ;; (break "Ok ok ok...")
      (mk:load-system module-name
                      :compile-during-load t
                      :verbose nil))))

(pushnew 'ccl-mk-defsystem-module-provider
         ccl:*module-provider-functions*)
)

#+#.(cl:if (cl:and (cl:find-package "EXT")
                   (cl:find-symbol "*MODULE-PROVIDER-FUNCTIONS*" "EXT"))
        '(and)
        '(or))
(progn
  (defun cmucl-mk-defsystem-module-provider (name)
    (let ((module-name (string-downcase (string name))))
      (when (mk:find-system module-name :load-or-nil)
	(mk:load-system module-name
			:compile-during-load t
			:verbose nil))))

  (pushnew 'cmucl-mk-defsystem-module-provider ext:*module-provider-functions*)
  )


;;; Language-Dependent Characteristics
;;; ==================================
;;;
;;; This section is used for defining language-specific behavior of
;;; defsystem. If the user changes a language definition, it should
;;; take effect immediately -- they shouldn't have to reload the
;;; system definition file for the changes to take effect.

(defvar *language-table* (make-hash-table :test #'equal)
  "Hash table that maps from languages to language structures.")


(defun find-language (name)
  (gethash name *language-table*))


(defstruct (language (:print-function print-language))
  (name :lisp :type keyword :read-only t)  ; The name of the language (a keyword)
  (compiler (constantly t) :type function) ; The function used to compile files in the language
  (loader (constantly t) :type function)   ; The function used to load files in the language
  (source-extension "lisp"
                    :type (or null string)
                    :read-only t) ; Filename extensions for source files
  (binary-extension "fasl"
                    :type (or null string)
                    :read-only t) ; Filename extensions for binary files
  )


(defun print-language (language stream depth)
  (declare (ignore depth))
  (format stream "#<~:@(~A~): ~A ~A>"
          (language-name language)
          (language-source-extension language)
	  (language-binary-extension language)))


(defun compile-function (component)
  (or (component-compiler component)
      (let ((language (find-language (or (component-language component)
					 :lisp))))
	(when language (language-compiler language)))
      #'compile-file))


(defun load-function (component)
  (or (component-loader component)
      (let ((language (find-language (or (component-language component)
					 :lisp))))
	(when language (language-loader language)))
      #'load))


(defun default-source-extension (component)
  (let ((language (find-language (or (component-language component)
				     :lisp))))
    (or (when language (language-source-extension language))
	(car *filename-extensions*))))


(defun default-binary-extension (component)
  (let ((language (find-language (or (component-language component)
				     :lisp))))
    (or (when language (language-binary-extension language))
	(cdr *filename-extensions*))))


(defmacro define-language (name
                           &key
                           compiler
                           loader
                           (source-extension (car *filename-extensions*))
                           (binary-extension (cdr *filename-extensions*))
                           )
  (let ((language (gensym "LANGUAGE")))
    `(let ((,language (make-language :name ,name
				     :compiler ,compiler
				     :loader ,loader
				     :source-extension ,source-extension
				     :binary-extension ,binary-extension)))
       (setf (gethash ,name *language-table*) ,language)
       ,name)))

#||
;;; Test System for verifying multi-language capabilities.
(defsystem foo
  :language :lisp
  :components ((:module c :language :c :components ("foo" "bar"))
	       (:module lisp :components ("baz" "barf"))))

||#

;;; Lisp Language Definition
;;; ------------------------

(define-language :lisp
  :compiler #'compile-file
  :loader #'load
  :source-extension (car *filename-extensions*)
  :binary-extension (cdr *filename-extensions*))


;;; PseudoScheme Language Definition
;;; --------------------------------

(defun scheme-compile-file (filename &rest args)
  (let ((scheme-package (find-package '#:scheme)))
    (apply (symbol-function (find-symbol (symbol-name 'compile-file)
					 scheme-package))
	   filename
	   (funcall (symbol-function
		     (find-symbol (symbol-name '#:interaction-environment)
				  scheme-package)))
	   args)))


(define-language :scheme
  :compiler #'scheme-compile-file
  :loader #'load
  :source-extension "scm"
  :binary-extension "bin")


;;; C Language Definition
;;; ---------------------

;;; This is very basic. Somebody else who needs it can add in support
;;; for header files, libraries, different C compilers, etc. For example,
;;; we might add a COMPILER-OPTIONS slot to the component defstruct.

(defparameter *c-compiler* "gcc")


#-(or symbolics (and :lispworks :harlequin-pc-lisp))
(defun run-unix-program (program arguments)

  ;; arguments should be a list of strings, where each element is a
  ;; command-line option to send to the program.

  (let ((cmd-string (format nil "~A~@[ ~{~A~^ ~}~]"
		            program arguments)))
    (declare (type string cmd-string)
             (ignorable cmd-string))
    
    #+:lucid (run-program program :arguments arguments)
    #+:allegro (excl:run-shell-command cmd-string)
    #+(or :kcl :ecl) (system cmd-string)
    #+(or :cmu :scl) (extensions:run-program program arguments)
    #+:openmcl (ccl:run-program program arguments)
    #+:sbcl (sb-ext:run-program program arguments)
    #+:lispworks (system:call-system-showing-output
		  cmd-string)
    #+clisp (#+lisp=cl ext:run-program #-lisp=cl lisp:run-program
             program :arguments arguments)
    #+abcl (ext:run-shell-command cmd-string)
    ))


#+(or symbolics (and :lispworks :harlequin-pc-lisp))
(defun run-unix-program (program arguments)
  (declare (ignore program arguments))
  (error "MK::RUN-UNIX-PROGRAM: this does not seem to be a UN*X system.")
  )


;;; This is inspired by various versions - all very UNIX/Linux
;;; dependent - appearing in ASDF and UFFI.  The original versions and Copyrights
;;; are by Dan Barlow, Kevin Rosenberg and many others.
;;; This version should be more liberal.

(defvar *default-shell* "/bin/sh")

#+(or windows ms-windows win32)
(eval-when (:load-toplevel :execute)
  ;; Lets assume a "standard" Cygwin installation.
  (if (probe-file (pathname "C:\\cygwin\\bin\\sh.exe"))
      (setf *default-shell* "C:\\cygwin\\bin\\sh.exe")
      (setf *default-shell* nil)))


(defun run-shell-command (command-control-string
                          arguments
                          &key
                          (output *trace-output*)
                          (shell *default-shell*)
                          )
   "Executes a shell 'command' in an underlying process.
RUN-SHELL-COMMAND interpolate ARGS into CONTROL-STRING as if by FORMAT, and
synchronously execute the result using a Bourne-compatible shell, with
output to *trace-output*.  Returns the shell's exit code."

  (declare (ignorable shell output))

  (let ((command (apply #'format nil command-control-string arguments)))
    #+sbcl
    (sb-impl::process-exit-code
     (sb-ext:run-program shell
                         (list "-c" command)
                         :input nil
                         :output output))
    
    #+(or cmu scl)
    (ext:process-exit-code
     (ext:run-program shell
                      (list "-c" command)
                      :input nil
                      :output output))

    #+allegro
    (excl:run-shell-command command :input nil :output output)
    
    #+(and lispworks win32)
    (system:call-system-showing-output (format nil "cmd /c ~A" command)
                                       :output-stream output)

    #+(and lispworks (not win32))
    (system:call-system-showing-output command
                                       :shell-type shell
                                       :output-stream output)
    
    #+clisp				;XXX not exactly *trace-output*, I know
    (ext:run-shell-command command :output :terminal :wait t)

    #+openmcl
    (nth-value 1
	       (ccl:external-process-status
		(ccl:run-program shell
                                 (list "-c" command)
				 :input nil
                                 :output output
				 :wait t)))

    #+abcl (ext:run-shell-command command) ; Just a place holder FTTB.

    #-(or openmcl clisp lispworks allegro scl cmu sbcl abcl)
    (error "MK: RUN-SHELL-PROGRAM not implemented for this Lisp")
    ))


#||
(defun c-compile-file (filename &rest args &key output-file error-file)
  ;; gcc -c foo.c -o foo.o
  (declare (ignore args))
  (run-unix-program *c-compiler*
		    (format nil "-c ~A~@[ -o ~A~]"
			    filename
			    output-file)))
||#

#||
(defun c-compile-file (filename &rest args &key output-file error-file)
  ;; gcc -c foo.c -o foo.o
  (declare (ignore args error-file))
  (run-unix-program *c-compiler*
		    `("-c" ,filename ,@(if output-file `("-o" ,output-file)))))
||#


;;; The following code was inserted to improve C compiler support (at
;;; least under Linux/GCC).
;;; Thanks to Espen S Johnsen.
;;;
;;; 20001118 Marco Antoniotti.

(defun default-output-pathname (path1 path2 type)
  (if (eq path1 t)
      (translate-logical-pathname
       (merge-pathnames (make-pathname :type type) (pathname path2)))
      (translate-logical-pathname (pathname path1))))


(defun run-compiler (program
		     arguments
		     output-file
		     error-file
		     error-output
		     verbose)
  #-(or cmu scl) (declare (ignore error-file error-output))

  (flet ((make-useable-stream (&rest streams)
	   (apply #'make-broadcast-stream (delete nil streams)))
	 )
    (let (#+(or cmu scl) (error-file error-file)
	  #+(or cmu scl) (error-file-stream nil)
	  (verbose-stream nil)
	  (old-timestamp (file-write-date output-file))
	  (fatal-error nil)
	  (output-file-written nil)
	  )
      (unwind-protect
	   (progn
	     #+(or cmu scl)
	     (setf error-file
		   (when error-file
		     (default-output-pathname error-file
			                      output-file
                     		              *compile-error-file-type*))

		   error-file-stream
		   (and error-file
			(open error-file
			      :direction :output
			      :if-exists :supersede)))

	     (setf verbose-stream
		   (make-useable-stream
		    #+cmu error-file-stream
		    (and verbose *trace-output*)))

	     (format verbose-stream "Running ~A~@[ ~{~A~^ ~}~]~%"
		     program
		     arguments)

	     (setf fatal-error
		   #-(or cmu scl)
		   (and (run-unix-program program arguments) nil) ; Incomplete.
		   #+(or cmu scl)
		   (let* ((error-output
			   (make-useable-stream error-file-stream
						(if (eq error-output t)
						    *error-output*
						  error-output)))
			  (process
			   (ext:run-program program arguments
					    :error error-output)))
		     (not (zerop (ext:process-exit-code process)))))

	     (setf output-file-written
		   (and (probe-file output-file)
			(not (eql old-timestamp
				  (file-write-date output-file)))))


	     (when output-file-written
	       (format verbose-stream "~A written~%" output-file))
	     (format verbose-stream "Running of ~A finished~%"
		     program)
	     (values (and output-file-written output-file)
		     fatal-error
		     fatal-error))

	#+(or cmu scl)
	(when error-file
	  (close error-file-stream)
	  (unless (or fatal-error (not output-file-written))
	    (delete-file error-file)))

	(values (and output-file-written output-file)
		fatal-error
		fatal-error)))))


;;; C Language definitions.

(defun c-compile-file (filename &rest args
				&key
				(output-file t)
				(error-file t)
				(error-output t)
				(verbose *compile-verbose*)
				debug
				link
				optimize
				cflags
				definitions
				include-paths
				library-paths
				libraries
				(error t))
  (declare (ignore args))

  (flet ((map-options (flag options &optional (func #'identity))
	   (mapcar #'(lambda (option)
		       (format nil "~A~A" flag (funcall func option)))
		   options))
	 )
    (let* ((output-file (default-output-pathname output-file filename "o"))
	   (arguments
	    `(,@(when (not link) '("-c"))
	      ,@(when debug '("-g"))
	      ,@(when optimize (list (format nil "-O~D" optimize)))
	      ,@cflags
	      ,@(map-options
		 "-D" definitions
		 #'(lambda (definition)
		     (if (atom definition)
			 definition
		       (apply #'format nil "~A=~A" definition))))
	      ,@(map-options "-I" include-paths #'truename)
	      ,(namestring (truename filename))
	      "-o"
	      ,(namestring (translate-logical-pathname output-file))
	      ,@(map-options "-L" library-paths #'truename)
	      ,@(map-options "-l" libraries))))

      (multiple-value-bind (output-file warnings fatal-errors)
	  (run-compiler *c-compiler*
			arguments
			output-file
			error-file
			error-output
			verbose)
	(if (and error (or (not output-file) fatal-errors))
	    (error "Compilation failed")
	    (values output-file warnings fatal-errors))))))


(define-language :c
  :compiler #'c-compile-file
  :loader #+:lucid #'load-foreign-files
          #+:allegro #'load
          #+(or :cmu :scl) #'alien:load-foreign
          #+:sbcl #'sb-alien:load-foreign
	  #+(and :lispworks :unix (not :linux) (not :macosx)) #'link-load:read-foreign-modules
	  #+(and :lispworks :unix (or :linux :macosx)) #'fli:register-module
	  #+(and :lispworks :win32) #'fli:register-module
          #+(or :ecl :gcl :kcl) #'load ; should be enough.
          #-(or :lucid
		:allegro
		:cmu
		:sbcl
		:scl
		:lispworks
		:ecl :gcl :kcl)
	  (lambda (&rest args)
	    (declare (ignore args))
	    (cerror "Continue returning NIL."
		    "Loader not defined for C foreign libraries in ~A ~A."
		    (lisp-implementation-type)
		    (lisp-implementation-version)))
  :source-extension "c"
  :binary-extension "o")


;;; Fortran Language definitions
;;; ----------------------------
;;;
;;; From Matlisp.

(export '(*fortran-compiler* *fortran-options*))

(defparameter *fortran-compiler* "g77")
(defparameter *fortran-options* '("-O"))

(defun fortran-compile-file (filename &rest args
				      &key output-file error-file
				      &allow-other-keys)
  (declare (ignore error-file args))
  (let ((arg-list
	 (append *fortran-options*
		 `("-c" ,filename ,@(if output-file `("-o" ,output-file))))))
    (run-unix-program *fortran-compiler* arg-list)))


(define-language :fortran
    :compiler #'fortran-compile-file
    :loader #'identity
    :source-extension "f"
    :binary-extension "o")


;;; Java Language Definition
;;; ------------------------

;;; This is very basic.  Essentially just a test to have Java
;;; files as dependencies.

(defparameter *java-compiler* "javac")
(defparameter *java-options* '())

(defun java-compile-file (filename &rest args
                                   &key output-file error-file
                                   &allow-other-keys)
  (declare (ignore args output-file error-file))
  (run-unix-program *java-compiler* (append *java-options* (list filename))))


(define-language :java
    :compiler #'java-compile-file
    :loader #'identity
    :source-extension "java"
    :binary-extension "class")


;;; Javascript Language Definition
;;; ------------------------------

;;; This is very basic.  Essentially just a test to have Javascript
;;; files as dependencies.

(defun javascript-compile-file (filename &rest args
                                         &key output-file error-file
                                         &allow-other-keys)
  (declare (ignore filename args output-file error-file))
  0 ; A "status".
  )


(define-language :javascript
    :compiler #'javascript-compile-file
    :loader #'identity
    :source-extension "js"
    :binary-extension "js")


;;; Python Language Definition
;;; --------------------------

;;; This is very basic.  Essentially just a test to have Python
;;; files as dependencies (cf., Javascript)

(defun python-compile-file (filename &rest args
                                     &key output-file error-file
                                     &allow-other-keys)
  (declare (ignore filename args output-file error-file))
  0 ; A "status".
  )


(define-language :python
    :compiler #'python-compile-file
    :loader #'identity
    :source-extension "py"
    :binary-extension "py")


;;; AR support
;;; ----------
;;;
;;; How to create a library (archive) of object files

(export '(*ar-program* build-lib))

(defparameter *ar-program* "ar")

(defun build-lib (libname directory)
  (let ((args (list "rv" (truename libname))))
    (tell-user-generic "Building archive ~A.~%" libname)
    (run-unix-program *ar-program*
		      (append args
			      (mapcar #'truename (directory directory))))))


;;; Component Operations
;;; ====================

;;; Define :compile/compile and :load/load operations

(eval-when (:load-toplevel :execute)
(component-operation :compile  'compile-and-load-operation)
(component-operation 'compile  'compile-and-load-operation)
(component-operation :load     'load-file-operation)
(component-operation 'load     'load-file-operation)
)


(defun compile-and-load-operation (component force)
  ;; FORCE was CHANGED. this caused defsystem during compilation to only
  ;; load files that it immediately compiled.

  (let ((changed (compile-file-operation component force)))
    ;; Return T if the file had to be recompiled and reloaded.
    (if (and changed (component-compile-only component))

	;; For files which are :compile-only T, compiling the file
	;; satisfies the need to load.
	changed

	;; If the file wasn't compiled, or :compile-only is nil,
	;; check to see if it needs to be loaded.
	(and (load-file-operation component force) ; FORCE was CHANGED ???
	     changed))))


(defun unmunge-lucid (namestring)
  ;; Lucid's implementation of COMPILE-FILE is non-standard, in that
  ;; when the :output-file is a relative pathname, it tries to munge
  ;; it with the directory of the source file. For example,
  ;; (compile-file "src/globals.lisp" :output-file "bin/globals.sbin")
  ;; tries to stick the file in "./src/bin/globals.sbin" instead of
  ;; "./bin/globals.sbin" like any normal lisp. This hack seems to fix the
  ;; problem. I wouldn't have expected this problem to occur with any
  ;; use of defsystem, but some defsystem users are depending on
  ;; using relative pathnames (at least three folks reported the problem).

  (cond ((null-string namestring) namestring)
	((char= (char namestring 0) #\/)
	 ;; It's an absolute namestring
	 namestring)
	(t
	 ;; Ugly, but seems to fix the problem.
	 (concatenate 'string "./" namestring))))

#+gcl
(defun ensure-directories-exist (arg0 &key verbose)
  (declare (ignore arg0 verbose))
  ())


(defun compile-file-operation (component force)
  ;; Returns T if the file had to be compiled.
  (let ((must-compile
	 ;; For files which are :load-only T, loading the file
	 ;; satisfies the demand to recompile.
	 (and (null (component-load-only component)) ; not load-only
	      (or (find force '(:all :new-source-all t) :test #'eq)
		  (and (find force '(:new-source :new-source-and-dependents)
			     :test #'eq)
		       (needs-compilation component nil)))))
	(source-pname (component-full-pathname component :source)))

    (cond ((and must-compile (probe-file source-pname))
	   (with-tell-user ("Compiling source" component :source)
	     (let ((output-file
		    #+:lucid
		     (unmunge-lucid (component-full-pathname component
							     :binary))
		     #-:lucid
		     (component-full-pathname component :binary)))

	       ;; make certain the directory we need to write to
	       ;; exists [pvaneynd@debian.org 20001114]
	       ;; Added PATHNAME-HOST following suggestion by John
	       ;; DeSoi [marcoxa@sourceforge.net 20020529]

	       (ensure-directories-exist
		(make-pathname
		 :host (pathname-host output-file)
		 :directory (pathname-directory output-file)))

	       (or *oos-test*
		   (apply (compile-function component)
			  source-pname
			  :output-file
			  output-file

			  #+(or :cmu :scl)
			  :error-file

			  #+(or :cmu :scl)
			  (and *cmu-errors-to-file*
			       (component-full-pathname component :error))

			  #+cmu
			  :error-output
			  #+cmu
			  *cmu-errors-to-terminal*

			  (component-compiler-options component)
			  ))))
	   must-compile)
	  (must-compile
	   (tell-user "Source file not found. Not compiling"
		      component :source :no-dots :force)
	   nil)
	  (t nil))))


;;; compiled-file-p --
;;; See CLOCC/PORT/sys.lisp:compiled-file-p

(eval-when (:load-toplevel :execute :compile-toplevel)
  (when (find-package "PORT")
    (import (find-symbol "COMPILED-FILE-P" "PORT"))))


(eval-when (:load-toplevel :execute :compile-toplevel)
(unless (fboundp 'compiled-file-p)
  (defun compiled-file-p (file-name)
    "Return T if the FILE-NAME is a filename designator for a valid compiled.
Signal an error when it is not a filename designator.
Return NIL when the file does not exist, or is not readable,
or does not contain valid compiled code."
    #+clisp
    (with-open-file (in file-name :direction :input :if-does-not-exist nil)
      (handler-bind ((error #'(lambda (c)
                                (declare (ignore c))
                                (return-from compiled-file-p nil))))
	(and in (char= #\( (peek-char nil in nil #\a))
	     (let ((form (read in nil nil)))
	       (and (consp form)
		    (eq (car form) 'SYSTEM::VERSION)
		    (null (eval form)))))))
    #-clisp (declare (ignorable file-name))
    #-clisp t))
)


(defun needs-compilation (component force)
  ;; If there is no binary, or it is older than the source
  ;; file, then the component needs to be compiled.
  ;; Otherwise we only need to recompile if it depends on a file that changed.
  (declare (ignore force))
  (let ((source-pname (component-full-pathname component :source))
        (binary-pname (component-full-pathname component :binary)))
    (and
     ;; source must exist
     (probe-file source-pname)
     (or
      ;; We force recompilation.
      #|(find force '(:all :new-source-all) :test #'eq)|#
      ;; no binary
      (null (probe-file binary-pname))
      ;; old binary
      (< (file-write-date binary-pname)
         (file-write-date source-pname))
      ;; invalid binary
      (not (compiled-file-p binary-pname))))))


(defun needs-loading (component &optional (check-source t) (check-binary t))
  ;; Compares the component's load-time against the file-write-date of
  ;; the files on disk.
  (let ((load-time (component-load-time component))
	(source-pname (component-full-pathname component :source))
	(binary-pname (component-full-pathname component :binary)))
    (or
     #|| ISI Extension ||#
     (component-load-always component)

     ;; File never loaded.
     (null load-time)
     ;; Binary is newer.
     (when (and check-binary
		(probe-file binary-pname))
       (< load-time
	  (file-write-date binary-pname)))
     ;; Source is newer.
     (when (and check-source
		(probe-file source-pname))
       (< load-time
	  (file-write-date source-pname))))))


;;; load-file-operation --
;;;
;;; Need to completely rework this function...

(defun load-file-operation (component force)
  ;; Returns T if the file had to be loaded
  (let* ((binary-pname (component-full-pathname component :binary))
	 (source-pname (component-full-pathname component :source))
	 (binary-exists (probe-file binary-pname))
	 (source-exists (probe-file source-pname))
	 (source-needs-loading (needs-loading component t nil))
	 (binary-needs-loading (needs-loading component nil t))
	 ;; needs-compilation has an implicit source-exists in it.
	 (needs-compilation (if (component-load-only component)
				source-needs-loading
				(needs-compilation component force)))
	 (check-for-new-source
	  ;; If force is :new-source*, we're checking for files
	  ;; whose source is newer than the compiled versions.
	  (find force '(:new-source :new-source-and-dependents :new-source-all)
		:test #'eq))
	 (load-binary (or (find force '(:all :new-source-all t) :test #'eq)
			  binary-needs-loading))
	 (load-source
	  (or *load-source-instead-of-binary*
	      (and load-binary (component-load-only component))
	      (and check-for-new-source needs-compilation)))
	 (compile-and-load
	  (and needs-compilation
               (or load-binary check-for-new-source)
	       (compile-and-load-source-if-no-binary component)))
         )
    ;; When we're trying to minimize the files loaded to only those
    ;; that need be, restrict the values of load-source and load-binary
    ;; so that we only load the component if the files are newer than
    ;; the load-time.
    (when (and *minimal-load*
               (not (find force '(:all :new-source-all)
		          :test #'eq)))
      (when load-source (setf load-source source-needs-loading))
      (when load-binary (setf load-binary binary-needs-loading)))

    (when (or load-source load-binary compile-and-load)
      (cond (compile-and-load
	     ;; If we're loading the binary and it is old or nonexistent,
	     ;; and the user says yes, compile and load the source.
	     (compile-file-operation component t)
	     (with-tell-user ("Loading binary"   component :binary)
	       (or *oos-test*
		   (progn
		     (funcall (load-function component) binary-pname)
		     (setf (component-load-time component)
			   (file-write-date binary-pname)))))
	     t)
	    ((and source-exists
		  (or (and load-source	; implicit needs-comp...
			   (or *load-source-instead-of-binary*
			       (component-load-only component)
			       (not *compile-during-load*)))
		      (and load-binary
                           (not binary-exists)
			   (load-source-if-no-binary component))))
	     ;; Load the source if the source exists and:
	     ;;   o  we're loading binary and it doesn't exist
	     ;;   o  we're forcing it
	     ;;   o  we're loading new source and user wasn't asked to compile
	     (with-tell-user ("Loading source" component :source)
	       (or *oos-test*
		   (progn
		     (funcall (load-function component) source-pname)
		     (setf (component-load-time component)
			   (file-write-date source-pname)))))
	     t)
	    ((and binary-exists load-binary)
	     (with-tell-user ("Loading binary"   component :binary)
	       (or *oos-test*
		   (progn
		     (funcall (load-function component) binary-pname)
		     (setf (component-load-time component)
			   (file-write-date binary-pname)))))
	     t)
	    ((and (not binary-exists) (not source-exists))
	     (tell-user-no-files component :force)
	     (when *files-missing-is-an-error*
	       (cerror "Continue, ignoring missing files."
		       "~&Source file ~S ~:[and binary file ~S ~;~]do not exist."
		       source-pname
		       (or *load-source-if-no-binary*
			   *load-source-instead-of-binary*)
		       binary-pname))
	     nil)
	    (t
	     nil)))))


(eval-when (:load-toplevel :execute)
(component-operation :clean 'delete-binaries-operation)
(component-operation 'clean 'delete-binaries-operation)
(component-operation :delete-binaries 'delete-binaries-operation)
(component-operation 'delete-binaries 'delete-binaries-operation)
)

(defun delete-binaries-operation (component force)
  (when (or (eq force :all)
	    (eq force t)
	    (and (find force '(:new-source :new-source-and-dependents
                               :new-source-all)
		       :test #'eq)
		 (needs-compilation component nil)))
    (let ((binary-pname (component-full-pathname component :binary)))
      (when (probe-file binary-pname)
	(with-tell-user ("Deleting binary" component :binary)
          (or *oos-test*
              (delete-file binary-pname)))))))


;; when the operation = :compile, we can assume the binary exists in test mode.
;;	((and *oos-test*
;;	      (eq operation :compile)
;;	      (probe-file (component-full-pathname component :source)))
;;	 (with-tell-user ("Loading binary"   component :binary)))

(defun binary-exists (component)
  (probe-file (component-full-pathname component :binary)))

;;; or old-binary

(defun compile-and-load-source-if-no-binary (component)
  (when (not (or *load-source-instead-of-binary*
		 (and *load-source-if-no-binary*
		      (not (binary-exists component)))))
    (cond ((component-load-only component)
	   #||
	   (let ((prompt (prompt-string component)))
	     (format t "~A- File ~A is load-only, ~
                        ~&~A  not compiling."
		     prompt
		     (component-full-pathname component :source)
		     prompt))
	   ||#
	   nil)

	  ((eq *compile-during-load* :query)
	   (let* ((prompt (prompt-string component))
		  (compile-source
		   (y-or-n-p-wait
		    #\y 30
		    "~&~A- Binary file ~A is old or does not exist. ~
                     ~&~A  Compile (and load) source file ~A instead? "
		    prompt
		    (component-full-pathname component :binary)
		    prompt
		    (component-full-pathname component :source))))
	     (unless (y-or-n-p-wait
		      #\y 30
		      "~A- Should I bother you if this happens again? "
		      prompt)
	       (setq *compile-during-load*
		     (y-or-n-p-wait
		      #\y 30
		      "~A- Should I compile while loading the system? "
		      prompt)))		; was compile-source, then t
	     compile-source))
	  (*compile-during-load*)
	  (t nil))))


(defun load-source-if-no-binary (component)
  (and (not *load-source-instead-of-binary*)
       (or (and *load-source-if-no-binary*
		(not (binary-exists component)))
	   (component-load-only component)
	   (when *bother-user-if-no-binary*
	     (let* ((prompt (prompt-string component))
		    (load-source
		     (y-or-n-p-wait #\y 30
                                    "~&~A- Binary file ~A does not exist. ~
                                     ~&~A  Load source file ~A instead? "
                                    prompt
                                    (component-full-pathname component :binary)
                                    prompt
                                    (component-full-pathname component :source))))
	       (setq *bother-user-if-no-binary*
		     (y-or-n-p-wait #\n 30
                                    "~&~A- Should I bother you if this happens again? "
                                    prompt))
	       (unless *bother-user-if-no-binary*
		 (setq *load-source-if-no-binary* load-source))
	       load-source)))))


;;; Allegro Common Lisp API
;;; =======================


;;; Allegro Toplevel Commands
;;; -------------------------
;;;
;;; Creates toplevel command aliases for Allegro CL.

#+:allegro
(top-level:alias ("compile-system" 8)
  (system &key force (minimal-load mk:*minimal-load*)
	  test verbose version)
  "Compile the specified system"

  (mk:compile-system system :force force
		     :minimal-load minimal-load
		     :test test :verbose verbose
		     :version version))


#+:allegro
(top-level:alias ("load-system" 5)
  (system &key force (minimal-load mk:*minimal-load*)
	  (compile-during-load mk:*compile-during-load*)
	  test verbose version)
  "Compile the specified system"

  (mk:load-system system :force force
		  :minimal-load minimal-load
		  :compile-during-load compile-during-load
		  :test test :verbose verbose
		  :version version))


#+:allegro
(top-level:alias ("show-system" 5) (system)
  "Show information about the specified system."

  (mk:describe-system system))


#+:allegro
(top-level:alias ("describe-system" 9) (system)
  "Show information about the specified system."

  (mk:describe-system system))


#+:allegro
(top-level:alias ("system-source-size" 9) (system)
  "Show size information about source files in the specified system."

  (mk:system-source-size system))


#+:allegro
(top-level:alias ("clean-system" 6)
  (system &key force test verbose version)
  "Delete binaries in the specified system."

  (mk:clean-system system :force force
		   :test test :verbose verbose
		   :version version))


#+:allegro
(top-level:alias ("edit-system" 7)
  (system &key force test verbose version)
  "Load system source files into Emacs."

  (mk:edit-system system :force force
		  :test test :verbose verbose
		  :version version))


#+:allegro
(top-level:alias ("hardcopy-system" 9)
  (system &key force test verbose version)
  "Hardcopy files in the specified system."

  (mk:hardcopy-system system :force force
		      :test test :verbose verbose
		      :version version))


#+:allegro
(top-level:alias ("make-system-tag-table" 13) (system)
  "Make an Emacs TAGS file for source files in specified system."

  (mk:make-system-tag-table system))


;;; Allegro Make System Fasl
;;; ------------------------

#+:excl
(defun allegro-make-system-fasl (system destination
					&optional (include-dependents t))
  (excl:shell
   (format nil "rm -f ~A; cat~{ ~A~} > ~A"
	   destination
	   (if include-dependents
	       (files-in-system-and-dependents system :all :binary)
	       (files-in-system system :all :binary))
	   destination)))


;;; Pathnames, Directories and Files within Systems
;;; ===============================================

(defun component-source-pathnames (c)
  "Return a list of source pathanmes associated to each component in C.

The list is returned by a pre-order traversal of the component
hierarchical structure.  Files are obviously the base case."
  (declare (type component c))

  (typecase c
    (file (list (component-pathname c :source)))
    (module (list* (component-root-dir c :source)
                   (mapcan #'component-source-pathnames (component-components c))))
    ))


(defun files-which-need-compilation (system)
  (mapcar #'(lambda (comp) (component-full-pathname comp :source))
	  (remove nil
		  (file-components-in-component
		   (find-system system :load) :new-source))))


(defun files-in-system-and-dependents (name &optional
                                            (force :all)
					    (type :source)
                                            version)
  ;; Returns a list of the pathnames in system and dependents in load order.
  (let ((system (find-system name :load)))
    (multiple-value-bind (*version-dir* *version-replace*)
	(translate-version version)
      (let ((*version* version))
	(let ((result (file-pathnames-in-component system type force)))
	  (dolist (dependent (reverse (component-depends-on system)))
	    (setq result
		  (append (files-in-system-and-dependents dependent
							  force type version)
			  result)))
	  result)))))


(defun files-in-system (name &optional
                             (force :all)
                             (type :source)
                             version)
  ;; Returns a list of the pathnames in system in load order.
  (let ((system (if (and (component-p name)
                         (member (component-type name)
                                 '(:defsystem :system :subsystem)))
                    name
                    (find-system name :load))))
    (multiple-value-bind (*version-dir* *version-replace*)
	(translate-version version)
      (let ((*version* version))
	(file-pathnames-in-component system type force)))))


(defun file-pathnames-in-component (component type &optional (force :all))
  (mapcar #'(lambda (comp)
              ;; (pathname (component-full-pathname comp type))
              (pathname (component-pathname comp type))
              )
	  (file-components-in-component component force)))


(defun file-components-in-component (component &optional (force :all)
					       &aux result changed)
  (case (component-type component)
    ((:file :private-file :static-file) #| old (:file :private-file) |#
     (when (setq changed
		 (or (find force '(:all t) :test #'eq)
		     (and (not (non-empty-listp force))
			  (needs-compilation component nil))))
       (setq result
	     (list component))))

    ((:module :system :subsystem :defsystem)
     (dolist (module (component-components component))
       (multiple-value-bind (r c)
	   (file-components-in-component
	    module
	    (cond ((and (some #'(lambda (dependent)
				  (member dependent changed))
			      (component-depends-on module))
			(or (non-empty-listp force)
			    (eq force :new-source-and-dependents)))
		   ;; The component depends on a changed file and force agrees.
		   :all)
		  ((and (non-empty-listp force)
			(member (component-name module) force
				:test #'string-equal :key #'string))
		   ;; Force is a list of modules and the component is
		   ;; one of them.
		   :all)
		  (t force)))
	 (when c
	   (push module changed)
	   (setq result (append result r))))))
    )
  (values result changed))


(eval-when (:load-toplevel :execute)
  ;; Remove :compile-toplevel; SBCL does have a point.
  ;; (:load-toplevel :compile-toplevel :execute)

  (setf (symbol-function 'oos) (symbol-function 'operate-on-system)))


;;; Additional Component Operations
;;; ===============================

;;; Edit Operation
;;; --------------

;;; Should this conditionalization be (or :mcl (and :CCL (not :lispworks)))?

#||
#+:ccl
(defun edit-operation (component force)
  "Always returns nil, i.e. component not changed."
  (declare (ignore force))
  ;;
  (let* ((full-pathname (make::component-full-pathname component :source))
         (already-editing\? #+:mcl (dolist (w (CCL:windows :class
                                                           'fred-window))
                                     (when (equal (CCL:window-filename w)
                                                  full-pathname)
                                       (return w)))
                            #-:mcl nil))
    (if already-editing\?
        #+:mcl (CCL:window-select already-editing\?) #-:mcl nil
        (ed full-pathname)))
  nil)

 
#+:allegro
(defun edit-operation (component force)
  "Edit a component - always returns nil, i.e. component not changed."
  (declare (ignore force))
  (let ((full-pathname (component-full-pathname component :source)))
    (ed full-pathname))
  nil)

#+(or :ccl :allegro)
(make::component-operation :edit 'edit-operation)

#+(or :ccl :allegro)
(make::component-operation 'edit 'edit-operation)
||#
				     

;;; Hardcopy System
;;; ---------------

(defparameter *print-command* "enscript -2Gr" ; "lpr"
  "Command to use for printing files on UNIX systems.")


#+:allegro
(defun hardcopy-operation (component force)
  "Hardcopy a component - always returns nil, i.e. component not changed."
  (declare (ignore force))
  (let ((full-pathname (component-full-pathname component :source)))
    (excl:run-shell-command (format nil "~A ~A"
				    *print-command* full-pathname)))
  nil)

#+:allegro
(make::component-operation :hardcopy 'hardcopy-operation)
#+:allegro
(make::component-operation 'hardcopy 'hardcopy-operation)


;;; System Source Size
;;; ------------------

(defun system-source-size (system-name &optional (force :all))
  "Prints a short report and returns the size in bytes of the source
files in SYSTEM-NAME."

  (let* ((file-list (files-in-system system-name force :source))
         (total-size (file-list-size file-list)))
    (format t "~&~a/~a (~:d file~:p) totals ~:d byte~:p (~:d kB)"
            system-name force (length file-list)
            total-size (round total-size 1024))
    total-size))


(defun file-list-size (file-list)
  "Returns the size in bytes of the files in FILE-LIST."

  (let ((total-size 0))
    (dolist (file file-list)
      (with-open-file (stream file)
        (incf total-size (file-length stream))))
    total-size))


;;; System Tag Table
;;; ----------------

#+:allegro
(defun make-system-tag-table (system-name)
  "Makes an Emacs tag table using the GNU etags program."
  (let ((files-in-system (files-in-system system-name :all :source)))

    (format t "~&Making tag table...")
    (excl:run-shell-command (format nil "etags ~{~a ~}" files-in-system))
    (format t "done.~%")))


;;; :static-file
;;; 
;;; Notes:
;;;
;;; 20211102 MA: this is Madhu's :static-file hack.  Essentially this
;;; is a different component type that uses :language to introduce
;;; specialized load and compile operations (which, FTTB, are
;;; practically no-ops).
;;; The problem with the original definition is that it introduces a
;;; :empty source and binary extension that then necessitates a change
;;; in COMPONENT-EXTENSION to special case it.  Yet, the final result
;;; in Madhu's code is always NIL.  Hence the defition below.
;;; The implementation strategy is left as-is in order to minimize
;;; other changes, e.g., in CREATE-COMPONENT.


;;; static-file-loader-fun
;;; (LOAD filespec &rest args &key verbose print if-does-not-exist external-format)
;;;  => generalized-boolean

(defun static-file-loader-fun (filespec &rest args)
  (format t "~S~%" `(mk-static-file-loader ,filespec ,@args))
  (values t))


;;; static-file-compiler
;;; (COMPILE-FILE input-file &key output-file verbose print external-format)
;;;  => output-truename, warnings-p, failure-p

(defun static-file-compiler-fun (input-file &rest args &key output-file &allow-other-keys)
  (format t "~S~%" `(mk-static-file-compiler ,input-file ,@args))
  (values (probe-file output-file) nil nil))


(mk:define-language :static-file
		    :loader #'static-file-loader-fun
		    :compiler #'static-file-compiler-fun
		    :source-extension nil ; Was :empty in Madhu's code.
		    :binary-extension nil ; Ditto.
                    )


;;; find-component --

#+madhu
(defun find-component (base path)
  (when (setq base (typecase base
		     (component base)
		     (t (find-system base))))
    (if (null path)
	base
	(let* ((path (if (atom path) (list path) path))
	       (components (component-components base))
               )

	  (loop
	     (cond ((or (endp path) (endp components))
		    (return nil))

		   ((equal (canonicalize-component-name (first components))
			   (if (stringp (first path))
			       (first path)
			       (string-downcase (string (first path)))))
		    (cond ((rest path)
			   (setq components (component-components
					     (first components)))
			   (setq path (rest path)))
			  (t (return (first components)))))

		   (t (setq components (rest components)))))))))


(defun find-component* (base &rest path)
  "Finds a component that is a (sub) component of BASE, following PATH.

The result is either a component (if found) or NIL.

Exceptional Situations:

The function signals a type error if BASE is neither a MK::COMPONENT
or a string or symbol designating a MK:SYSTEM (looked up via
MK:FIND-SYSTEM).
"
  (let ((base (etypecase base
                (component base)
                ((or string symbol) (find-system base :load-or-nil))
                ))
        )
    (labels ((find-component-aux (base path)
               (if (null path)
                   base
                   (let ((next-base (find (first path)
                                          (component-components base)
                                          :test #'equal
                                          :key #'component-name))
                         )
                     (when next-base
                       (find-component-aux next-base (rest path))))
                   ))
             )
      (when base
        (find-component-aux base path))
      )))


(defun find-component (base path)
  "Finds a component that is a (sub) component of BASE, following PATH.

The result is either a component (if found) or NIL.

Notes:

This function has this signature to be compatible with madhu's
version.

See Also:

MK:FIND-COMPONENT*
"
  (declare (type list path))
  (apply #'find-component* base path))


;;; Epilogue
;;; --------

(provide 'make)
(pushnew :mk-defsystem *features*)


;;; end of file -- defsystem.lisp --
