overview
--------
elm (erlang library manager) lets you install and upgrade your erlang libraries
without using a central repository. Based on a simple config file, it pulls
source code directly from your favorite git, mercurial, or svn repositories.

installation
------------
1. `git clone git://github.com/dweldon/elm.git`
2. create a .elm file in your home directory (see below)
3. run elm with no arguments for help
4. make sure to have hg, git, or svn installed as needed for your repositories

.elm files
----------
Create a .elm file in your home directory. The following example shows a .elm
file which will allow us to install ibrowse, mochiweb, and webmachine:

    #/usr/local/lib/erlang/lib
    git://github.com/cmullaparthi/ibrowse.git
    http://mochiweb.googlecode.com/svn/trunk
    http://bitbucket.org/justin/webmachine

If the first line begins with a #, elm interprets this as the library
installation directory (otherwise `code:lib_dir()` is used). The remaining lines
are the repository urls that elm will use for code updates. All blank lines and
lines which begin with # (other than the first line) will be ignored.

how it works
------------
elm detects the repository type and library name based on the repository urls in
the .elm file. Because of this, some repository types may not work automatically
(see limitations). Code is automatically built if elm sees a Makefile or a rebar
binary in the install directory.

limitations
-----------
* only hg, svn, and git are currently supported
* svn repositories currently only work on googlecode.com
* hg repositories currently only work on googlecode.com and bitbucket.org
* automatic building currently only works for libraries which use make or rebar

examples
--------
install mochiweb and ibrowse
    elm install mochiweb ibrowse
    ibrowse installed... built
    mochiweb installed... built

check that that these were installed
    elm list
    + ibrowse
    + mochiweb
      webmachine

some time later, upgrade all installed libraries
    elm upgrade
    ibrowse upgraded... built

remove mochiweb
    elm remove mochiweb
    mochiweb removed
