overview
--------
elm (erlang library manager) lets you install, uninstall, and update your erlang
libraries with a simple command-line interface. Because elm uses your vcs tools,
(git, svn, and hg), it can rapidly detect which libraries are out of date and
update them with minimal network usage. Dependency checking is coming soon.

installation
------------
1. `git clone git://github.com/dweldon/elm.git`
2. `cd elm && make`
3. place elm in your path, e.g. `sudo ln -s /usr/local/lib/erlang/lib/elm/elm /usr/local/bin/elm`
4. install hg, git, and/or svn as needed for your repositories

getting started
---------------
1. see availabe commands with `elm help`
2. build your .elm file (see next section)
3. install your libraries, e.g. `elm install mochiweb ibrowse`

.elm file
---------
The following example sets your library install directory and adds ibrowse,
mochiweb, and webmachine to the list of available libraries.

    elm set-directory /usr/local/lib/erlang/lib
    elm add-url git://github.com/cmullaparthi/ibrowse.git
    elm add-url http://mochiweb.googlecode.com/svn/trunk
    elm add-url http://bitbucket.org/justin/webmachine

This will create a .elm file in your $HOME directory that looks like:

    #/usr/local/lib/erlang/lib 
    git://github.com/dweldon/elm.git
    git://github.com/cmullaparthi/ibrowse.git
    http://mochiweb.googlecode.com/svn/trunk
    http://bitbucket.org/justin/webmachine

examples
--------
install mochiweb and ibrowse
    elm install mochiweb ibrowse
    installing mochiweb... built
    installing ibrowse... built

see which libraries are installed
    elm list
    + elm
    + ibrowse
    + mochiweb
      webmachine

update all installed libraries
    elm update
    updating ibrowse... built

uninstall mochiweb
    elm uninstall mochiweb
    uninstalled mochiweb

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
* hg repositories currently only work on googlecode.com, basho.com, and bitbucket.org
* automatic building currently only works for libraries which use make or rebar
