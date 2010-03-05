#!/usr/bin/env escript

%% Copyright (c) 2010 David Weldon
%% This program is free software: you can redistribute it and/or modify
%% it under the terms of the GNU General Public License as published by
%% the Free Software Foundation, either version 3 of the License, or
%% (at your option) any later version.
%%
%% This program is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU General Public License for more details.
%%
%% You should have received a copy of the GNU General Public License
%% along with this program.  If not, see <http://www.gnu.org/licenses/>.

-export([main/1]).

main(["list"]) ->
    {install_dir, _, libs, Libs} = read_config(),
    [print_lib(L) || L <- Libs];
main(["upgrade"]) ->
    {install_dir, Dir, libs, Libs} = read_config(),
    [upgrade(Dir, L) || L <- Libs];
main(["install"|LibNames]) ->
    {install_dir, Dir, libs, Libs} = read_config(),
    Filter = fun({name, Name, _, _, _, _, _, _}) ->
                 lists:member(Name, LibNames)
             end,
    ToInstall = lists:filter(Filter, Libs),
    [install(Dir, L) || L <- ToInstall];
main(["remove"|LibNames]) ->
    {install_dir, Dir, libs, _} = read_config(),
    [remove(Dir, L) || L <- LibNames];
main(_) ->
    usage().

usage() ->
    io:format("~nUsage:~n"),
    io:format("\telm list - list all available libraries~n"),
    io:format("\telm upgrade - upgrade installed libraries~n"),
    io:format("\telm install [Libraries] - install the named libraries~n"),
    io:format("\telm remove [Libraries] - remove the named libraries~n"),
    io:format("~n"),
    io:format("\texample:~n"),
    io:format("\telm install mochiweb ibrowse estring~n~n"),
    halt(1).

print_lib({name, Name, type, _, inst, true, url, _}) ->
    io:format("+ ~s~n", [Name]);
print_lib({name, Name, type, _, inst, false, url, _}) ->
    io:format("  ~s~n", [Name]).

upgrade(_, {name, _, type, _, inst, false, url, _}) ->
    ok;
upgrade(Dir, {name, Name, type, git, inst, true, url, _}) ->
    Path = filename:join(Dir, Name),
    file:set_cwd(Path),
    Response = os:cmd("git pull"),
    check_upgrade_and_build(Name, Response, "Updating", Path);
upgrade(Dir, {name, Name, type, svn, inst, true, url, _}) ->
    Path = filename:join(Dir, Name),
    file:set_cwd(Path),
    Response = os:cmd("svn update"),
    check_upgrade_and_build(Name, Response, "Updated to revision", Path);
upgrade(Dir, {name, Name, type, hg, inst, true, url, _}) ->
    Path = filename:join(Dir, Name),
    file:set_cwd(Path),
    Response = os:cmd("hg pull"),
    os:cmd("hg update"),
    check_upgrade_and_build(Name, Response, "adding changesets", Path).

% assumes Path is the full path to the library
check_upgrade_and_build(Name, Response, Contains, Path) ->
    case contains(Response, Contains) of
        true ->
            io:format("~s upgraded... ", [Name]),
            build(Path);
        false -> ok
    end.

remove(Dir, Name) ->
    Path = filename:join(Dir, Name),
    os:cmd("rm -rf " ++ Path),
    case filelib:is_dir(Path) of
        false ->
            io:format("~s removed~n", [Name]);
        true ->
            io:format("~s not removed~n", [Name])
    end.

install(Dir, Lib = {name, _, type, _, inst, true, url, _}) ->
    upgrade(Dir, Lib);
install(Dir, {name, Name, type, git, inst, false, url, Url}) ->
    file:set_cwd(Dir),
    os:cmd("git clone " ++ Url),
    check_install_and_build(Name, filename:join(Dir, Name));
install(Dir, {name, Name, type, svn, inst, false, url, Url}) ->
    file:set_cwd(Dir),
    os:cmd("svn co " ++ Url ++ " " ++ Name),
    check_install_and_build(Name, filename:join(Dir, Name));
install(Dir, {name, Name, type, hg, inst, false, url, Url}) ->
    file:set_cwd(Dir),
    os:cmd("hg clone " ++ Url),
    check_install_and_build(Name, filename:join(Dir, Name)).

% assumes Path is the full path to the library
check_install_and_build(Name, Path) ->
    case filelib:is_dir(Path) of
        true ->
            io:format("~s installed... ", [Name]),
            build(Path);
        false ->
            io:format("~s not installed~n", [Name])
    end.

% assumes Path is the full path to the library
build(Path) ->
    file:set_cwd(Path),
    {ok, Files} = file:list_dir("."),
    case {lists:member("Makefile", Files), lists:member("rebar", Files)} of
        {true, _} ->
            os:cmd("make"),
            io:format("built~n");
        {_, true} ->
            os:cmd("./rebar compile"),
            io:format("built~n");
        {false, false} ->
            io:format("not built~n")
    end.

% returns {install_dir, string(), libs, [Lib]}
% where Lib = {name, string(), type, atom(), inst, bool(), url, string()}
read_config() ->
    FileName = config_file_name(),
    validate(file, FileName),
    {ok, FileBinary} = file:read_file(FileName),
    Lines = string:tokens(erlang:binary_to_list(FileBinary), "\n"),
    Dir = install_dir(hd(Lines)),
    validate(dir, Dir),
    {install_dir, Dir, libs, lib_info(Dir, Lines, [])}.

% returns [Lib]
% where Lib = {name, string(), type, atom(), inst, bool(), url, string()}
lib_info(_, [], I) ->
    lists:reverse(I);
lib_info(Dir, [H|T], I) when hd(H) =/= $# ->
    Line = string:strip(H),
    case lib_info(Dir, Line) of
        {ok, Info} -> lib_info(Dir, T, [Info|I]);
        {error, unknown} -> lib_info(Dir, T, I)
    end;
lib_info(Dir, [_H|T], I) ->
    lib_info(Dir, T, I).

% returns {ok, Lib} | {error, unknown}
% where Lib = {name, string(), type, atom(), inst, bool(), url, string()}
lib_info(Dir, Url) ->
    case repository_type(Url) of
        {error, unknown} ->
            {error, unknown};
        {ok, Type} ->
            Name = lib_name(Url, Type),
            Installed = is_installed(Dir, Name),
            Info = {name, Name, type, Type, inst, Installed, url, Url},
            {ok, Info}
    end.    

is_installed(Dir, Name) ->
    Path = filename:join(Dir, Name),
    filelib:is_dir(Path).

% returns string()
lib_name(Url, git) ->
    lists:nth(2, lists:reverse(string:tokens(Url, "/.")));
lib_name(Url, svn) ->
    lists:nth(2, string:tokens(Url, "/."));
lib_name(Url, hg) ->
    string:strip(lists:last(string:tokens(Url, "/"))).

% returns {ok, git} | {ok, svn} | {ok, hg} | {error, unknown}
repository_type(Url) ->
    case {is_git(Url), is_svn(Url), is_hg(Url)} of
        {true, _, _} -> {ok, git};
        {_, true, _} -> {ok, svn};
        {_, _, true} -> {ok, hg};
        _ -> {error, unknown}
    end.

% git works for all git repositories
is_git(Url) ->
    ends_with(Url, ".git").

% svn only works on googlecode
is_svn(Url) ->
    contains(Url, "/svn/") andalso contains(Url, "googlecode.com").

% hg only works on bitbucket and googlecode
is_hg(Url) ->
    contains(Url, "bitbucket.org") orelse
    (contains(Url, "/hg/") andalso contains(Url, "googlecode.com")).

begins_with(String, SubString) ->
    string:substr(String, 1, length(SubString)) =:= SubString.

ends_with(String, SubString) ->
    begins_with(lists:reverse(String), lists:reverse(SubString)).

contains(String, SubString) ->
    string:str(String, SubString) > 0.

install_dir([$#|Path]) ->
    string:strip(Path);
install_dir(_) ->
    % return the code path by default
    code:lib_dir().

config_file_name() ->
    HomeDir = os:getenv("HOME"),
    case is_list(HomeDir) of
        false ->
            io:format("error: could not read $HOME~n"),
            halt(1);
        true ->
            filename:join(HomeDir, ".elm")
    end.

validate(file, FileName) ->
    case filelib:is_file(FileName) of
        false ->
            io:format("error: could not find file ~s~n", [FileName]),
            halt(1);
        true -> ok
    end;
validate(dir, DirName) ->
    case filelib:is_dir(DirName) of
        false ->
            io:format("error: could not find dir ~s~n", [DirName]),
            halt(1);
        true -> ok
    end.
