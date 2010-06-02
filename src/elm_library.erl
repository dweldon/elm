%% Copyright (c) 2010 David Weldon
%% This file is part of elm.
%% elm is free software: you can redistribute it and/or modify
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

-module(elm_library).
-include_lib("elm.hrl").
-export([display/0, install/1, update/0, uninstall/1]).

% @spec display() -> any()
display() ->
    [display(L) || L <- libraries()].

display(#library{name=Name, installed=true}) ->
    io:format("+ ~s~n", [Name]);
display(#library{name=Name, installed=false}) ->
    io:format("  ~s~n", [Name]).

% @spec install(Names::list()) -> any()
install(Names) ->
    Libraries = libraries(),
    InstallFun =
        fun(Name) ->
            case find_library(Name, Libraries) of
                {ok, Library} -> install_library(Library);
                error -> io:format("~s not found~n", [Name])
            end
        end,
    lists:map(InstallFun, Names).

% @spec install_library(Library::library()) -> any()
install_library(#library{name=Name, installed=true}) ->
    io:format("~s already installed~n", [Name]);
install_library(Library=#library{name=Name, module=Module}) ->
    io:format("installing ~s... ", [Name]),
    apply(Module, install, [Library]),
    case build_library(Library) of
        ok -> io:format("built~n");
        error -> io:format("not built~n")
    end.

% @spec update() -> any()
update() ->
    [update_library(Library) || Library <- libraries()].

% @spec update_library(Library::library()) -> any()
update_library(#library{installed=false}) -> ok;
update_library(Library=#library{name=Name, module=Module}) ->
    case apply(Module, update, [Library]) of
        ok ->
            io:format("updating ~s... ", [Name]),
            case build_library(Library) of
                ok -> io:format("built~n");
                error -> io:format("not built~n")
            end;
        error -> ok
    end.

% @spec uninstall(Names::list()) -> any()
uninstall(Names) ->
    Libraries = libraries(),
    UninstallFun =
        fun(Name) ->
            case find_library(Name, Libraries) of
                {ok, Library} -> uninstall_library(Library);
                error -> io:format("~s not found~n", [Name])
            end
        end,
    lists:map(UninstallFun, Names).

% @spec uninstall_library(Library::library()) -> any()
uninstall_library(#library{name=Name, installed=false}) ->
    io:format("~s not installed~n", [Name]);
uninstall_library(Library=#library{name=Name, installed=true}) ->
    io:format("uninstalled ~s~n", [Name]),
    Path = filename:join(Library#library.parent_directory, Name),
    os:cmd("rm -rf " ++ Path).

% @spec libraries() -> Libraries::list()
libraries() ->
    #config{directory=Directory, urls=Urls} = elm_config:read(),
    libraries(Directory, Urls, []).

libraries(_, [], Libraries) ->
    lists:reverse(Libraries);
libraries(Directory, [H|T], Libraries) ->
    case url_to_library(Directory, H) of
        {ok, L} -> libraries(Directory, T, [L|Libraries]);
        error -> libraries(Directory, T, Libraries)
    end.

% @spec url_to_library(Directory::string(), Url::string()) ->
% {ok, Library::library()} | error
url_to_library(Directory, Url) ->
     LibraryInfo =
        case {elm_library_git:name(Url),
              elm_library_svn:name(Url),
              elm_library_hg:name(Url)} of
            {error, error, error} -> error;
            {error, error, {ok, N}} -> {N, hg, elm_library_hg};
            {error, {ok, N}, error} -> {N, svn, elm_library_svn};
            {{ok, N}, error, error} -> {N, git, elm_library_git}
        end,
    case LibraryInfo of
        {Name, Type, Module} ->
            Installed = filelib:is_dir(filename:join(Directory, Name)),
            {ok, #library{name=Name, type=Type, installed=Installed, url=Url,
                          parent_directory=Directory, module=Module}};
        error -> error
    end.

% @spec build_library(Library::library()) -> ok | error
build_library(#library{name=Name, parent_directory=Directory}) ->
    file:set_cwd(filename:join(Directory, Name)),
    {ok, Files} = file:list_dir("."),
    case {lists:member("Makefile", Files), lists:member("rebar", Files)} of
        {true, _} ->
            os:cmd("make"),
            ok;
        {_, true} ->
            os:cmd("./rebar compile"),
            ok;
        {false, false} -> error
    end.

% @spec find_library(Name::string(), Libraries::list()) ->
% {ok, Library::library()} | error
find_library(_, []) -> error;
find_library(Name, [Library=#library{name=Name}|_]) -> {ok, Library};
find_library(Name, [_|Libraries]) -> find_library(Name, Libraries).
