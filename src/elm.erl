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

-module(elm).
-export([main/1]).
-include_lib("elm.hrl").

main(["list"]) ->
    elm_library:display();
main(["update"]) ->
    elm_library:update();
main(["install"|Names]) ->
    elm_library:install(Names);
main(["uninstall"|Names]) ->
    elm_library:uninstall(Names);
main(["set-directory"|[Directory|_]]) ->
    elm_config:set_directory(Directory);
main(["add-url"|[Url|_]]) ->
    elm_config:add_url(Url);
main(_) ->
    elm_config:read(),
    usage().

usage() ->
    io:format("~nelm usage:~n~n"),
    io:format("\telm list                   list all libraries~n"),
    io:format("\telm update                 update installed libraries~n"),
    io:format("\telm install [Names]        install the named libraries~n"),
    io:format("\telm uninstall [Names]      uninstall the named libraries~n"),
    io:format("\telm set-directory [Dir]    set the library install directory~n"),
    io:format("\telm add-url [URL]          add URL to the list of repository URLs~n"),
    io:format("~n"),
    io:format("\texample:~n"),
    io:format("\t\telm install mochiweb ibrowse estring~n~n").
