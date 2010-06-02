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

-module(elm_library_git).
-export([name/1, install/1, update/1]).
-include_lib("eunit/include/eunit.hrl").
-include_lib("elm.hrl").

% @spec name(Url::string()) -> {ok, Name::string()} | error
name(Url) ->
    case elm_string:ends_with(Url, ".git") of
        true -> {ok, lists:nth(2, lists:reverse(string:tokens(Url, "/.")))};
        false -> error
    end.

% @spec install(library()) -> any()
install(#library{type=git, url=Url, parent_directory=Directory}) ->
    file:set_cwd(Directory),
    os:cmd("git clone " ++ Url).

% @spec update(library()) -> ok | error
update(#library{name=Name, type=git, parent_directory=Directory}) ->
    file:set_cwd(filename:join(Directory, Name)),
    Response = os:cmd("git pull"),
    case elm_string:contains(Response, "Updating") of
        true -> ok;
        false -> error
    end.

name_test_() ->
    [?_assertEqual(error, name("")),
     ?_assertEqual({ok, "elm"}, name("git://github.com/dweldon/elm.git")),
     ?_assertEqual({ok, "elm"}, name("git@github.com:dweldon/elm.git")),
     ?_assertEqual(error, name("http://elm.googlecode.com/svn/trunk")),
     ?_assertEqual(error, name("http://bitbucket.org/name/elm")),
     ?_assertEqual(error, name("http://hg.basho.com/elm")),
     ?_assertEqual(error, name("https://elm.googlecode.com/hg/ elm")),
     ?_assertEqual(error, name("https://elm.googlecode.com/hg/elm"))].
