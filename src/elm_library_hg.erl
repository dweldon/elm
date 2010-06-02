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

-module(elm_library_hg).
-export([name/1, install/1, update/1]).
-include_lib("eunit/include/eunit.hrl").
-include_lib("elm.hrl").

% @spec name(Url::string()) -> {ok, Name::string()} | error
name(Url) ->
    IsBitbucket = elm_string:contains(Url, "bitbucket.org"),
    IsBasho = elm_string:contains(Url, "hg.basho.com"),
    IsGoogle = elm_string:contains(Url, "/hg/") andalso
               elm_string:contains(Url, "googlecode.com"),
    case IsBitbucket orelse IsBasho orelse IsGoogle of
        true -> {ok, string:strip(lists:last(string:tokens(Url, "/")))};
        false -> error
    end.

% @spec install(library()) -> any()
install(#library{type=hg, url=Url, parent_directory=Directory}) ->
    file:set_cwd(Directory),
    os:cmd("hg clone " ++ Url).

% @spec update(library()) -> ok | error
update(#library{name=Name, type=hg, parent_directory=Directory}) ->
    file:set_cwd(filename:join(Directory, Name)),
    Response = os:cmd("hg pull"),
    os:cmd("hg update"),
    case elm_string:contains(Response, "adding changesets") of
        true -> ok;
        false -> error
    end.

name_test_() ->
    [?_assertEqual(error, name("")),
     ?_assertEqual(error, name("git://github.com/dweldon/elm.git")),
     ?_assertEqual(error, name("git@github.com:dweldon/elm.git")),
     ?_assertEqual(error, name("http://elm.googlecode.com/svn/trunk")),
     ?_assertEqual({ok, "elm"}, name("http://bitbucket.org/name/elm")),
     ?_assertEqual({ok, "elm"}, name("http://hg.basho.com/elm")),
     ?_assertEqual({ok, "elm"}, name("https://elm.googlecode.com/hg/ elm")),
     ?_assertEqual({ok, "elm"}, name("https://elm.googlecode.com/hg/elm"))].
