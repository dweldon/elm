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

-module(elm_string).
-export([begins_with/2, ends_with/2, contains/2]).
-include_lib("eunit/include/eunit.hrl").

begins_with(String, SubString) ->
    string:substr(String, 1, length(SubString)) =:= SubString.

ends_with(String, SubString) ->
    begins_with(lists:reverse(String), lists:reverse(SubString)).

contains(String, SubString) ->
    string:str(String, SubString) > 0.

begins_with_test_() ->
    [?_assertEqual(true, begins_with("foobar", "foo")),
     ?_assertEqual(false, begins_with("foobar", "bar"))].

ends_with_test_() ->
    [?_assertEqual(false, ends_with("foobar", "foo")),
     ?_assertEqual(true, ends_with("foobar", "bar"))].

contains_test_() ->
    [?_assertEqual(true, contains("foobar", "foo")),
     ?_assertEqual(true, contains("foobar", "bar")),
     ?_assertEqual(true, contains("foobar", "oba")),
     ?_assertEqual(false, contains("foobar", "car"))].
