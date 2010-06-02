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

-module(elm_config).
-export([read/0, set_directory/1, add_url/1]).
-define(DEFAULT_DIRECTORY, "/usr/local/lib/erlang/lib").
-include_lib("elm.hrl").

% @spec read() -> Config::config()
% @doc read the .elm file and return a config record. if the .elm file does not
% exist, one is created.
read() ->
    create(),
    {ok, FileBinary} = file:read_file(file_name()),
    Lines = string:tokens(erlang:binary_to_list(FileBinary), "\n"),
    Directory = case hd(Lines) of
        "#" ++ D -> D;
        _ -> ?DEFAULT_DIRECTORY
    end,
    Urls = parse_urls(Lines),
    #config{directory=Directory, urls=Urls}.

% @spec set_directory(Directory::string()) -> any()
set_directory(Directory) ->
    case filelib:is_dir(Directory) of
        true ->
            OldConfig = read(),
            write(OldConfig#config{directory=Directory});
        false ->
            io:format("error: ~s is not valid~n", [Directory])
    end.

% @spec add_url(Url::string()) -> any
% @doc adds Url to the config file if it was not already a member.
add_url(Url) ->
    Config = read(),
    OldUrls = [string:strip(U) || U <- Config#config.urls],
    NewUrl = string:strip(Url),
    case lists:member(NewUrl, OldUrls) of
        true -> ok;
        false ->
            NewUrls = OldUrls ++ [NewUrl],
            write(Config#config{urls=NewUrls})
    end.

% @spec write(Config:config()) -> any()
write(#config{directory=Directory, urls=Urls}) ->
    Lines = string:join(["#" ++ Directory|Urls], "\n"),
    file:write_file(file_name(), Lines).

% @spec create() -> any()
% @doc create a .elm file in the user's home directory if it does not exist.
create() ->
    case filelib:is_file(file_name()) of
        true -> ok;
        false ->
            ElmUrl = "git://github.com/dweldon/elm.git",
            write(#config{directory=?DEFAULT_DIRECTORY, urls=[ElmUrl]})
    end.

% @spec parse_urls(Lines::list()) -> Urls::list()
% @doc returns a list of urls from the input list of lines - rejects blank lines
% and lines which begin with #.
parse_urls(Lines) -> parse_urls(Lines, []).

parse_urls([], Urls) ->
    lists:reverse(Urls);
parse_urls([H|T], Urls) ->
    case string:strip(H) of
        "#" ++ _ -> parse_urls(T, Urls);
        [] -> parse_urls(T, Urls);
        Url -> parse_urls(T, [Url|Urls])
    end.

% @spec file_name() -> FileName::string()
file_name() ->
    Home = os:getenv("HOME"),
    case is_list(Home) of
        true ->
            filename:join(Home, ".elm");
        false ->
            io:format("error: could not read $HOME~n"),
            halt(1)
    end.
