%%%-------------------------------------------------------------------
%%% Created : 29 Mar 2023 by Thiago Esteves <thiagocalori@gmail.com>
%%%
%%% @doc This file contains the app start/stop point
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(erlgame_grisp2_app).

-behaviour(application).

%%====================================================================
%% API functions
%%====================================================================

-export([start/2, stop/1]).

%%====================================================================
%% API functions implementation
%%====================================================================

start(_StartType, _StartArgs) ->
Dispatch = cowboy_router:compile([
    {'_', [
            {"/",             cowboy_static,      {priv_file, erlgame_grisp2, "index.html"}},
            {"/websocket",    erlgame_grisp2_wbs_server, []},
            {"/static/[...]", cowboy_static,      {priv_dir, erlgame_grisp2, "static"}}
    ]}
  ]),
  {ok, _} = cowboy:start_clear(http, [{port, 4000}], #{
    env => #{dispatch => Dispatch}
  }),
  erlgame_grisp2_sup:start_link().

stop(_State) ->
  ok = cowboy:stop_listener(http),
  ok.

%%====================================================================
%% Internal functions
%%====================================================================
