% @doc erlgame_grisp2 public API.
-module(erlgame_grisp2).

-behavior(application).

% Callbacks
-export([start/2]).
-export([stop/1]).

%--- Callbacks -----------------------------------------------------------------

% @private
start(_Type, _Args) -> erlgame_grisp2_sup:start_link().

% @private
stop(_State) -> ok.
