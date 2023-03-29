% @doc erlgame_grisp2 public API.
-module(erlgame_grisp2).

-behavior(application).

% Callbacks
-export([start/2]).
-export([stop/1]).

%--- Callbacks -----------------------------------------------------------------

% @private
start(_Type, _Args) ->
    {ok, Supervisor} = erlgame_grisp2_sup:start_link(),
    LEDs = [1, 2],
    [grisp_led:flash(L, red, 500) || L <- LEDs],
    timer:sleep(5000),
    grisp_led:off(2),
    Random = fun() ->
        {rand:uniform(2) - 1, rand:uniform(2) -1, rand:uniform(2) - 1}
    end,
    grisp_led:pattern(1, [{100, Random}]),
    {ok, Supervisor}.

% @private
stop(_State) -> ok.
