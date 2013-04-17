-module(facebook_canvas_app).
-behaviour(application).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([start/2, stop/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", facebook_canvas_handler, []}
        ]}
    ]),
    {ok, _} = cowboy:start_http(http, 1, [{ip, {0,0,0,0}},{port, 8080}], [
        {env, [{dispatch, Dispatch}]}
    ]),
    facebook_canvas_sup:start_link().

stop(_State) ->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
