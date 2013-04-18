-module(facebook_canvas_app).
-behaviour(application).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([start/2, stop/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {'_', [{"/", facebook_canvas_handler, []}]}
    ]),
    {ok, Host} = application:get_env(facebook_canvas, bind_host),
    {ok, Port} = application:get_env(facebook_canvas, bind_port),
    {ok, Pool} = application:get_env(facebook_canvas, listeners),
    {ok, _} = cowboy:start_http(http, Pool,
        [{ip, Host},{port, Port}],
        [{env, [{dispatch, Dispatch}]}]
    ),
    facebook_canvas_sup:start_link().

stop(_State) ->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
