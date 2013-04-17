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
    {ok, AppID} = application:get_env(application_id),
    {ok, SecretKey} = application:get_env(secret_key),
    {ok, RedirectURL} = application:get_env(redirect_url),
    lager:info("AppID: ~p", [AppID]),
    lager:info("SecretKey: ~p", [string:copies("*", length(SecretKey))]),
    lager:info("RedirectURL: ~p", [RedirectURL]),
    facebook_canvas_sup:start_link().

stop(_State) ->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
