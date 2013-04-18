-module(facebook_canvas_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(state, {endpoint}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(_Transport, Req, []) ->
    {{IP, Port}, _} = cowboy_req:peer(Req),
    Endpoint = lists:flatten(io_lib:format("~s:~B", [inet_parse:ntoa(IP), Port])),
    {ok, Req, #state{endpoint=Endpoint}}.

handle(Req, State=#state{endpoint=Endpoint}) ->
    lager:debug("Serving canvas to '~s'...", [Endpoint]),
    try serve_canvas(Req, State) of
        {ok, Body, NewState} ->
            {ok, Reply} = cowboy_req:reply(200, [{<<"content-type">>, <<"text/html">>}], Body, Req),
            {ok, Reply, NewState}
    catch
        error:ErrorCode when is_integer(ErrorCode) ->
            lager:error("Serving iframe to '~s': error ~B", [Endpoint, ErrorCode]),
            {ok, Reply} = cowboy_req:reply(ErrorCode, Req),
            {ok, Reply, State};
        error:{ErrorCode, ErrorMsg} when is_integer(ErrorCode), is_binary(ErrorMsg) ->
            lager:error("Serving iframe to '~s': error ~B (~s)", [Endpoint, ErrorCode, ErrorMsg]),
            {ok, Reply} = cowboy_req:reply(ErrorCode, [], ErrorMsg, Req),
            {ok, Reply, State}
    end.

serve_canvas(Req, State=#state{endpoint=Endpoint}) ->
    case cowboy_req:method(Req) of
        {<<"POST">>, _} -> ok;
        _               -> error({405, <<"Method not allowed">>})
    end,
    case cowboy_req:has_body(Req) of
        true  -> ok;
        false -> error({403, <<"Missing body">>})
    end,
    case cowboy_req:qs_val(<<"error">>, Req) of
        {undefined, _} -> ok;
        {<<"access_denied">>, _} ->
            %% TODO: Redirect somewhere else?
            lager:debug("Serving canvas to '~s': user denied authorization", [Endpoint])
    end,
    {ok, PostVars, _} = cowboy_req:body_qs(Req),
    case proplists:get_value(<<"signed_request">>, PostVars) of
        undefined -> error({400, <<"Invalid request">>});
        SignedRequest when is_binary(SignedRequest) ->
            Request = parse_signed_request(SignedRequest),
            case proplists:get_value(<<"user_id">>, Request) of
                undefined ->
                    {Headers, _} = cowboy_req:headers(Req),
                    {Referer, _} = cowboy_req:header(<<"referer">>, Req),
                    {RealUserIP, _} = cowboy_req:header(<<"x-real-ip">>, Req),
                    [ lager:debug("\t~s: ~s", [Key,Val]) || {Key,Val} <- Headers ],
                    lager:debug("\tRequest: ~p", [Request]),
                    {User} = proplists:get_value(<<"user">>, Request),
                    Locale = proplists:get_value(<<"locale">>, User),
                    Country = proplists:get_value(<<"country">>, User),
                    {ok, Scope} = application:get_env(facebook_canvas, scope),
                    {ok, AppID} = application:get_env(facebook_canvas, app_id),
                    {ok, AppNamespace} = application:get_env(facebook_canvas, app_namespace),
                    {ok, Content} = login_dtl:render([
                        {app_id, AppID},
                        {app_namespace, AppNamespace},
                        {country, Country},
                        {locale, Locale},
                        {referer, Referer},
                        {scope, Scope},
                        {user_ip, RealUserIP}
                    ]),
                    lager:debug("Serving canvas to '~s': redirecting to authorization", [Endpoint]),
                    {ok, Content, State};
                UserID when is_binary(UserID) ->
                    {Headers, _} = cowboy_req:headers(Req),
                    {Referer, _} = cowboy_req:header(<<"referer">>, Req),
                    {RealUserIP, _} = cowboy_req:header(<<"x-real-ip">>, Req),
                    [ lager:debug("\t~s: ~s", [Key,Val]) || {Key,Val} <- Headers ],
                    lager:debug("\tRequest: ~p", [Request]),
                    {User} = proplists:get_value(<<"user">>, Request),
                    Locale = proplists:get_value(<<"locale">>, User),
                    Country = proplists:get_value(<<"country">>, User),
                    {ok, Scope} = application:get_env(facebook_canvas, scope),
                    {ok, AppID} = application:get_env(facebook_canvas, app_id),
                    {ok, AppNamespace} = application:get_env(facebook_canvas, app_namespace),
                    {ok, Content} = play_dtl:render([
                        {app_id, AppID},
                        {app_namespace, AppNamespace},
                        {country, Country},
                        {locale, Locale},
                        {referer, Referer},
                        {scope, Scope},
                        {user_id, UserID},
                        {user_ip, RealUserIP}
                    ]),
                    lager:debug("Serving canvas to '~s': ok", [Endpoint]),
                    {ok, Content, State}
            end
    end.

terminate(_Reason, _Req, _State) ->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

decode_base64url(Data0) ->
    Data1 = re:replace(Data0, "-", "+", [global, {return, binary}]),
    Data2 = re:replace(Data1, "_", "/", [global, {return, binary}]),
    case size(Data2) rem 4 of
        2 -> base64:decode(<<Data2/binary, "==">>);
        3 -> base64:decode(<<Data2/binary,  "=">>);
        _ -> base64:decode(Data2)
    end.

parse_signed_request(SignedRequest) ->
    [SHA, Data] = re:split(SignedRequest, "\\."),
    Json = decode_base64url(Data),
    {User} = jiffy:decode(Json),
    {ok, SecretKey} = application:get_env(facebook_canvas, app_secret),
    case proplists:get_value(<<"algorithm">>, User) of
        <<"HMAC-SHA256">> ->
            ProvidedSHA = decode_base64url(SHA),
            ExpectedSHA = hmac:hmac256(SecretKey, Data),
            case ExpectedSHA =:= ProvidedSHA of
                true  -> User;
                false -> error({403, <<"Invalid signature">>})
            end;
        _ ->
            error({400, <<"Encryption algorithm unsupported.">>})
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
