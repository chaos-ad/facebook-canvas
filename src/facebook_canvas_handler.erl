-module(facebook_canvas_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Transport, Req, []) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    {{IP, Port}, _} = cowboy_req:peer(Req),
    Endpoint = lists:flatten(io_lib:format("~s:~B", [inet_parse:ntoa(IP), Port])),
    lager:debug("Serving iframe to '~s'...", [Endpoint]),
    try
        case cowboy_req:qs_val(<<"code">>, Req) of
            {undefined, _} ->
                %% Need to login:
                {ErrorReason, _} = cowboy_req:qs_val(<<"error_reason">>, Req),
                case ErrorReason of
                    <<"user_denied">> ->
                        lager:debug("Serving iframe to '~s': user denied the login", [Endpoint]),
                        {ok, login_page(Req), State};
                    undefined ->
                        {ok, PostVars, _} = cowboy_req:body_qs(Req),
                        case proplists:get_value(<<"signed_request">>, PostVars) of
                            undefined -> error(400);
                            SignedRequest when is_binary(SignedRequest) ->
                                _ = parse_signed_request(SignedRequest),
                                lager:debug("Serving iframe to '~s': redirecting to auth page", [Endpoint]),
                                {ok, login_page(Req), State}
%                                 case proplists:get_value(<<"user_id">>, User) of
%                                     undefined -> {ok, login_page(Req), State};
%                                     UserID    -> {ok, play_page(UserID, Req), State}
%                                 end
                        end
                end;
            {Code, _} ->
                {ok, play_page(Code, Req), State}
        end
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
    {ok, SecretKey} = application:get_env(facebook_canvas, secret_key),
    case proplists:get_value(<<"algorithm">>, User) of
        <<"HMAC-SHA256">> ->
            ProvidedSHA = decode_base64url(SHA),
            ExpectedSHA = hmac:hmac256(SecretKey, Data),
            case ExpectedSHA =:= ProvidedSHA of
                true  -> User;
                false -> error(403)
            end;
        _ ->
            error({400, <<"unexpected encryption algorithm">>})
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% login_cancelled(Req) ->


login_page(Req) ->
    {ok, AppID} = application:get_env(facebook_canvas, application_id),
    {ok, RedirectURL} = application:get_env(facebook_canvas, redirect_url),
    Template = "https://www.facebook.com/dialog/oauth?client_id=~s&redirect_uri=~s&scope=~s",
    URL = iolist_to_binary(io_lib:format(Template, [AppID, http_uri:encode(RedirectURL), scope()])),
    Reply = <<"<html><head><script>top.location.href = '", URL/binary, "';</script></head></html>">>,
    cowboy_req:reply(200, [{<<"content-type">>, <<"text/html">>}], Reply, Req).

play_page(UserID, Req) when is_list(UserID) ->
    play_page(iolist_to_binary(UserID), Req);
play_page(UserID, Req) when is_binary(UserID) ->
    Reply = <<"<html><head>Hi there, ", UserID/binary, "!<br/>Here is your game, play it!</head></html>">>,
    cowboy_req:reply(200, [{<<"content-type">>, <<"text/html">>}], Reply, Req).

scope() ->
    "email".
