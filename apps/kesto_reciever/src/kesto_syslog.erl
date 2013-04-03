-module(kesto_syslog).
-include_lib("eunit/include/eunit.hrl").
-export([parse/1, decode_priority/1, format/1, couchdoc/1, sortable_datetime_str/1]).


%% @type datetime() = {{Year, Month, Day}, {Hour, Minute, Second}}
%%      Year   = integer(),
%%      Month  = integer(),
%%      Day    = integer(),
%%      Hour   = integer(),
%%      Minute = integer(),
%%      Second = integer().

%% @type priority() = integer().
%% An integer encoding the facility and priority of a syslog message.
%% Defined in <a href="http://tools.ietf.org/html/rfc3164#section-4.1.1">RFC3164</a>

%% @type facility() = integer().
%% An integer between 0 and 23 inclusive that indicates the syslog facility. 
%% Defined in <a href="http://tools.ietf.org/html/rfc3164#section-4.1.1">RFC3164</a>

%% @type severity() = integer().
%% An integer between 0 and 7 inclusive that indicates the syslog severity. 
%% Defined in <a href="http://tools.ietf.org/html/rfc3164#section-4.1.1">RFC3164</a>

%% @type msg() = {Priority, DateTime, Host, Tag, Body}
%%      Priority = priority(),
%%      DateTime = datetime(),
%%      Host     = string(),
%%      Tag      = string(),
%%      Body     = string().


%% @spec parse(Message::string()) -> msg() | bad_message
%% @doc Parses a string into a syslog message tuple
parse(Message) ->
    try 
        {ok, Tokens, _} = syslog_lexer:string(Message),
        {ok, ParsedMessage} = syslog_parser:parse(Tokens),
        ParsedMessage
    catch error:{badmatch, Error} ->
        io:format("Couldn't parse: ~p~n~p~n", [Message, Error]),
        bad_message
    end.

%% @spec decode_priority(Priority::priority()) -> {facility(), severity()}
%% @doc Decodes a priority value into facility and severity
decode_priority(Priority) ->
    {decode_facility(Priority div 8), 
     decode_severity(Priority rem 8)}.

decode_facility(Facility) ->
    case Facility of
        0 -> kern;
        1 -> user;
        2 -> mail;
        3 -> system;
        4 -> auth;
        5 -> internal;
        6 -> lpr;
        7 -> nns;
        8 -> uucp;
        9 -> clock;
        10 -> authpriv;
        11 -> ftp;
        12 -> ntp;
        13 -> audit;
        14 -> alert;
        15 -> clock2; % ?
        16 -> local0;
        17 -> local1;
        18 -> local2;
        19 -> local3;
        20 -> local4;
        21 -> local5;
        22 -> local6;
        23 -> local7;
        _ -> undefined
    end.

decode_severity(Severity) ->
    case Severity of
        0 -> emerg;
        1 -> alert;
        2 -> crit;
        3 -> err;
        4 -> warn;
        5 -> notice;
        6 -> info;
        7 -> debug;
        _ -> undefined
    end.
    
%% @spec format(Msg::msg()) -> string()
%% @doc Pretty-print a syslog message tuple
format({Priority, Timestamp, Host, Tag, Body}) ->
    string:join([httpd_util:rfc1123_date(Timestamp), integer_to_list(Priority), Host, Tag, Body], " ").

couchdoc({Priority, Timestamp, Host, Tag, Body}) ->
    [
        {<<"priority">>, list_to_binary(integer_to_list(Priority))},
        {<<"timestamp">>, list_to_binary(sortable_datetime_str(Timestamp))},
        {<<"host">>, list_to_binary(Host)},
        {<<"tag">>, list_to_binary(Tag)},
        {<<"body">>, list_to_binary(Body)}
    ].

sortable_datetime_str(Datetime) ->
    S = lists:map(
            fun(Input) -> 
                lists:concat(
                    lists:map(
                      fun(E) -> 
                        Str = integer_to_list(E),
                        case length(Str) of
                          1 -> "0" ++ Str;
                          _ -> Str
                        end 
                      end,
                      tuple_to_list(Input)
                    )
                )
            end,
        tuple_to_list(Datetime)),
    
    lists:concat(S).

datetime_test() ->
    "20091213194050" = sortable_datetime_str({{2009, 12, 13}, {19, 40, 50}}),
    "20090113010405" = sortable_datetime_str({{2009, 1, 13}, {1, 4, 5}}),
    true.

parse_test() ->
    {{Year, _, _}, _} = calendar:now_to_local_time(now()),

    {30,
     {{Year, 12, 13}, {19, 40, 50}},
     "localhost", % host has been left off, so assume localhost
     "thttpd[1340]",
     "192.168.1.138 - admin \"GET /cgi-bin/Qdownload/html/1260751250.rcsv HTTP/1.1\" 200 138 \"http://illmatic:8080/cgi-bin/Qdownload/html/rlist.html\" \"Mozilla/5.0 (Macintosh; U; Intel Mac OS X 10.6; en-US; rv:1.9.1.5) Gecko/2009110\""
    } = parse("<30>Dec 13 19:40:50 thttpd[1340]: 192.168.1.138 - admin \"GET /cgi-bin/Qdownload/html/1260751250.rcsv HTTP/1.1\" 200 138 \"http://illmatic:8080/cgi-bin/Qdownload/html/rlist.html\" \"Mozilla/5.0 (Macintosh; U; Intel Mac OS X 10.6; en-US; rv:1.9.1.5) Gecko/2009110\""),

    {30,
     {{Year, 12, 13}, {19, 41, 03}},
     "localhost", % host has been left off, so assume localhost
     "thttpd[1340]",
     "spawned CGI process 24156 for file 'cgi-bin/Qdownload/refresh.cgi'"
    } = parse("<30>Dec 13 19:41:03 thttpd[1340]: spawned CGI process 24156 for file 'cgi-bin/Qdownload/refresh.cgi'"),
    
    {147, 
     {{Year, 11, 18}, {19, 17, 55}},
     "myhost",
     "mytag[909]",
     "yo what's really real"} = parse("<147>Nov 18 19:17:55 myhost mytag[909]: yo what's really real"),

    {147, 
     {{Year, 11, 18}, {19, 17, 55}},
     "myhost",
     "mytag[909]",
     "yo"} = parse("<147>Nov 18 19:17:55 myhost mytag[909]: yo"),
    
    %{4,
    % {{Year, 12, 20}, {16, 27, 32}},
    % "ccabanilla-mac",
    % "com.apple.launchd.peruser.501[522] (org.apache.couchdb[59972])",
    % "Exited with exit code: 1"} = parse("<4>Dec 20 16:27:32 ccabanilla-mac com.apple.launchd.peruser.501[522] (org.apache.couchdb[59972]): Exited with exit code: 1"),
     
    %{5,
    % {{Year, 12, 20}, {16, 27, 32}},
    % "ccabanilla-mac",
    % "[0x0-0x99099].com.fluidapp.FluidInstance.Gmail[32480]",
    % "Sun Dec 20 16:27:32 ccabanilla-mac FluidInstance[32480] <Error>: kCGErrorIllegalArgument: CGSGetWindowBounds: NULL window"} = parse("<5>Dec 20 16:27:32 ccabanilla-mac [0x0-0x99099].com.fluidapp.FluidInstance.Gmail[32480]: Sun Dec 20 16:27:32 ccabanilla-mac FluidInstance[32480] <Error>: kCGErrorIllegalArgument: CGSGetWindowBounds: NULL window"),

    bad_message = parse("asdf"),



    true.