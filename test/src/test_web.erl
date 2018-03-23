%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc Web server for test.

-module(test_web).
-author("Mochi Media <dev@mochimedia.com>").

-export([start/1, stop/0, loop/2]).

%% External API

start(Options) ->
    {DocRoot, Options1} = get_option(docroot, Options),
    Loop = fun (Req) ->
                   ?MODULE:loop(Req, DocRoot)
           end,
    mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options1]).

stop() ->
    mochiweb_http:stop(?MODULE).

loop(Req, DocRoot) ->
    "/" ++ Path = Req:get(path),
    Method = Req:get(method),
    try
        case {Path, Method} of
        	{"", 'GET'} ->
        		Req:serve_file(Path, DocRoot);
        	{"httpreq.js", 'GET'} ->
        		Req:serve_file(Path, DocRoot);
        	{"test/get", 'GET'} ->
        		test_web_test:get(Req);
        	{"test/get/0", 'GET'} ->
        		test_web_test:get(Req);
        	{"test/get/1", 'GET'} ->
        		test_web_test:get(Req);
        	{"test/get/2", 'GET'} ->
        		test_web_test:get(Req);
        	{"test/head", 'HEAD'} ->
        		test_web_test:head(Req);
        	{"test/post", 'POST'} ->
        		test_web_test:post(Req);
        	{"test/put", 'PUT'} ->
        		test_web_test:put(Req);
        	{"test/delete", 'DELETE'} ->
        		test_web_test:delete(Req);
        	{"test/get_prms1", 'GET'} ->
        		test_web_test:get_prms1(Req);
        	{"test/get_prms2", 'GET'} ->
        		test_web_test:get_prms2(Req);
        	{"test/get_prms3", 'GET'} ->
        		test_web_test:get_prms3(Req);
        	{"test/post_prms1", 'POST'} ->
        		test_web_test:post_prms1(Req);
        	{"test/post_prms2", 'POST'} ->
        		test_web_test:post_prms2(Req);
        	{"test/post_prms3", 'POST'} ->
        		test_web_test:post_prms3(Req);
        	{"test/post_prms4", 'POST'} ->
        		test_web_test:post_prms4(Req);
        	{"test/post_prms_json", 'POST'} ->
        		test_web_test:post_prms_json(Req);
        	{"test/get_timeout", 'GET'} ->
        		test_web_test:get_timeout(Req);
        	{"test/get_timeout/0", 'GET'} ->
        		test_web_test:get_timeout(Req);
         {"test/get_timeout/1", 'GET'} ->
        		test_web_test:get_timeout(Req);
        	{"test/get_timeout/2", 'GET'} ->
        		test_web_test:get_timeout(Req);
        	{"test/get_timeout/3", 'GET'} ->
        		test_web_test:get_timeout(Req);
        	{"test/get_timeout/4", 'GET'} ->
        		test_web_test:get_timeout(Req);
        	{"test/get_timeout/5", 'GET'} ->
        		test_web_test:get_timeout(Req);
       	{"test/head_timeout", 'HEAD'} ->
        		test_web_test:head_timeout(Req);
        	{"test/post_timeout", 'POST'} ->
        		test_web_test:post_timeout(Req);
        	{"test/put_timeout", 'PUT'} ->
        		test_web_test:put_timeout(Req);
        	{"test/delete_timeout", 'DELETE'} ->
        		test_web_test:delete_timeout(Req);
        	{"test/get_err", 'GET'} ->
        		test_web_test:get_err(Req);
        	{"test/head_err", 'HEAD'} ->
        		test_web_test:head_err(Req);
        	{"test/post_err", 'POST'} ->
        		test_web_test:post_err(Req);
        	{"test/put_err", 'PUT'} ->
        		test_web_test:put_err(Req);
        	{"test/delete_err", 'DELETE'} ->
        		test_web_test:delete_err(Req);
        	{"eorder", 'GET'} ->
        		Req:serve_file("eorder.html", DocRoot);
        	{"eorder_test", 'GET'} ->
        		test_web_eorder:test(Req);
        	{"eorder_test", 'HEAD'} ->
        		test_web_eorder:test(Req);
        	{"eorder_test", 'POST'} ->
        		test_web_eorder:test(Req);
        	{"eorder_test", 'PUT'} ->
        		test_web_eorder:test(Req);
        	{"eorder_test", 'DELETE'} ->
        		test_web_eorder:test(Req);
        	{"eorder_save", 'POST'} ->
        		test_web_eorder:save(Req);
        	{"eorder_show", 'GET'} ->
        		test_web_eorder:show(Req);
            _ ->
                Req:respond({501, [{"Content-Type", "text/plain"}], "wrong req"})
        end
    catch
        Type:What ->
            Report = ["web request failed",
                      {path, Path},
                      {type, Type}, {what, What},
                      {trace, erlang:get_stacktrace()}],
            error_logger:error_report(Report),
            Req:respond({500, [{"Content-Type", "text/plain"}],
                         "request failed, sorry\n"})
    end.

%% Internal API

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.

%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

you_should_write_a_test() ->
    ?assertEqual(
       "No, but I will!",
       "Have you written any tests?"),
    ok.

-endif.
