-module(test_web_test).
-export([get/1, head/1, post/1, put/1, delete/1, get_err/1, head_err/1,
  post_err/1, put_err/1, delete_err/1, get_timeout/1, head_timeout/1,
  post_timeout/1, put_timeout/1, delete_timeout/1]).


-include_lib("csv/include/csv.hrl").

get(Req) ->
	Req:respond({"200 ok", [{"Content-Type", "text/plain"}], "get_resp"}).

head(Req) ->
	Req:respond({"200 ok", [{"Content-Type", "text/plain"}], ""}).

post(Req) ->
	Req:respond({"200 ok", [{"Content-Type", "text/plain"}], "post_resp"}).

put(Req) ->
	Req:respond({"200 ok", [{"Content-Type", "text/plain"}], "put_resp"}).

delete(Req) ->
	Req:respond({"200 ok", [{"Content-Type", "text/plain"}], "delete_resp"}).

get_err(Req) ->
	Req:respond({"400 some_err", [{"Content-Type", "text/plain"}], "get_resp"}).

head_err(Req) ->
	Req:respond({"400 some_err", [{"Content-Type", "text/plain"}], ""}).

post_err(Req) ->
	Req:respond({"400 some_err", [{"Content-Type", "text/plain"}], "post_resp"}).

put_err(Req) ->
	Req:respond({"400 some_err", [{"Content-Type", "text/plain"}], "put_resp"}).

delete_err(Req) ->
	Req:respond({"400 some_err", [{"Content-Type", "text/plain"}], "delete_resp"}).

get_timeout(Req) ->
	timer:sleep(4000),
	Req:respond({"200 ok", [{"Content-Type", "text/plain"}], "get_resp"}).

head_timeout(Req) ->
	timer:sleep(4000),
	Req:respond({"200 ok", [{"Content-Type", "text/plain"}], ""}).

post_timeout(Req) ->
	timer:sleep(4000),
	Req:respond({"200 ok", [{"Content-Type", "text/plain"}], "post_resp"}).

put_timeout(Req) ->
	timer:sleep(4000),
	Req:respond({"200 ok", [{"Content-Type", "text/plain"}], "put_resp"}).

delete_timeout(Req) ->
	timer:sleep(4000),
	Req:respond({"200 ok", [{"Content-Type", "text/plain"}], "delete_resp"}).

save(Req) ->
	Prms = Req:parse_post(),
	Name = proplists:get_value("name", Prms),
	Res = proplists:get_value("data", Prms),
	{ok, App} = application:get_application(),
	csv:put_frecs(#csv{}, [[Name, Res]], code:priv_dir(App) ++ "/eorder.csv"),
	Req:respond({200, [{"Content-Type", "text/plain"}], "q"}).

show(Req) ->
	{ok, App} = application:get_application(),
	{ok, Data} = csv:get_frecs(#csv{}, code:priv_dir(App) ++ "/eorder.csv"),
	Fmt_data = fmt_data(Data),
	{ok, Tmpl} = htmltmpl:open(code:priv_dir(App) ++ "/eorder_show.html"),
	Res = htmltmpl:apply(Tmpl, [{"loop", Fmt_data}]),
	Req:respond({200, [{"Content-Type", "text/html"}], Res}).

fmt_data(Data) ->
	fmt_data(Data, []).

fmt_data([], Res) ->
	Res;
fmt_data([[N,D]|T], Res) ->
	I = [{"name", N}, {"data", D}],
	fmt_data(T, [I|Res]).
