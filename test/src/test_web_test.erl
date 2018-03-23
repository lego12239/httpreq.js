-module(test_web_test).
-export([get/1, head/1, post/1, put/1, delete/1, get_prms1/1, get_prms2/1,
  get_prms3/1, post_prms1/1, post_prms2/1, post_prms3/1, post_prms4/1,
  post_prms_json/1, get_err/1, head_err/1,
  post_err/1, put_err/1, delete_err/1, get_timeout/1, head_timeout/1,
  post_timeout/1, put_timeout/1, delete_timeout/1]).


-include_lib("csv/include/csv.hrl").

get(Req) ->
	Req:respond({"200 ok", [{"Content-Type", "text/plain"}, {"Cache-Control", "no-cache"}, {"Expires", "01 Dec 1980 00:00:00 +0400"}], "get_resp"}).

head(Req) ->
	Req:respond({"200 ok", [{"Content-Type", "text/plain"}, {"Cache-Control", "no-cache"}, {"Expires", "01 Dec 1980 00:00:00 +0400"}], ""}).

post(Req) ->
	Req:respond({"200 ok", [{"Content-Type", "text/plain"}, {"Cache-Control", "no-cache"}, {"Expires", "01 Dec 1980 00:00:00 +0400"}], "post_resp"}).

put(Req) ->
	Req:respond({"200 ok", [{"Content-Type", "text/plain"}, {"Cache-Control", "no-cache"}, {"Expires", "01 Dec 1980 00:00:00 +0400"}], "put_resp"}).

delete(Req) ->
	Req:respond({"200 ok", [{"Content-Type", "text/plain"}, {"Cache-Control", "no-cache"}, {"Expires", "01 Dec 1980 00:00:00 +0400"}], "delete_resp"}).

get_prms1(Req) ->
	Prms = Req:parse_qs(),
	P1 = proplists:get_value("p1", Prms),
	P2 = proplists:get_value("p2", Prms),
	get_prms1_(Req, P1, P2).

get_prms1_(Req, "val1", "val2") ->
	Req:respond({"200 ok", [{"Content-Type", "text/plain"}, {"Cache-Control", "no-cache"}, {"Expires", "01 Dec 1980 00:00:00 +0400"}], "ok"});
get_prms1_(Req, _, _) ->
	Req:respond({"400 prms_err", [{"Content-Type", "text/plain"}, {"Cache-Control", "no-cache"}, {"Expires", "01 Dec 1980 00:00:00 +0400"}], "wrong parameters"}).

get_prms2(Req) ->
	Prms = Req:parse_qs(),
	P1 = proplists:get_value("p1", Prms),
	P2 = proplists:get_value("p2", Prms),
	get_prms2_(Req, P1, P2).

get_prms2_(Req, "val1?=&", "val2?=&#/q") ->
	Req:respond({"200 ok", [{"Content-Type", "text/plain"}, {"Cache-Control", "no-cache"}, {"Expires", "01 Dec 1980 00:00:00 +0400"}], "ok"});
get_prms2_(Req, _, _) ->
	Req:respond({"400 prms_err", [{"Content-Type", "text/plain"}, {"Cache-Control", "no-cache"}, {"Expires", "01 Dec 1980 00:00:00 +0400"}], "wrong parameters"}).

get_prms3(Req) ->
	Prms = Req:parse_qs(),
	P1 = proplists:get_value("p1", Prms),
	P2 = proplists:get_all_values("p2", Prms),
	get_prms3_(Req, P1, P2).

get_prms3_(Req, "val1?=&", L) when is_list(L) ->
	L1 = lists:sort(L),
	case L1 of
		["extraval2&", "val2?=&#/q"] ->
			Req:respond({"200 ok", [{"Content-Type", "text/plain"}, {"Cache-Control", "no-cache"}, {"Expires", "01 Dec 1980 00:00:00 +0400"}], "ok"});
		_ ->
			Req:respond({"400 prms_err", [{"Content-Type", "text/plain"}, {"Cache-Control", "no-cache"}, {"Expires", "01 Dec 1980 00:00:00 +0400"}], "wrong parameters"})
	end;			
get_prms3_(Req, _, _) ->
	Req:respond({"400 prms_err", [{"Content-Type", "text/plain"}, {"Cache-Control", "no-cache"}, {"Expires", "01 Dec 1980 00:00:00 +0400"}], "wrong parameters"}).

post_prms1(Req) ->
	Prms = Req:parse_post(),
	P1 = proplists:get_value("p1", Prms),
	P2 = proplists:get_value("p2", Prms),
	post_prms1_(Req, P1, P2).

post_prms1_(Req, "val1?=&", "val2?=&#/q") ->
	Req:respond({"200 ok", [{"Content-Type", "text/plain"}, {"Cache-Control", "no-cache"}, {"Expires", "01 Dec 1980 00:00:00 +0400"}], "ok"});
post_prms1_(Req, _, _) ->
	Req:respond({"400 prms_err", [{"Content-Type", "text/plain"}, {"Cache-Control", "no-cache"}, {"Expires", "01 Dec 1980 00:00:00 +0400"}], "wrong parameters"}).

post_prms2(Req) ->
	Prms = mochiweb_multipart:parse_form(Req),
	P1 = proplists:get_value("p1", Prms),
	P2 = proplists:get_value("p2", Prms),
	post_prms2_(Req, P1, P2).

post_prms2_(Req, "val1?=&", "val2?=&#/q") ->
	Req:respond({"200 ok", [{"Content-Type", "text/plain"}, {"Cache-Control", "no-cache"}, {"Expires", "01 Dec 1980 00:00:00 +0400"}], "ok"});
post_prms2_(Req, _, _) ->
	Req:respond({"400 prms_err", [{"Content-Type", "text/plain"}, {"Cache-Control", "no-cache"}, {"Expires", "01 Dec 1980 00:00:00 +0400"}], "wrong parameters"}).

post_prms3(Req) ->
	Prms = Req:parse_post(),
	P1 = proplists:get_value("p1", Prms),
	P2 = proplists:get_all_values("p2", Prms),
	post_prms3_(Req, P1, P2).

post_prms3_(Req, "val1?=&", L) when is_list(L) ->
	L1 = lists:sort(L),
	case L1 of
		["extraval2&", "val2?=&#/q"] ->
			Req:respond({"200 ok", [{"Content-Type", "text/plain"}, {"Cache-Control", "no-cache"}, {"Expires", "01 Dec 1980 00:00:00 +0400"}], "ok"});
		_ ->
			Req:respond({"400 prms_err", [{"Content-Type", "text/plain"}, {"Cache-Control", "no-cache"}, {"Expires", "01 Dec 1980 00:00:00 +0400"}], "wrong parameters"})
	end;			
post_prms3_(Req, _, _) ->
	Req:respond({"400 prms_err", [{"Content-Type", "text/plain"}, {"Cache-Control", "no-cache"}, {"Expires", "01 Dec 1980 00:00:00 +0400"}], "wrong parameters"}).

post_prms4(Req) ->
	Prms = mochiweb_multipart:parse_form(Req),
	P1 = proplists:get_value("p1", Prms),
	P2 = proplists:get_all_values("p2", Prms),
	post_prms4_(Req, P1, P2).

post_prms4_(Req, "val1?=&", L) when is_list(L) ->
	L1 = lists:sort(L),
	case L1 of
		["extraval2&", "val2?=&#/q"] ->
			Req:respond({"200 ok", [{"Content-Type", "text/plain"}, {"Cache-Control", "no-cache"}, {"Expires", "01 Dec 1980 00:00:00 +0400"}], "ok"});
		_ ->
			Req:respond({"400 prms_err", [{"Content-Type", "text/plain"}, {"Cache-Control", "no-cache"}, {"Expires", "01 Dec 1980 00:00:00 +0400"}], "wrong parameters"})
	end;
post_prms4_(Req, _, _) ->
	Req:respond({"400 prms_err", [{"Content-Type", "text/plain"}, {"Cache-Control", "no-cache"}, {"Expires", "01 Dec 1980 00:00:00 +0400"}], "wrong parameters"}).

post_prms_json(Req) ->
	Body = Req:recv_body(),
	{struct, Json} = mochijson2:decode(Body),
	post_prms_json_(Req, lists:sort(Json)).

post_prms_json_(Req, [{<<"p1">>, <<"val1?=&">>},{<<"p2">>, L}])
when is_list(L) ->
	L1 = lists:sort(L),
	case L1 of
		[<<"extraval2&">>, <<"val2?=&#/q">>] ->
			Req:respond({"200 ok", [{"Content-Type", "text/plain"}, {"Cache-Control", "no-cache"}, {"Expires", "01 Dec 1980 00:00:00 +0400"}], "ok"});
		_ ->
			Req:respond({"400 prms_err", [{"Content-Type", "text/plain"}, {"Cache-Control", "no-cache"}, {"Expires", "01 Dec 1980 00:00:00 +0400"}], "wrong parameters"})
	end;
post_prms_json_(Req, _) ->
	Req:respond({"400 prms_err", [{"Content-Type", "text/plain"}, {"Cache-Control", "no-cache"}, {"Expires", "01 Dec 1980 00:00:00 +0400"}], "wrong parameters"}).

get_err(Req) ->
	Req:respond({"400 some_err", [{"Content-Type", "text/plain"}, {"Cache-Control", "no-cache"}, {"Expires", "01 Dec 1980 00:00:00 +0400"}], "get_resp"}).

head_err(Req) ->
	Req:respond({"400 some_err", [{"Content-Type", "text/plain"}, {"Cache-Control", "no-cache"}, {"Expires", "01 Dec 1980 00:00:00 +0400"}], ""}).

post_err(Req) ->
	Req:respond({"400 some_err", [{"Content-Type", "text/plain"}, {"Cache-Control", "no-cache"}, {"Expires", "01 Dec 1980 00:00:00 +0400"}], "post_resp"}).

put_err(Req) ->
	Req:respond({"400 some_err", [{"Content-Type", "text/plain"}, {"Cache-Control", "no-cache"}, {"Expires", "01 Dec 1980 00:00:00 +0400"}], "put_resp"}).

delete_err(Req) ->
	Req:respond({"400 some_err", [{"Content-Type", "text/plain"}, {"Cache-Control", "no-cache"}, {"Expires", "01 Dec 1980 00:00:00 +0400"}], "delete_resp"}).

get_timeout(Req) ->
	timer:sleep(4000),
	Req:respond({"200 ok", [{"Content-Type", "text/plain"}, {"Cache-Control", "no-cache"}, {"Expires", "01 Dec 1980 00:00:00 +0400"}], "get_resp"}).

head_timeout(Req) ->
	timer:sleep(4000),
	Req:respond({"200 ok", [{"Content-Type", "text/plain"}, {"Cache-Control", "no-cache"}, {"Expires", "01 Dec 1980 00:00:00 +0400"}], ""}).

post_timeout(Req) ->
	timer:sleep(4000),
	Req:respond({"200 ok", [{"Content-Type", "text/plain"}, {"Cache-Control", "no-cache"}, {"Expires", "01 Dec 1980 00:00:00 +0400"}], "post_resp"}).

put_timeout(Req) ->
	timer:sleep(4000),
	Req:respond({"200 ok", [{"Content-Type", "text/plain"}, {"Cache-Control", "no-cache"}, {"Expires", "01 Dec 1980 00:00:00 +0400"}], "put_resp"}).

delete_timeout(Req) ->
	timer:sleep(4000),
	Req:respond({"200 ok", [{"Content-Type", "text/plain"}, {"Cache-Control", "no-cache"}, {"Expires", "01 Dec 1980 00:00:00 +0400"}], "delete_resp"}).

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
