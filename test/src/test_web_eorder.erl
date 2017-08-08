-module(test_web_eorder).
-export([test/1, save/1, show/1]).


-include_lib("csv/include/csv.hrl").

test(Req) ->
	timer:sleep(3000),
	Req:respond({200, [{"Content-Type", "text/plain"}], "q"}).

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
