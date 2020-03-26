-module(dmdbserver_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
	    {'_', [
			{"/", cowboy_static, {priv_file, dmdbserver, "client/index.html"}},
			{"/[...]", cowboy_static, {priv_dir, dmdbserver, "root", [{mimetypes, dmdbserver_app, mimetype}]}},
			{"/client/[...]", cowboy_static, {priv_dir, dmdbserver, "client"}},
			{"/dm_images/[...]", cowboy_static, {priv_dir, dmdbserver, "dm_images"}},
			{"/icons/[...]", cowboy_static, {priv_dir, dmdbserver, "icons"}},
		    {"/websocket", ws, []}
	    ]}
	]),
	Port = case application:get_env(dmdbserver, port) of
			{ok, PortNumber} when is_integer(PortNumber) -> PortNumber;
			_ -> 8080
		end,
	{ok, _} = cowboy:start_clear(my_http_listener,
		[{port, Port}],
	    #{env => #{dispatch => Dispatch}}
	),
	collaborative_decks:start_link(),
    dmdbserver_sup:start_link().

stop(_State) ->
    ok.
