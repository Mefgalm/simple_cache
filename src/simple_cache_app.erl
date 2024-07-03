-module(simple_cache_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
            {'_', [{"/api/cache_server", simple_cache_handler, []}]}
        ]),
    {ok, _} = cowboy:start_clear(my_http_listener,
        [{port, 8080}],
        #{env => #{dispatch => Dispatch}}
        ),    

    simple_cache_sup:start_link().

stop(_State) ->
    ok.
