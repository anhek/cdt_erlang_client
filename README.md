Chrome DevTools Erlang Client
=====

Build
-----

    $ rebar3 compile

Run
-----
    $ chromium-browser --remote-debugging-port=9222
    $ ERL_FLAGS="-args_file env/local.vmargs" rebar3 shell
    
Usage
-----
    > Tab = cdt_erlang_client:new_tab("http://test.com").
    or
    > [Tab] = cdt_erlang_client:get_tab("test.*").
    
    > cdt_erlang_client:listen_ws(Tab).
    > flush().
    > cdt_erlang_client:unlisten_ws(Tab).
