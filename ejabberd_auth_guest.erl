-module(ejabberd_auth_guest).
-author('andrew@amxl.com').

-include("jlib.hrl").
-include("ejabberd.hrl").
-include("mod_guest.hrl").

-export([start/1,
         plain_password_required/0,
         get_password/2,
         check_password/5,
         check_password/3,
         store_type/0,
         set_password/3,
         try_register/3,
         dirty_get_registered_users/0,
         get_vh_registered_users/1,
         is_user_exists/2,
         remove_user/2,
         remove_user/3
        ]).

start(_Host) ->
    ok.

set_password(_,_,_) ->
     ok.

try_register(_User, _Server, _Password) ->
    ok.

dirty_get_registered_users() ->
    [].

get_vh_registered_users(_Server) ->
    [].

store_type() -> plain.

plain_password_required() ->
    true.

check_password(User, Server, _Password) ->
    mod_guest:process_guest_login(User, Server).

check_password(User, Server, _Password, _Digest, _DigestGen) ->
    mod_guest:process_guest_login(User, Server).

get_password(_User, _Server) -> [].

is_user_exists(User, _Server) ->
     mod_guest:is_guest_name(User).

remove_user(_User, _Server) ->
    ok.

remove_user(_User, _Server, _Password) ->
    ok.

