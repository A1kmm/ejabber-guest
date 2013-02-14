-module(mod_guest).
-author('andrew@amxl.com').

-include("jlib.hrl").
-include("ejabberd.hrl").
-include("mod_guest.hrl").
-behaviour(gen_mod).

-export([start/2,
         stop/1,
         is_packet_allowed/6,
         is_guest_name/1,
         process_guest_login/2
        ]).

start(Host, Opts) ->
    mnesia:create_table(guest_access,
                        [{attributes, record_info(fields, guest_access)}]),
    %% Load guest_access from Opts...
    lists:foldl(fun(R,_) -> mnesia:dirty_write(guest_access, R) end, ok,
                gen_mod:get_opt(guest_access, Opts, [])),
    ejabberd_hooks:add(privacy_check_packet, Host, ?MODULE, is_packet_allowed, 50).

stop(Host) ->
    mnesia:delete_table(guest_access),
    ejabberd_hooks:delete(privacy_check_packet, Host, ?MODULE, is_packet_allowed, 50).

is_guest_name(Name) ->
    LName = jlib:nodeprep(Name),
    string:sub_string(LName, 1, 6)=="guest-".

is_packet_allowed(_, User, Server, _PrivacyList, {From, To, _Packet}, _Dir) ->
    case is_guest_name(User) of
        false -> allow;
        true -> case _Dir of
                    in -> 
                        check_guest_interaction(Server, To);
                    out ->
                        check_guest_interaction(Server, From)
                end
    end.

check_guest_interaction(Server, #jid{ luser=RemoteLUser, lserver=RemoteLServer }) ->
    case RemoteLServer == Server of
        % Guests may only interact with the local server...
        false -> deny;
        true ->
            % No guest-guest communication.
            case is_guest_name(RemoteLUser) of
                false -> case catch mnesia:dirty_read(guest_access, RemoteLUser) of
                             [#guest_access{}] -> allow;
                             _ -> deny
                         end;
                true -> deny
            end
    end.

simulated_roster_iq(User1, User2, Server, Type) ->
    JID1=jlib:make_jid(User1, Server, ""),
    JID2=jlib:make_jid(User2, Server, ""),
    ejabberd_hooks:run(roster_out_subscription,
                       Server,
                       [User1, Server, JID2, Type]),
    ejabberd_hooks:run(roster_in_subscription,
                       Server,
                       [ok, User2, Server, JID1, Type, ""]).

add_and_accept_local(User1, User2, Server) ->
    simulated_roster_iq(User1, User2, Server, subscribe),
    simulated_roster_iq(User1, User2, Server, subscribed),
    simulated_roster_iq(User2, User1, Server, subscribe),
    simulated_roster_iq(User2, User1, Server, subscribed).

process_guest_login(User, Server) ->
    case mod_guest:is_guest_name(User) of
        true ->
            SubscribeUsers = mnesia:dirty_select(
                               guest_access,
                               [{#guest_access{username='$1',
                                               default_subscribe=true},
                                [], ['$1']}]),
            lists:foldl(
              fun(U, _) ->
                      add_and_accept_local(User, U, Server),
                      add_and_accept_local(U, User, Server)
              end, [], SubscribeUsers),
            true;
        false -> false
    end.
