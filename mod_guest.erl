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
         process_guest_login/2,
         do_handle_deletions/2,
         register_session/3,
         unregister_session/3
        ]).

-define(GUEST_CLEANUP_TIME, 10).

start(Host, Opts) ->
    ets:new(guest_data, [set, named_table, {keypos, 1}]),
    %% Load guest_access from Opts...
    ets:insert(guest_data,
               {guest_access_list, gen_mod:get_opt(guest_access, Opts, [])}),
    register(handle_deletions,
             spawn(?MODULE, do_handle_deletions, [dict:new(), dict:new()])),
    ets:insert(guest_data,
               {guest_timer,
               timer:send_interval(timer:seconds(?GUEST_CLEANUP_TIME),
                                   handle_deletions, deroster_old_guests)}
              ),
    ejabberd_hooks:add(privacy_check_packet, Host, ?MODULE, is_packet_allowed, 50),
    ejabberd_hooks:add(sm_register_connection_hook, Host, ?MODULE,
                       register_session, 50),
    ejabberd_hooks:add(sm_remove_connection_hook, Host, ?MODULE,
                       unregister_session, 50).

stop(Host) ->
    handle_deletions ! cancel,
    timer:cancel(ets:lookup_element(guest_data, guest_timer, 2)),
    ets:delete_all_objects(guest_data),
    ejabberd_hooks:delete(privacy_check_packet, Host, ?MODULE, is_packet_allowed, 50),
    ejabberd_hooks:delete(sm_register_connection_hook, Host, ?MODULE, register_session, 50),
    ejabberd_hooks:delete(sm_remove_connection_hook, Host, ?MODULE, unregister_session, 50).

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
                true -> deny;
                false ->
                  case lists:keyfind(RemoteLUser, 1, ets:lookup_element(guest_data, guest_access_list, 2)) of
                      false -> deny;
                      _ -> allow
                  end
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
    simulated_roster_iq(User2, User1, Server, subscribed),
    simulated_roster_iq(User2, User1, Server, subscribe),
    simulated_roster_iq(User1, User2, Server, subscribed).

process_guest_login(User, Server) ->
    case mod_guest:is_guest_name(User) of
        true ->
            SubscribeUsers =
                lists:filter(fun(X) -> element(2, X) end,
                  ets:lookup_element(guest_data, guest_access_list, 2)),
            lists:foldl(
              fun({U, _}, _) ->
                ?DEBUG("mod_guest: Calling add_and_accept_local ~s, ~s, ~s", [User, U, Server]),
                add_and_accept_local(User, U, Server)
              end, [], SubscribeUsers),
            
            true;
        false -> false
    end.

register_session(_SID, #jid { luser = RemoteUser, lserver = RemoteServer }, _Info) ->
    case is_guest_name(RemoteUser) of
        false -> false;
        true -> handle_deletions ! {add_guest, RemoteUser, RemoteServer}
    end.

unregister_session(_SID, #jid { luser = RemoteUser, lserver = RemoteServer }, _Info) ->
    case is_guest_name(RemoteUser) of
        false -> false;
        true -> handle_deletions ! {remove_guest, RemoteUser, RemoteServer}
    end.

do_handle_deletions(SessionCount, OfflineGuests) ->
    receive
        cancel ->
            false;
        deroster_old_guests ->
            TNOW = erlang:now(),
            OldGuests = dict:filter(fun(_, T) -> timer:now_diff(TNOW, T) >= ?GUEST_CLEANUP_TIME * 1000000 end, OfflineGuests),
            dict:fold(fun({User,Server},_,_) -> finalise_guest(User, Server) end,
                      false, OldGuests),
            do_handle_deletions(dict:filter(fun(S, _) -> S > 0 end,
                                            SessionCount),
                                dict:filter(fun(_, T) -> timer:now_diff(TNOW, T) < ?GUEST_CLEANUP_TIME * 1000000 end, OfflineGuests)
                               );
        {add_guest, UserName, Server} ->
            SessionCount2 = dict:update(UserName, fun(X) -> X + 1 end, 1,
                                        SessionCount),
            do_handle_deletions(SessionCount2,
                                update_offline_dict(SessionCount2, UserName,
                                                    Server,
                                                    OfflineGuests));
        {remove_guest, UserName, Server} ->
            SessionCount2 = dict:update(UserName, fun(X) -> X - 1 end, 0,
                                        SessionCount),
            do_handle_deletions(SessionCount2,
                                update_offline_dict(SessionCount2, UserName, Server,
                                                    OfflineGuests));
        debug ->
            ?INFO_MSG("mod_guest status: SessionCount=~w OfflineGuest=~w",
                      [SessionCount, OfflineGuests]),
            do_handle_deletions(SessionCount, OfflineGuests);
        update ->
            update_deletion_handler(SessionCount, OfflineGuests)
    end.

update_deletion_handler(_SessionCount, _OfflineGuests) ->
    {}.

update_offline_dict(SessionCount, UserName, Server, OfflineGuests) ->
    case dict:fetch(UserName, SessionCount) of
        Count when Count =< 0 ->
            dict:store({UserName, Server}, erlang:now(), OfflineGuests);
        _ ->
            dict:erase({UserName, Server}, OfflineGuests)
    end.

finalise_guest(User, Server) ->
    ?DEBUG("mod_guest: finalise guest ~s@~s", [User, Server]),
    ejabberd_hooks:run(remove_user, Server, [User, Server]),
    true.
