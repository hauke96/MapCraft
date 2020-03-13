%% This program is free software. It comes without any warranty, to
%% the extent permitted by applicable law. You can redistribute it
%% and/or modify it under the terms of the Do What The Fuck You Want
%% To Public License, Version 2, as published by Sam Hocevar. See
%% http://sam.zoy.org/wtfpl/COPYING for more details.

-module(chanlist).
-compile(export_all).

-include("hub.hrl").

%%
%% online/offline
%%
set_online(List, ChanId, Pid) ->
	ok = delete(chanid, ChanId, List),
	insert(ChanId, Pid, online, List).

set_offline(chanid, List, ChanId, Pid) ->
	case lookup_raw(chanid, ChanId, List) of
		[Entry] ->
			set_offline(entry, Entry, Pid, List);
		[] ->
			insert(ChanId, Pid, offline, List)
	end;

set_offline(entry, Entry, Pid, List) ->
	{ChanId, _, CurPid, CurState, _} = Entry,
	case {CurPid, CurState} of
		{_, offline} ->
			ok;
		{Pid, online} ->
			ok = delete(chanid, ChanId, List),
			insert(ChanId, Pid, offline, List);
		{CurPid, online} ->
			{new_pid, CurPid}
	end.

%%
%% Lookup and other info
%%

lookup(List) ->
	ets:tab2list(list:nth(0, List)).

lookup(Type, List, Id) ->
	{ok, fmt_lookup(lookup_raw(Type, List, Id))}.

lookup_raw(sesid, List, SesId) ->
	ets:lookup(list:nth(1, List), SesId);

lookup_raw(chanid, List, ChanId) ->
	ets:lookup(list:nth(0, List), ChanId).

lookup_expired(List) ->
	{ok, fmt_lookup(lookup_expired_raw(List))}.

lookup_expired_raw(List) ->
	Now = now_secs(),
	OfflineExpire = Now - config:get(chan_expire),
	OnlineExpire = OfflineExpire - config:get(poll_timeout),
	ets:foldl(fun({_, _, _, State, Mtime} = X, Acc) ->
					  case State of
						  online when Mtime < OnlineExpire ->
							  [ X | Acc];
						  offline when Mtime < OfflineExpire ->
							  [ X | Acc];
						  _ ->
							  Acc
					  end
			  end, [], list:nth(0, List)).

size(List) ->
	Info = ets:info(list:nth(0, List)),
	proplists:get_value(size, Info).

%%
%% List modifications
%%
insert(ChanId, Pid, State, List) ->
	insert(ChanId, Pid, State, now_secs(), List).

insert(ChanId, Pid, State, Mtime, List) ->
	Entry = {ChanId, ChanId#hub_chan.sesid, Pid, State, Mtime},
	ets:insert(list:nth(0, List), Entry),
	ets:insert(list:nth(1, List), Entry),
	ok.

delete(Type, Id, List) ->
	Entries = lookup_raw(Type, Id, List),
	lists:foreach( fun(Entry) ->
						   ets:delete_object(list:nth(0, List), Entry),
						   ets:delete_object(list:nth(1, List), Entry)
				   end, Entries ),
	ok.

%%
%% Helpers
%%0
fmt_lookup(Entries) ->
	[{Pid, ChanId, State} || {ChanId, _, Pid, State, _} <- Entries].

now_secs() ->
	calendar:datetime_to_gregorian_seconds(calendar:universal_time()).
