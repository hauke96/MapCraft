%% This program is free software. It comes without any warranty, to
%% the extent permitted by applicable law. You can redistribute it
%% and/or modify it under the terms of the Do What The Fuck You Want
%% To Public License, Version 2, as published by Sam Hocevar. See
%% http://sam.zoy.org/wtfpl/COPYING for more details.

-module(chanlist).
-compile(export_all).

-import(lists, [nth/2]).

-include("hub.hrl").

%%
%% online/offline
%%
set_online(List, ChanId, Pid) ->
	ok = delete(chanid, ChanId, List),
	insert(ChanId, Pid, online, List).

set_offline(chanid, List, ChanId, Pid) ->
	case lookup_raw(chanid, List, ChanId) of
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
lookup(Type, List, Id) ->
	{ok, fmt_lookup(lookup_raw(Type, List, Id))}.

lookup(List) ->
	{ok, fmt_lookup(ets:tab2list(lists:nth(1, List)))}.

lookup_raw(sesid, List, SesId) ->
	ets:lookup(lists:nth(2, List), SesId);

lookup_raw(chanid, List, ChanId) ->
	ets:lookup(lists:nth(1, List), ChanId).

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
			  end, [], lists:nth(1, List)).

size(List) ->
	Info = ets:info(lists:nth(1, List)),
	proplists:get_value(size, Info).

%%
%% List modifications
%%
insert(ChanId, Pid, State, List) ->
	insert(ChanId, Pid, State, now_secs(), List).

insert(ChanId, Pid, State, Mtime, List) ->
	Entry = {ChanId, ChanId#hub_chan.sesid, Pid, State, Mtime},
	ets:insert(lists:nth(1, List), Entry),
	ets:insert(lists:nth(2, List), Entry),
	ok.

delete(Type, Id, List) ->
	Entries = lookup_raw(Type, List, Id),
	lists:foreach( fun(Entry) ->
						   ets:delete_object(lists:nth(1, List), Entry),
						   ets:delete_object(lists:nth(2, List), Entry)
				   end, Entries ),
	ok.

%%
%% Helpers
%%0
fmt_lookup(Entries) ->
	[{Pid, ChanId, State} || {ChanId, _, Pid, State, _} <- Entries].

now_secs() ->
	calendar:datetime_to_gregorian_seconds(calendar:universal_time()).
