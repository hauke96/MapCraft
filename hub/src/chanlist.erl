%% This program is free software. It comes without any warranty, to
%% the extent permitted by applicable law. You can redistribute it
%% and/or modify it under the terms of the Do What The Fuck You Want
%% To Public License, Version 2, as published by Sam Hocevar. See
%% http://sam.zoy.org/wtfpl/COPYING for more details.

-module(chanlist).
-compile(export_all).

-include("hub.hrl").

%%
%% Helpers
%%
bySes() ->
	"byses".

byChan() ->
	"bychan".

new_chanlist() ->
	%% TODO: somehow this need to be moved into chanlist.erl
	ets:new(bySes(), [set, protected]),
	ets:new(byChan(),  [bag, protected, {keypos, 2}]).

%%
%% online/offline
%%
set_online(ChanId, Pid) ->
	ok = delete(chanid, ChanId),
	insert(ChanId, Pid, online).

set_offline(chanid, ChanId, Pid) ->
	case lookup_raw(chanid, ChanId) of
		[Entry] ->
			set_offline(entry, Entry, Pid);
		[] ->
			insert(ChanId, Pid, offline)
	end;

set_offline(entry, Entry, Pid) ->
	{ChanId, _, CurPid, CurState, _} = Entry,
	case {CurPid, CurState} of
		{_, offline} ->
			ok;
		{Pid, online} ->
			ok = delete(chanid, ChanId),
			insert(ChanId, Pid, offline);
		{CurPid, online} ->
			{new_pid, CurPid}
	end.

%%
%% Lookup and other info
%%
lookup(Type, Id) ->
	{ok, fmt_lookup(lookup_raw(Type, Id))}.

lookup() ->
	{ok, fmt_lookup(lookup_raw())}.

lookup_raw(sesid, SesId) ->
	ets:lookup(bySes(), SesId);

lookup_raw(chanid, ChanId) ->
	ets:lookup(byChan(), ChanId).

lookup_raw() ->
    ets:tab2list(byChan()).

lookup_expired() ->
	{ok, fmt_lookup(lookup_expired_raw())}.

lookup_expired_raw() ->
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
			  end, [], byChan()).

size() ->
	Info = ets:info(byChan()),
	proplists:get_value(size, Info).

%%
%% List modifications
%%
insert(ChanId, Pid, State) ->
	insert(ChanId, Pid, State, now_secs()).

insert(ChanId, Pid, State, Mtime) ->
	Entry = {ChanId, ChanId#hub_chan.sesid, Pid, State, Mtime},
	ets:insert(byChan(), Entry),
	ets:insert(bySes(), Entry),
	ok.

delete(Type, Id) ->
	Entries = lookup_raw(Type, Id),
	lists:foreach( fun(Entry) ->
						   ets:delete_object(byChan(), Entry),
						   ets:delete_object(bySes(), Entry)
				   end, Entries ),
	ok.

%%
%% Helpers
%%0
fmt_lookup(Entries) ->
	[{Pid, ChanId, State} || {ChanId, _, Pid, State, _} <- Entries].

now_secs() ->
	calendar:datetime_to_gregorian_seconds(calendar:universal_time()).
