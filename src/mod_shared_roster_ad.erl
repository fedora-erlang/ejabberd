%%%----------------------------------------------------------------------
%%% File    : mod_shared_roster.erl
%%% Author  : Alexey Shchepin <alexey@sevcom.net>
%%% Author  : Stanislav Bogatyrev <realloc@realloc.spb.ru>
%%% Purpose : Shared roster management
%%% Created :  5 Mar 2005 by Alexey Shchepin <alexey@sevcom.net>
%%% Id      : $Id: mod_shared_roster_ad.erl,v 1.1 2006/06/23 12:40:03 jcollie Exp $
%%%----------------------------------------------------------------------

-module(mod_shared_roster_ad).
-author('alexey@sevcom.net').
-author('realloc@realloc.spb.ru').
-vsn('$Revision: 1.1 $ ').

-behaviour(gen_mod).

-export([start/2, stop/1,
	 get_user_roster/2,
	 get_subscription_lists/3,
	 get_jid_info/4,
	 in_subscription/5,
	 out_subscription/4,
	 list_groups/1,
	 create_group/2,
	 create_group/3,
	 delete_group/2,
	 get_group_opts/2,
	 set_group_opts/3,
	 get_group_users/2,
	 get_group_explicit_users/2,
	 add_user_to_group/3,
	 remove_user_from_group/3]).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("mod_roster.hrl").
-include("eldap/eldap.hrl").

-record(sr_group, {group_host, opts}).
-record(sr_user, {us, group_host}).

start(Host, _Opts) ->
    mnesia:create_table(sr_group,
			[{disc_copies, [node()]},
			 {attributes, record_info(fields, sr_group)}]),
    mnesia:create_table(sr_user,
			[{disc_copies, [node()]},
			 {type, bag},
			 {attributes, record_info(fields, sr_user)}]),
    mnesia:add_table_index(sr_user, group_host),
    ejabberd_hooks:add(roster_get, Host,
		       ?MODULE, get_user_roster, 70),
    ejabberd_hooks:add(roster_in_subscription, Host,
        	       ?MODULE, in_subscription, 30),
    ejabberd_hooks:add(roster_out_subscription, Host,
        	       ?MODULE, out_subscription, 30),
    ejabberd_hooks:add(roster_get_subscription_lists, Host,
		       ?MODULE, get_subscription_lists, 70),
    ejabberd_hooks:add(roster_get_jid_info, Host,
        	       ?MODULE, get_jid_info, 70),
    
    %ejabberd_hooks:add(remove_user, Host,
    %    	       ?MODULE, remove_user, 50),
    LDAPServers = ejabberd_config:get_local_option({ad_servers, Host}),
    RootDN = ejabberd_config:get_local_option({ad_rootdn, Host}),
    Password = ejabberd_config:get_local_option({ad_password, Host}),
    eldap:start_link("mod_shared_roster_ad", LDAPServers, 389, RootDN, Password).
   

  
stop(Host) ->
    ejabberd_hooks:delete(roster_get, Host,
			  ?MODULE, get_user_roster, 70),
    ejabberd_hooks:delete(roster_in_subscription, Host,
        		  ?MODULE, in_subscription, 30),
    ejabberd_hooks:delete(roster_out_subscription, Host,
        		  ?MODULE, out_subscription, 30),
    ejabberd_hooks:delete(roster_get_subscription_lists, Host,
        		  ?MODULE, get_subscription_lists, 70),
    ejabberd_hooks:delete(roster_get_jid_info, Host,
        		  ?MODULE, get_jid_info, 70).
    %ejabberd_hooks:delete(remove_user, Host,
    %    		  ?MODULE, remove_user, 50),


get_user_roster(Items, US) ->
    {U, S} = US,
    DisplayedGroups = get_user_displayed_groups_ad(US),
    SRUsers = 
	lists:foldl(
	  fun(Group, Acc1) ->
		  lists:foldl(
		    fun(User, Acc2) ->
			    dict:append(User, Group, Acc2)
		    end, Acc1, get_group_users_ad(S, Group))
	  end, dict:new(), DisplayedGroups),
    {NewItems1, SRUsersRest} =
	lists:mapfoldl(
	  fun(Item, SRUsers1) ->
		  {_, _, {U1, S1, _}} = Item#roster.usj,
		  US1 = {U1, S1},
		  case dict:find(US1, SRUsers1) of
		      {ok, _GroupNames} ->
			  {Item#roster{subscription = both, ask = none},
			   dict:erase(US1, SRUsers1)};
		      error ->
			  {Item, SRUsers1}
		  end
	  end, SRUsers, Items),
    SRItems = [#roster{usj = {U, S, {U1, S1, ""}},
		       us = US,
		       jid = {U1, S1, ""},
		       name = get_user_fn(U1,S1),
		       subscription = both,
		       ask = none,
		       groups = GroupNames} ||
		  {{U1, S1}, GroupNames} <- dict:to_list(SRUsersRest)],
    SRItems ++ NewItems1.

get_subscription_lists({F, T}, User, Server) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    US = {LUser, LServer},
    DisplayedGroups = get_user_displayed_groups_ad(US),
    SRUsers =
	lists:usort(
	  lists:flatmap(
	    fun(Group) ->
		    get_group_users_ad(LServer, Group)
	    end, DisplayedGroups)),
    SRJIDs = [{U1, S1, ""} || {U1, S1} <- SRUsers],
    {lists:usort(SRJIDs ++ F), lists:usort(SRJIDs ++ T)}.

get_jid_info({Subscription, Groups}, User, Server, JID) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    US = {LUser, LServer},
    {U1, S1, _} = jlib:jid_tolower(JID),
    US1 = {U1, S1},
    DisplayedGroups = get_user_displayed_groups_ad(US),
    SRUsers = 
	lists:foldl(
	  fun(Group, Acc1) ->
		  lists:foldl(
		    fun(User1, Acc2) ->
			    dict:append(
			      User1, Group, Acc2)
		    end, Acc1, get_group_users_ad(LServer, Group))
	  end, dict:new(), DisplayedGroups),
    case dict:find(US1, SRUsers) of
	{ok, GroupNames} ->
	    NewGroups = if
			    Groups == [] -> GroupNames;
			    true -> Groups
			end,
	    {both, NewGroups};
	error ->
	    {Subscription, Groups}
    end.

in_subscription(Acc, User, Server, JID, Type) ->
    process_subscription(in, User, Server, JID, Type, Acc).

out_subscription(User, Server, JID, Type) ->
    process_subscription(out, User, Server, JID, Type, false).

process_subscription(Direction, User, Server, JID, _Type, Acc) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    US = {LUser, LServer},
    {U1, S1, _} = jlib:jid_tolower(jlib:jid_remove_resource(JID)),
    US1 = {U1, S1},
    DisplayedGroups = get_user_displayed_groups_ad(US),
    SRUsers =
	lists:usort(
	  lists:flatmap(
	    fun(Group) ->
		    get_group_users_ad(LServer, Group)
	    end, DisplayedGroups)),
    case lists:member(US1, SRUsers) of
	true ->
	    case Direction of
		in ->
		    {stop, false};
		out ->
		    stop
	    end;
	false ->
	    Acc
    end.

list_groups(Host) ->
    get_user_displayed_groups_ad({"",Host}).


create_group(Host, Group) ->
    create_group(Host, Group, []).

create_group(Host, Group, Opts) ->
    R = #sr_group{group_host = {Group, Host}, opts = Opts},
    F = fun() ->
		mnesia:write(R)
	end,
    mnesia:transaction(F).

delete_group(Host, Group) ->
    F = fun() ->
		mnesia:delete({sr_group, {Group, Host}})
	end,
    mnesia:transaction(F).

get_group_opts(Host, Group) ->
    case catch mnesia:dirty_read(sr_group, {Group, Host}) of
	[#sr_group{opts = Opts}] ->
	    Opts;
	_ ->
	    error
    end.

set_group_opts(Host, Group, Opts) ->
    R = #sr_group{group_host = {Group, Host}, opts = Opts},
    F = fun() ->
		mnesia:write(R)
	end,
    mnesia:transaction(F).



get_user_groups(US) ->
    Host = element(2, US),
    case catch mnesia:dirty_read(sr_user, US) of
	Rs when is_list(Rs) ->
	    [Group || #sr_user{group_host = {Group, H}} <- Rs, H == Host];
	_ ->
	    []
    end ++ get_all_users_groups(Host).

is_group_enabled(Host, Group) ->
    case catch mnesia:dirty_read(sr_group, {Group, Host}) of
	[#sr_group{opts = Opts}] ->
	    not lists:member(disabled, Opts);
	_ ->
	    false
    end.

get_group_opt(Host, Group, Opt, Default) ->
    case catch mnesia:dirty_read(sr_group, {Group, Host}) of
	[#sr_group{opts = Opts}] ->
	    case lists:keysearch(Opt, 1, Opts) of
		{value, {_, Val}} ->
		    Val;
		false ->
		    Default
	    end;
	_ ->
	    false
    end.

get_group_users(Host, Group) ->
    case get_group_opt(Host, Group, all_users, false) of
	true ->
	    ejabberd_auth:get_vh_registered_users(Host);
	false ->
	    []
    end ++ get_group_explicit_users(Host, Group).

get_group_explicit_users(Host, Group) ->
    case catch mnesia:dirty_index_read(
		 sr_user, {Group, Host}, #sr_user.group_host) of
	Rs when is_list(Rs) ->
	    [R#sr_user.us || R <- Rs];
	_ ->
	    []
    end.

get_group_name(Host, Group) ->
    get_group_opt(Host, Group, name, Group).

get_all_users_groups(Host) ->
    lists:filter(
      fun(Group) -> get_group_opt(Host, Group, all_users, false) end,
      list_groups(Host)).

get_user_displayed_groups(US) ->
    Host = element(2, US),
    DisplayedGroups1 =
	lists:usort(
	  lists:flatmap(
	    fun(Group) ->
		    case is_group_enabled(Host, Group) of
			true ->
			    get_group_opt(Host, Group, displayed_groups, []);
			false ->
			    []
		    end
	    end, get_user_groups(US))),
    [Group || Group <- DisplayedGroups1, is_group_enabled(Host, Group)].




add_user_to_group(Host, US, Group) ->
    R = #sr_user{us = US, group_host = {Group, Host}},
    F = fun() ->
		mnesia:write(R)
	end,
    mnesia:transaction(F).

remove_user_from_group(Host, US, Group) ->
    R = #sr_user{us = US, group_host = {Group, Host}},
    F = fun() ->
		mnesia:delete_object(R)
	end,
    mnesia:transaction(F).



find_user_attr(User, Host) ->
    Attr = ejabberd_config:get_local_option({ad_uidattr, Host}),
    Filter = eldap:equalityMatch(Attr, User),
    Base = ejabberd_config:get_local_option({ad_base, Host}),
    
    case eldap:search("mod_shared_roster_ad",
                      [{base, Base},
                       {filter, Filter},
                       {attributes, []}]) of
        #eldap_search_result{entries = [E | _]} ->
            E;
        _ ->
            false
    end.

get_user_displayed_groups_ad(US) ->
    {_, Host} = US,
    AdGroup = ejabberd_config:get_local_option({ad_group, Host}),
    FilterGroup = eldap:equalityMatch("memberOf", AdGroup),
    Base = ejabberd_config:get_local_option({ad_base, Host}),
    
    case eldap:search("mod_shared_roster_ad",
                      [{base, Base},
                       {filter, FilterGroup},
                       {attributes, []}]) of
        #eldap_search_result{entries = E} ->
	    lists:usort(lists:map(
	      fun(X) ->
		      case X of
			  #eldap_entry{attributes = Attributes} ->
			      ldap_get_value(Attributes,"department");
			  false ->
			      ""	    
		      end
		      end, E
	     ));
	    
        _ ->
            []
    end.

get_eldap_id(Host, Name) ->
    atom_to_list(gen_mod:get_module_proc(Host, Name)).


get_group_users_ad(Host, Group) ->
    Attr = ejabberd_config:get_local_option({ad_uidattr, Host}),
%    AdGroup = ejabberd_config:get_local_option({ad_group, Host}),
    FilterPerson = eldap:equalityMatch("objectCategory", "person"),
    FilterComp = eldap:equalityMatch("objectClass", "computer"),
    FilterHidden = eldap:equalityMatch("description", "hidden"),
%    FilterGroup = eldap:equalityMatch("memberOf", AdGroup),
    FilterDep = eldap:equalityMatch("department", Group),
    FilterLive = eldap:equalityMatch("userAccountControl", "66050"),
    FilterDef = eldap:present(Attr),
    Filter = eldap:'and'([
			  FilterDef,
			  FilterPerson,
%			  FilterGroup,
			  FilterDep,
			  eldap:'not'(FilterComp),
			  eldap:'not'(FilterHidden),
			  eldap:'not'(FilterLive)]),
    Base = ejabberd_config:get_local_option({ad_base, Host}),
    case eldap:search(get_eldap_id(Host, ejabberd),
		      [{base, Base},
		       {filter, Filter},
		       {attributes, [Attr]}]) of
	#eldap_search_result{entries = Es} ->
	    lists:flatmap(
	      fun(E) ->
		      case lists:keysearch(Attr, 1, E#eldap_entry.attributes) of
			  {value, {_, [U]}} ->
			      case jlib:nodeprep(U) of
				  error ->
				      [];
				  LU ->
				      [{LU, Host}]
			      end;
			  _ ->
			      []
		      end
	      end, Es);
	_ ->
	    []
    end.





ldap_get_value(E,Attribute) ->
    case lists:filter(fun({A,_}) ->
			      string:equal(A,Attribute)
		      end,E) of
	[{_,[Value|_]}] ->
	    Value;
	_ -> 
	    none
    end.

get_user_fn(User, Host) ->
    case find_user_attr(User,Host) of
	#eldap_entry{attributes = Attributes} ->
	    ldap_get_value(Attributes,"cn");
	
	false ->
	    ""	    
    end.


