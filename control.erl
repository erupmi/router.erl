-module(control).
-import(router, [start/1]).
-export([graphToNetwork/1, extendNetwork/4]).

debug() -> false.

%% logging
log(Content) ->
    case debug() of
        true ->
            io:format("~s", [Content ++ "\n"]);
        false ->
            false
    end.

updateInEdges(RouterName, RouterTable, Graph) ->
    Count = 
        lists:foldl(
          fun ({_, Edges}, Accu1) ->
                  lists:foldl(
                    fun ({Connected, _}, Accu2) ->
                            if RouterName == Connected ->
                                    Accu2 + 1;
                               true ->
                                    Accu2
                            end
                    end, Accu1, Edges)
          end, 0, Graph),
    ets:insert(RouterTable, {'$NoInEdges', Count}).

%% init router table
initTable([], _IdMap, _RouterTable) ->
    [];
initTable([{Node, Edges} | Xs], IdMap, RouterTable) ->
    NewEdges = lists:filter(fun (E) -> ets:member(RouterTable, E) == false end, Edges),
    {_, NodeId} = lists:keyfind(Node, 1, IdMap),
    lists:foreach(fun(E) -> ets:insert(RouterTable, {E, NodeId}) end, NewEdges),
    initTable(Xs, IdMap, RouterTable).

%% help us to build a network with first Pid as return value
routerInit([], IdMap, _) ->
    IdMap;
routerInit([{Dest, Names} | Xs], IdMap, Graph) ->
    {_, NewPid} = lists:keyfind(Dest, 1, IdMap),
    NewPid ! {control, self(), self(), 0,
              fun (RouterName, RouterTable) ->
                      initTable(Names, IdMap, RouterTable),
                      updateInEdges(RouterName, RouterTable, Graph)
              end},
    receive
        {abort, _Pid, _SeqNum} -> 
            log("cannot intialize"),
            exit(1);
        {committed, Pid, SeqNum} ->
            if (Pid /= NewPid) or (SeqNum /= 0) ->
                    log("error responese"),
                    exit(1);
               true ->
                    routerInit(Xs, IdMap, Graph)   
            end
    end.

graphToNetwork(Graph) ->
    PidList = lists:map(fun ({Dest, _}) -> {Dest, router:start(Dest)} end, Graph),
    routerInit(Graph, PidList, Graph),
    {_, ResultPid} = hd(PidList),
    ResultPid.

%% init router table for single new node
initNewNodeTable([], _) ->
    [];
initNewNodeTable([{NodeId, Edges} | Xs], RouterTable) ->
    lists:foreach(fun(E) -> ets:insert(RouterTable, {E, NodeId}) end, Edges),
    initNewNodeTable(Xs, RouterTable).

%% generate new node
generateNewNode(TargetRouter, NewNodeName, Edges, RouterName, RouterTable) ->
    if TargetRouter == RouterName ->
            NewPid = start(NewNodeName),
            ets:insert(RouterTable, {NewNodeName, NewPid}),
            NewPid ! {control, self(), self(), 0,
                      fun(_, Table) ->
                              initNewNodeTable(Edges, Table),
                              %% it only has one incoming edge
                              %% which is the parent node
                              ets:insert(Table, {'$NoInEdges', 1})
                      end},
            log("wait for reply"),
            receive
                {committed, NewPid, 0} -> [NewPid];
                {abort    , NewPid, 0} -> abort
            end;
       true ->
            log("insert new router table column"),
            {_, BridgePid} = hd(ets:lookup(RouterTable, TargetRouter)),
            ets:insert(RouterTable, {NewNodeName, BridgePid}),
            case lists:any(fun({Pid, _}) -> Pid == self() end, Edges) of
                true ->
                    {_, Num} = hd(ets:lookup(RouterTable, '$NoInEdges')),
                    ets:insert(RouterTable, {'$NoInEdges', Num + 1}),
                    [];
                false ->
                    []
            end
    end.

extendNetwork(RootPid, SeqNum, From, {NewNodeName, Edges}) ->
    log("start extending"),
    RootPid ! {control, self(), self(), SeqNum,
               fun(RouterName, RouterTable) -> 
                       generateNewNode(From, NewNodeName, Edges, RouterName, RouterTable)
               end},
    log("extent request sent"),
    receive
        {committed, RootPid, SeqNum} -> true;
        {abort    , RootPid, SeqNum} -> false
    after 10000 -> false
    end.
