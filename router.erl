-module(router).
-export([start/1]).

%% get time stamp in mili seconds
%% from https://gist.github.com/DimitryDushkin/5532071
getTimestamp() ->
  {Mega, Sec, Micro} = os:timestamp(),
  (Mega*1000000 + Sec)*1000 + round(Micro/1000).

timelimit() -> 5000.

debug() -> false.

%% log for debugging
log(RouterName, Content) ->
    case debug() of
        true ->
            io:format("~s", [atom_to_list(RouterName) ++ ": " ++ Content ++ "\n"]);
        false -> [] % do nothing
    end.

%% backup the RouterTable for later recovery
backupRouter(RouterTable) ->
    RouterList = ets:tab2list(RouterTable),
    BackupTable = ets:new(backupTable, [set]),
    lists:foldl(fun(X, Table) ->
                        ets:insert(Table, X),
                        Table
                end, BackupTable, RouterList).

%% recover the RouterTable to discard chages
recoverRouter(RouterTable, BackupTable) ->
    BackupList = ets:tab2list(BackupTable),
    ets:delete(BackupTable),
    ets:delete(RouterTable),
    NewTable = ets:new(routerTable, [set]),
    lists:foldl(fun(X, Table) ->
                        ets:insert(Table, X),
                        Table
                end, NewTable, BackupList).

%% send message to target node, return target Pid
sendTo(Target, RouterTable, Msg) ->
    {_, NextPid} = hd(ets:lookup(RouterTable, Target)),
    NextPid ! Msg,
    NextPid.

%% export the start function
start(RouterName) ->
    spawn(
      fun() -> 
              RouterTable = ets:new(routerTable, [set]),
              handler(RouterName, RouterTable)
      end).

%% handle message request
handleMessage(RouterName, RouterTable, Dest, _From, Pid, Trace) ->
    NextTrace = Trace ++ [RouterName],
    if Dest == RouterName -> 
            %% when this is the destination, tell the controler
            Pid ! {trace, self(), NextTrace};
       true ->
            %% otherwise forward the message
            Msg = {message, Dest, self(), Pid, NextTrace},
            sendTo(Dest, RouterTable, Msg)
    end.

%% boardcast message to subnodes of the tree
bcastSubNodes(RouterName, RouterTree, Msg) ->
    {_, _, SubNodes} = lists:keyfind(RouterName, 1, RouterTree),
    lists:foreach(
     fun (Pid) when Pid /= self() ->
             Pid ! Msg
     end, sets:to_list(SubNodes)).

%% propagate control 
propagateControl(RouterName, RouterTree, ControlFun) ->
    bcastSubNodes(RouterName, RouterTree, {lockCanCommit, ControlFun}).

%% wait from sub nodes
%%% when it's done, send a message to parent node
waitFromSubsHelper(RouterName, _, RouterTable, Coordinator, 0, RouterTree) ->
    if RouterName /= Coordinator ->
            Msg = {lockEnd, self()},
            log(RouterName, "reply to " ++ atom_to_list(Coordinator)),
            %% since the coordinator in the table does not necessary have
            %% corresponding pid, let's find the true corresponding ID first
            {_, TruePid, _} = lists:keyfind(Coordinator, 1, RouterTree),
            {TrueCoordinator, _} = lists:keyfind(TruePid, 2, ets:tab2list(RouterTable)),
            sendTo(TrueCoordinator, RouterTable, Msg),
            true;
       true ->
            true
    end;
waitFromSubsHelper(RouterName, SubNodes, RouterTable, Coordinator, N, RouterTree) ->
    receive
        {lockEnd, From} ->
            case sets:is_element(From, SubNodes) of
                true ->
                    log(RouterName, "wait release once"),
                    waitFromSubsHelper(RouterName, SubNodes,
                                       RouterTable, Coordinator, N - 1, RouterTree);
                false ->
                    log(RouterName, "illegal wait sub nodes"),
                    exit(1)
            end
    end.
waitFromSubs(RouterName, RouterTree, RouterTable, Coordinator) ->
    {_, _, SubNodes} = lists:keyfind(RouterName, 1, RouterTree),
    log(RouterName, "waiting for " ++ integer_to_list(sets:size(SubNodes)) ++ " nodes"),
    waitFromSubsHelper(RouterName, SubNodes, RouterTable, Coordinator, sets:size(SubNodes), RouterTree).
                
%% prepare 2 pc phrases
%% convert the network from a graph to a tree
%% for easier node managment
prepareLock(RouterName, ForwardTree, BackwardTree, RouterTable, Coordinator) ->
    log(RouterName, "is preparing lock"),
    bcastSubNodes(RouterName, ForwardTree, {lockStartPrepare, ForwardTree, BackwardTree}), 
    waitFromSubs(RouterName, BackwardTree, RouterTable, Coordinator).

printTree(_, []) ->
    true;
printTree(RouterName, [{Name, SourcePid, Subs} | Xs]) ->
    case debug() of
        true ->
            io:format(atom_to_list(Name) ++ "[~p]:", [SourcePid]),
            sets:fold(fun(ID, _) -> io:format(" ~p,", [ID]) end, true, Subs),
            io:format("~n", []);
        false ->
            true
    end,
    printTree(RouterName, Xs).

%% convert a graph to forward tree for easier control via BFS
%% the starting node is coordinator itself
graphToForwardTreeHelper(Result, [], _, _) ->
    Result;
graphToForwardTreeHelper(Result, [SourcePid | Xs], Graph, Visited) ->
    {Name, _, Neighbors} = lists:keyfind(SourcePid, 2, Graph),
    UniqueNeighbors = sets:subtract(Neighbors, Visited),
    NewVisisted = sets:union(Neighbors, Visited),
    graphToForwardTreeHelper([{Name, SourcePid, UniqueNeighbors}] ++ Result,
                             Xs ++ sets:to_list(UniqueNeighbors), Graph, NewVisisted).
graphToForwardTree(Graph) ->
    Sets = sets:new(),
    SelfSet = sets:add_element(self(), Sets), % exclude myself
    graphToForwardTreeHelper([], [self()], Graph, SelfSet).

%% convert a graph to backward tree to recieve message
graphToBackwardTreeHelper(Result, [], _, _) ->
    Result;
graphToBackwardTreeHelper(Result, [SourcePid | Xs], Graph, Visited) ->
    {Name, Pid, _} = lists:keyfind(SourcePid, 2, Graph),
    SubNodes = 
        lists:foldl(
          fun ({_, Sub, Neighbors}, Accu) ->
                  case sets:is_element(Pid, Neighbors) of
                      false -> Accu;
                      true -> sets:add_element(Sub, Accu)
                  end
          end, sets:new(), Graph),
    UniqueSubs = sets:subtract(SubNodes, Visited),
    NewVisisted = sets:union(SubNodes, Visited),
    graphToBackwardTreeHelper([{Name, SourcePid, UniqueSubs}] ++ Result,
                              Xs ++ sets:to_list(UniqueSubs), Graph, NewVisisted).
graphToBackwardTree(Graph) ->
    Sets = sets:new(),
    SelfSet = sets:add_element(self(), Sets), % exclude myself
    graphToBackwardTreeHelper([], [self()], Graph, SelfSet).

%% retrieve the neighbors of each node
retrieveNeighbors(_, 0, Graph) ->
    Graph;
retrieveNeighbors(RouterName, Count, Graph) ->
    receive
        {pcPutGraph, Name, Pid, _, Neighbors} ->
            case lists:keyfind(Name, 1, Graph) of
                {_, _, _} ->
                    log(RouterName, "a node should not response neighbors twice"),
                    exit(1);
                false ->
                    retrieveNeighbors(RouterName, Count - 1,
                                      [{Name, Pid, Neighbors}] ++ Graph)
            end
    end.

%% boardcast the whole network and their get nearby nodes
getNetworkGraph(RouterName, RouterTable) ->
    Visited = ets:foldl(
     fun ({Name, _}, Acc) ->
             case (Name == '$NoInEdges') or sets:is_element(Name, Acc) of
                 false ->
                     log(RouterName, "deliver message to " ++ atom_to_list(Name)),
                     sendTo(Name, RouterTable, {pcGetGraph, RouterName, Name}),
                     sets:add_element(Name, Acc);
                 true ->
                     Acc
             end
     end, sets:new(), RouterTable),
    OtherGraph = retrieveNeighbors(RouterName, sets:size(Visited), []),
    MyNodes = getNearbyNodes(RouterTable),
    %% append in the first because we start from this node
    [{RouterName, self(), MyNodes}] ++ OtherGraph.

%% wait from sub nodes
%% when it's done, send a message to parent node
%% we call control function until last message has arrived
%% so we won't sent duplicated messages
waitSubsControlHelper(RouterName, _, RouterTable, ControlFun, 0, Result) ->
    StartTime = getTimestamp(),
    Children = ControlFun(RouterName, RouterTable),
    case isTimeout(StartTime) of
        true -> {Children, Result, timeout};
        false -> {Children, Result, intime}
    end;
waitSubsControlHelper(RouterName, SubNodes, RouterTable, ControlFun, N, Result) ->
    receive
        {lockControlEnd, From, FromRes} ->
            case sets:is_element(From, SubNodes) of
                true ->
                    waitSubsControlHelper(RouterName, SubNodes, RouterTable, 
                                          ControlFun, N - 1, Result ++ [FromRes]);
                false ->
                    log(RouterName, "illegal control wait sub nodes"),
                    exit(1)
            end
    after timelimit() ->
            {abort, Result, timeout}
    end.
waitSubsControl(RouterName, RouterTree, RouterTable, ControlFun) ->
    {_, _, SubNodes} = lists:keyfind(RouterName, 1, RouterTree),
    waitSubsControlHelper(RouterName, SubNodes, RouterTable, 
                          ControlFun, sets:size(SubNodes), []).

%% kill all the spawned children
killChildren(Children) ->
    if Children /= abort ->
            lists:foreach(fun (C) -> exit(C, normal) end, Children);
       true ->
            true
    end.

%% get the coordinator(aka the paren node)
getCoordinator(RouterTree) ->
    lists:foldl(
      fun({ParentName, _, Subs}, Acc) ->
              case sets:is_element(self(), Subs) of
                  true -> ParentName;
                  false -> Acc
              end
      end, false, RouterTree).  

% check if an action has timeout 
isTimeout(StartTime) ->
    (getTimestamp() - StartTime) > timelimit().

%% waiting for abort or commit
uncertainWait(RouterName, RouterTable, ForwardTree, BackwardTree) ->
    BackupTable = backupRouter(RouterTable),
    Coordinator = getCoordinator(BackwardTree),
    receive
        {lockCanCommit, ControlFun} ->
            ControlMsg = {lockCanCommit, ControlFun},
            bcastSubNodes(RouterName, ForwardTree, ControlMsg)
    end,
    {_, _, SubNodes} = lists:keyfind(RouterName, 1, BackwardTree),
    {Children, SubsRes, Timeout} = 
        waitSubsControl(RouterName, BackwardTree, RouterTable, ControlFun),
    %% since the coordinator in the table does not necessary have
    %% corresponding pid, let's find the true corresponding ID first
    {_, TruePid, _} = lists:keyfind(Coordinator, 1, BackwardTree),
    {TrueCoordinator, _} = lists:keyfind(TruePid, 2, ets:tab2list(BackupTable)),
    case Timeout of
        timeout -> 
            %% we dont need to send extra message to tell we have failed
            %% since it's a chain effect
            %% other nodes will failed as well
            killChildren(Children),
            recoverRouter(RouterTable, BackupTable);
        intime ->
            if Children == abort ->
                    log(RouterName, "can abort"),
                    Signal = canAbort;
               true ->
                    log(RouterName, "can commit"),
                    Signal = canCommit
            end,
            case sets:size(SubNodes) of
                0 ->
                    %% if we don't control any nodes, reply directly
                    log(RouterName, "reply to parent directly"),
                    EndMsg = {lockControlEnd, self(), Signal},
                    sendTo(TrueCoordinator, BackupTable, EndMsg);
                _ ->
                    log(RouterName, "check sub result then update"),
                    case lists:any(fun (E) -> E == canAbort end, SubsRes) of
                        true ->
                            CoordinatorMsg = {lockControlEnd, self(), canAbort},
                            sendTo(TrueCoordinator, BackupTable, CoordinatorMsg);
                        false ->
                            CoordinatorMsg = {lockControlEnd, self(), Signal},
                            sendTo(TrueCoordinator, BackupTable, CoordinatorMsg)
                    end
            end,
            %% preapring second phase, if we didn't get reply for a while
            %% it must be timeour
            receive
                {doAbort} ->
                    %% abort changes, use backup table and kill all children
                    log(RouterName, "abort change"),
                    bcastSubNodes(RouterName, ForwardTree, {doAbort}),
                    waitFromSubs(RouterName, BackwardTree, BackupTable, Coordinator),
                    killChildren(Children),
                    recoverRouter(RouterTable, BackupTable);
                {doCommit} ->
                    %% save children and use new table
                    log(RouterName, "commit changes"),
                    bcastSubNodes(RouterName, ForwardTree, {doCommit}),
                    waitFromSubs(RouterName, BackwardTree, BackupTable, Coordinator),
                    ets:delete(BackupTable),
                    RouterTable
            after timelimit() -> 
                    log(RouterName, "time out"),
                    killChildren(Children),
                    recoverRouter(RouterTable, BackupTable)
            end
    end.

%% handle control request
%% the control is only used to communicate with controller
%%% when from equals to control pid, it's a special startup initialization
handleControl(RouterName, RouterTable, _, Controller, SeqNum, ControlFun) 
  when SeqNum /= 0 ->
    BackupTable = backupRouter(RouterTable),
    log(RouterName, "start to propagate control message"),
    %% first phase 2PC
    %% bcast to other nodes
    %% use backup table instead, since we may revoke changes
    NetworkGraph = getNetworkGraph(RouterName, BackupTable),
    ForwardTree = graphToForwardTree(NetworkGraph),
    BackwardTree = graphToBackwardTree(NetworkGraph),
    printTree(RouterName, ForwardTree),
    printTree(RouterName, BackwardTree),
    log(RouterName, "get forward and backward trees"),
    %% we use router name as coordinator here because we don't want to send message 
    %% to any parents since we are coordinator
    prepareLock(RouterName, ForwardTree, BackwardTree, BackupTable, RouterName),
    propagateControl(RouterName, ForwardTree, ControlFun),
    %% enter uncertain stage
    {Children, SubsRes, Timeout} = 
        waitSubsControl(RouterName, BackwardTree, RouterTable, ControlFun),
    case Timeout of
        timeout ->
            %% timeout and abort all opeartions
            log(RouterName, "**TIMEOUT ABORT**"),
            killChildren(Children),
            Controller ! {abort, self(), SeqNum},
            recoverRouter(RouterTable, BackupTable);
        intime ->
            %% second phase 2PC
            case lists:any(fun (E) -> E == canAbort end, SubsRes) of
                true -> 
                    log(RouterName, "**NODE FAILED ABORT**"),
                    AbortMsg = {doAbort},
                    bcastSubNodes(RouterName, ForwardTree, AbortMsg),
                    waitFromSubs(RouterName, BackwardTree, BackupTable, RouterName),  
                    Controller ! {abort, self(), SeqNum},
                    killChildren(Children),
                    recoverRouter(RouterTable, BackupTable);
                false ->
                    log(RouterName, "**COMMIT ALL**"),
                    CommitMsg = {doCommit},
                    bcastSubNodes(RouterName, ForwardTree, CommitMsg),
                    waitFromSubs(RouterName, BackwardTree, BackupTable, RouterName), 
                    Controller ! {committed, self(), SeqNum},
                    ets:delete(BackupTable),
                    RouterTable
            end
    end.

%% stop a process
handleStop(RouterName, RouterTable) ->
    ets:delete(RouterTable), 
    log(RouterName, "stopped"),
    exit(normal).

%% get nearby nodes
getNearbyNodes(RouterTable) ->
    ets:foldl(
          fun({Name, Pid}, Accu) ->
                  %% this is a special name
                  if Name /= '$NoInEdges' -> 
                          sets:add_element(Pid, Accu);
                     true ->
                          Accu
                  end
          end, sets:new(), RouterTable).

%% return nearby nodes or forward to target nodes
handleGetGraph(RouterName, RouterTable, From, Target) ->
    if RouterName == Target ->
            Neighbors = getNearbyNodes(RouterTable),
            Msg = {pcPutGraph, RouterName, self(), From, Neighbors},
            sendTo(From, RouterTable, Msg);
       true ->
            Msg = {pcGetGraph, From, Target},
            sendTo(Target, RouterTable, Msg)
    end.

%% wait conquer requests to decide if we should 
%% wait again or become controller
%% choose the least seqnum
waitConquer(0, _, _, SeqNum) when SeqNum /= 0 ->
    SeqNum;
waitConquer(N, RouterName, RouterTable, MySeq) when MySeq /= 0 ->
    receive
        {conquer, Originate, Target, OrigSeq} ->
            timer:sleep(1),
            if Target /= RouterName ->
                    sendTo(Target, RouterTable, {conquer, Originate, Target, OrigSeq}),
                    waitConquer(N, RouterName, RouterTable, MySeq);
               true ->
                    %% we don't have to reply to another coordinator
                    %% because we have sent a conquer message
                    %% and another node could compare it to get final result
                    if MySeq =< OrigSeq ->
                            waitConquer(N - 1, RouterName, RouterTable, MySeq);
                       true ->
                            waitConquer(N - 1, RouterName, RouterTable, OrigSeq)
                    end
            end
    end.

%% initialization message, it only commucates with the controller
%% otherwise prepare to lock the whole network for atomic opeartions
lockNetwork(RouterName, RouterTable, Controller, Controller, 0, ControlFun) ->
    Children = ControlFun(RouterName, RouterTable),
    if Children /= abort ->
            Controller ! {committed, self(), 0};
       true ->
            Controller ! {abort, self(), 0}
    end,
    log(RouterName, "network node started"),
    RouterTable;
lockNetwork(RouterName, RouterTable, From, Controller, SeqNum, ControlFun) ->
    log(RouterName, "prepare to lock the whole network"),
    Visited = 
        ets:foldl(
          fun ({Name, _}, Acc) ->
                  case (Name == '$NoInEdges') or sets:is_element(Name, Acc) of
                      false ->
                          sendTo(Name, RouterTable, {conquer, RouterName, Name, SeqNum}),
                          sets:add_element(Name, Acc);
                      true ->
                          Acc
                  end
          end, sets:new(), RouterTable),
    log(RouterName, "wait for decision"),
    Result = waitConquer(sets:size(Visited), RouterName, RouterTable, SeqNum),
    if Result == SeqNum ->
            log(RouterName, "lock succeed"),
            handleControl(RouterName, RouterTable, From, Controller, SeqNum, ControlFun);
       true ->
            log(RouterName, "insufficient priority, try later"),
            NewTable = lockPhase(RouterName, RouterTable),
            lockNetwork(RouterName, NewTable, From, Controller, SeqNum, ControlFun)
    end.

%% this node has been entered conquered phase
%% it won't recieve other messages
conquerPhase(RouterName, RouterTable, Command, Msg) ->
    case Command of
        conquer ->
            {Coordinator, TargetName, TargetSeq} = Msg,
            Break = false,
            if RouterName /= TargetName ->
                    log(RouterName, "forward conquer message to " ++ atom_to_list(TargetName)),
                    sendTo(TargetName, RouterTable, 
                           {conquer, Coordinator, TargetName, TargetSeq});
               true ->
                    %% since we are not coordinators, just reply the message
                    log(RouterName, "get conquer message, reply to " ++ atom_to_list(Coordinator)),
                    sendTo(Coordinator, RouterTable,
                           {conquer, Coordinator, Coordinator, TargetSeq})
            end;
        %% if we start to get graph, it means one node has decide itself as coordinator
        pcGetGraph ->
            {From, Target} = Msg,
            Break = true,
            handleGetGraph(RouterName, RouterTable, From, Target)
    end,
    if Break ->
            log(RouterName, "break the loop"),
            lockPhase(RouterName, RouterTable);
       true ->
            receive
                {conquer, NewCoordinator, NewTargetName, NewTargetSeq} -> 
                    conquerPhase(RouterName, RouterTable, conquer, 
                                 {NewCoordinator, NewTargetName, NewTargetSeq});
                {pcGetGraph, NewFrom, NewTarget} ->
                    conquerPhase(RouterName, RouterTable, pcGetGraph, {NewFrom, NewTarget})
            end
    end.

lockPhase(RouterName, RouterTable) ->
    receive
        {conquer, Coordinator, TargetName, TargetSeq} ->
            if RouterName /= TargetName ->
                    log(RouterName, "forward conquer message to " ++ atom_to_list(TargetName)),
                    sendTo(TargetName, RouterTable, 
                           {conquer, Coordinator, TargetName, TargetSeq});
               true ->
                    log(RouterName, "get conquer message, reply to " ++ atom_to_list(Coordinator)),
                    sendTo(Coordinator, RouterTable,
                           {conquer, Coordinator, Coordinator, TargetSeq})
            end,
            lockPhase(RouterName, RouterTable);
        {pcGetGraph, From, Target} ->
            handleGetGraph(RouterName, RouterTable, From, Target),
            lockPhase(RouterName, RouterTable);
        {pcPutGraph, SourceName, SourceID, Dest, Neighbors} ->
            if Dest == RouterName ->
                    % not supposed to recieve here
                    log(RouterName, "wrong put graph"),
                    exit(1);
               true ->
                    sendTo(Dest, RouterTable, 
                           {pcPutGraph, SourceName, SourceID, Dest, Neighbors})
            end,
            lockPhase(RouterName, RouterTable);
        %% other coordinator only response to pcGetGraph after they get all sub nodes response
        %% so, when the selected coordinator get all graph, it means all remaning coordinator
        %% made their decision, all nodes are not flowing other message 
        %% and the control process is safe to start
        {lockStartPrepare, ForwardTree, BackwardTree} ->
            Coordinator = getCoordinator(BackwardTree),
            log(RouterName, "parent node " ++ atom_to_list(Coordinator)),
            prepareLock(RouterName, ForwardTree, BackwardTree, RouterTable, Coordinator),
            NewTable = uncertainWait(RouterName, RouterTable, ForwardTree, BackwardTree),
            NewTable
    end.

%% a request handler
handler(RouterName, RouterTable) ->
    receive
        %% the followings are required protocols
        {message, Dest, From, Pid, Trace} -> 
            handleMessage(RouterName, RouterTable, Dest, From, Pid, Trace),
            handler(RouterName, RouterTable);
        {control, From, Pid, SeqNum, ControlFun} -> 
            NewTable = lockNetwork(RouterName, RouterTable, From, Pid, SeqNum, ControlFun),
            handler(RouterName, NewTable);
        {dump, From} ->
            Dump = ets:match(RouterTable, '$1'),
            From ! {table, self(), Dump},
            handler(RouterName, RouterTable);
        stop -> handleStop(RouterName, RouterTable);
        %% the followings are 2pc protocols
        {conquer, Coordinator, TargetName, TargetSeq} ->
            NewTable = 
                conquerPhase(RouterName, RouterTable, conquer, 
                             {Coordinator, TargetName, TargetSeq}),
            handler(RouterName, NewTable)
    end.
