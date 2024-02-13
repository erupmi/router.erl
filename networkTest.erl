-module(networkTest).
-export([runTest/0, verifyNetwork/2, probeNetwork/2]).

%% Simple router network consisting of four nodes
%%
simpleNetworkGraph () ->
  [{red  , [{white, [white, green]},
	    {blue , [blue]}]},
   {white, [{red, [blue]},
	    {blue, [green, red]}]},
   {blue , [{green, [white, green, red]}]},
   {green, [{red, [red, blue, white]}]}
  ].

%% Use `control' to build a simple router network and probe this
%% network to determine whether it has the correct structure
%%
runTest () ->

  io:format ("*** Starting router network...~n"),
  Graph = simpleNetworkGraph (),
  RedPid = control:graphToNetwork (Graph),

  io:format ("*** Sending message red -> green...~n"),
  RedPid ! {message, green, self (), self (), []},
  receive 
    {trace, GreenPid, Trace} -> io:format ("Received trace: ~w~n", [Trace])
  after timeout ()           -> io:format ("!!! Message to green seems lost~n")
  end,

  RedPid ! {dump, self ()},
  receive 
    {table, RedPid, Table} ->
      io:format ("*** Table Dump of red:~n"),
      io:format ("~w~n", [Table])
  after 1000 -> io:format ("!!! Can't obtain dump~n")
  end,

  verifyNetwork (RedPid, Graph).

%% Verify that the network accessible via `RootPid' implements the
%% routing graph `Graph'; do so by an exhaustive path coverage of the network
%%
verifyNetwork (RootPid, Graph) ->
  NodeNames  = [Name || {Name, _} <- Graph],
  io:format ("*** Verifying network containing nodes: ~w~n", [NodeNames]),
  NodeMap    = ets:new (undef, [private]),
  NodeAssocs = [{Name, element (1, probeNetwork (RootPid, Name))} 
		|| Name <- NodeNames],
  Undefined  = [Name || {Name, Pid} <- NodeAssocs, Pid == undef],
  if length (Undefined) /= 0 -> 
      io:format ("*** Network contains undefined node(s): ~w~n", [Undefined]);
     true ->
      ets:insert (NodeMap, NodeAssocs),
      [traceNetwork (From, To, NodeMap, Graph) 
       || From <- NodeNames, To <- NodeNames, From /= To],
      io:format ("*** Network verification complete.~n")
  end.

%% Run a trace between `From' and `To' (where `NodeMap' provides a
%% mapping from symbolic node names to pids) and check whether the
%% resulting trace conforms to the network graph `Graph'
%%
traceNetwork (From, To, NodeMap, Graph) ->
  io:format ("~w -> ~w: ", [From, To]),
  FromPid = ets:lookup_element (NodeMap, From, 2),
  Trace = element (2, probeNetwork (FromPid, To)),
  if length (Trace) == 0 -> io:format ("UNDEFINED ROUTE!~n");
     true ->
      io:format ("~w ", [Trace]),
      case checkTrace (To, Trace, Graph) of
	true  -> io:format ("ok~n");
	false -> io:format ("INCORRECT~n")
      end
  end.

%% For a given message trace and a network graph description,
%% determine whether the trace is valid in that graph.
%%
checkTrace (_   , []                  , Graph) -> false;
checkTrace (Dest, [Node]              , Graph) -> Dest == Node;
checkTrace (Dest, [Node, Next | Nodes], Graph) ->
  Edges = case [Edges || {GNode, Edges} <- Graph, GNode == Node] of
	    [Res] -> Res;
	    []    -> exit ("checkTrace: Missing graph node");
	    _     -> exit ("checkTrace: Duplicate graph nodes")
	  end,
  case [EdgeDest || {EdgeDest, Label} <- Edges, lists:member (Dest, Label)] of
    [EdgeDest] when EdgeDest == Next ->         % next node ok in graph
      checkTrace (Dest, [Next | Nodes], Graph);
    [EdgeDest]                       -> false;  % next node is wrong
    []                               -> exit ("checkTrace: Incomplete labels");
    _                                -> exit ("checkTrace: Ambiguous labels")
  end.

%% Attempt to route a message to `Node' via `RootPid' returning both
%% the pid of the node that returns the trace and the trace itself
%%
%% * Uses a timeout to identify unreachable nodes (in which case the
%%   atom 'undef' is returned instead of the pid)
%%
probeNetwork (RootPid, Node) ->
  RootPid ! {message, Node, self (), self (), []},
  receive
    {trace, Pid, Trace} -> {Pid, Trace}
  after timeout ()      -> {undef, []}
  end.

%% Timeout value
%%
timeout () -> 5000.	% 5s
