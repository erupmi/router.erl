-module(extendTest).
-export([runTest/0]).

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

%% Extended router network consisting of five nodes
%%
extendedNetworkGraph () ->
  [{red  , [{white, [white, green, black]},
	    {blue , [blue]}]},
   {white, [{red, [blue]},
	    {blue, [green, red]},
	    {black, [black]}]},
   {blue , [{green, [white, green, red, black]}]},
   {green, [{red, [red, blue, white, black]}]},
   {black, [{red, [red, green]},
	    {green, [white, blue]}]}
  ].

%% Extend the simple network graph by one new node and verify the
%% resulting network against a graph specification
%%
runTest () ->
  io:format ("*** Starting router network...~n"),
  Graph = simpleNetworkGraph (),
  RedPid = control:graphToNetwork (Graph),
  networkTest:verifyNetwork (RedPid, Graph),

  {WhitePid, _} = networkTest:probeNetwork (RedPid, white),
  {BluePid , _} = networkTest:probeNetwork (RedPid, blue ),
  {GreenPid, _} = networkTest:probeNetwork (RedPid, green),
  if (WhitePid == undef) or (BluePid == undef) or (GreenPid == undef) -> 
      io:format ("*** ERROR: Corrupt network!~n");
     true -> true
  end,

    io:format("*** Concurrent test 1 ~n***"),
    WhitePid ! {control, self(), self(), 13, 
                fun(Name, _) -> 
                      timer:sleep(500), 
                 %%       io:format(atom_to_list(Name) ++ "13~n"),
                        [] 
                end},
    RedPid ! {control, self(), self(), 12,
              fun(Name, _) -> 
                 %%     io:format(atom_to_list(Name) ++ " 12~n"),
                      abort 
              end},
    GreenPid ! {control, self(), self(), 11, 
                fun(Name, _) ->
                %%        io:format(atom_to_list(Name) ++ " 11~n"),
                        []
                end},
    receive
        {committed, WhitePid, 13} -> io:format ("*** ... concurrent 1 done.~n");
        {abort    , WhitePid, 13} -> 
            io:format ("*** ERROR: concurrent 1 failed!~n")
    after 20000              ->
            io:format ("*** ERROR: concurrent 1 timed out!~n")
    end,
    networkTest:verifyNetwork (RedPid,  Graph),

  io:format ("*** Extending network...~n"),
  case control:extendNetwork (RedPid, 1, white, 
			      {black, [{RedPid  , [red, green]}, 
				       {GreenPid, [white, blue]}
				      ]})
    of
    true  -> io:format ("*** ...done.~n");
    false -> io:format ("*** ERROR: Extension failed!~n")
  end,
    networkTest:verifyNetwork (RedPid, extendedNetworkGraph ()),
  RedPid ! {dump, self ()},
  receive 
    {table, RedPid, Table} ->
      io:format ("*** Table Dump of red:~n"),
      io:format ("~w~n", [Table])
  after 1000 -> io:format ("!!! Can't obtain dump~n")
  end,
  WhitePid ! {dump, self ()},
  receive 
      {table, WhitePid, Table2} ->
      io:format ("*** Table Dump of white:~n"),
      io:format ("~w~n", [Table2])
  after 1000 -> io:format ("!!! Can't obtain dump~n")                
  end,

    io:format("*** Concurrent test 2 ~n***"),
    WhitePid ! {control, self(), self(), 23, fun(_, _) -> timer:sleep(500), [] end},
    RedPid ! {control, self(), self(), 22, fun(_, _) -> abort end},
    GreenPid ! {control, self(), self(), 21, fun(_, _) -> [] end},
    receive
        {committed, WhitePid, 23} -> io:format ("*** ... concurrent 2 done.~n");
        {abort    , WhitePid, 23} -> 
            io:format ("*** ERROR: concurrent 2 faield!~n")
    after 20000              ->
            io:format ("*** ERROR: concurrent 2 timed out!~n")
    end,
    networkTest:verifyNetwork (RedPid, extendedNetworkGraph ()).
    
