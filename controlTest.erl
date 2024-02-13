-module(controlTest).
-export([runTest/0]).

%% Circular network consisting of three nodes.
%%
circularNetwork3 () ->
    [{red  , [{white, [white, blue]}]},
     {white, [{blue , [red, blue]}]},
     {blue , [{red  , [red, white]}]}
    ].

%% Reverse circle as in circularNetwork3/0
%%
reverseCircularNetwork3 () ->
    [{red  , [{blue , [white, blue]}]},
     {white, [{red  , [red, blue]}]},
     {blue , [{white, [red, white]}]}
    ].

%% Build a circular network and then use a control request to change the 
%% direction of all edges; verify the original and reversed network
%%
runTest () ->
    io:format ("*** Starting router network...~n"),
    CGraph = circularNetwork3 (),
    RedPid = control:graphToNetwork (CGraph),
    networkTest:verifyNetwork (RedPid, CGraph),

    {WhitePid, _} = networkTest:probeNetwork (RedPid, white),
    {BluePid , _} = networkTest:probeNetwork (RedPid, blue ),
    if (WhitePid == undef) or (BluePid == undef) -> 
            io:format ("*** ERROR: Corrupt network!~n");
       true -> true
    end,

    io:format ("*** Reversing routing cycle...~n"),
    RedPid ! {control, self (), self (), 1, 
              fun (Name, Table) -> 
                      case Name of
                          red   -> ets:insert (Table, [{white, BluePid }, 
                                                       {blue , BluePid }]);
                          white -> ets:insert (Table, [{red  , RedPid  }, 
                                                       {blue , RedPid  }]);
                          blue  -> ets:insert (Table, [{red  , WhitePid}, 
                                                       {white, WhitePid}])
                      end,
                      io:format(atom_to_list(Name) ++ " flip new Table: ~w~n", [ets:match(Table, '$1')]),
                      []
              end},
    receive
        {committed, RedPid, 1} -> io:format ("*** ...done.~n");
        {abort    , RedPid, 1} -> 
            io:format ("*** ERROR: Re-configuration failed!~n")
    after 10000              ->
            io:format ("*** ERROR: Re-configuration timed out!~n")
    end,
    networkTest:verifyNetwork (RedPid, reverseCircularNetwork3 ()),

    io:format("*** Reverse again...~n"),
    RedPid ! {control, self (), self (), 2, 
              fun (Name, Table) -> 
                      case Name of
                          red   -> ets:insert (Table, [{white, WhitePid }, 
                                                       {blue , WhitePid }]);
                          white -> ets:insert (Table, [{red  , BluePid  }, 
                                                       {blue , BluePid  }]);
                          blue  -> ets:insert (Table, [{red  , RedPid}, 
                                                       {white, RedPid}])
                      end,
                      io:format(atom_to_list(Name) ++ "flip new Table: ~w~n", [ets:match(Table, '$1')]),
                      []
              end},
    receive
        {committed, RedPid, 2} -> io:format ("*** ...done.~n");
        {abort    , RedPid, 2} -> 
            io:format ("*** ERROR: Re-configuration failed!~n")
    after 10000              ->
            io:format ("*** ERROR: Re-configuration timed out!~n")
    end,
    networkTest:verifyNetwork (RedPid, CGraph),

    io:format("*** Abort Test...~n"),
    RedPid ! {control, self (), self (), 2, 
              fun (Name, Table) -> 
                      case Name of
                          red   -> ets:insert (Table, [{white, BluePid }, 
                                                       {blue , BluePid }]),
                                   [];
                          white -> ets:insert (Table, [{red  , RedPid  }, 
                                                       {blue , RedPid  }]),
                                   [];
                          blue  -> ets:insert (Table, [{red  , RedPid}, 
                                                       {white, RedPid}]),
                                   abort
                      end
              end},
    receive
        {committed, RedPid, 2} -> io:format ("*** you are supposed to abort.~n");
        {abort    , RedPid, 2} -> 
            io:format ("*** good, it should fail ~n")
    after 10000              ->
            io:format ("*** ERROR: Re-configuration timed out!~n")
    end,
    networkTest:verifyNetwork (RedPid, CGraph),

    io:format ("*** Second reverse cycle..~n"),
    RedPid ! {control, self (), self (), 1, 
              fun (Name, Table) -> 
                      case Name of
                          red   -> ets:insert (Table, [{white, BluePid }, 
                                                       {blue , BluePid }]);
                          white -> ets:insert (Table, [{red  , RedPid  }, 
                                                       {blue , RedPid  }]);
                          blue  -> ets:insert (Table, [{red  , WhitePid}, 
                                                       {white, WhitePid}])
                      end,
                      io:format(atom_to_list(Name) ++ " flip new Table: ~w~n", [ets:match(Table, '$1')]),
                      []
              end},
    receive
        {committed, RedPid, 1} -> io:format ("*** ...done.~n");
        {abort    , RedPid, 1} -> 
            io:format ("*** ERROR: Re-configuration failed!~n")
    after 10000              ->
            io:format ("*** ERROR: Re-configuration timed out!~n")
    end,
    networkTest:verifyNetwork (RedPid, reverseCircularNetwork3 ()),
    
    io:format ("*** Time out test ***..~n"),
    RedPid ! {control, self (), self (), 1, 
              fun (Name, Table) -> 
                      case Name of
                          red   -> ets:insert (Table, [{white, BluePid }, 
                                                       {blue , BluePid }]);
                          white -> ets:insert (Table, [{red  , RedPid  }, 
                                                       {blue , RedPid  }]),
                                   timer:sleep(timer:seconds(6));
                          blue  -> ets:insert (Table, [{red  , WhitePid}, 
                                                       {white, WhitePid}])
                      end,
                      io:format(atom_to_list(Name) ++ " flip new Table: ~w~n", [ets:match(Table, '$1')]),
                      []
              end},
    receive
        {committed, RedPid, 1} -> io:format ("*** ...not supposed to finish.~n");
        {abort    , RedPid, 1} -> 
            io:format ("** good, it should failed again**~n")
    after 10000              ->
            io:format ("*** ERROR: Re-configuration timed out!~n")
    end,
    networkTest:verifyNetwork (RedPid, reverseCircularNetwork3 ()),

    io:format("*** Second revert back to orignal again...~n"),
    RedPid ! {control, self (), self (), 2, 
              fun (Name, Table) -> 
                      case Name of
                          red   -> ets:insert (Table, [{white, WhitePid }, 
                                                       {blue , WhitePid }]);
                          white -> ets:insert (Table, [{red  , BluePid  }, 
                                                       {blue , BluePid  }]);
                          blue  -> ets:insert (Table, [{red  , RedPid}, 
                                                       {white, RedPid}])
                      end,
                      io:format(atom_to_list(Name) ++ "flip new Table: ~w~n", [ets:match(Table, '$1')]),
                      []
              end},
    receive
        {committed, RedPid, 2} -> io:format ("*** ...done.~n");
        {abort    , RedPid, 2} -> 
            io:format ("*** ERROR: Re-configuration failed!~n")
    after 10000              ->
            io:format ("*** ERROR: Re-configuration timed out!~n")
    end,
    networkTest:verifyNetwork (RedPid, CGraph).
