-module(comm).

%-import(maps, [get/2, keys/1]).
-import(tool, [hamming/2, applyOn/3, getVoisinId/2, getFDB/2]).
-import(tool, [gD/2]).

-export([send/3, prevent/3, broadCast/4, broadCast/5, repBroad/6, endBroad/5]).

%   =
%   Communication module.
%   =

    %   ===
    %       Communication with only one node
    %           (type, Cible, Args).
    %   ===                            
send(askLink, Receiver, {Sender, Id})          ->   mess(Receiver, {u, {askLink, Sender, Id}, self()});
                                          
send(newOpp, Receiver, {Opp, Id})              ->   mess(Receiver, {u, {newOpp, Opp, Id}, self()});

send(addNode, Receiver, {PID})                 ->   mess(Receiver, {u, {add, PID}, self()});

send(neighChange, Receiver, {Id, Node})        ->   mess(Receiver, {u, {newWeight, Node, Id}, self()});

send(endBroad, Receiver, {Id, BCId})           ->   mess(Receiver, {u, {endBroad, BCId, Id}, self()});

send(repBroad, Receiver, {Id, BCId, Message})  ->   mess(Receiver, {u, {repBroad, Id, BCId, Message}, self()});

send(addObj, Receiver, {ObjId, Obj})           ->   mess(Receiver, {u, {addObj, ObjId, Obj}, self()});

send(getObj, Receiver, {ObjId, Client})        ->   mess(Receiver, {u, {getObj, ObjId, Client}, self()});

send(directAdd, Receiver, {Parent, OId, Id})   ->   mess(Receiver, {u, {broadcast, {objAdded, OId,Id}, Parent}, self()});

send(directRmNode, Receiver, {Parent, Id})     ->   mess(Receiver, {u, {broadcast, {nodeRm, Id}, Parent}, self()});

send(reAttrib, Receiver, {ObjId})              ->   mess(Receiver, {u, {reAttrib, ObjId}, self()});

send(rmTmp, Receiver, {IdBC})                  ->   mess(Receiver, {u, {rmTmp, IdBC}, self()});

send(toClient, Client, Value)                  ->   mess(Client, Value);

send(bye, Receiver, {Id})                      ->   mess(Receiver, {u, {bye, Id}, self()});

send(giveId, Receiver, {Id, Addr})             ->   mess(Receiver, {reg, Id, Addr}).


    %   ====
    %       Communication with all neighbors
    %           (type, Map, Args).
    %   ====
prevent(newNode, Map, {SelfId, SNode, Id, Node, SenderId}) ->  mess(Node, {reg, Id}),
                                                               prevent(propagate, Map, {SelfId, SNode, Id, Node, SenderId});


prevent(propagate, Map, {SelfId, SNode, Id, Node, SenderId})  ->  tool:applyOn(fun pNewNode/3, lists:delete(tool:getFDB(SelfId, SenderId) ,maps:keys(Map)), 
                                                                                                                                {SelfId, SNode, Id, Node, Map});

prevent(neighChange, Map, {Id, Node})                         ->  tool:applyOn(fun pNewWeight/3, maps:keys(Map), {Node, Id, Map}).


    %   ====
    %       BroadCasts :
    %           BroadCast : ID -> {Parent, MapValueReceived}.
    %   ====
broadCast(Data, IdParent, IdBC, Message, Decide) -> 
    case IdParent of %check if parent is well linked
        null -> broadCastOk(Data, IdParent, IdBC, Message, Decide);
        
        _ -> case maps:is_key(tool:getFDB(IdParent, gD(i, Data)), gD(m, Data)) of
                true -> broadCastOk(Data, IdParent, IdBC, Message, Decide);
                
                false -> gD(b, Data)
             end
    end.
    
broadCastOk(Data, IdParent, IdBC, Message, Decide) -> 
    Map = gD(m, Data),
    Id = gD(i, Data),
    BCMap = gD(b, Data),
    case maps:is_key(IdBC, BCMap) of
        true ->  repBroad(Data, IdParent, IdBC, Message, null, Decide);
                                                                   
        false -> case {maps:size(Map), IdParent} of
                                                                                
            {0, null}  -> Decide(Data, IdBC, null, Message, maps:new()), maps:put(IdBC, {IdParent, maps:new()}, BCMap);
                                                                                
            {_, null}  -> tool:applyOn(fun pBroadCast/3, maps:keys(Map),{Map, Message, IdBC, Id}), maps:put(IdBC, {IdParent, maps:new()}, BCMap);
                                                                                
            {1, _}  -> send(repBroad, maps:get(tool:getFDB(Id, IdParent),Map),{Id, IdBC, {Message, Decide(Data, IdBC, IdParent, Message, null)}}),
                                                                                           maps:put(IdBC, {Id, IdParent, maps:new()}, BCMap);
                                                                                
            _ -> tool:applyOn(fun pBroadCast/3, lists:delete(tool:getFDB(Id, IdParent), maps:keys(Map)),{Map, Message, IdBC, Id}), 
                                maps:put(IdBC, {IdParent, maps:new()}, BCMap)
        end
    end.
                                                         
repBroad(Data, IdSender, IdBC, Message, Result, Decide) -> 
    Map = gD(m, Data),
    Id = gD(i, Data),
    BCMap = gD(b, Data),
    NBCMap = addInfoBCMap(IdBC, BCMap, maps:get(IdBC, BCMap), IdSender, Result),
    case { getBCParent(maps:get(IdBC, NBCMap)) , maps:size(Map) - maps:size(getBCMap(maps:get(IdBC, NBCMap)))} of
        
        {null, 0} -> Decide(Data, IdBC, null, Message, getBCMap(maps:get(IdBC, NBCMap)));
                                                                
        {null, _} -> null;
                                                            
        {IdParent, 1}  -> send(repBroad, maps:get(tool:getFDB(Id, IdParent), Map),
                                         {Id, IdBC, {Message, Decide(Data, IdBC, IdParent, Message, getBCMap(maps:get(IdBC, NBCMap)))}});
                                         
        {_, _}  -> null
    end,
    NBCMap.

endBroad(Id, IdSender, IdBC, Map, BCMap) ->
    case maps:is_key(IdBC, BCMap) of
        true    ->  tool:applyOn(fun pEndBroadCast/3, lists:delete(tool:getFDB(Id, IdSender), maps:keys(Map)), {Map, IdBC, Id}),
                    maps:remove(IdBC, BCMap);
          
        false   ->  BCMap       
    end.

addInfoBCMap(IdBc, BCMap, {IdParent, Map}, Key, Info) -> maps:put(IdBc, {IdParent, maps:put(Key, Info, Map)}, BCMap).

getBCParent({IdParent, _})  -> IdParent.

getBCMap({_, Map}) -> Map.

    % ===
    %   BroadCast without answer.
    % ===
broadCast(Data, IdParent, Message, Decide) ->
    case {Decide(Data, Message), IdParent} of
        {{true, D}, null}  ->  tool:applyOn(fun pBroadCast/3, maps:keys(gD(m, Data)), {gD(m, Data), Message, gD(i, Data)}),
                               D; 
   
        {{true, D}, _}  ->  tool:applyOn(fun pBroadCast/3, 
                                        lists:delete(tool:getFDB(IdParent, gD(i, Data)), maps:keys(gD(m, Data))), 
                                        {gD(m, Data), Message, gD(i, Data)}),
                            D;
        
        {{false, D}, _} -> D
    end.

    %   ====
    %       Function sendable to applyOn.
    %   ====
pNewNode(Key, {SelfId, _, NewId, NNode, Map}, null) ->  
    pNewNode(maps:get(Key, Map) , {SelfId, NewId, NNode}, tool:hamming(NewId, tool:getVoisinId(SelfId, Key))),
    null;
                                                                                                                  
pNewNode(Node, {SId, NewId, NNode}, K) when K < 3       ->  mess(Node, {u,{newNode, SId, NewId, NNode}, self()});
pNewNode(_, {_, _, _}, _)                               ->  null.


pNewWeight(Key, {Node, Id, Map}, _) ->  mess(maps:get(Key, Map), {u, {newWeight, Node, Id}, self()}).                                  

pBroadCast(Key, {Map, Message, IdBC, IdSender}, _)  -> mess(maps:get(Key, Map), {u, {broadcast ,Message, IdBC, IdSender}, self()});

pBroadCast(Key, {Map, Message, IdSender}, _)  -> mess(maps:get(Key, Map), {u, {broadcast ,Message, IdSender}, self()}).

pEndBroadCast(Key, {Map, IdBC, IdSender}, _) -> mess(maps:get(Key, Map), {u, {endBroad, IdBC, IdSender}, self()}).
                
%   =
%   Debug
%   =

mess(Dest, Message) -> say(s, Dest, Message),
                       Dest!Message.

say(s, To, Message) -> io:fwrite("[~p][ ->](~p) send : ~p ~n", [self(), To, Message]).
