-module(comm).

%-import(maps, [get/2, keys/1]).
-import(tool, [getPID/1, hamming/2, applyOn/3, getVoisinId/2, getFDB/2]).

-export([send/3, prevent/3, broadCast/7]).

%   =
%   Communication module.
%   =



    %   ===
    %       Communication with only one node
    %           (type, Cible, Args).
    %   ===                            
send(askLink, Receiver, {Sender, Id})   ->  mess(Receiver, {u, {askLink, Sender, Id}, self()});
                                          
send(newOpp, Receiver, {Opp, Id})       ->  mess(Receiver, {u, {newOpp, Opp, Id}, self()});

send(addNode, Receiver, {PID})          ->  mess(Receiver, {u, {add, PID}, self()}); %TODO FINISH THIS

send(neighChange, Receiver, {Id, Node}) ->  mess(Receiver, {u, {newWeight, Node, Id}, self()});

send(repBroad, Receiver, {Id, BCId, Res})  -> mess(Receiver, {u, {repBroad, Id, BCId, Res}, self()}).


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
    %       BroadCasts : a utiliser rarement si possible.
    %           BroadCast : ID -> {Parent, MapValueReceived}.
    %   ====
broadCast(Id, IdParent, IdBC, Map, BCMap, Message, Decide)    -> case maps:is_key(IdBC, BCMap) of
                                                                    true ->  NBCMap = addInfoBCMap(IdBC, BCMap, maps:get(IdBC, BCMap), IdParent, null),
                                                                             NBCMap;
                                                            
                                                                    false -> case {maps:size(Map), IdParent} of
                                                                                
                                                                                {0, null}  -> Decide({null, Map}, null),
                                                                                              BCMap;
                                                                                
                                                                                {1, null}  -> tool:applyOn(fun pBroadCast/3, maps:keys(Map),{Map, Message, IdBC, Id}),maps:put(IdBC, {IdParent, maps:new()}, BCMap);
                                                                                
                                                                                {1, _}  -> send(repBroad, maps:get(tool:getFDB(Id, IdParent),Map),{Id, IdBC, Decide({IdParent, Map}, null)}),
                                                                                           BCMap;
                                                                                
                                                                                _ -> tool:applyOn(fun pBroadCast/3, lists:delete(tool:getFDB(Id, IdParent), maps:keys(Map)),{Map, Message, IdBC, Id}),maps:put(IdBC, {IdParent, maps:new()}, BCMap)
                                                                             end
                                                                 end.
                                                         
repBroad(Id, IdSender, IdBC, Map, BCMap, Result, Decide) -> NBCMap = addInfoBCMap(IdBC, BCMap, maps:get(IdBC, BCMap), IdSender, Result),
                                                         case { getBCParent(maps:get(IdBC, NBCMap)) , maps:size(Map) - maps:size(getBCMap(maps:get(IdBC, NBCMap))} of
                                                            {null, 0} -> Decide({null, Map}, getBCMap(maps:get(IdBC, NBCMap)));
                                                            
                                                            {IdParent, 1}  -> Decide({IdParent}, IdParent)
                                                         end.

addInfoBCMap(IdBc, BCMap, {IdParent, Map}, Key, Info) -> maps:put(IdBc, {IdParent, maps:put(Key, Info, Map)}, BCMap).

getBCParent({IdParent, _})  -> IdParent.

getBCMap({_, Map}) -> Map.



    %   ====
    %       Function sendable to applyOn.
    %   ====
pNewNode(Key, {SelfId, SNode, NewId, NNode, Map}, null) ->  pNewNode(maps:get(Key, Map) , {SelfId, NewId, NNode}, 
                                                                                          tool:hamming(NewId, tool:getVoisinId(SelfId, Key))),
                                                            null;
                                                                                                                  
pNewNode(Node, {SId, NewId, NNode}, K) when K < 3       ->  mess(Node, {u,{newNode, SId, NewId, NNode}, self()});
pNewNode(_, {_, _, _}, _)                               ->  null.


pNewWeight(Key, {Node, Id, Map}, _) ->  mess(maps:get(Key, Map), {u, {newWeight, Node, Id}, self()}).                                  

pBroadCast(Key, {Map, Message, IdBC, IdSender}, _)  -> mess(maps:get(Key, Map), {u, {broadcast ,Message, IdBC, IdSender}, self()}).
                
%   =
%   Debug
%   =

mess(Dest, Message) -> say(s, tool:getPID(Dest), Message),
                       tool:getPID(Dest)!Message.

say(s, To, Message) -> io:fwrite("[~p][ ->](~p) send : ~p ~n", [self(), To, Message]).
