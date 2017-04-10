-module(comm).

-import(maps, [get/2, keys/1]).
-import(tool, [getPID/1, hamming/2, applyOn/3, getVoisinId/2, getFDB/2]).

-export([send/3, prevent/3]).

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

send(neighChange, Receiver, {Id, Node}) ->  mess(Receiver, {u, {newWeight, Node, Id}, self()}).


    %   ====
    %       Communication with all neighbors
    %           (type, Map, Args).
    %   ====
prevent(newNode, Map, {SelfId, SNode, Id, Node, SenderId}) ->  mess(Node, {reg, Id}),
                                                               prevent(propagate, Map, {SelfId, SNode, Id, Node, SenderId});


prevent(propagate, Map, {SelfId, SNode, Id, Node, SenderId})  ->  tool:applyOn(fun pNewNode/3, lists:delete(tool:getFDB(SelfId, SenderId) ,maps:keys(Map)), 
                                                                                                                                {SelfId, SNode, Id, Node, Map});

prevent(neighChange, Map, {Id, Node})                      ->  tool:applyOn(fun pNewWeight/3, maps:keys(Map), {Node, Id, Map}).

    %   ====
    %       Function sendable to applyOn.
    %   ====
pNewNode(Key, {SelfId, SNode, NewId, NNode, Map}, null) ->  pNewNode(maps:get(Key, Map) , {SelfId, NewId, NNode}, 
                                                                                          tool:hamming(NewId, tool:getVoisinId(SelfId, Key))),
                                                            null;
                                                                                                                  
pNewNode(Node, {SId, NewId, NNode}, K) when K < 3       ->  mess(Node, {u,{newNode, SId, NewId, NNode}, self()});
pNewNode(Node, {SId, NewId, NNode}, K)                  ->  null.


pNewWeight(Key, {Node, Id, Map}, _) ->  mess(maps:get(Key, Map), {u, {newWeight, Node, Id}, self()}).                                  


                
%   =
%   Debug
%   =

mess(Dest, Message) -> say(s, tool:getPID(Dest), Message),
                       tool:getPID(Dest)!Message.

say(s, To, Message) -> io:fwrite("[~p][ ->](~p) send : ~p ~n", [self(), To, Message]).
