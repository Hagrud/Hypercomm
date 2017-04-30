-module(broad).

-export([broadAdd/4, broadAddRep/5]).
-export([broadNew/4, broadNewRep/5]).

-import(comm, [broadCast/7, repBroad/8, mess/2]).
-import(tool, [gD/2]).

% ===
%   EndBroad
% ===

endBroad(Id, IdBC) ->   comm:send(endBroad, self(), {Id, IdBC}).

% ===
%   Add node broadcast.
% ===

broadAdd(Data, IdBC, IdParent, PID) -> comm:broadCast(Data, IdParent, IdBC, {add, PID}, fun addNode/5).

broadAddRep(Data, IdBC, IdSender, Res, PID) -> comm:repBroad(Data, IdSender, IdBC, {add, PID}, Res, fun addNode/5).


addNode(Data, IdBC, null, {add, PID}, Result) -> NResult = maps:put(0, {self(), maps:size(gD(m, Data))}, Result),
                                                 {Host, _} = tool:getMinTuple(NResult),
                                                 endBroad(gD(i, Data), IdBC),
                                                 comm:send(addNode, Host, {PID});

addNode(Data, _, _, {add, _}, null) -> {self(), maps:size(gD(m, Data))};

addNode(Data, _, _, {add, _},Result) -> NResult = maps:put(0, {self(), maps:size(gD(m, Data))}, Result),
                                        tool:getMinTuple(NResult).
                                                
% ===
%   Prevent new node.
% ===

broadNew(Data, IdBC, IdParent, {PID, Id}) -> comm:broadCast(Data, IdParent, IdBC, {new, PID, Id}, fun newNode/5).

broadNewRep(Data, IdBC, IdSender, Res, {PID, Id}) -> comm:repBroad(Data, IdSender, IdBC, {new, PID, Id}, Res, fun newNode/5).

newNode(Data, IdBC, null, {new, PID, Id}, _) -> io:fwrite("~p passe ici lolololol ~n" ,[self()]),
                                                endBroad(gD(i, Data), IdBC),
                                                newNode(gD(i, Data), Id, PID);
 
newNode(Data, _, _, {new, PID, Id}, _) -> newNode(gD(i, Data), Id, PID).                                                   

newNode(SelfId, Id, PID) -> io:fwrite("~p ca passe ~n" ,[self()]), 
                            case tool:hamming(Id, SelfId) of
                                1 ->    comm:send(askLink, PID, {self(), SelfId}),
                                        comm:send(askLink, self(), {PID, Id}),
                                        null;
                                        
                                _ ->    null
                            end.
                                        
                                        
                                        
                                        
                                        


