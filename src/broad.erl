-module(broad).

-export([broadAdd/4, broadAddRep/5]).

-import(comm, [broadCast/7, repBroad/8, mess/2]).

% ===
%   EndBroad
% ===

endBroad(Id, IdBC) ->   comm:send(endBroad, {self(), 0}, {Id, IdBC}).

% ===
%   Add node broadcast.
% ===

broadAdd({Node, Map, Opp, Id, BCMap}, IdBC, IdParent, PID) -> comm:broadCast(Id, IdParent, IdBC, Map, BCMap, {add, PID}, fun addNode/3).

broadAddRep({Node, Map, Opp, Id, BCMap}, IdBC, IdSender, Res, PID) -> comm:repBroad(Id, IdSender, IdBC, Map, BCMap, {add, PID}, Res, fun addNode/3).


addNode({Id, IdBC, null, Map}, {add, PID}, Result) -> NResult = maps:put(0, {self(), maps:size(Map)}, Result),
                                                      {Host, _} = tool:getMinTuple(NResult),
                                                      endBroad(Id, IdBC),
                                                      comm:send(addNode, {Host, 0}, {PID});

addNode({_, _, _, Map}, {add, PID}, null) -> {self(), maps:size(Map)};

addNode({_, _,IdParent, Map}, {add, PID},Result) -> NResult = maps:put(0, {self(), maps:size(Map)}, Result),
                                                    tool:getMinTuple(NResult).
                                                
                                                
                                                
