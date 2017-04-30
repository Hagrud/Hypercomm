-module(broad).

-export([broadAdd/4, broadAddRep/5]).
-export([broadNew/4, broadNewRep/5]).
-export([broadAddObj/3, broadAddObj/4, broadAddObjRep/4]).
-export([broadGetObj/4, broadGetObjRep/5]).
-export([directObjAdded/4, directObjRm/3]).

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


addNode(Data, IdBC, null, {add, PID}, Result) -> 
    NResult = maps:put(0, {self(), maps:size(gD(m, Data))}, Result),
    {Host, _} = tool:getMinTuple(NResult),
    endBroad(gD(i, Data), IdBC),
    comm:send(addNode, Host, {PID});

addNode(Data, _, _, {add, _}, null) -> {self(), maps:size(gD(m, Data))};

addNode(Data, _, _, {add, _},Result) -> 
    NResult = maps:put(0, {self(), maps:size(gD(m, Data))}, Result),
    tool:getMinTuple(NResult).
                                                
% ===
%   Prevent new node.
% ===

broadNew(Data, IdBC, IdParent, {PID, Id}) -> comm:broadCast(Data, IdParent, IdBC, {new, PID, Id}, fun newNode/5).

broadNewRep(Data, IdBC, IdSender, Res, {PID, Id}) -> comm:repBroad(Data, IdSender, IdBC, {new, PID, Id}, Res, fun newNode/5).

newNode(Data, IdBC, null, {new, PID, Id}, _) -> 
    endBroad(gD(i, Data), IdBC),
    newNode(gD(i, Data), Id, PID);
 
newNode(Data, _, _, {new, PID, Id}, _) -> newNode(gD(i, Data), Id, PID).                                                   

newNode(SelfId, Id, PID) -> 
    case tool:hamming(Id, SelfId) of
        1 ->    comm:send(askLink, PID, {self(), SelfId}),
                comm:send(askLink, self(), {PID, Id}),
                null;
                                        
        _ ->    null
    end.

% ===
%   Save Object on two nodes.
% ===
    %Only the first one
broadAddObj(Data, IdBC, null, {Obj}) -> 
    comm:send(addObj, self(), {IdBC, Obj}),
    broadAddObj(Data, IdBC, null).
    
    %For all.                                         
broadAddObj(Data, IdBC, IdParent) -> comm:broadCast(Data, IdParent, IdBC, {addObj}, fun addObj/5).

broadAddObjRep(Data, IdBC, IdSender, Res) -> comm:repBroad(Data, IdSender, IdBC, {addObj}, Res, fun addObj/5).

    %Initiator
addObj(Data, IdBC, null, {addObj}, Result) -> 
    [{{PID1, ID1}, _}, {{PID2, ID2}, _}] = addObj(Data, IdBC, 0, {addObj}, Result),
    Object = maps:get(IdBC, gD(o, Data)),
    
        %Remove the tmp save if needed.
    case {PID1, PID2, self()} of
        {SELF, _, SELF} -> ok;
        {_, SELF, SELF} -> ok;      
        {_, _, _}       -> comm:send(rmTmp, self(), {IdBC})
    end,
        %Save the object
    comm:send(addObj, PID1, {IdBC, Object}),
    comm:send(addObj, PID2, {IdBC, Object}),
        %Broadcast the new information.
    comm:send(directAdd, self(), {null, IdBC, ID1}),
    comm:send(directAdd, self(), {null, IdBC, ID2}),
        %End the broadcast.
    endBroad(gD(i, Data), IdBC);
                                                
                                              
    %Leaf
addObj(Data, _, _, {addObj}, null) -> [{{self(),gD(i, Data)} , maps:size(gD(o, Data))}, null];
    
    %Node
addObj(Data, _, _, {addObj}, Result) ->  
    NResult = maps:put(-1, [{{self(),gD(i, Data)}, maps:size(gD(o, Data))}, null], Result),
    tool:applyOn(fun getTwoNode/3, maps:keys(NResult), {NResult}).

    % Utility.
getTwoNode(Elem, {Map}, Ret) -> getTwoNode(maps:get(Elem, Map), Ret).

getTwoNode(A, null) -> A;
getTwoNode(null, B) -> B;
getTwoNode(A, B)    -> 
    [RetA, RetB, _, _] = lists:merge(fun oAddObj/2, A, B),
    [RetA, RetB].

oAddObj(_, null) -> true;
oAddObj(null, _) -> false;
oAddObj({_, SizeA}, {_, SizeB}) when SizeA > SizeB -> false;
oAddObj(_, _) -> true. 
                          
% ===
%   Get an Object
% ===
broadGetObj(Data, IdBC, IdParent, {ObjId, Client}) -> comm:broadCast(Data, IdParent, IdBC, 
                                                                    {getObj, ObjId, Client}, fun getObj/5).
                                                                          
broadGetObjRep(Data, IdBC, IdSender, Res, {ObjId, Client}) -> comm:repBroad(Data, IdSender, IdBC, 
                                                                           {getObj, ObjId, Client}, Res, fun getObj/5).
                                                                    

getObj(Data, _, null, {getObj, ObjId, Client}, Result) -> 
    List = getContainer(Data, Result, ObjId),
    case List of
        []      -> comm:send(toClient, Client, {ObjId, notFound});
        
        [H|_]   -> comm:send(getObj, H, {ObjId, Client})
    end;
    
getObj(Data, _, _, {getObj, ObjId, _}, Result) -> getContainer(Data, Result, ObjId).


getContainer(Data, null, ObjId) ->
    case maps:is_key( ObjId, gD(o, Data)) of
        true -> [self()];
        
        false -> []
    end;
        
getContainer(Data, Result, ObjId) ->
    case maps:is_key( ObjId, gD(o, Data)) of
        true -> tool:toList(maps:put(-1, [self()], Result));
        
        false -> tool:toList(Result)
    end.
           
    
%%%% Broadcast direct :
                 
directObjAdded(Data, IdParent, ObjId, NodeId) -> comm:broadCast(Data, IdParent, {objAdded, ObjId, NodeId}, fun objAdded/2).


objAdded(Data, {objAdded, ObjId, NodeId}) ->
    case tool:aIsRegistered(gD(a, Data), ObjId, NodeId) of
        true -> {false, Data};
        
        false -> NData = tool:sD(a, Data, tool:aRegister(gD(a, Data), ObjId, NodeId)),
                 {true, NData}
    end.

    %From all the topology
directObjRm(Data, IdParent, ObjId) -> comm:broadCast(Data, IdParent, {objRm, ObjId}, fun objRm/2).

objRm(Data, {objRm, ObjId}) ->
    NData = tool:sD(o, Data, maps:remove(ObjId, gD(o, Data))),
    case maps:is_key(ObjId, gD(a, NData)) of
        true -> {true, tool:sD(a, NData, tool:aRm(gD(a, NData), ObjId))};
        
        false -> {false, Data}
    end.

    %From one node





                                  


