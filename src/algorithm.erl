-module(algorithm).

-import(lists, [append/2]).

-import(tool, [getNextFreeKey/1, getVoisinId/2, hamming/2, getMinTuple/1]).
-import(comm, [send/3, prevent/3]).

-import(tool, [gD/2, sD/3]).

-export([loop/1, s_/1, create_first/0]).
-export([unLinkTo/2]).


s_(h)   ->  spawn(algorithm, create_first, []);
s_(w)   ->  spawn(algorithm, loop, [waiting]).
% =====
%   Loop   
%     Data = {Node , Voisins, Op, Id}
%     Node = {PID, Nvoisins}
% =====

create_first() -> loop({ maps:new(), maps:new(), maps:new(), 0, maps:new()}, null, 5000, tool:time_m()).

loop(waiting) -> receive
                    {reg, Id, Addr}   ->  say(r, {reg, Id}),
                                          loop({maps:new(), maps:new(), Addr, Id, maps:new()}, null, 5000, tool:time_m());
                                    
                    _           ->  io:fwrite("close ~n")
                 end.
                 
loop(null, _, _, _) -> say("Node shut down");
      % ==
      %   Main Loop
      % ==        
loop(Data, Cible, Time, Start) -> say("start loop."),
              receive
                           %Special
                    {sayData}         -> say(Data),
                                         reloop(Data, Cible, Start);
                                         
                    {ping, Sender}    ->    Sender!{pong, self()},
                                            reloop(Data, Cible, Start);
                            
                           %Know sender (but useless (for debuging)).
                    {u, Message, PID} -> say(r, Message, PID),
                                         reloop(exec(Message, Data), Cible, Start);
                                                                                
                           %Unknow sender.        
                    Message -> say(r, Message),
                               reloop(exec(Message, Data), Cible, Start)
                               
              %after Time    -> nodeDesync(Data, Cible),
              %                 loop(Data, newCible(gD(m, Data)), 5000, tool:time_m())
              end.
             
reloop(Data, Cible, Start) -> loop(Data, Cible, tool:max(0, 5000 + (Start - tool:time_m())), Start).

    % ==         
    %   Basic Commands     
    % ==   
exec(Message, Data) -> 
    case Message of
            %Add a new neighbor.          
        {add, PID}  ->  sD(b, Data, addNode(Data, PID));
        
            %Deconnection
        {bye, Id}   ->  disconnect(Data, Id);
                        
            %Add a new node to the topology.
        {put, PID}  ->  sD(b, Data, putNode(Data, PID));
                        
            %Register the thread.
        {erlRegist, Name} ->  erlang:register(Name, self()),
                              Data;
                                                       
                                                       
            %Data management:
                %Intern
        {addObj, ObjId, Obj}            -> sD(o, Data, addObject(Data, ObjId, Obj));
        
        {reAttrib, ObjId}               -> sD(b, Data, reAttrib(Data, ObjId));
        
        {getObj, ObjId, Client}         -> getObj(Data, ObjId, Client),
                                           Data;
                                           
        {rmTmp, IdBC}                   -> sD(o, Data, maps:remove(IdBC, gD(o, Data)));
        
                %Extern
        {saveObject, Object, Client}    -> saveObject(Data, Object, Client);
        
        {getObject, ObjId, Client}      -> sD(b, Data, getObject(Data, ObjId, Client));
        
        {objRm, ObjId}                  -> broadExec(Data, {objRm, ObjId}, null);
                                                             
                %Create link
        {askLink, Node, Id} -> sD(m, Data, linkTo(Data, Node, Id));
                        
                %broadCast
        {broadcast, SubMessage, IdBC, IDSender} ->  sD(b, Data, broadExec(Data, SubMessage, IdBC, IDSender));
        
        {broadcast ,SubMessage, IdSender}          ->  broadExec(Data, SubMessage, IdSender);
                    
        {repBroad, SenderId, IdBC, Result} -> sD(b, Data, repBroad(Data, SenderId, IdBC, Result));
        
        {endBroad, IdBC, IDSender} ->   sD(b, Data, comm:endBroad(gD(i, Data), IDSender, IdBC, gD(m, Data), gD(b, Data)));
        
            %ShutDown
        {stop}  ->  stop(Data),
                    null;
                        
        _       ->  say(" Unknow message."),
                    Data
    end.
                 
       % ==
       %    BroadCast commands.
       % ==
       
broadExec(Data, Message, IdParent)  -> 
    case Message of
        {objAdded, ObjId, NodeId}   -> broad:directObjAdded(Data, IdParent, ObjId, NodeId);
        
        {objRm, ObjId}              -> broad:directObjRm(Data, IdParent, ObjId);
        
        {nodeRm, Id}                -> broad:directNodeRm(Data, IdParent, Id);
    
        _               -> say("Warning receive unknow broadcast (without Id)."),
                           Data
    end.
    
    
broadExec(Data, Message, BCId, IdParent)  -> 
    case Message of
        {add, PID}      -> broad:broadAdd(Data, BCId, IdParent, PID);
                                                
        {new, PID, Id}  -> broad:broadNew(Data, BCId, IdParent, {PID, Id});
                                                
        {addObj, IdObj}        -> broad:broadAddObj(Data, BCId, IdParent, IdObj);
        
        {getObj, ObjId, Client}
                        -> broad:broadGetObj(Data, BCId, IdParent, {ObjId, Client});
                                                
        _               -> say("Warning receive unknow broadcast."),
                           gD(b, Data)
    end.
                                                 
repBroad(Data, IdSender, BCId, {Message, Res}) -> 
    case Message of
        {add, PID}      ->  broad:broadAddRep(Data, BCId, IdSender, Res, PID);
                                                    
        {new, PID, Id}  ->  broad:broadNewRep(Data, BCId, IdSender, null, {PID, Id});
        
        {addObj, IdObj} ->  broad:broadAddObjRep(Data, BCId, IdSender, IdObj, Res);
        
        {getObj, ObjId, Client}
                        ->  broad:broadGetObjRep(Data, BCId, IdSender, Res, {ObjId, Client});
                                                    
        _               ->  say("Warning receive unknow rep broadcast."),
                            gD(b, Data)
    end.
    
    % ===
    %   Lost of a node.
    % ===
disconnect(Data, Id) -> 
    comm:send(directRmNode, self(), {null, Id}),
    sD(m, Data, unLinkTo(Data, Id)).
                                                  
% =====
%   Build the struct.
% =====
    %L'id est un hash md5 de la structure à l'instant.
putNode(Data, PID) ->   broad:broadAdd(Data, tool:getHash(Data), null, PID).

addNode(Data, PID)  ->  add(Data, PID, tool:getVoisinId(gD(i, Data) , tool:getNextFreeKey(gD(m, Data)))).
  
add( Data, PID, Id) ->  send(giveId, PID, {Id, gD(a, Data)}),
                        broad:broadNew(Data, tool:getHash(Data), null, {PID, Id}).
                        
% =====
%   Create/Destroy Link.
% =====
linkTo(Data, Node, Id)  -> maps:put(tool:getFDB(gD(i, Data), Id), Node, gD(m, Data)).

unLinkTo(Data, Id)      -> maps:remove(tool:getFDB(gD(i, Data), Id), gD(m, Data)).
                                                                                                            
% ====
%   Data Management
% ====
addObject(Data, ObjId, Obj)      -> maps:put(ObjId, Obj, gD(o, Data)).

saveObject(Data, Object, Client) -> IdObject = tool:getHash(Data),
                                    NData = sD(o, Data, addObject(Data, IdObject, Object)),
                                    comm:send(toClient, Client, IdObject),
                                    sD(b, NData, broad:broadAddObj(Data, IdObject, null, IdObject)).
                                    
reAttrib(Data, ObjId)            -> broad:broadAddObj(Data, tool:getHash(Data), null, ObjId).
                                    
getObject(Data, ObjId, Client)   -> broad:broadGetObj(Data, tool:getHash(Data), null, {ObjId, Client}).

getObj(Data, ObjId, Client) -> 
    case maps:is_key(ObjId, gD(o, Data)) of
        true    ->  comm:send(toClient, Client, {ObjId, maps:get(ObjId, gD(o, Data))});
        
        false   ->  comm:send(toClient, Client, {ObjId, notFound})

    end.
    
stop(Data) -> tool:applyOn(fun stop/3, maps:keys(gD(m, Data)), {gD(m, Data), gD(i, Data)}).

stop(Elem, {Map, Id}, _) -> comm:send(bye, maps:get(Elem, Map), {Id}).

% ====
%   Debug function
% ====
say(Value) -> io:fwrite("[~p][   ] : ~p ~n", [self(), Value]).

say(r, Message) -> say(r, Message, "?").
say(r, Message, From) -> io:fwrite("[~p][<- ](~p) receive : ~p ~n", [self(), From, Message]).



