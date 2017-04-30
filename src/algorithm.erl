-module(algorithm).

%-import(maps, [new/0, put/3]).
-import(lists, [append/2]).

-import(tool, [getLC/2,getNextFreeKey/1, getVoisinId/2, hamming/2, getMinTuple/1]).
-import(comm, [send/3, prevent/3]).
%-import(broad, [broadAdd/4, broadAddRep/5]).

-import(tool, [gD/2, sD/3]).

-export([loop/1, s_/1, create_first/0]).


% TODO list:
%   Add new node
    %   getNextID
    %   prevent Voisins
%
%

s_(h)   ->  spawn(algorithm, create_first, []);
s_(w)   ->  spawn(algorithm, loop, [waiting]).
% =====
%   Loop   
%     Data = {Node , Voisins, Op, Id}
%     Node = {PID, Nvoisins}
% =====

create_first() -> loop({self(), maps:new(), maps:new(), 0, maps:new()}).

loop(waiting) -> receive
                    {reg, Id}   ->  say(r, {reg, Id}),
                                    loop({self(), maps:new(), maps:new(), Id, maps:new()});
                                    
                    _           ->  io:fwrite("close ~n")
                 end;
                 
loop(null) -> say("Node shut down");
      % ==
      %   Main Loop
      % ==        
loop(Data) -> say("start loop."),
              receive
                           %Debug
                    {sayData}         -> say(Data),
                                         loop(Data);
              
                           %Know sender
                    {n, Message, PID} -> say(r, Message, PID),
                                         loop(exec(Message, Data, PID));
              
                           %Know sender (but useless (for debuging)).
                    {u, Message, PID} -> say(r, Message, PID),
                                         loop(exec(Message, Data));
                                                                                
                           %Unknow sender.        
                    Message -> say(r, Message),
                               loop(exec(Message, Data))
              end.
    % ==         
    %   Basic Commands     
    % ==   
exec(Message, Data) -> 
    case Message of
            %Add a new neighbor.          
        {add, PID}  ->  sD(b, Data, addNode(Data, PID));
                        
            %Add a new node to the topology.
        {put, PID}  ->  sD(b, Data, putNode(Data, PID));
                        
            %Register the thread.
        {erlRegist, Name} ->  erlang:register(Name, self()),
                              Data;
                                                             
            %Create link
        {askLink, Node, Id} ->  sD(m, Data, linkTo(Data, Node, Id));
                        
            %broadCast
        {broadcast, SubMessage, IdBC, IDSender} ->  sD(b, Data, broadExec(Data, SubMessage, IdBC, IDSender));
                    
        {repBroad, SenderId, IdBC, Result} -> sD(b, Data, repBroad(Data, SenderId, IdBC, Result));
        
        {endBroad, IdBC, IDSender} ->   sD(b, Data, comm:endBroad(gD(i, Data), IDSender, IdBC, gD(m, Data), gD(b, Data)));

            %Arret
        {stop}  ->  null;
                        
        _       ->  say(" Unknow message."),
                    Data
    end.
                 
                 
exec(Message, Data, Sender) -> case Message of
                                    {ping} ->   Sender!{pong, self()},
                                                loop(Data)
                               end.
       % ==
       %    BroadCast commands.
       % == 
broadExec(Data, Message, BCId, IdParent)  -> case Message of
                                                {add, PID} -> broad:broadAdd(Data, BCId, IdParent, PID);
                                                
                                                {new, PID, Id} -> broad:broadNew(Data, BCId, IdParent, {PID, Id});
                                                
                                                _          -> gD(b, Data)
                                             end.
                                             
repBroad(Data, IdSender, BCId, {Message, Res}) -> case Message of
                                                    {add, PID} -> broad:broadAddRep(Data, BCId, IdSender, Res, PID);
                                                    
                                                    {new, PID, Id} -> broad:broadNewRep(Data, BCId, IdSender, null, {PID, Id});
                                                    
                                                    _          ->   gD(b, Data)
                                                  end.
                                                  
% =====
%   Build the struct.
% =====
    %L'id est un hash md5 de la structure à l'instant.
putNode(Data, PID) ->   broad:broadAdd(Data, tool:getHash(Data), null, PID).

addNode(Data, PID)  ->  add(Data, PID, tool:getVoisinId(gD(i, Data) , tool:getNextFreeKey(gD(m, Data)))).
  
add( Data, PID, Id) ->  send(giveId, PID, {Id}),
                        broad:broadNew(Data, tool:getHash(Data), null, {PID, Id}).
                        
% =====
%   Create Link.
% =====
linkTo(Data, Node, Id)  -> maps:put(tool:getFDB(gD(i, Data), Id), Node, gD(m, Data)).
                                                                                                            
% ====
%   Refresh data.
% ====
refreshNode({Node, Map, Opp, Id, BCMap}, LNode, LId)    -> loop({Node, maps:put(tool:getFDB(LId, Id), LNode, Map), Opp, Id, BCMap}).

% ====
%   Debug function
% ====
say(Value) -> io:fwrite("[~p][   ] : ~p ~n", [self(), Value]).

say(r, Message) -> say(r, Message, "?").
say(r, Message, From) -> io:fwrite("[~p][<- ](~p) receive : ~p ~n", [self(), From, Message]).



