-module(algorithm).

%-import(maps, [new/0, put/3]).
-import(lists, [append/2]).

-import(tool, [getLC/2, getNextFreeKey/1, getVoisinId/2, hamming/2, getMinTuple/1]).
-import(comm, [send/3, prevent/3]).

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

create_first() -> loop({{self(),0}, maps:new(), null, 0, maps:new()}).

loop(waiting) -> receive
                    {reg, Id}   ->  say(r, {reg, Id}),  %say(lists:append("Receive Id : ", integer_to_list(Id))),
                                    loop({{self(), 0}, maps:new(), null, Id, maps:new()});
                                    
                    _           ->  io:fwrite("close ~n")
                 end;
      % ==
      %   Main Loop
      % ==           
loop(Data) -> say("start loop."),
              receive
                           %Debug
                    {sayData}         -> say("Say Data"),
                                         say(Data),
                                         loop(Data);
              
                           %Know sender
                    {n, Message, PID} -> say(r, Message, PID),
                                         exec(Message, Data, PID);
              
                           %Know sender (but useless (for debuging)).
                    {u, Message, PID} -> say(r, Message, PID),
                                         exec(Message, Data);
                                                                                
                           %Unknow sender.        
                    Message -> say(r, Message),
                               exec(Message, Data)
              end.
    % ==         
    %   Basic Commands     
    % ==   
exec(Message, Data) -> case Message of
                            %Ajout d'un nouveau noeud                    
                        {add, PID}          ->  addNode(Data, PID);
                                                             
                            %Creation d'un lien
                        {askLink, Node, Id} ->  linkTo(Data, Node, Id);
                        
                        {newNode, SenderId, NewId, Node} -> propagate(Data, NewId, Node, SenderId);
                        
                        {newWeight, Node, Id} ->  refreshNode(Data, Node, Id);
                        
                            %broadCast
                        {broadcast, SubMessage, IdBC, IDSender} -> broadExec(Data, SubMessage, IdBC, IDSender);
                        
                        {repBroad, Id, IdBC, Result} -> repBroad(Data, Id, IdBC, Result);
                        
                        %        
                            %Arret
                        {stop}        ->  say("close");
                        
                        _             ->  say(" Unknow message."),
                                          loop(Data)
                    end.
                 
                 
exec(Message, Data, Sender) -> case Message of
                                    {ping} ->   Sender!{pong, self()},
                                                loop(Data)
                               end.    
       % ==
       %    BroadCast commands.
       % == 
broadExec(Data, Message, BCId, IdParent)  -> case Message of
                                                {getMinNode} ->  io:fwrite("getMinNode message ~n"),
                                                                 getMinNode(Data, BCId, IdParent)
                                             end.
                                             
repBroad(Data, IdSender, BCId, {Message, Res})    -> case Message of
                                                {getMinNode} -> getMinNodeRep(Data, BCId, IdSender, Res)
                                               end.


getMinNode({Node, Map, Opp, Id, BCMap}, IdBC, IdParent) -> loop({Node, Map, Opp, Id, comm:broadCast(Id, IdParent, IdBC, Map, BCMap, {getMinNode}, fun dMinNode/2) }).

getMinNodeRep({Node, Map, Opp, Id, BCMap}, IdBC, IdSender, Res) -> loop({Node, Map, Opp, Id, comm:repBroad(Id, IdSender, IdBC, Map, BCMap, Res, fun dMinNode/2)}).

dMinNode({null, Map}, Result) -> NResult = maps:put(0, {self(), maps:size(Map)}, Result),
                                 say("Broadcast initiatior decide."),
                                 say(tool:getMinTuple(NResult));

dMinNode({_, Map}, null) -> {self(), maps:size(Map)};

dMinNode({IdParent, Map}, Result) -> NResult = maps:put(0, {self(), maps:size(Map)}, Result),
                                     tool:getMinTuple(NResult).
     
% =====
%   Build the struct.
% =====
addNode({Node, Map, Opp, Id, BCMap}, PID)  ->  say("try adding node"),
                                               addNode({Node, Map, Opp, Id, BCMap}, PID, tool:getLC(Node, Map)).

addNode({Node, Map, Opp, Id, BCMap}, PID, {Key, T}) when Key > 0   ->  send(addNode, maps:get(Key, Map), {PID}),
                                                                       loop({Node, Map, Opp, Id, BCMap});
                                                                                       
addNode({Node, Map, Opp, Id, BCMap}, PID, _)                       ->  add({Node, Map, Opp, Id, BCMap}, PID, tool:getVoisinId(Id, tool:getNextFreeKey(Map))).

    %Ajout initial. (pas le même que les autres car opp = null).
add({{Addr,0}, Map, null, 0, BCMap}, PID, _) -> comm:prevent(newNode, Map, { 0, {Addr,0}, 1, {PID, 0}, 0}),
                                                comm:send(askLink, {PID, 0}, {{Addr, 0}, 0}),
                                                comm:send(newOpp, {PID, 0}, {{Addr, 0}, 0}),
                                                linkTo({{Addr,0}, Map, null, 0, BCMap}, {PID, 0}, 1);


add({Node, Map, Opp, Id, BCMap}, PID, LId) ->   comm:prevent(newNode, Map, { Id, Node, LId, {PID, 0}, Id}),
                                                comm:send(askLink, {PID, 0}, {Node, Id}),
                                                %   comm:send(newOpp, {PID, 0}, {Node, 0}), -> TODO a revoir.
                                                linkTo({Node, Map, Opp, Id, BCMap}, {PID, 0}, LId).

    % = 
    %   Propagation      
    % =
propagate({SNode, Map, Opp, SelfId, BCMap}, Id, Node, SenderId) -> case {tool:hamming(SelfId, Id), maps:is_key( tool:getFDB(SelfId, Id), Map)} of
                                                                        {1, false} -> prevent(propagate, Map, {SelfId, SNode, Id, Node, SenderId}),
                                                                                      comm:send(askLink, Node, {SNode, SelfId}),
                                                                                      linkTo({SNode, Map, Opp, SelfId, BCMap}, Node, Id);
                                                                                  
                                                                        {2, _}     -> prevent(propagate, Map, {SelfId, SNode, Id, Node, SenderId}),
                                                                                      loop({SNode, Map, Opp, SelfId, BCMap});
                                                                                                           
                                                                        _          -> loop({SNode, Map, Opp, SelfId, BCMap})
                                                                   end.

                                                                        %prevent(newNode, Map, {SelfId, SNode, Id, Node, SenderId}),
                                                                        %loop({SNode, Map, Opp, SelfId}).

% =====
%   Create Link.
% =====
linkTo({{Addr, N}, Map, Opp, Id, BCMap}, Node, LId)    ->  prevent(neighChange, Map, {Id, {Addr, N+1}}),
                                                           send(neighChange, Node, {Id, {Addr, N+1}}),
                                                           loop({ {Addr, N+1}, 
                                                                maps:put(tool:getFDB(Id, LId), Node, Map), Opp, Id, BCMap}).
                                                                                                            
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



