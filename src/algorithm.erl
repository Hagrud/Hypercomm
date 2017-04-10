-module(algorithm).

%-import(maps, [new/0, put/3]).
-import(lists, [append/2]).

-import(tool, [getLC/2, getNextFreeKey/1, getVoisinId/2]).
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

create_first() -> loop({{self(),0}, maps:new(), null, 0}).

loop(waiting) -> receive
                    {reg, Id}   ->  say(r, {reg, Id}),  %say(lists:append("Receive Id : ", integer_to_list(Id))),
                                    loop({{self(), 0}, maps:new(), null, Id});
                                    
                    _           ->  io:fwrite("close ~n")
                 end;
                 
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
              
exec(Message, Data) -> case Message of
                            %Ajout d'un nouveau noeud                    
                        {add, PID}          ->  addNode(Data, PID);
                                     
                            %Creation d'un lien
                        {askLink, Node, Id} ->  linkTo(Data, Node, Id);
                        
                       % {newNode, SenderId, NewId, Node} -> propagate(Data, NewId, Node, SenderId);
                        
                        {newWeight, Node, Id} ->  refreshNode(Data, Node, Id);
                        
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
              
% =====
%   Build the struct.
% =====
addNode({Node, Map, Opp, Id}, PID)  ->  say("try adding node"),
                                        addNode({Node, Map, Opp, Id}, PID, tool:getLC(Node, Map)).

addNode({Node, Map, Opp, Id}, PID, {Key, T}) when Key > 0   ->  send(addNode, maps:get(Key, Map), {PID}),
                                                                loop({Node, Map, Opp, Id});
                                                                                       
addNode({Node, Map, Opp, Id}, PID, _)                       ->  add({Node, Map, Opp, Id}, PID, tool:getVoisinId(Id, tool:getNextFreeKey(Map))).

    %Ajout initial. (pas le même que les autres car opp = null).
add({{Addr,0}, Map, null, 0}, PID, _) ->    comm:prevent(newNode, Map, { 0, {Addr,0}, 1, {PID, 0}, 0}),
                                            comm:send(askLink, {PID, 0}, {{Addr, 0}, 0}),
                                            comm:send(newOpp, {PID, 0}, {{Addr, 0}, 0}),
                                            linkTo({{Addr,0}, Map, null, 0}, {PID, 0}, 1);


add({Node, Map, Opp, Id}, PID, LId) ->  comm:prevent(newNode, Map, { Id, Node, LId, {PID, 0}, Id}),
                                        comm:send(askLink, {PID, 0}, {Node, Id}),
                                        %   comm:send(newOpp, {PID, 0}, {Node, 0}), -> TODO a revoir.
                                        linkTo({Node, Map, Opp, Id}, {PID, 0}, LId).

    % = 
    %   Propagation
    %       TODO : Take hamming distance and Map        
    % =
%propagate({SNode, Map, Opp, SelfId}, Id, Node, SenderId)    ->  case tool:hamming() of
%                                                                    1   ->  case maps:is_key( Tool:getFDB(SelfId, Id), Map) of  
%                                                                                false -> prevent(newNode, Map, {selfId, SNode, Id, Node, SenderId})
%                                                                            end;
                                                                    %;
                                                                      
 %                                                                   _   ->  loop({SNode, Map, Opp, SelfId})
 %                                                               end.

                                                                        %prevent(newNode, Map, {SelfId, SNode, Id, Node, SenderId}),
                                                                        %loop({SNode, Map, Opp, SelfId}).

% =====
%   Create Link.
% =====
linkTo({{Addr, N}, Map, Opp, Id}, Node, LId)    ->  prevent(neighChange, Map, {Id, {Addr, N+1}}),
                                                    send(neighChange, Node, {Id, {Addr, N+1}}),
                                                    loop({ {Addr, N+1}, 
                                                            maps:put(tool:getFDB(Id, LId), Node, Map),
                                                            Opp, Id }).
                                                                                                            
% ====
%   Refresh data.
% ====
refreshNode({Node, Map, Opp, Id}, LNode, LId)    -> loop({Node, maps:put(tool:getFDB(LId, Id), LNode, Map), Opp, Id}).

% ====
%   Debug function
% ====

%say(id, {_, _, _, Id}, Value) -> io:fwrite("[~p] id : [~p] ~p ~n", [self(), Id, Value]).

say(Value) -> io:fwrite("[~p][   ] : ~p ~n", [self(), Value]).

say(r, Message) -> say(r, Message, "?").
say(r, Message, From) -> io:fwrite("[~p][<- ](~p) receive : ~p ~n", [self(), From, Message]).



