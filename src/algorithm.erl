-module(algorithm).

%-import(maps, [new/0, put/3]).
-import(lists, [append/2]).

-import(tool, [getLC/2, getNextFreeKey/1, getVoisinId/2, hamming/2, getMinTuple/1]).
-import(comm, [send/3, prevent/3]).
-import(broad, [broadAdd/4, broadAddRep/5]).

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
exec(Message, Data) -> 
    case Message of
            %Ajout d'un nouveau voisin  (Sur ce noeud).          
        {add, PID}  ->  addNode(Data, PID);
                        
            %Ajout d'un nouveau noeud au reseau.
        {put, PID}  ->  putNode(Data, PID);
                        
        {reg, Name} ->  erlang:register(Name, self()),
                        loop(Data);
                                                             
            %Creation d'un lien
        {askLink, Node, Id} ->  linkTo(Data, Node, Id);
                        
        {newNode, SenderId, NewId, Node} -> propagate(Data, NewId, Node, SenderId);
                        
        {newWeight, Node, Id} ->  refreshNode(Data, Node, Id);
                        
            %broadCast
        {broadcast, SubMessage, IdBC, IDSender} ->  {Node, Map, Opp, Id, _} = Data,
                                                    loop({Node, Map, Opp, Id, broadExec(Data, SubMessage, IdBC, IDSender)});
                    
        {repBroad, Id, IdBC, Result} -> {Node, Map, Opp, SelfId, _} = Data,
                                        loop({Node, Map, Opp, SelfId, repBroad(Data, Id, IdBC, Result)});


        {endBroad, IdBC, IDSender} ->   {Node, Map, Opp, Id, BCMap} = Data,
                                        loop({Node, Map, Opp, Id, comm:endBroad(Id, IDSender, IdBC, Map, BCMap)});

            %Arret
        {stop}  ->  loop(null);
                        
        _       ->  say(" Unknow message."),
                    loop(Data)
    end.
                 
                 
exec(Message, Data, Sender) -> case Message of
                                    {ping} ->   Sender!{pong, self()},
                                                loop(Data)
                               end.
       % ==
       %    BroadCast commands.
       % == 
broadExec(Data, Message, BCId, IdParent)  -> {_, _, _, _, BCMap} = Data,
                                             case Message of
                                                {add, PID} -> broad:broadAdd(Data, BCId, IdParent, PID);
                                                
                                                _          -> BCMap
                                             end.
                                             
repBroad(Data, IdSender, BCId, {Message, Res}) -> {Node, Map, Opp, Id, BCMap} = Data,
                                                  case Message of
                                                    {add, PID} -> broadAddRep(Data, BCId, IdSender, Res, PID);
                                                    
                                                    _          -> BCMap
                                                  end.
  
% =====
%   Build the struct.
% =====
    %L'id est un hash md5 de la structure à l'instant.
putNode(Data, PID) ->   {Node, Map, Opp, Id, _ } = Data,
                        loop({Node, Map, Opp, Id, broad:broadAdd(Data, tool:getHash(Data), null, PID)}).

addNode({Node, Map, Opp, Id, BCMap}, PID)  ->  add({Node, Map, Opp, Id, BCMap}, PID, tool:getVoisinId(Id, tool:getNextFreeKey(Map))).

    
add({Node, Map, Opp, Id, BCMap}, PID, LId) ->   comm:prevent(newNode, Map, { Id, Node, LId, {PID, 0}, Id}),
                                                comm:send(askLink, {PID, 0}, {Node, Id}),
                                                linkTo({Node, Map, Opp, Id, BCMap}, {PID, 0}, LId).

    % = 
    %   Propagation      useless on va broadcast.
    % =
propagate({SNode, Map, Opp, SelfId, BCMap}, Id, Node, SenderId) -> case {tool:hamming(SelfId, Id), maps:is_key( tool:getFDB(SelfId, Id), Map)} of
                                                                        {1, false} -> prevent(propagate, Map, {SelfId, SNode, Id, Node, SenderId}),
                                                                                      comm:send(askLink, Node, {SNode, SelfId}),
                                                                                      linkTo({SNode, Map, Opp, SelfId, BCMap}, Node, Id);
                                                                                  
                                                                        {2, _}     -> prevent(propagate, Map, {SelfId, SNode, Id, Node, SenderId}),
                                                                                      loop({SNode, Map, Opp, SelfId, BCMap});
                                                                                                           
                                                                        _          -> loop({SNode, Map, Opp, SelfId, BCMap})
                                                                   end.

% =====
%   Create Link.
% =====
linkTo({{Addr, N}, Map, Opp, Id, BCMap}, Node, LId)    ->  prevent(neighChange, Map, {Id, {Addr, N+1}}),
                                                           send(neighChange, Node, {Id, {Addr, N+1}}),
                                                           loop({ {Addr, N+1}, maps:put(tool:getFDB(Id, LId), Node, Map), Opp, Id, BCMap}).
                                                                                                            
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



