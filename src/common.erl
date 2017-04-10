%% @author pep
%% @doc @todo Add description to common.


-module(common).

%% ====================================================================
%% API functions
%% ====================================================================
-export([spawn_/1]).
-export([create_hypercube/0, create_waitNode/0]).

%% ====================================================================
%% Internal functions
%% ====================================================================

% =====
%  node definition
%       ID      : id (en binaire ce serait cool).
%       Type    : main, copy, ghost.
%       Voisins : liste de voisins. {Id, PID, BitChanged}
%       Same    : noeud qui contient les même info. {Id, PID}
%       Ghost   : noeud en attente (contient une copy). {Id, PID}
% =====

spawn_(hypercube)	-> spawn(common, create_hypercube, []);
spawn_(waitingNode) -> spawn(common, create_waitNode, []).


create_hypercube() -> io:fwrite("~p say : hypercube created ~n", [self()]),
					  main_loop({0, main, [], null, null}).
					  
create_waitNode() -> io:fwrite("~p say : create new waiting node ~n", [self()]),
                     waiting_loop({null, ghost, [], null, null}).
					  
main_loop(Data) -> receive
					{addNode, PID} -> addNode(Data, PID);
					{stop}         -> io:fwrite("~p exit with value ~p", [self(), Data])
			   	   end.

waiting_loop(Data) -> receive
                        {attribID, ID}    -> attributeID(Data, ID);
					    {toLink, Node, 0} -> addVoisin(Data, Node);
					    {promote}         -> promote(Data);
					    {stop}            -> io:fwrite("~p exit with value ~p", [self(), Data])
                      end.

% =====
%   AddNode
%       if node as no ghost:
%           we need to create the ghost.
% =====

    % add the first node.
addNode({Id, main, Voisins, null, null}, PID) -> io:fwrite("~p add node ~n", [self()]),
                                                 PID!{attribID, 1},
                                                 PID!{toLink, {Id, self(), 0}},
                                                 PID!{promote},
										         main_loop({Id, main, [{1, PID, 0}, Voisins], {1, PID}, null}).
										         

% =====
%   Attribute ID:
% =====
attributeID({_, Type, Voisins, Copy, Ghost}, NewID) ->  io:fwrite("~p take id ~p ~n", [self(), NewID]),
                                                        waiting_loop({NewID, Type, Voisins, Copy, Ghost}).

% =====
%   Add voisins:
% =====
addVoisin({Id, Type, Voisins, Copy, Ghost}, Node) ->    io:fwrite("~p get linked to ~p ~n", [self(), Node]),
                                                        waiting_loop({Id, Type, [Node,Voisins], Copy, Ghost}).
                                                        
% =====
%   Promote:
% =====
promote({Id, ghost, Voisins, Copy, Ghost}) -> main_loop({Id, copy, Voisins, Copy, Ghost}).

% =====
%   Broadcast:
% =====
broadCast(Sender, Message, Voisins, MessageId) -> null.

% =====
%   Apply on list not elems.
% =====
applyOnListExcept(_, [], _) -> null;
applyOnListExcept(Func, [Except|T], Except) -> applyOnListExcept(Func, T, Except);
applyOnListExcept(Func, [H|T], Except) -> Func(H),
                                          applyOnListExcept(Func, T, Except).

