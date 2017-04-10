-module(tool).

-import(maps, [keys/1, get/2, is_key/2]).


-export([hamming/2, getFDB/2]).
-export([getPID/1, getVoisinId/2]).
-export([getLC/2, getNextFreeKey/1]).



-export([applyOn/3]).



% ====
%   Operation on Node
% ====
getKL({ _, KnowLinks}) -> KnowLinks.

getPID({PID, _}) -> PID.

% ====
%   Get Next free key.
% ====
getNextFreeKey(Map)    -> getNextFreeKey(Map, 1).

getNextFreeKey(Map, N) -> case maps:is_key(N, Map) of
                            true -> getNextFreeKey(Map, N+1);
                            
                            false -> N
                          end.
% =====
%   Key min voisins.
% =====
%getMin({Id, Type, Voisins, Op}) ->  applyOn(Min, maps:keys(Voisins), {getNVOisins({Id, Type, Voisins, Op}), Voisins}).
getLC(Node, Map) -> applyOn(fun minNode/3, maps:keys(Map), {getKL(Node), Map}).
%
%
%
%
%
%%%%%%%%%%% TODO Revoir
minNode(Elem, {N, Voisins}, null) -> minNode(Elem, {N, Voisins}, {-1, N});
minNode(Elem, {_, Voisins}, {Id, Val}) -> getMinTuple(Val, Id, getKL(maps:get(Elem, Voisins)), Elem).

getMinTuple(Val1, _, Val2, Id2) when Val2 < Val1 -> {Id2, Val2};
getMinTuple(Val1, Id1, _, _) -> {Id1, Val1}.

getNVoisin({_, _, Voisins, _}) -> maps:size(Voisins).


% =====
%   Hamming distance computation
% =====
hamming(A, B) -> count_bits(differBits(A,B)).
differBits(A, B) -> A bxor B.

count_bits(0) -> 0;
count_bits(A) -> (A band 2#1) + count_bits(A bsr 1).

% =====
%   Get first different bit.
% =====
getFDB(A, B) when A == B    -> 0;
getFDB(A, B)                -> getFDB(A, B, 0).

    %Ne pas exporter !
getFDB(A, B, N) -> case ((A bxor B) band (2#1 bsl N)) of
                        0 -> getFDB(A, B, N+1);
                        _ -> N+1
                   end.
                   
% ====
%   Get node id.
% ====
getVoisinId(Id, Key) -> (Id band ( bnot (2#1 bsl (Key-1)))) bor ((bnot Id) band (2#1 bsl (Key-1))).
                   
% =====
%   Apply on List.
% =====
applyOn(_, [], _) -> null;
applyOn(Func, [H|T], Args) -> Func(H, Args, applyOn(Func, T, Args)).

