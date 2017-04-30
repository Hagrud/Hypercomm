-module(tool).

-import(maps, [keys/1, get/2, is_key/2]).


-export([hamming/2, getFDB/2]).
-export([getVoisinId/2]).
-export([getLC/2, getNextFreeKey/1]).


-export([getHash/1]).
-export([getMinTuple/1]).
-export([applyOn/3]).


-export([gD/2, sD/3]).
% ====
%   Operation on Data = {PID, Map, Objects, ID, BCMap}
% ====
    %Getter
gD(p, {PID, _, _, _, _}) -> PID;
gD(m, {_, Map, _, _, _}) -> Map;
gD(o, {_, _, Obj, _, _}) -> Obj;
gD(i, {_, _, _,  ID, _}) -> ID;
gD(b, {_, _, _, _, BCM}) -> BCM.

    %Setter
sD(p, {_, Map, Obj,  ID, BCM}, New)   -> {New, Map, Obj,  ID, BCM};
sD(m, {PID, _, Obj,  ID, BCM}, New)   -> {PID, New, Obj,  ID, BCM};
sD(o, {PID, Map, _,  ID, BCM}, New)   -> {PID, Map, New,  ID, BCM};
sD(i, {PID, Map, Obj, _, BCM}, New)   -> {PID, Map, Obj, New, BCM};
sD(b, {PID, Map, Obj, ID, _}, New)    -> {PID, Map, Obj,  ID, New}.
    
    
% ====
%   Get hash from state of a node (we hope it is unique).
% ====
getHash({Node, Map, Opp, Id, BCMap}) -> Time = erlang:system_time(nano_seconds),
                                        [Id, crypto:hash(md5, [maps:keys(Map), maps:keys(BCMap)]), Time].

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

getMinTuple(Map) -> applyOn(fun getMinTuple/3, maps:keys(Map), Map).

getMinTuple(Elem, Map, null)        -> maps:get(Elem, Map);
getMinTuple(Elem, Map, {Id1, Val1}) -> case maps:get(Elem, Map) of
                                            {Id2, Val2} -> getMinTuple(Val2, Id2, Val1, Id1);
                                            
                                            _ -> {Id1, Val1}
                                       end.

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

