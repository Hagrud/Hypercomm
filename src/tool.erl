-module(tool).

-import(maps, [keys/1, get/2, is_key/2]).


-export([hamming/2, getFDB/2]).
-export([getVoisinId/2]).
-export([getNextFreeKey/1]).


-export([getHash/1]).
-export([getMinTuple/1]).
-export([applyOn/3]).


-export([gD/2, sD/3]).
-export([time_m/0, max/2]).
-export([toList/1]).
-export([aIsRegistered/3, aUnRegister/3, aRegister/3]).
% ====
%   Operation on Data = {Map, Objects, Address, ID, BCMap}
% ====
    %Getter
gD(m, { Map, _, _, _, _}) -> Map;
gD(o, { _, Obj, _, _, _}) -> Obj;
gD(a, { _, _, Add, _, _}) -> Add;
gD(i, { _, _, _,  ID, _}) -> ID;
gD(b, { _, _, _, _, BCM}) -> BCM.

    %Setter
sD(m, { _, Obj, Add,  ID, BCM}, New) -> {New, Obj, Add, ID, BCM};
sD(o, {Map, _, Add, ID, BCM}, New)   -> {Map, New, Add, ID, BCM};
sD(a, {Map, Obj, _, ID, BCM}, New)   -> {Map, Obj, New,  ID, BCM};
sD(i, {Map, Obj, Add, _, BCM}, New)  -> {Map, Obj, Add, New, BCM};
sD(b, {Map, Obj, Add, ID, _}, New)   -> {Map, Obj, Add,  ID, New}.
   
% ====
%   Get the time in milliseconds
% ==== 
time_m() -> erlang:system_time(milli_seconds).
    
max(X, Y) when X > Y    -> X;
max(_, Y)               -> Y.
% ====
%   Get hash from state of a node (we hope it is unique).
% ====
getHash(Data) ->  Time = erlang:system_time(nano_seconds),
                  [gD(i, Data), crypto:hash(md5, [maps:keys(gD(m, Data)), maps:keys(gD(b, Data)), maps:keys(gD(a, Data))]), erlang:integer_to_list(Time)].

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
%getLC(Node, Map) -> applyOn(fun minNode/3, maps:keys(Map), {getKL(Node), Map}).
%
%
%
%
%
%%%%%%%%%%% TODO Revoir
%minNode(Elem, {N, Voisins}, null) -> minNode(Elem, {N, Voisins}, {-1, N});
%minNode(Elem, {_, Voisins}, {Id, Val}) -> getMinTuple(Val, Id, getKL(maps:get(Elem, Voisins)), Elem).

getMinTuple(Map) -> applyOn(fun getMinTuple/3, maps:keys(Map), Map).

getMinTuple(Elem, Map, null)        -> maps:get(Elem, Map);
getMinTuple(Elem, Map, {Id1, Val1}) -> case maps:get(Elem, Map) of
                                            {Id2, Val2} -> getMinTuple(Val2, Id2, Val1, Id1);
                                            
                                            _ -> {Id1, Val1}
                                       end.

getMinTuple(Val1, _, Val2, Id2) when Val2 < Val1 -> {Id2, Val2};
getMinTuple(Val1, Id1, _, _) -> {Id1, Val1}.

%getNVoisin({_, _, Voisins, _}) -> maps:size(Voisins).


% =====
%   Hamming distance computation
% =====
hamming(A, B) -> count_bits(differBits(A,B)).
differBits(A, B) -> A bxor B.

count_bits(0) -> 0;
count_bits(A) -> (A band 2#1) + count_bits(A bsr 1).

% ===
%   Get list from a map of list.
% ===
toList(Map) -> tool:applyOn(fun toList/3, maps:keys(Map), {Map}).
    
toList(_, _, null) -> [];
toList(Elem, {Map}, Ret) -> lists:append([maps:get(Elem, Map), Ret]).

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


%%%
%%  Function on Address map.
%%%
aIsRegistered(Addr, IDObj, IDNode) -> 
    case maps:is_key(IDObj, Addr) of
        true    -> lists:member(IDNode, maps:get(IDObj, Addr));
        
        false   -> false
    end.
    
aRegister(Addr, IDObj, IDNode) ->
    NAddr = aAtoBReg(Addr, IDObj, IDNode),
    aAtoBReg(NAddr, IDNode, IDObj).
    
aUnRegister(Addr, IDObj, IDNode) ->
    NAddr = aAtoBUnReg(Addr, IDObj, IDNode),
    aAtoBUnReg(NAddr, IDNode, IDObj).

aAtoBReg(Addr, A, B) -> 
    case maps:is_key(A, Addr) of
        true    ->  case lists:member(B, maps:get(A, Addr)) of
                        false -> NList = lists:append([[B], maps:get(A, Addr)]),
                                 maps:put(A, NList, Addr);
                        
                        true -> Addr
                    end;
                    
        false   ->  maps:put(A, [B], Addr)
    end.
    
aAtoBUnReg(Addr, A, B) ->
    case maps:is_key(A, Addr) of
        true    ->  case lists:member(B, maps:get(A, Addr)) of
                        false -> Addr;
                        
                        true -> NList = lists:delete(B, maps:get(A, Addr)),
                                maps:put(A, NList, Addr)
                    end;
                    
        false   ->  Addr
    end.










