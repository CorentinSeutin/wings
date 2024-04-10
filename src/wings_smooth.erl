
-module(wings_smooth).
-export([smooth/1]).

-include("wings.hrl").
-import(lists, [foldl/3,reverse/1]).


%print_list(List, String) ->
%    io:format("~s: ",[String]),
%    print_list(List).
%
%print_list([]) ->
%    io:format("~n"),
%    ok;
%print_list([Head | Tail]) ->
%    io:format("~w-", [Head]),
%    print_list(Tail).

%% Return a sorted list of vertex
sorted_vertex_list(Edges,We) ->
    EdgesList = gb_sets:to_list(Edges),
    add_next_vertex(EdgesList, We,[]).

%% Adds the next vertex AT THE END of the Acc
add_next_vertex([], _We, Acc) -> Acc;
add_next_vertex([H|T], We, []) ->
    Vs = wings_vertex:from_edges(gb_sets:from_list([H]),We),
    add_next_vertex(T, We, Vs);
add_next_vertex(Edges, We, Acc) -> 
    Prev = lists:last(Acc),
    Result = find_next_vertex(Edges,Prev,We),
    case Result of
        {Edge, V} -> 
            L = lists:delete(Edge, Edges),
            add_next_vertex(L, We, Acc++[V]);
        _ -> add_vertex_reverse(Edges, We, Acc)
    end.

%% like add_next_vertex but it adds vertices IN FRONT of Acc
add_vertex_reverse([], _We, Acc) -> Acc;
add_vertex_reverse(Edges, We, [Next|_]=Acc) ->
    Result = find_next_vertex(Edges,Next,We),
    case Result of
        {Edge, V} -> 
            L = lists:delete(Edge, Edges),
            add_vertex_reverse(L, We, [V]++Acc);
        _ -> Acc
    end.

%% Find an Edge from Edges which has vertex V, returns the Edge and the other vertex
find_next_vertex([]=_Egdes, _V, _We) -> nil;
find_next_vertex([Edge|T]=_Egdes, V, We) -> 
    Vs = wings_vertex:from_edges(gb_sets:from_list([Edge]),We),
    case Vs of 
        [V0, V1] when V0 =:= V -> 
            %io:format("V1 is next~n"),
            {Edge,V1};
        [V0, V1] when V1 =:= V -> 
            %io:format("V0 is next~n"),
            {Edge,V0};
        _ -> find_next_vertex(T,V,We)
    end.

%% Function to smooth edges
smooth(St0) ->
    wings_sel:map(fun(Edges, We0) ->
        Vs = sorted_vertex_list(Edges, We0),
        io:format("Contenu de la liste Vs : ~p~n", [Vs]),

        case lists:last(Vs) =:= lists:nth(1,Vs) of
            true -> smooth_loop(Vs, We0); % It's a loop
            _ -> smooth_segment(Vs, We0) % It's not a loop
        end
    end, St0).

%% Calculates the new position of the Current vertex based on the Previous and Next vertices
new_position(Previous,Current,Next, Vtab) ->
    PosP = array:get(Previous, Vtab),
    PosC = array:get(Current, Vtab),
    PosN = array:get(Next, Vtab),
    e3d_vec:add(e3d_vec:add(e3d_vec:divide(PosP,4), e3d_vec:divide(PosN,4) ), e3d_vec:divide(PosC,2)).

%% Smooth function for a loop
smooth_loop([], We) -> We;
smooth_loop([H|T]=Vs, #we{vp=Vtab0}=We) ->
    %print_list(Vs, "Vs")
    Fist = lists:nth(2,Vs),
    Vtab = smooth_loop_1(T, Vtab0, H, Fist, Vtab0),
    We#we{vp=Vtab}.

smooth_loop_1([],_,_,_,Acc) -> Acc; % never happens
smooth_loop_1([Curr], Vtab, Prev, Next, Acc) ->
    Pos = new_position(Prev, Curr, Next, Vtab),
    array:set(Curr, Pos, Acc);
smooth_loop_1([Curr,Next|T], Vtab, Prev, First, Acc) ->
    Pos = new_position(Prev, Curr, Next, Vtab),
    NewAcc = array:set(Curr, Pos, Acc),
    smooth_loop_1([Next]++T, Vtab, Curr, First, NewAcc).
    
smooth_segment([],We) -> We;
smooth_segment([H|T], #we{vp=Vtab0}=We) ->
    %print_list(Vs, "Vs")
    Vtab = smooth_segment_1(T, Vtab0, H, Vtab0),
    We#we{vp=Vtab}.

%% Smooth function for a segment
smooth_segment_1([],_,_,Acc) -> Acc; % never happens
smooth_segment_1([_],_,_,Acc) -> Acc; % never happens
smooth_segment_1([Curr,Next], Vtab, Prev, Acc) ->
    Pos = new_position(Prev, Curr, Next, Vtab),
    array:set(Curr, Pos, Acc);
smooth_segment_1([Curr,Next|T], Vtab, Prev, Acc) ->
    Pos = new_position(Prev, Curr, Next, Vtab),
    NewAcc = array:set(Curr, Pos, Acc),
    smooth_segment_1([Next]++T, Vtab, Curr, NewAcc).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SMOOTH V1 - TO REMOVE !!!!! %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%smooth_segment(Vs, #we{vp=Vtab0}=We) -> 
%    print_list(Vs, "Vs"),
%    io:format("Contenu de la liste Edges : ~p~n", [Vtab0]),
%    V0 = lists:nth(1,Vs),
%    T = lists:delete(V0, Vs),
%    Vtab = smooth_segment_2(T, V0, We, Vtab0),
%
%    io:format("Contenu de la liste Edges : ~p~n", [Vtab]),
%    We#we{vp=Vtab}.
%
%smooth_segment_2(Vs, V0, #we{vp=Vtab0}=We, Acc0) ->
%    print_list(Vs, "Vs"),
%    case Vs of 
%        [V1,V2] ->
%            Pos1 = array:get(V0, Vtab0),
%            Pos2 = array:get(V1, Vtab0),
%            Pos3 = array:get(V2, Vtab0),
%            NewPos =  e3d_vec:add(e3d_vec:add(e3d_vec:divide(Pos1,4), e3d_vec:divide(Pos3,4) ), e3d_vec:divide(Pos2,2)),  
%            array:set(V1, NewPos, Acc0);
%        [V1,V2|T] ->
%            Pos1 = array:get(V0, Vtab0),
 %           Pos2 = array:get(V1, Vtab0),
 %           Pos3 = array:get(V2, Vtab0),
 %           NewPos =  e3d_vec:add(e3d_vec:add(e3d_vec:divide(Pos1,4), e3d_vec:divide(Pos3,4) ), e3d_vec:divide(Pos2,2)),  
 %           Acc = array:set(V1, NewPos, Acc0),
 %           smooth_segment_2([V2]++T, V1, We, Acc)
 %   end.

%smooth_loop(Vs, #we{vp=Vtab0}=We) -> 
%    print_list(Vs, "Vs"),
%    io:format("Contenu de la liste Edges : ~p~n", [Vtab0]),
%    V0 = lists:nth(1,Vs),
%    V1 = lists:nth(2,Vs),
%    T = lists:delete(V0, Vs),
%    Vtab = smooth_loop_2(T, V0, We, Vtab0, V1),
%
%    io:format("Contenu de la liste Edges : ~p~n", [Vtab]),
%    We#we{vp=Vtab}.
%
%smooth_loop_2(Vs, V0, #we{vp=Vtab0}=We, Acc0, First) ->
%    print_list(Vs, "Vs"),
%    case Vs of 
%        [V1] ->
%            Pos = new_position(V0, V1, First),
%            %Pos1 = array:get(V0, Vtab0),
%            %Pos2 = array:get(V1, Vtab0),
%            %Pos3 = array:get(First, Vtab0),
%            %NewPos =  e3d_vec:add(e3d_vec:add(e3d_vec:divide(Pos1,4), e3d_vec:divide(Pos3,4) ), e3d_vec:divide(Pos2,2)),  
%            array:set(V1, Pos, Acc0);
%        [V1,V2|T] ->
%            Pos = new_position(V0, V1, V2),
%            %Pos1 = array:get(V0, Vtab0),
%            %Pos2 = array:get(V1, Vtab0),%
%            %Pos3 = array:get(V2, Vtab0),
%            %NewPos =  e3d_vec:add(e3d_vec:add(e3d_vec:divide(Pos1,4), e3d_vec:divide(Pos3,4) ), e3d_vec:divide(Pos2,2)),  
%            Acc = array:set(V1, Pos, Acc0),
%            smooth_loop_2([V2]++T, V1, We, Acc, First)
%    end.

%find_next_vertex(V,List,We) ->
%    case List of
%        [] -> {[],-1};
%        [Edge|T] -> 
%            [V0,V1] = wings_vertex:from_edges(gb_sets:from_list([Edge]),We),
%            if 
%                V0 =:= V -> %io:format("COUCOU1~n"),
%                            {Edge,V1};
%                V1 =:= V -> {Edge,V0};
%                true -> %io:format("OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO~n"),
%                    find_next_vertex(V,T,We)
%            end
%    end.

%sort_edge_1(EdgesList, We, Acc) ->
%    case EdgesList of
%        [] -> Acc;
%        [Edge0|T] -> 
%            [V0,V1] = wings_vertex:from_edges(gb_sets:from_list([Edge0]),We),
%            print_list(EdgesList,"Edges"),
%            io:format("Contenu de la liste Edge : ~p~n", [[V0,V1]]),
%            case Acc of
%                [] -> sort_edge_1(T, We,[V0,V1]);
%                _ -> 
%                    sort_edge_2(EdgesList, We, Acc)
%            end
%    end.

%sort_edge_2(List,We,Acc) ->
%    {Edge,V} = find_next_vertex(lists:last(Acc),List,We),
%    case V of 
%        -1 -> 
%            Acc;
%        _ ->
%            print_list(wings_vertex:from_edges(gb_sets:from_list([Edge]),We),"Edge"),
%            print_list(List,"List before"),
%            L = lists:delete(Edge, List),
%            print_list(L,"List after"),
%            print_list(Acc,"Acc"),
%            sort_edge_2(L, We, Acc++[V])
%    end.


%smooth_segment(Vs) -> 

%%
%%%% Smooth edges forming a loop
%%smooth_loop(Edges, We) ->
%%    MF = fun(V, WeAcc) ->
%%        %% Implement smoothing logic for each vertex
%%        NewPos = smooth_vertex(V, WeAcc),
%%        wings_vertex:set(V, NewPos, WeAcc)
%%    end,
%%    wings_vertex:map(MF, Edges, We).
%%
%%%% Smooth edges forming a straight line, excluding endpoints
%%smooth_straight(Edges, We) ->
%%    MF = fun(Ed, WeSmooth) ->
%%        {V1, V2} = wings_edge:to_vertices(Ed, WeSmooth),
        %% Implement smoothing logic for each vertex except extremity
%%        case is_endpoint(V1, V2, Edges, WeSmooth) of
%%            true -> %% Do not smooth endpoints
%%                    WeSmooth;
%%            false -> %% Smooth non-endpoint vertices
%%                    NewPos1 = smooth_vertex(V1, WeAcc),
%%                    NewPos2 = smooth_vertex(V2, WeAcc),
%%                    We1 = wings_vertex:set(V1, NewPos1, WeAcc),
%%                    wings_vertex:set(V2, NewPos2, We1)
%%        end
%%    end,
%%    lists:foldl(MF, We, Edges).

%% Return Vtab with the position of V updated depending on their position in We
%% /!\ TEMPORARY /!\ In that instance NewPos = PosV + 5 
%% /!\ TODO /!\ In that instance NewPos = NeiboursPos / 4 + Pos / 2 (?)
%update_position(V, #we{vp=Vtab0}, AccVtab) ->
%    %io:format("Contenu de V : ~p~n", [V]),
%    {X, Y, Z} = array:get(V, Vtab0),
%    NewPos = {X + 5.0, Y + 5.0, Z + 5.0},
%    Vtab = array:set(V, NewPos, AccVtab),
%    Vtab.

%smooth_straight(Vs, #we{vp=Vtab}=We) ->
%    io:format("Contenu de Vtab : ~p~n", [Vtab]),
%    io:format("Contenu de Vs : ~p~n", [Vs]),
	%Positions = [array:get(V, Vtab) || V <- Vs],
    %io:format("Contenu de Positions : ~p~n", [Positions]),
    %PositionsModifiees = modifier_positions(Positions),
    %io:format("Contenu de Positions : ~p~n", [PositionsModifiees]),
%%	Plane = e3d_vec:normal(PositionsModifiees),
%%	wings_vertex:flatten(Vs, Plane, We).

    %% Pour chaque V de Vs -> update_position(V, Acc) où Acc = Vtab 
    %% - *update_position* change revoie un nouveau Vtab avec la position de V modifiée
    %% - comme Vtab change à chaque interation, les changement dans update_position sont fait en 
    %% fonction de We (soit en fonction des position de départ)
%    NewVtab = lists:foldl(fun(V, Acc) -> update_position(V, We, Acc) end, Vtab, Vs),

    %io:format("Contenu de We : ~p~n", [We]),
%    We1 = We#we{vp=NewVtab},
    %io:format("Contenu de We1 : ~p~n", [We1]),
%    We1.



%modifier_positions([]) ->
%    [];
%modifier_positions([_ | RestePositions]) ->
%    NouveauX = 5.0,
%    NouveauY = 5.0,
%    NouveauZ = 5.0,
%    [{NouveauX, NouveauY, NouveauZ} | RestePositions].

%%%% Check if a vertex is an endpoint of the edge set
%%is_endpoint(V1, V2, Edges, We) ->
%%    case lists:member(V1, endpoints(Edges, We)) or lists:member(V2, endpoints(Edges, We)) of
%%        true -> true;
%%        false -> false
%%    end.
%%
%%%% Extract endpoints of the edge set
%%endpoints(Edges, We) ->
%%    lists:flatten([lists:nth(N, wings_edge:to_vertices(E, We)) || E <- Edges, N <- [1, 2]]).
%%
%%%% Smooth a single vertex
%%smooth_vertex(V, We) ->
%%    %% Implement your smoothing logic for a single vertex
%%    %% For simplicity, assuming applying a smoothing ratio of 1/2 to the vertex
%%    {X, Y, Z} = wings_vertex:pos(V, We),
%%    %% Get neighboring vertices
%%    Neighbors = wings_vertex:get_neighbours(V, We),
%%    case Neighbors of
%%        [PrevV, NextV] ->
%%            {PrevX, PrevY, PrevZ} = wings_vertex:pos(PrevV, We),
%%            {NextX, NextY, NextZ} = wings_vertex:pos(NextV, We),
%%            NewX = (PrevX + NextX) / 4 + X / 2,
%%            NewY = (PrevY + NextY) / 4 + Y / 2,
%%            NewZ = (PrevZ + NextZ) / 4 + Z / 2,
%%            {NewX, NewY, NewZ};
%%        _ ->
%%            %% If vertex doesn't have exactly two neighbors, return original position
%%            {X, Y, Z}
%%    end.
