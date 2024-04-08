
-module(wings_smooth).
-export([smooth/1]).

-include("wings.hrl").
-import(lists, [foldl/3,reverse/1]).

find_next_vertex(V,List,We) ->
    case List of
        [] -> {[],-1};
        [Edge|T] -> 
            [V0,V1] = wings_vertex:from_edges(gb_sets:from_list([Edge]),We),
            if 
                V0 =:= V -> {[V0,V1],V1};
                V1 =:= V -> {[V0,V1],V0};
                true -> find_next_vertex(V,T,We)
            end
    end.

sort_edge(Edges, We) ->
    EdgesList = gb_sets:to_list(Edges),
    io:format("Contenu de la liste TAIL : ~p~n", [EdgesList]),
    sort_edge_1(EdgesList, We,[]).

sort_edge_1(EdgesList, We, Acc) ->
                    io:format("Contenu de la liste Edge : ~p~n", [length(EdgesList)]),
    case EdgesList of
        [] -> Acc;
        [Edge0] -> 
            [V0,V1] = wings_vertex:from_edges(gb_sets:from_list([Edge0]),We),
            io:format("Contenu de la liste Edge : ~p~n", [[V0,V1]]),
            case Acc of
                [] -> [V0,V1];
                _ -> case lists:last(Acc) =:= V0 of
                        true -> Acc++[V1];
                        _ -> Acc++[V0]
                    end
            end;
        [Edge0|T] -> 
            [V0,V1] = wings_vertex:from_edges(gb_sets:from_list([Edge0]),We),
            io:format("Contenu de la liste Edge : ~p~n", [[V0,V1]]),
            case Acc of
                [] -> sort_edge_1(T, We,[V0,V1]);
                _ -> 
                    {Edge,V} = find_next_vertex(lists:last(Acc),T,We),
                    case V of 
                        -1 -> 
                            Acc;
                        _ ->
                            L = lists:delete(Edge, T),
                            sort_edge_1(L, We, Acc++[V])
                    end
            end
    end.


        
            %sort_edge_1(EdgesList, Acc::to_vertices(Value,We), We)

%% Function to smooth edges
smooth(St0) ->
    wings_sel:map(fun(Edges, We0) ->
        Vs = sort_edge(Edges, We0),
        io:format("Contenu de la liste Vs : ~p~n", [Vs]),

        case lists:last(Vs) =:= lists:nth(1,Vs) of
            %Loop
            true -> Vs0 = Vs++[lists:nth(1,Vs)],
                    smooth_loop(Vs0, We0);
            %Not a loop
            _ -> smooth_loop(Vs, We0) %smooth_segment(Vs, We0)
        end

        %case wings_edge_loop:edge_loop_vertices(Edges,We0) of
        %    [Vs] -> We = smooth_straight(Vs, We0);
        %    _ -> We = We0
        %end,
        %We
    end, St0).

smooth_loop(Vs, #we{vp=Vtab0}=We) -> 
    io:format("Contenu de la liste Edges : ~p~n", [Vtab0]),
    V0 = lists:nth(1,Vs),
    V1 = lists:nth(2,Vs),
    VN = lists:nth(length(Vs) -1,Vs),
    Pos0 = array:get(V0, Vtab0),
    Pos1 = array:get(V1, Vtab0),
    PosN = array:get(VN, Vtab0),

    NewPos =  e3d_vec:add(e3d_vec:add(e3d_vec:divide(Pos1,4), e3d_vec:divide(PosN,4) ), e3d_vec:divide(Pos0,2)),
    Acc = array:set(V0, NewPos, Vtab0),
    T = lists:delete(V0, Vs),
    Vtab = smooth(T, V0, We, Acc),

    io:format("Contenu de la liste Edges : ~p~n", [Vtab]),
    We#we{vp=Vtab}.

smooth(Vs, V0, #we{vp=Vtab0}=We, Acc0) ->
    case Vs of 
        [V1,V2,V3] ->
            Pos1 = array:get(V1, Vtab0),
            Pos2 = array:get(V2, Vtab0),
            Pos3 = array:get(V3, Vtab0),
            NewPos =  e3d_vec:add(e3d_vec:add(e3d_vec:divide(Pos1,4), e3d_vec:divide(Pos3,4) ), e3d_vec:divide(Pos2,2)),  
            array:set(V2, NewPos, Acc0);
        [V1,V2|T] ->
            Pos1 = array:get(V0, Vtab0),
            Pos2 = array:get(V1, Vtab0),
            Pos3 = array:get(V2, Vtab0),
            NewPos =  e3d_vec:add(e3d_vec:add(e3d_vec:divide(Pos1,4), e3d_vec:divide(Pos3,4) ), e3d_vec:divide(Pos2,2)),  
            Acc = array:set(V1, NewPos, Acc0),
            smooth([V2]++T, V1, We, Acc)
    end.


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
