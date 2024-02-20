%%
%%  wpc_polyhedron.erl --
%%
%%     Geodesic Dome Plugin
%%
%%  Copyright (c) 2003-2011 Anthony D'Agostino
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wpc_polyhedron).
-export([init/0, menu/2, command/2]).
-include_lib("wings/src/wings.hrl").

init() -> true.

menu({shape}, []) ->
    polyhedron_menu();
menu({shape}, Menu) ->
    Menu ++ [separator|polyhedron_menu()];
menu(_, Menu) -> Menu.

polyhedron_menu() ->
    [{?__(1,"polyhedron"), polyhedron, [option]}].

command({shape,{polyhedron, Arg}}, St) -> make_polyhedron(Arg, St);
command(_, _) -> next.

%%% The rest are local functions.

make_polyhedron(Arg, St) when is_atom(Arg) ->
    wings_dialog:dialog_preview({shape,polyhedron}, Arg, ?__(1,"Polyhedron Options"), dialog(), St);
make_polyhedron(Arg, _) ->
    ArgDict = dict:from_list(Arg),
    BaseFlag = dict:fetch(baseflag, ArgDict),
    Rot_X = dict:fetch(rot_x, ArgDict),
    Rot_Y = dict:fetch(rot_y, ArgDict),
    Rot_Z = dict:fetch(rot_z, ArgDict),
    Mov_X = dict:fetch(mov_x, ArgDict),
    Mov_Y = dict:fetch(mov_y, ArgDict),
    Mov_Z = dict:fetch(mov_z, ArgDict),
    Ground = dict:fetch(ground, ArgDict),

    {Verts, Faces} = polyhedron_main(BaseFlag),    
    Vs = wings_shapes:transform_obj({Rot_X,Rot_Y,Rot_Z},{Mov_X,Mov_Y,Mov_Z},Ground, Verts),
    Name = ?__(2,":polyhedron"),
    ObjName = [Name],
    {new_shape, ObjName, Faces, Vs}.

dialog() ->
    BaseFlag = get_pref(baseflag, tetrahedron),

    [
        {hradio, [{?__(1,"Tetrahedron"), tetrahedron},
           {?__(2,"Hexahedron"), hexahedron},
	       {?__(3,"Octahedron"), octahedron},
	       {?__(4,"Icosahedron"), icosahedron},
           {?__(5,"Dodecahedron"), dodecahedron}],
	       BaseFlag,
	       [{key,baseflag}, {title, ?__(10,"Type")}]},

     wings_shapes:transform_obj_dlg()
    ].

polyhedron_main(BaseFlag) ->
    case BaseFlag of
	tetrahedron -> {Verts, Faces} = tetrahedron();
	hexahedron -> {Verts, Faces} = hexahedron();
	octahedron  -> {Verts, Faces} = octahedron();
	icosahedron -> {Verts, Faces} = icosahedron();
	dodecahedron -> {Verts, Faces} = dodecahedron()
    end,
    {Verts, Faces}.

icosahedron() ->
    H = math:sqrt(1/5),
    L = math:sqrt(4/5),
    S = math:pi()*2/5,
    A = L*math:cos(S),
    B = L*math:sin(S),
    C = L*math:cos(S*2),
    D = L*math:sin(S*2),
    E = 0.0,
    F = 1.0,
    Verts = [
    {E,H,L},{E,F,E},{B,H,A},{-E,-H,-L},{D,H,C},{-D,H,C},{B,-H,-A},
    {D,-H,-C},{-D,-H,-C},{-B,H,A},{E,-F,E},{-B,-H,-A}],
    Faces = [
    [1,0,2],[9,0,1],[1,5,9],[1,4,5],[1,2,4],[0,8,7],[9,11,8],
    [5,3,11],[4,6,3],[2,7,6],[3,5,4],[6,4,2],[2,0,7],[8,0,9],
    [11,9,5],[10,3,6],[10,6,7],[10,7,8],[10,8,11],[10,11,3]],
    {Verts, Faces}.

octahedron() ->
    Verts = [
    {1.0,0.0,0.0},{0.0,1.0,0.0},{0.0,0.0,1.0},
    {-1.0,0.0,0.0},{0.0,-1.0,0.0},{0.0,0.0,-1.0}],
    Faces = [[1,2,0],[1,0,5],[1,5,3],[1,3,2],[4,0,2],[4,5,0],[4,3,5],[4,2,3]],
    {Verts, Faces}.

tetrahedron() ->
    Verts = [
    {0.000000,0.612372,0.000000},{0.000000,-0.204124,0.577350},
    {-0.500000,-0.204124,-0.288675},{0.500000,-0.204124,-0.288675}],
    Faces = [[2,1,0],[3,2,0],[1,3,0],[2,3,1]],
    {Verts, Faces}.

hexahedron() ->
    Verts = [
    {-0.50000000,-0.50000000,-0.50000000},
    {-0.50000000,-0.50000000,0.50000000},
    {-0.50000000,0.50000000,-0.50000000},
    {-0.50000000,0.50000000,0.50000000},
    {0.50000000,-0.50000000,-0.50000000},
    {0.50000000,-0.50000000,0.50000000},
    {0.50000000,0.50000000,-0.50000000},
    {0.50000000,0.50000000,0.50000000} ],
    Faces = [[0,4,5,1], [1,3,2,0], [1,5,7,3],
    [2,6,4,0], [3,7,6,2], [4,6,7,5]],
    {Verts, Faces}.

dodecahedron() -> %NOT INPLEMENTED
    Verts = [
    {-10.50000000,-0.50000000,14.50000000},
    {-10.50000000,-0.50000000,15.50000000},
    {-10.50000000,0.50000000,14.50000000},
    {-10.50000000,0.50000000,15.50000000},
    {-9.50000000,-0.50000000,14.50000000},
    {-9.50000000,-0.50000000,15.50000000},
    {-9.50000000,0.50000000,14.50000000},
    {-9.50000000,0.50000000,15.50000000} ],
    Faces = [[5,9,10,6], [6,8,7,5], [6,10,12,8],
    [7,11,9,5], [8,12,11,7], [9,11,12,10]],
    {Verts, Faces}.

get_pref(Key, Def) ->
    wpa:pref_get(?MODULE, Key, Def).
