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


tetrahedron() ->
    Verts = [
    {0.0,0.47628967,0.0}, {0.0,-0.34020691,0.57735027}, 
    {-0.5,-0.34020691,-0.28867513}, {0.5,-0.34020691,-0.28867513}],
    Faces = [[0,2,1], [0,3,2], [1,3,0], [2,3,1]],
    {Verts,Faces}.

hexahedron() ->
    Verts = [
    {-0.5,-0.5,-0.5}, {-0.5,-0.5,0.5}, {-0.5,0.5,-0.5}, 
    {-0.5,0.5,0.5}, {0.5,-0.5,-0.5}, {0.5,-0.5,0.5}, 
    {0.5,0.5,-0.5}, {0.5,0.5,0.5}],
    Faces = [[0,4,5,1], [1,3,2,0], [1,5,7,3], [2,6,4,0], 
    [3,7,6,2], [4,6,7,5]],
    {Verts,Faces}.

octahedron() ->
    Verts = [{0.70710678,0.0,0.0}, {-0.70710678,0.0,0.0}, 
    {0.0,0.70710678,0.0}, {0.0,-0.70710678,0.0}, 
    {0.0,0.0,0.70710678}, {0.0,0.0,-0.70710678}],
    Faces = [[0,4,3], [0,5,2], [1,4,2], [1,5,3], 
    [2,4,0], [2,5,1], [3,4,1], [3,5,0]],
    {Verts,Faces}.

icosahedron() ->
    Verts = [{0.0,0.95105652,0.0}, {0.85065081,0.4253254,0.0}, {0.26286556,0.4253254,0.80901699}, 
    {-0.68819096,0.4253254,0.5}, {-0.68819096,0.4253254,-0.5}, {0.26286556,0.4253254,-0.80901699}, 
    {-0.85065081,-0.4253254,0.0}, {-0.26286556,-0.4253254,-0.80901699}, {0.68819096,-0.4253254,-0.5}, 
    {0.68819096,-0.4253254,0.5}, {-0.26286556,-0.4253254,0.80901699}, {0.0,-0.95105652,0.0}],
    Faces = [[0,2,1], [0,3,2], [0,4,3], [0,5,4], [1,5,0], [1,8,5], 
    [1,9,8], [2,9,1], [2,10,9], [3,10,2], [4,6,3], [4,7,6], [5,7,4], 
    [5,8,7], [6,10,3], [6,11,10], [7,11,6], [8,11,7], [9,11,8], [10,11,9]],
    {Verts,Faces}.

dodecahedron() -> 
    Verts = [{-0.5,0.0,1.30901699}, {0.5,0.0,1.30901699}, {-0.80901699,-0.80901699,-0.80901699}, 
    {-0.80901699,-0.80901699,0.80901699}, {-0.80901699,0.80901699,-0.80901699}, 
    {-0.80901699,0.80901699,0.80901699}, {0.80901699,-0.80901699,-0.80901699}, 
    {0.80901699,-0.80901699,0.80901699}, {0.80901699,0.80901699,-0.80901699}, 
    {0.80901699,0.80901699,0.80901699}, {1.30901699,0.5,0.0}, {1.30901699,-0.5,0.0}, 
    {-1.30901699,0.5,0.0}, {-1.30901699,-0.5,0.0}, {-0.5,0.0,-1.30901699}, {0.5,0.0,-1.30901699}, 
    {0.0,1.30901699,0.5}, {0.0,1.30901699,-0.5}, {0.0,-1.30901699,0.5}, {0.0,-1.30901699,-0.5}],
    Faces = [[0,3,18,7,1], [0,5,12,13,3], [1,9,16,5,0], [2,19,18,3,13], [4,17,8,15,14], 
    [6,15,8,10,11], [7,11,10,9,1], [8,17,16,9,10], [11,7,18,19,6], [12,5,16,17,4], 
    [13,12,4,14,2], [14,15,6,19,2]],
    {Verts,Faces}.

get_pref(Key, Def) ->
    wpa:pref_get(?MODULE, Key, Def).
