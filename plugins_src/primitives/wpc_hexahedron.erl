%%
%%  wpc_nhexahedron.erl --
%%
%%     N-Hexahedron and N-Gon Plugin
%%
%%  Copyright (c) 2003-2011 Anthony D'Agostino
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%

-module(wpc_hexahedron).
-export([init/0,menu/2,command/2]).
-import(math, [cos/1,sin/1,pi/0]).
-include_lib("wings/src/wings.hrl").

init() -> true.

menu({shape}, []) ->
    menu();
menu({shape}, Menu) ->
    [Hexahedron|Ngon] = menu(),
    [Hexahedron] ++ Menu ++ [separator|Ngon];
menu(_, Menu) -> Menu.

menu() ->
    [{hexahedron_str(),hexahedron,?__(1,"Create a hexahedron"),[option]},
     {?__(2,"N-Gon"),ngon,[option]}].

command({shape,{hexahedron, Arg}}, St) -> make_hexahedron(Arg, St);
command({shape,{ngon, Arg}}, St) -> make_ngon(Arg, St);
command(_, _) -> next.

hexahedron_str() ->
    ?__(1,"Hexahedron").

%%% The rest are local functions.

% =============
% === Hexahedron ===
% =============
make_hexahedron(Arg, St) when is_atom(Arg) ->
    wings_dialog:dialog_preview({shape,hexahedron}, Arg, ?__(1,"Hexahedron Options"),
				hexahedron_dialog(), St);
make_hexahedron(Arg, _) ->
    % set_pref(Arg),	% don't save
    ArgDict = dict:from_list(Arg),
    Nres = dict:fetch(nres, ArgDict),
    X = dict:fetch(xhexahedron, ArgDict)/2,
    Y = dict:fetch(yhexahedron, ArgDict)/2,
    Z = dict:fetch(zhexahedron, ArgDict)/2,
    Rot_X = dict:fetch(rot_x, ArgDict),
    Rot_Y = dict:fetch(rot_y, ArgDict),
    Rot_Z = dict:fetch(rot_z, ArgDict),
    Mov_X = dict:fetch(mov_x, ArgDict),
    Mov_Y = dict:fetch(mov_y, ArgDict),
    Mov_Z = dict:fetch(mov_z, ArgDict),
    Ground = dict:fetch(ground, ArgDict),

    SpherizeFlag = dict:fetch(spherizeflag, ArgDict),
    Verts = hexahedron_verts(Nres+1),
    Faces = hexahedron_faces(Nres+1, Nres+1),
    {Vs0, Fs} = clean_indexed_mesh(Verts, Faces),
    Vs1 = transform_mesh(SpherizeFlag, {X,Y,Z}, Vs0),
    Vs = wings_shapes:transform_obj({Rot_X,Rot_Y,Rot_Z},{Mov_X,Mov_Y,Mov_Z},Ground, Vs1),
    {new_shape,hexahedron_str(),Fs,Vs}.

hexahedron_dialog() ->
    Nres = get_pref(nres, 1),
    SpherizeFlag = get_pref(spherizeflag, false),
    [{hframe,
      [{slider, {text, Nres,
		 [{key,nres},{range,{1,20}}]}}],[{title, ?__(1,"Number of Cuts")}]},
        {hframe,[
            {label_column, [
                {wings_s:dir(x), {text,2.0,[{key,xhexahedron},{range,{0.0,infinity}}]}},
                {wings_s:dir(y), {text,2.0,[{key,yhexahedron},{range,{0.0,infinity}}]}},
                {wings_s:dir(z), {text,2.0,[{key,zhexahedron},{range,{0.0,infinity}}]}}
            ]}
        ],[{margin,false}]},
        {hradio,[{?__(2,"Yes"), true},
                 {?__(3,"No"), false}],
                SpherizeFlag, [{key,spherizeflag}, {title, ?__(4,"Spherize")}]},
        wings_shapes:transform_obj_dlg()
    ].

hexahedron_verts(Nres) ->
    A = math:sqrt(1.0/8.0*Nres),
    B = math:sqrt(1.0/4.0),
    C = 0.0,
    Verts = [{C, A, B}, {C, A, -B}, {-B, -A, C}, {B, -A, C}],
    Verts.

transform_mesh(false, Box, Vs) ->
    [transform(Box,V) || V <- Vs];
transform_mesh(true, Box, Vs) ->
    [transform(Box,e3d_vec:norm(V)) || V <- Vs].

transform({Xs,Ys,Zs}, {Xp,Yp,Zp}) ->
    {Xp*Xs, Yp*Ys, Zp*Zs}.

hexahedron_faces(Nres, Nres) ->
    Faces = [[0, 1, 2], [0, 2, 3], [0, 3, 1], [1, 3, 2]],
    Faces.

% =============
% === N-Gon ===
% =============
make_ngon(Arg, St) when is_atom(Arg) ->
    wings_dialog:dialog_preview({shape,ngon}, Arg, ?__(1,"N-Gon Options"), ngon_dialog(), St);
make_ngon(Arg, _) ->
    ArgDict = dict:from_list(Arg),
    NumVerts = dict:fetch(numverts, ArgDict),
    Radius = dict:fetch(radius, ArgDict),
    Rot_X = dict:fetch(rot_x, ArgDict),
    Rot_Y = dict:fetch(rot_y, ArgDict),
    Rot_Z = dict:fetch(rot_z, ArgDict),
    Mov_X = dict:fetch(mov_x, ArgDict),
    Mov_Y = dict:fetch(mov_y, ArgDict),
    Mov_Z = dict:fetch(mov_z, ArgDict),
    Ground = dict:fetch(ground, ArgDict),

    Vs1 = ngon_verts(NumVerts, Radius),
    Vs = wings_shapes:transform_obj({Rot_X,Rot_Y,Rot_Z},{Mov_X,Mov_Y,Mov_Z},Ground, Vs1),
    Fs = ngon_faces(NumVerts),
    {new_shape,?__(2,"N-Gon"),Fs,Vs}.

ngon_dialog() ->
    NumVerts = get_pref(numverts, 5),
    Radius = get_pref(radius, 1.0),
    [{vframe, [
        {label_column, [
            {?__(3,"Number of Verts"), {slider, {text, NumVerts,
                                                 [{key, numverts}, {range, {3, 20}}]}}},
            {?__(4,"Radius"), {slider, {text, Radius, [{key, radius}, {range, {0.1, 20.0}}]}}}]
        },
        wings_shapes:transform_obj_dlg()]
    }].

ngon_verts(NumVerts, Radius) ->
    Nres = NumVerts,
    Delta = 2*pi()/Nres,
    [{Radius*cos(I*Delta),
      0.0,
      Radius*sin(I*Delta)} || I <- lists:seq(0, Nres-1)].

ngon_faces(NumVerts) ->
    Nres = NumVerts,
    BotFaces = lists:seq(0, Nres-1),
    TopFaces = lists:reverse(BotFaces),
    Faces = [TopFaces, BotFaces],
    Faces.

clean_indexed_mesh(Verts, Faces) ->
    Raw = e3d_util:indexed_to_raw(Verts, Faces),
    e3d_util:raw_to_indexed(Raw).

get_pref(Key, Def) ->
    wpa:pref_get(?MODULE, Key, Def).
