-module(search_in_file).
-export([search_polygon_parameters/2]).
-import(string,[concat/2]).

%Example:
%   o Tetra
%   # 4 vertices / 4 faces / position = -20 0 15
%   v -20.00000000   0.47628967  15.00000000
%   v -20.00000000  -0.34020691  15.57735027
%   v -20.50000000  -0.34020691  14.71132487
%   v -19.50000000  -0.34020691  14.71132487
%   f 1 3 2
%   f 1 4 3
%   f 2 4 1
%   f 3 4 2

%Returns: [4,4,-20,0,15,v1,v2,v3,v3,f1,f2,f3,f4]
%with vi = [xi,yi,zi] and fi = [face1,face2,face3]
search_polygon_parameters(File, Name) ->
    Str = concat("o ",Name),
    N_vertices = 0,
    case file:read_line(File) of
        {ok, Line} ->
            if
                Str =:= Line -> 
                    % "# 4 vertices / 4 faces / position = -20 0 15"
                    case file:read_line(File) of
                        {ok, Line2} ->
                            Res = get_parameters_in_line(Line2),
                            [N_vertices | Others] = Res,
                            [N_faces | _] = Others,

                            case search_vertices(File,N_vertices,[]) of
                                {all_found,Vertices_list} -> 
                                    Res++Vertices_list,

                                    case search_faces(File,N_faces,[]) of
                                        {all_found,Faces_list} -> Res++Faces_list;
                                        {eof_found,Str2} -> {eof_found,Str2};
                                        {error, Reason} -> {error, Reason}
                                    end;
                                {eof_found,Str2} -> {eof_found,Str2};
                                {error, Reason} -> {error, Reason}
                            end;
                        eof -> {eof_found, "eof found.\n"};
                        {error, Reason} -> {error, Reason}
                    end;
                Str =/= Line -> search_polygon_parameters(File, Name)
            end;
        eof -> {eof_found, "eof found.\n"};
        {error, Reason} -> {error, Reason}
    end.

% Get : # 4 vertices / 4 faces / position = -20 0 15
get_parameters_in_line(Line) ->
    Res = [],
    % N_VERTICES INSTRUCTIONS
    % ["4 vertices / 4 faces / position = -20 0 15"]
    Line3_ = string:tokens(Line, "# "),
    [Line3] = Line3_,
    % ["4 vertices","4 faces","position = -20 0 15"]
    Words = string:tokens(Line3, " / "),
    % N_vertices_str = "4 vertices"
    [ N_vertices_str | Parameters] = Words,
    % ["4","vertices"]
    N_vertices_words = string:tokens(N_vertices_str, " "),
    [ N_vertices_ | _] = N_vertices_words,
    % "4"
    N_vertices = list_to_integer(N_vertices_),
    %####################################

    % N_FACES INSTRUCTIONS
    % N_faces_str = "4 faces"
    [ N_faces_str | Parameters2] = Parameters,
    % ["4","faces"]
    N_faces_words = string:tokens(N_faces_str, " "),
    [ N_faces_ | _] = N_faces_words,
    % "4"
    N_faces = list_to_integer(N_faces_),
    %####################################

    % POSITION INSTRUCTIONS
    % N_faces_str = "position = -20 0 15"
    [ Pos_str ] = Parameters2,
    % ["position","-20 0 15"]
    Pos_words = string:tokens(Pos_str, " = "),
    % "-20 0 15"
    [_,Position] = Pos_words,
    % ["-20","0","15"]
    Pos_xyz = string:tokens(Position, " "),
    % ["-20" | "0","15"]
    [Pos_x_ | Pos_yz] = Pos_xyz,
    Pos_x = list_to_integer(Pos_x_),
    [Pos_y_ | Pos_z_] = Pos_yz,
    Pos_y = list_to_integer(Pos_y_),
    Pos_z = list_to_integer(Pos_z_),
    Res++N_vertices++N_faces++Pos_x++Pos_y++Pos_z.

%VERTICES SEARCH
search_vertices(File,N_vertices,Vertices_list) when N_vertices =/= 0 -> 
    case search_coordinates(File) of
        {ok,Coordinates} -> 
            Vertices = [Coordinates],
            search_vertices(File,N_vertices-1,Vertices_list++Vertices);
        {eof_found,Str} -> {eof_found,Str};
        {error, Reason} -> {error, Reason}
    end;

search_vertices(_,N_vertices,Vertices_list) when N_vertices =:= 0 ->
    {all_found,Vertices_list}.

search_coordinates(File) ->
    case file:read_line(File) of
        {ok, Line} ->
            Words = string:tokens(Line, " "),
            [ _ | Coordinates] = Words,
            [X_ | XY] = Coordinates,
            X = [list_to_integer(X_)],
            [Y_ | Z_] = XY,
            Y = [list_to_integer(Y_)],
            Z = [list_to_integer(Z_)],
            {ok,X++Y++Z};
        eof -> {eof_found, "eof found.\n"};
        {error, Reason} -> {error, Reason}
    end.

%FACES SEARCH
search_faces(File,N_faces,Faces_list) when N_faces =/= 0 -> 
    case search_faces_list(File) of
        {ok,Faces_} -> 
            Faces = [Faces_],
            search_vertices(File,N_faces-1,Faces_list++Faces);
        {eof_found,Str} -> {eof_found,Str};
        {error, Reason} -> {error, Reason}
    end;

search_faces(_,N_faces,Faces_list) when N_faces =:= 0 ->
    {all_found,Faces_list}.

search_faces_list(File) ->
    case file:read_line(File) of
        {ok, Line} ->
            Words = string:tokens(Line, " "),
            [ _ | Faces] = Words,
            [X_ | XY] = Faces,
            X = [list_to_integer(X_)],
            [Y_ | Z_] = XY,
            Y = [list_to_integer(Y_)],
            Z = [list_to_integer(Z_)],
            {ok,X++Y++Z};
        eof -> {eof_found, "eof found.\n"};
        {error, Reason} -> {error, Reason}
    end.