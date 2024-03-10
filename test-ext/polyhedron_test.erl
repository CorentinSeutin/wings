-module(polyhedron_test).

%-compile({parse_transform, tools}).
-include_lib("eunit/include/eunit.hrl").
%-include_lib("../src/wings.hrl").
-export([go/0]).
-import(wpc_polyhedron, [compare_min_max/2]).

go() -> start().

start() ->
    ?assert(
        compare_min_max_test()
    ).

compare_min_max_test() ->
    ?assert(
        ( 
            compare_min_max_one_test() andalso
            compare_min_max_two_test() andalso
            compare_min_max_three_test() 
        ) =:= ok
    ).

%%%%%%%%%%%%%%%%%%%compare_min_max tests%%%%%%%%%%%%%%%%%%%
%Max should be replaced
compare_min_max_one_test() -> 
    ?assert(
        wpc_polyhedron:compare_min_max({1,2,3},[{0,0,0},{0,0,0}]) =:= 
        [{0,0,0},{1,2,3}]
    ).

%Min should be replaced
compare_min_max_two_test() -> 
    ?assert(
        wpc_polyhedron:compare_min_max({1,2,3},[{4,4,4},{4,4,4}]) =:= 
        [{1,2,3},{4,4,4}]
    ).

%Min and Max should not be replaced
compare_min_max_three_test() -> 
    ?assert(
        wpc_polyhedron:compare_min_max({0.5,0.5,0.5},[{0,0,0},{1,1,1}]) =:= 
        [{0,0,0},{1,1,1}]
    ).

%Compilation line : 
    %erl -pa ../plugins_src/primitives/