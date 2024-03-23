%%
%% wings_smooth.erl
%%
%% This module contains the smooth command for edges.
%%

-module(wings_smooth).
-export ([smooth/1]).

-include("wings.hrl").
-import(lists, [foldl/3,mapfoldl/3,reverse/1,sort/1,seq/2]).

%%%
%%% Smooth edges command
%%%

smooth(St0) ->
    io:format(" **** TEST DE PRINT **** "),
    
    St0.
