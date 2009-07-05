%% Author: saket kunwar
%% Created: Feb 18, 2009
%% Description: TODO: Add description to vertex_server
-module(vertex_server).
-import(dijk,[rpc/2]).
%%
%% Include files
%%

%%
%% Exported Functions
-export([handle/2,add/1,bulkadd/1,init/0]).

%%
%% API Functions
%%

init()->
    	[].

add(V)->rpc(vertex_server,{addvertex,V}).
bulkadd([H|T])->
                add(H),
                bulkadd(T);
bulkadd([])->
    		ok.

%%
%% Local Functions
%%

handle({addvertex,V},L)->
    			S=spawn (fun()->dijk:loopvertex(V) end),
    			{L,lists:append([{V,S}],L)}.
