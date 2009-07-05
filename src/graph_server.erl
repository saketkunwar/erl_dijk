%% Author: saket kunwar
%% Created: Feb 16, 2009
%% Description: TODO: get the vertex label so that it's easier to use it from the shell
-module(graph_server).
-import(dijk,[rpc/2]).
%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([init/0,addvertex/0,createedge/3,getvertex/2,allvertices/0,handle/2,traverse/2,make_graph/0,searchall/0]).

%%
%% API Functions


addvertex()->rpc(graph_server,addvertex).
createedge(Vfrom,Vto,Weight)->rpc(graph_server,{create_edge,Vfrom,Vto,Weight}).
getvertex(N,L)->rpc(graph_server,{get_vertex,N,L}).
allvertices()->rpc(graph_server,all_vertices).
traverse(Source,Dest)->rpc(graph_server,{traverse,Source,Dest}).
searchall()->rpc(graph_server,search_all).
%%callback routines
init()->
		digraph:new().

handle(addvertex,Digraph)->
			{digraph:add_vertex(Digraph),Digraph};
handle({create_edge,Vfrom,Vto,Weight},Digraph)->
			{{digraph:add_edge(Digraph,Vfrom,Vto,[Weight]),digraph:add_edge(Digraph,Vto,Vfrom,[Weight])},Digraph};
%%do for list of vertex,
handle({get_vertex,N,L},Digraph)->
			{digraph:vertex(Digraph,lists:nth(N,L)),Digraph};
handle(all_vertices,Digraph)->
			{digraph:vertices(Digraph),Digraph};
handle({traverse,Source,Dest},Digraph)->
			{dijk:traverse(Digraph,Source,Dest),Digraph};
handle(search_all,Digraph)->{dijk:traverse_all(Digraph,digraph:vertices(Digraph)),Digraph}.


%%usage example
%%ListofVertex=graph_server:make_graph()
%%{Source,[]}=graph_server:getvertex(N,ListofVertex)
%%{Dest,[]}=graph_server:getvertex(N,ListofVertex)
%%graph_server:createedge(From,To,Weight)
%%graph_server:traverse([Source],[Dest])

%%create initial graph initialization
make_graph()->
		A=addvertex(),
		B=addvertex(),
		C=addvertex(),
		D=addvertex(),
		E=addvertex(),
		I=addvertex(),
		L=addvertex(),
		H=addvertex(),
		Q=addvertex(),
		F=addvertex(),
		K=addvertex(),
		Z=addvertex(),

		createedge(A,B,2),
		createedge(A,C,1),
		createedge(B,D,1),
		createedge(B,E,3),
		createedge(E,I,1),
		createedge(I,L,4),
		createedge(I,H,1),
		createedge(Q,H,1),
		createedge(E,F,2),
		createedge(F,K,2),
		createedge(F,Z,2),
		
        
		%%searchall(),
        
        vertex_server:bulkadd(allvertices()),
        Sp=traverse([E],[K]),
		io:format("sort path vertices ~p~n",[Sp]),
        Cond=whereis(vertex_server),
			if 
				(Cond==undefined)->
					ok;
				true->
					vertex_server! {pass_message,Sp}
				end,
        %%unregister if the servers have no further use
        unregister(graph_server),
        unregister(vertex_server),
        
        
        ok.
		