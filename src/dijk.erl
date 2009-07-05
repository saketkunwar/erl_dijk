%% Author: saket kunwar
%% Created: Feb 16, 2009
%% Description: TODO: Add description to graph
-module(dijk).
-import(digraph,[add_vertex/1,add_edge/4]).
%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([init_example/0,loopvertex/1,init_example2/0,trav/9,get_edge_weight/2,count/2,current/3,start/0,rpc/2,traverse/3,traverse_all/2]).

%%
%% API Functions
%%

%%i.e start(graph_server,graph_server)		
start()->
		Mod=Name=graph_server,
		register(Name,spawn (fun()->loopgraph(Name,Mod,Mod:init()) end)),
        ModV=NameV=vertex_server,
        %% cheak the loopvertice vs loopvertex loopvertex for just one and loopvertice for list of vertex????
        register(NameV,spawn(fun()->loopvertice(NameV,ModV,ModV:init()) end)).

rpc(Name,Request)->	
		Name ! {self(),Request},
		receive
			{Name,Response}->Response
		end.


loopgraph(Name,Mod,OldState)->
	receive
		%%example  A=add_vertex(G)
		
		
		{From,Request}->
			try Mod:handle(Request,OldState) of
				{Response,NewState}->
				From ! {Name,Response},
				loopgraph(Name,Mod,NewState)
			catch
				_:Why ->
					log_error(Name,Request,Why),
					From ! {Name,crash},
					loopgraph(Name,Mod,OldState)
			end
	end.

log_error(Name,Request,Why)->
				io:format("~p request ~p ~n caused exception ~p~n",[Name,Request,Why]).
	


vertices_init([H|T],List)->
	S=spawn (fun()->loopvertex(H) end),
	vertices_init(T,[{H,S}|List]);

%%get loopvertices to send paa_message
vertices_init([],L)->
		L.
	
loopvertex(V)->
	receive
		{message,M}->
			io:format("received ~p~n",[M]),
			loopvertex(V);
		{stream,[H|T],M}->
			io:format("~p received message--~p and forward to next node ~n",[V,M]),
			H! {stream,T,"hello node"},
			loopvertex(V);
		{stream,[],M}->
			io:format("~p received message---~p--end of list ~n",[V,M]),
			loopvertex(V);
		Any->
			io:format("received ~p~n",[Any]),
			loopvertex(V)
	end.
	
verticeservice(L)->
		receive
			{pass_message,V}->  %%V is short path
				current(L,lists:reverse(V),[]),
				verticeservice(L);
			Any->
				io:format("Any received ~p~n",[Any]),
				verticeservice(L)
		end.
loopvertice(Name,Mod,OldState)->
	receive
	
        {pass_message,V}->  %%V is short path
				current(OldState,lists:reverse(V),[]),
				loopvertice(Name,Mod,OldState);
		
		{From,Request}->
			try Mod:handle(Request,OldState) of
				{Response,NewState}->
				From ! {Name,Response},
				loopvertice(Name,Mod,NewState)
			catch
				_:Why ->
					log_error(Name,Request,Why),
					From ! {Name,crash},
					loopvertice(Name,Mod,OldState)
			end
	end.

current(L,[H|T],Pidlist)->
			{value,{_,Pid}}=lists:keysearch(H,1,L),
			current(L,T,[Pid|Pidlist]);
current(_,[],[H|T])->
			H!{stream,T,"hello node"}.
			

%%need to define both in and out vertex since this is a undirectional grap constructed from digraph
create_edge(Graph,Vertex1,Vertex2,Weight)->
    		{add_edge(Graph,Vertex1,Vertex2,[Weight]),add_edge(Graph,Vertex2,Vertex1,[Weight])}.
            

out_neighbours_edges(G,From)->
    {digraph:out_neighbours(G,From),digraph:out_edges(G,From)}.

get_edge_weight([H|T],Weights)->
    {_,_,_,[Edgeweight]}=H,
	get_edge_weight([X||X<-T],[Edgeweight|Weights]);
get_edge_weight([],Weights)->
    Weights.

%%has_unvisited_neighbour(Neighbor,Visit,Expression),
has_unvisited_neighbour([H|T],Visit,Ex)->
		Bool=lists:member(H,Visit),
		has_unvisited_neighbour(T,Visit,Bool and Ex); %% or B=fun(L)->fun(X)->lists:member(X,L) end) end, Qx=B(L), Qx(H).
has_unvisited_neighbour([],_,Ex)->
		Ex.

compare_edges(G,Eh,[EdgesH|EdgesT],_)->
	[Wh|_]=get_edge_weight([digraph:edge(G,Eh)],[]),
	[Wh2|_]=get_edge_weight([digraph:edge(G,EdgesH)],[]),
	if 
		(Wh<Wh2)->
			Elow=Eh,
			Wlow=Wh;
		(Wh2<Wh)->
			Elow=EdgesH,
			Wlow=Wh2;
		(Wh==Wh2)->
			Elow=Eh,
			Wlow=Wh
	end,
	compare_edges(G,Elow,[X||X<-EdgesT],Wlow);

compare_edges(_,Elow,[],Wlow)->
	{Elow,Wlow}.

%% there is no direct function to get edge from vertice to vertice in digraph so i cooked this crude stuff 	
%% works by finding the common edge of all 'out' vertices from source to all 'in' vertice of dest	
edgeVtoV(G,Vertice_from,Vertice_to)->
	comp(digraph:out_edges(G,Vertice_from),digraph:in_edges(G,Vertice_to)).
comp([Eh|Et],E2)->
	Bool=fun(Y)->(fun(X)->lists:member(X,Y) end) end,
	Ismem=Bool(E2),
	Exp=Ismem(Eh),
	if 
		(Exp==true)->
			comp([],Eh);
		true->
			comp(Et,E2)
	end;
comp([],Edge)->
		Edge.

count([_|T],C)->
	count(T,C+1);
count([],C)->
	C.
	
%%
%% Local Functions
%%
traverse(G,[Source],[Hd|Td])->
	Sp=trav(G,[Source],[Hd],[],[Source],[],[],[],[Source]),
	traverse(G,Sp,Td);
traverse(_,Sp,[])->
			Sp.
traverse_all(G,L=[H|_])->
		lists:map(fun(_)->tr_all(G,[H],L--[H])end,L).
tr_all(G,[He|Te],[S|D])->
		traverse(G,[He],[S]),
		tr_all(G,[He|Te],D);
tr_all(_,_,[])->
		ok.		



trav(G,[Sh|_],[Th|Tt],Queue,Visited,Prev,TravelledEdge,Deadend,ShortestPath)->
	
	if        
		(Sh==Th)->
            	
			Sp=lists:reverse(ShortestPath),
			io:format("target found ---from source ~p to target ~p~n",[lists:nth(1,Sp),Sh]),
			shortest_path(G,Sp),
			Sp;			
		true->
      		{N,E}=out_neighbours_edges(G,Sh),
			[Nh|_]=N,
			[Eh|_]=E,
	
			Max=count(N,0),
			if 
			(Max==1)->   %%condition if num of neigbour is only 1 then it is a dead end
				
				{_,V11,V22,_}=digraph:edge(G,Eh),
				DeadendEdge=edgeVtoV(G,V22,V11),
				%%OppDeadendEdge=edgeVtoV(G,V11,V22),

				%%condition add for dead start
				Qu=count(Queue,From=0),
				Vv=count([Prev],From=0),
				if 
					((Qu==0) and (Vv==1))->
						%%dead reckoning
						trav(G,[V22],[Th|Tt],Queue,[Nh|Visited],Sh,lists:append([DeadendEdge],[DeadendEdge|TravelledEdge]),[Sh|Deadend],[Nh|ShortestPath]);
					true->
						%%this E ere is dead end edge not dead end get the reverse of E no need to add to visited since it already there
						trav(G,[V22],[Th|Tt],Queue,Visited,Sh,lists:append([DeadendEdge],[DeadendEdge|TravelledEdge]),[Sh|Deadend],lists:subtract(ShortestPath,[V11]))  
					end;
			true->
				Re=lists:subtract(E,TravelledEdge),  
				Alltravelled=count(Re,0),
				if
					(Alltravelled==0)->
					[Backto|_]=ShortestPath,
					{Nb,_}=out_neighbours_edges(G,Backto),
					%%use fun of fun here
					Expr=has_unvisited_neighbour(Nb,Visited,true),
					if 
						(Expr==false)->
							Shortlist=ShortestPath;
						true->
							Shortlist=lists:subtract(ShortestPath,[Backto])
					end,
					trav(G,[Backto],[Th|Tt],[Prev],Visited,[Sh],TravelledEdge,Deadend,Shortlist); %%need to fix the queue here
		
				true->
					[Eeh|Eet]=Re,
					{Elow,_}=compare_edges(G,Eeh,Eet,[]),
					{_,V111,V222,_}=digraph:edge(G,Elow),
					OppEdge=edgeVtoV(G,V222,V111),
					ThisEdge=edgeVtoV(G,V111,V222),
					{_,_,V2,_}=digraph:edge(G,Elow),
					trav(G,[],[Th|Tt],[V2],Visited,Prev,lists:append([ThisEdge],[OppEdge|TravelledEdge]),Deadend,ShortestPath)  %%or travelled is Elow
				end
			end
		end;
trav(G,[],[Th|Tt],[Qh|Qt],Visited,_,TravelledEdge,Deadend,ShortestPath)->
	[Vh|_]=Visited,
	trav(G,[Qh],[Th|Tt],[Qh|Qt],[Qh|Visited],Vh,TravelledEdge,Deadend,[Qh|ShortestPath]). %%change vh Here prev

%%shortest_path(G,List_of_Path,[])
%%fun of fun here
shortest_path(G,[Vh|Vt])->
		pathcalc(G,[Vh|Vt],Vt,[]).
pathcalc(G,[Vh|Vt],[Vht|Vtt],W)->
		[Wh|_]=get_edge_weight([digraph:edge(G,edgeVtoV(G,Vh,Vht))],[]),
		pathcalc(G,Vt,Vtt,[Wh|W]);
pathcalc(_,_,[],W)->
		io:format("shortest path with total weight= ~p~n",[lists:sum(W)]).

%%hard coded example
init_example()->
    G=digraph:new(),
   
    A=add_vertex(G),
    B=add_vertex(G),
    C=add_vertex(G),
    D=add_vertex(G),
    E=add_vertex(G),
    Z=add_vertex(G),
  
    create_edge(G,A,B,1),
    create_edge(G,A,C,2),
    create_edge(G,B,D,3),
    create_edge(G,D,Z,4),
    create_edge(G,C,E,5),
    create_edge(G,B,E,6),
    create_edge(G,C,D,7),
    create_edge(G,E,Z,8),
    create_edge(G,B,C,9),
    create_edge(G,D,E,10),
    
   
    %%register(service,spawn (fun()->verticeservice(vertices_init(digraph:vertices(G),[])) end)),
    traverse(G,[A],[Z]),
    %%unregister(service),
    digraph:info(G).

init_example2()->
		
		G=digraph:new(),
		A=add_vertex(G),
		B=add_vertex(G),
		C=add_vertex(G),
		D=add_vertex(G),
		E=add_vertex(G),
		I=add_vertex(G),
		L=add_vertex(G),
		H=add_vertex(G),
		Q=add_vertex(G),
		F=add_vertex(G),
		K=add_vertex(G),
		Z=add_vertex(G),

		create_edge(G,A,B,2),
		create_edge(G,A,C,1),
		create_edge(G,B,D,1),
		create_edge(G,B,E,3),
		create_edge(G,E,I,1),
		create_edge(G,I,L,4),
		create_edge(G,I,H,1),
		create_edge(G,Q,H,1),
		create_edge(G,E,F,2),
		create_edge(G,F,K,2),
		create_edge(G,F,Z,2),
		
		
		register(vertex_server,spawn (fun()->verticeservice(vertices_init(digraph:vertices(G),[])) end)),
    	digraph:info(G),
		Sp=traverse(G,[D],[Z]),
		%%traverse_all(G,digraph:vertices(G)),
        io:format("shortest path ~p~n",[Sp]),
        %%pass message
        Cond=whereis(vertex_server),
			if 
				(Cond==undefined)->
					ok;
				true->
					vertex_server! {pass_message,Sp}
				end,
        unregister(vertex_server).
		

		


	

