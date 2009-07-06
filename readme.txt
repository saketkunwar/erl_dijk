# Copyright 2009 saket kunwar
# 
#    Licensed under the Apache License, Version 2.0 (the "License");
#    you may not use this file except in compliance with the License.
#    You may obtain a copy of the License at
# 
#        http://www.apache.org/licenses/LICENSE-2.0
# 
#    Unless required by applicable law or agreed to in writing, software
#    distributed under the License is distributed on an "AS IS" BASIS,
#    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#    See the License for the specific language governing permissions and
#    limitations under the License.



This is a depth first graph traversal and shortest path search implentation based on dijkstra's algorithm.The nodes were created using erlangs digraph but edges in both direction were created
to make it undirectional or bi-directional.There is an API for shortest path search between two vertex and between all the vertex of the graph.
The code provides hard coded examples of digraph creation and shortest path search.
THough there is also a implementation of a transactional graph server and vertex_server so that 
vertex can been added without the destruction of the graph state and recompilation.
i.e new vertex and edges can be added to the graph from the erlang shell with the graph_server
%%usage example
dijk:start().
ListofVertex=graph_server:make_graph().  %%initialize some graph
{Source,[]}=graph_server:getvertex(N,ListofVertex).  %%where N is the vertex num
{Dest,[]}=graph_server:getvertex(N,ListofVertex).    %%where N is the vertex num
vertex_server:add(Vertex).
V1=graph_server:addvertex().
V2=graph_server:addvertex().
graph_server:createedge(V1,V2,2).
graph_server:traverse(Source,Dest).


Each vertex also spawns it's own process.THere is a functionality for forwarding  a message from one
vertex process to another i.e from source vertex to destination vertex on the shortest path

install
--------
goto source directory src
and type
make
all bin files will be stored in directory ebin


To-do
need to get the vertex label for easier use from the shell


Author saket kunwar
email me for comments: saketkunwar2005@gmail.com
Feb 17,2009