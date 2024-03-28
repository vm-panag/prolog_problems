%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Vassilis Panagakis         %
% vertexcover.pl             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- lib(ic).
:- lib(branch_and_bound).

/* uncomment following line in case you haven't compiled graph.pl file */

/* :- compile(graph). */ 

/*** predicate that finds the optimal vertex cover of a graph ***/

vertexcover(NNodes, Density, Cover) :-
   create_graph(NNodes, Density, Graph),   %create a random graph
   def_vars(NNodes, Nodes),     
   state_constrs(Nodes, Graph), 
   Cost #= sum(Nodes),     
   bb_min(search(Nodes, 0, input_order, indomain, complete, []), Cost, _),
   findall(X, indexOf(Nodes, 1, X), Cover).

/* predicate where variables and their domains are defined */

def_vars(NNodes, Nodes) :-
   length(Nodes, NNodes),
   Nodes #:: [0,1].

/* predicate where constraints are defined */

state_constrs(_, []).
state_constrs(Nodes, [N1 - N2 | Graph]) :-
   n_th(N1, Nodes, Node1),
   n_th(N2, Nodes, Node2),
   Node1 + Node2 #> 0,      
   state_constrs(Nodes, Graph).

/* predicate that returns the domain value of the n_th element in the Graph list */

n_th(1, [Node | _], Node).
n_th(N, [_ | Nodes], Node) :-
   N \= 1,
   N1 is N - 1,
   n_th(N1, Nodes, Node).

/* predicate that returns an element from a list based on a specific index (indexing starts at 0) */

indexOf([Element | _], Element, 1).
indexOf([_ | L], Element, Index):-
	indexOf(L, Element, Index1), 
	Index is Index1 + 1.
	

% predicate that creates a graph of "NNodes" nodes and "Density" density

create_graph(NNodes, Density, Graph) :-
   cr_gr(1, 2, NNodes, Density, [], Graph).


cr_gr(NNodes, _, NNodes, _, Graph, Graph).
cr_gr(N1, N2, NNodes, Density, SoFarGraph, Graph) :-
   N1 < NNodes,
   N2 > NNodes,
   NN1 is N1 + 1,
   NN2 is NN1 + 1,
   cr_gr(NN1, NN2, NNodes, Density, SoFarGraph, Graph).
cr_gr(N1, N2, NNodes, Density, SoFarGraph, Graph) :-
   N1 < NNodes,
   N2 =< NNodes,
   rand(1, 100, Rand),
   (Rand =< Density ->
      append(SoFarGraph, [N1 - N2], NewSoFarGraph) ;
      NewSoFarGraph = SoFarGraph),
   NN2 is N2 + 1,
   cr_gr(N1, NN2, NNodes, Density, NewSoFarGraph, Graph).

rand(N1, N2, R) :-
   random(R1),
   R is R1 mod (N2 - N1 + 1) + N1.
