:- use_module(library(lists)).
:- use_module(library(clpfd)).
:- use_module(library(sort)).

graph_size(6).
start(1).
dest(6).
edge(1,2,4).
edge(1,3,2).
edge(2,3,5).
edge(2,4,10).
edge(3,5,3).
edge(4,5,4).
edge(4,6,11).

min_cost :-
	get_start_and_dest(Sd),
	get_intermediate_vertices(Sd, I),
	power_set(I, P),
	possible_subgraphs(Sd, P, G),
	set_of_egdes(E),
	min_cost_list(G, E, L),
	min_member(K, L),
	write('min_cost('),
	write(K),
	write(')'),
	!.

%%%%%%% helper functions %%%%%%%%%
get_start_and_dest([S|D]) :-
	start(S),
	findall(X, dest(X), D).

get_intermediate_vertices(Sd, L) :-
	graph_size(N),
	range(1, N, R),
	remove(R, Sd, L).

range(X, Y, []) :-
	X > Y,
	!.
range(X, Y, [X|T]) :-
	X1 #= X + 1,
	range(X1, Y, T).

remove(L, [], L) :-
	!.
remove(L1, [Hr|Tr], R) :-
	delete(L1, Hr, L2),
	remove(L2, Tr, R).

power_set(S, P) :-
	bagof(X, subseq(S, X), P).

subseq([], []).
subseq([H|T1], [H|T2]) :-
	subseq(T1, T2).
subseq([_|T1], T2) :-
	subseq(T1, T2).

possible_subgraphs(_, [], []) :-
	!.
possible_subgraphs(Sd, [Hp|Tp], [H|T]) :-
	append(Sd, Hp, H),
	possible_subgraphs(Sd, Tp, T).

set_of_egdes(E) :-
	findall(edge(X,Y,Z), edge(X,Y,Z), E).

min_cost_list([], _, []) :-
	!.
min_cost_list([Hg|Tg], E, [H|T]) :-
	delete_edges(E, Hg, E1),
	mst(graph(Hg, E1), _, H),
	!,
	min_cost_list(Tg, E, T).
min_cost_list([_|Tg], E, [H|T]) :-
	max_edge_cost(E, H),
	min_cost_list(Tg, E, T).

max_edge_cost([], 0).
max_edge_cost([edge(_,_,V)|T], C) :-
	max_edge_cost(T, C1),
	C #= C1 + V.
	
delete_edges([edge(X,Y,V)|Ti], Hg, [edge(X,Y,V)|To]) :-
	memberchk(X, Hg),
	memberchk(Y, Hg),
	!,
	delete_edges(Ti, Hg, To).
delete_edges([edge(_,_,_)|Ti], Hg, To) :-
	delete_edges(Ti, Hg, To),
	!.
delete_edges([], _, []) :-
	!.

% minimum spanning tree
%* ref: http://www.ic.unicamp.br/~meidanis/courses/problemas-prolog/p84.prolog *%
mst(graph([N|Ns],GraphEdges), graph([N|Ns],TreeEdges), Sum) :- 
   predsort(compare_edge_values, GraphEdges, GraphEdgesSorted),
   transfer(Ns, GraphEdgesSorted, TreeEdgesUnsorted),
   sort(TreeEdgesUnsorted, TreeEdges),
   edge_cost_sum(TreeEdges, Sum),
   !.

compare_edge_values(Order, edge(X1,Y1,V1), edge(X2,Y2,V2)) :- 
	compare(Order, V1+X1+Y1, V2+X2+Y2).

edge_cost_sum([], 0).
edge_cost_sum([edge(_,_,V)|Es], S) :-
	edge_cost_sum(Es, S1),
	S #= S1 + V.

transfer([], _, []) :-
	!.
transfer(Ns, GEs, [GE|TEs]) :- 
   select(GE, GEs, GEs1),
   incident(GE, X, Y),
   acceptable(X, Y, Ns),
   delete(Ns, X, Ns1),
   delete(Ns1, Y, Ns2),
   transfer(Ns2, GEs1, TEs).

incident(edge(X,Y), X, Y).
incident(edge(X,Y,_), X, Y).

acceptable(X, Y, Ns) :-
	memberchk(X, Ns),
	\+ memberchk(Y,Ns),
	!.
acceptable(X, Y, Ns) :-
	memberchk(Y, Ns),
	\+ memberchk(X, Ns).