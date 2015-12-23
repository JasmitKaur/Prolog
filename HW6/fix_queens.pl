:- use_module(library(lists)).
:- use_module(library(clpfd)).

board_size(5).
pos(1,1).
pos(2,5).
pos(3,3).
pos(5,2).
pos(5,5).

moves :-
	input_coordinates(L1),
	output_coordinates(L2),
	min_distance(L1, L2, Min),
	write('moves('),
	write(Min),
	write(')').
	
%%%%%%%% helper functions %%%%%%%%%

% min distance between input and output coordinates
min_distance(I, O, M) :-
	gen_permutations(O, O1),
	distance_list(I, O1, L),
	min_member(M, L).

% generates permutation of output coordinates
gen_permutations([], []).
gen_permutations([H|T], Ts) :-
	findall(X, permute(H, X), Hs),
	gen_permutations(T, T1),
	append(Hs, T1, Ts).

delete1([(X,Y)|T], (X,Y), T).
delete1([(X1,Y1)|T], (X2,Y2), [(X1,Y1)|T2]) :-
	delete1(T, (X2,Y2), T2).

permute([], []).
permute([H|T], L) :-
	permute(T, L2),
	delete1(L, H, L2).

% list of co-ordinate distance
distance_list(_, [], []).
distance_list(I, [H|T], [Hs|Ts]) :-
	distance(I, H, Hs),
	distance_list(I, T, Ts).

distance([], [], 0).
distance([(X1,Y1)|Ti], [(X2,Y2)|To], N) :-
	(
		X1 == X2
	->
		A is abs(Y1 - Y2)
	;
		(
			Y1 == Y2
		->
			A is abs(X1 - X2)
		;
			T1 is X1 - X2,
			T2 is Y1 - Y2,
			(
				T1 == T2
			->
				A is abs(X1 - X2)
			;
				A is abs(X1 - X2) + abs(Y1 - Y2)
			)
		)
	),
	distance(Ti, To, N1),
	N is N1 + A.

input_coordinates(L) :-
	findall((X,Y), pos(X,Y), L).

output_coordinates(L) :-
	solutions(S),
	attach_index(S, L).

attach_index([], []).
attach_index([H|T], [Hs|Ts]) :-
	attach_h(H, Hs),
	attach_index(T, Ts).

attach_h([], []).
attach_h([H|T], [(I,H)|Ts]) :-
	length(T, L1),
	board_size(N),
	I #= N - L1,
	attach_h(T, Ts).

% to find all possible solutions of n-queens
% ref for n-queens : http://swish.swi-prolog.org/example/clpfd_queens.pl
solutions(L)	:-
	findall(X, n_queens(X), L).

n_queens(Qs) :-
	board_size(N),
	length(Qs, N),
	Qs ins 1..N,
	safe_queens(Qs),
    labeling([ff], Qs).

safe_queens([]).
safe_queens([Q|Qs]) :-
	safe_queens(Qs, Q, 1),
	safe_queens(Qs).

safe_queens([], _, _).
safe_queens([Q|Qs], Q0, D0) :-
	Q0 #\= Q,
	abs(Q0 - Q) #\= D0,
	D1 #= D0 + 1,
	safe_queens(Qs, Q0, D1).
