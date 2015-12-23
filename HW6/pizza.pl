:- use_module(library(lists)).
:- use_module(library(clpfd)).

n_pizzas(4).
pizza(1,10).
pizza(2,5).
pizza(3,20).
pizza(4,15).
n_vouchers(2).
voucher(1,1,1).
voucher(2,2,1).

cost :-
	l_pizzas(P),
	gen_vouchers(V),
	get_cost_list(P, V, L),
	min_member(K, L),
	write('cost('),
	write(K),
	write(')'),
	!.

%%%%%%%%%%%% helper functions %%%%%%%%%%%%


% make list of pizzas in sorted desc order
l_pizzas(L) :-
	findall(X, pizza(_,X), L1),
	my_sort(L1, L).

my_sort([], []) :-
	!.
my_sort(L, [H|T]) :-
	max_member(H, L),
	remove(L, H, L1),
	my_sort(L1, T).

remove([H|T], H, T) :-
	!.
remove([H|T], X, [H|T2]) :-
	remove(T, X, T2).
	
% generate all permutations of voucher indexes
gen_vouchers(L) :-
	n_vouchers(M),
	range(1, M, R),
	findall(X, permutation(R,X), L).

range(X, Y, []) :-
	X > Y,
	!.
range(X, Y, [X|T]) :-
	X1 #= X + 1,
	range(X1, Y, T).

% get_cost_list for all permutations of vouchers
get_cost_list(_, [], []) :-
	!.
get_cost_list(P, [V|Rest], [H|T]) :-
	get_pizza(P, V, H),
	get_cost_list(P, Rest, T).

% get cost of buying pizza with one sequence
% of vouchers
get_pizza([], _, 0) :-
	!.
get_pizza([H|T], [], C) :-
	get_pizza(T, [], C1),
	C #= C1 + H,
	!.
get_pizza(P, [Hv|Tv], C) :-
	voucher(Hv, B, F),
	get_first_B_pizzas(P, Rest_pizza1, B, C1),
	get_free_F_pizzas(Rest_pizza1, Rest_pizza2, F),
	get_pizza(Rest_pizza2, Tv, C2),
	C is C1 + C2.

get_first_B_pizzas([], [], _, 0) :-
	!.
get_first_B_pizzas(Tp, Tp, 0, 0) :-
	!.
get_first_B_pizzas([Hp|Tp], Rest, B, C) :-
	B1 #= B - 1,
	get_first_B_pizzas(Tp, Rest, B1, C1),
	C #= Hp + C1.

get_free_F_pizzas([], [], _) :-
	!.
get_free_F_pizzas(Tp, Tp, 0) :-
	!.
get_free_F_pizzas([_|Tp], Rest, F) :-
	F1 #= F - 1,
	get_free_F_pizzas(Tp, Rest, F1).