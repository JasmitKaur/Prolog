% packing

r(1).
s(0).
l(1).
t(2).

packing :-
	r(R),
	s(S),
	l(L),
	t(T),
	check(R, S, L, T).

%%%%%%%% helper functions %%%%%%%%%%

%sum should be equal to 4
check(R, S, L, T) :-
	A is R + S + L + T,
	A \= 4,
	write('no'),
	!.

% none of them can be equal to 3
check(R, S, L, T) :-
	(
		(R == 3;
		S == 3;
		L == 3;
		T == 3)
	->
		write('no')
	),
	!.

% if T is 2, then none other can be 2
check(R, S, L, T) :-
	(
		T == 2
	->
		(
			(R == 2;
			S == 2;
			L == 2)
		->
			write('no')
		)
	),
	!.
	
% all can not be equal to 1
check(R, S, L, T) :-
	R == 1,
	S == 1,
	L == 1,
	T == 1,
	write('no'),
	!.

% if r ans s is 1 then l should be 2
check(R, S, L, _) :-
	(
		(R == 1,
		S == 1)
	->
		(
			L \= 2
		->
			write('no')
		)
	),
	!.
	
% if r and l are 1 then t should be 2
check(R, _, L, T) :-
	(
		(R == 1,
		L == 1)
	->
		(
			T \= 2
		->
			write('no')
		)
	),
	!.

check(_, _, _, _) :-
	write('yes').