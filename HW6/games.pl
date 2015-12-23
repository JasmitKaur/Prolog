:- use_module(library(lists)).
:- use_module(library(clpfd)).

num(4).
cap(5).
refill(2).
fun(1,4).
fun(2,1).
fun(3,2).
fun(4,3).

total_fun :-
    cap(Cap),
    findall(Fun, fun(1, Cap, Fun), List),
    max_member(Max, List),
    output(Max).

%%%%%%%%%%% helper functions %%%%%%%%%%%%%%%

fun(N, _, 0) :-
    num(Last),
    N > Last,
    !.

fun(N, Amount, Fun) :-
    fun(N, F),
    (   
    	F > 0
      	->
    		between(1, Amount, I),
      		Fun1 #= I*F,
      		A4 #= Amount - I,
      		refill(A4, A2)
      	;
      		Fun1 #= F,
      		A3 #= Amount - 1,
      		refill(A3, A2)
    ),
    N1 #= N+1,
    fun(N1, A2, Fun2),
    Fun #= Fun1 + Fun2.

refill(A, A2) :-
    refill(R),
    A3 #= A + R,
    cap(C),
    (
    	A3 < C
        ->  
    		A2 #= A3
        ;
        	A2 #= C
    ).

output(Max) :-
    write(Max).

%ref : class notes
