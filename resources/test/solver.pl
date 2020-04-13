:- use_module(library(chr)).

:- chr_constraint zero/1, not_zero/1, eq/2, print_result.

:- op(500, xfx, eq).

A eq B \ A eq B <=> true.
zero(X) \ zero(X) <=> true.
not_zero(X) \ not_zero(X) <=> true.
X eq X <=> true.

zero(X), not_zero(X) <=> false.

zero(0) <=> true.
zero(s(_)) <=> false.
zero(add(0,0)) <=> true.
zero(add(s(_), _)) <=> false.
zero(add(_, s(_))) <=> false.
zero(add(N,M)) <=> zero(N), zero(M).
not_zero(0) <=> false.
not_zero(s(_)) <=> true.
not_zero(add(0,0)) <=> false.
not_zero(add(s(_), _)) <=> true.
not_zero(add(_, s(_))) <=> true.
not_zero(add(N,M)) <=> not_zero(N), zero(M) ; not_zero(M), zero(N) ; not_zero(N), not_zero(M).



print_result \ zero(X) <=> write(zero(X)), nl.
print_result \ not_zero(Y) <=> write(not_zero(Y)), nl.
print_result \ X eq Y <=> write(X eq Y), nl.

read_constraints :-
	read(X), !,
	eval_constraint(X).

eval_constraint(end_of_file).
eval_constraint(X) :-
	call(X),
	read_constraints.

main :- (read_constraints ; halt(1)), print_result, halt(0).