:- use_module(library(chr)).

constr(nat, 0).
constr(zero, 0).
constr(nat, s(N)) :- constr(nat, N).
constr(nonzero, s(N)) :- constr(nat, N).
constr(nat, add(N,M)) :- constr(nat, N), const(nat, M).
constr(nonzero, add(N,_)) :- constr(nonzero, N).
constr(nonzero, add(_,M)) :- constr(nonzero, M).
constr(P, if(_, Then, Else)) :- constr(P, Then), constr(P, Else).
constr(P, if(Cond, Then, _)) :- constr(nonzero, Cond), constr(P, Then).
constr(P, if(Cond, _, Else)) :- constr(zero, Cond), constr(P, Else). 

:- chr_constraint c/1.

c(b) ==> c(x).

% :- c(X).
%@ c(X).

% :- constr(P, if(s(0), s(0), 0)).
%@ P = nat ;
%@ P = nat ;
%@ P = nonzero ;
%@ false.
%@ false.