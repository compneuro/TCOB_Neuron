:- use_module(library(clpr)).
:- use_module(library('plot/plotter')).
:- use_module(library(autowin)).
%conditional_constraint(A,_) :- call(A),!.
%conditional_constraint(_,B):- \+(call(B)).

conditional_constraint(A, B) :-!, when(ground(B), conditional_constraint1(A,B)).
conditional_constraint1(A,B) :- call(B), !, call(A).
conditional_constraint1(_, _) :- !.

random_val(Min,Max,A) :- random(Min,Max,A).
random_val(Min,Max,A) :- random_val(Min,Max,A). 

index(I,L,X):- ground(I),J is round(I), nth1(J,L,X).
index(0,_,0).
index(_,_,_).

index([X|_], I, I, X) :- !.
index([_|T], I, N, X) :-
   J is I + 1, J=<N,
   index(T, J, N, X).

output([]).
output([X|T]) :-
   print(X), nl, output(T).

makearray([N], V) :- !,
   sizeof(V, N).
makearray([N|Rest], V) :-
   !, sizeof(V, N), makearrayofeach(Rest,V).
makearray(_,_). % when array dimensions are left unspecified.

makearrayofeach(NL,[V]) :-
   makearray(NL, V), !.
makearrayofeach(L, [V1|Rest]) :-
   makearray(L, V1), makearrayofeach(L,Rest).

sizeof([], 0) :- !.
sizeof([_|T], M) :-
   N is M-1,
   sizeof(T, N).

min(X,Y,Z) :- ((nonvar(X), nonvar(Y)) -> (Z is min(X,Y))
				      ; (nonvar(X) -> Z=X
				                   ; (nonvar(Y) -> Z=Y
						                 ;true))).

max(X,Y,Z) :- ((nonvar(X), nonvar(Y)) -> (Z is max(X,Y))
				      ; (nonvar(X) -> Z=X
					            ; (nonvar(Y) -> Z=Y
						                 ; true))).


makelistfromto(M, M, [M]):-!.
makelistfromto(N, M, []):- M<N. 
makelistfromto(M, N, [M]):- I is round(M),J is round(N),I=J,!. 
makelistfromto(N, M, [N|NtoM]) :-
   N < M,
   N1 is N+1,
   makelistfromto(N1, M, NtoM),!.

naf(B) :-
   call(B), !, fail.
naf(_).

r2i(R,I) :-  I is round(R).
% ------------------------------------------------------

removevar([], []).
removevar([X|Xs], Z) :-
   var(X), removevar(Xs, Z), !.
removevar([X|Xs], [X|Zs]) :-
   removevar(Xs,Zs).

removevar([], 0).
removevar(X,[]) :- var(X).
removevar(X,[X]) .

removevar1([], []).
removevar1([X|Xs], ['NaV'|Zs]) :-
   var(X), removevar1(Xs, Zs), !.
removevar1([X|Xs], [X|Zs]) :-
   removevar1(Xs,Zs).

removevar1([], 0).
removevar1(X,['NaV']) :- var(X).
removevar1(X,[X]) .

plot_graph(Title,L,XLo,XHi,YLo,YHi,Width,Height,Spacing,TG) :-
   removevar(L,LN),
   new(W, auto_sized_picture(Title)),
   send(W, display, new(P, plotter)),
   send(P, axis, new(X, plot_axis(x, XLo, XHi, Spacing, Width))),
   send(P, axis, plot_axis(y, YLo, YHi, @default, Height)),
   send(X, format, '%i'),
   send(P, graph, new(G, plot_graph)),
   plot_function(LN, 1, G,TG),
   send(W, open).

plot_function([], _,_,_) :-
        !.
plot_function([Y|T], X, G,TG) :-
   send(G, append, X, Y),
   X2 is X+TG,
   plot_function(T, X2, G,TG).

dump_to_file([],[]).
dump_to_file(N,V) :-
   open('output.csv',append,Stream),write_to_file(Stream,N,V),
   close(Stream).

write_to_file(_,[],_).
write_to_file(_,[[]],_).
write_to_file(Stream,[Name|Tail],[X|T]) :-
   get_val(Name,NN),write(Stream,NN),write(Stream,','),get_val(X,XN),
   removevar1(XN,NX),dumpval(Stream,NX),put(Stream,10),
   write_to_file(Stream,Tail,T).

get_val([[X|T]],[X|T]).
get_val(X,X).

dumpval(_,[]).
dumpval(_,[T]):- T='NaV'.
dumpval(Stream,[T]):-  write(Stream,T).
dumpval(Stream,[X|T]) :-
   write(Stream,X),write(Stream,','),dumpval(Stream,T).

addval(H,V) :-
   var(H),{H=V}.
addtoarray([H|_] , V ):-
   addval(H,V).
addtoarray([_|T],V ) :-
   addtoarray(T,V).

%logvar(T,N,V) :-
%   
%   write('Time = '),write(T),
%   findall(P,obj(P),L),
%   write(':Obj = '),write(L),
%   write(':Var = '),write(N),
%   write(':Val = '),
%   (ground(V)->write(V);write('NaV'),
 %   when(ground(V),logvar1(T,L,N,V))),nl.
%logvar1(T,L,N,V) :-
%   write('Time = '),write(T),
%   write(':Obj = '),write(L),
%   write(':Var = '),write(N),
%   write(':Val = '),write(V),nl.
writeobj([X]) :- write(X).
writeobj([]):- !.
writeobj([X|T]) :-!,
    write(X),write('.'),writeobj(T).

logvar(T,N,V,Obj) :-
   open('log.txt',append,St),
   tell(St),
   write('Time = '),write(St,T),
  % findall(P,obj(P),L),
   write(': Obj = '),write(Obj),
   write(': Var = '),write(N),
   write(': Val = '),
   (ground(V)->write(V);write('NaV'),!,
    when(ground(V),logvar1(T,L,N,V,Obj))),nl,
    tell(user_output),(is_stream(St)-> close(St);true),!.
logvar1(T,L,N,V,Obj) :-
   open('log.txt',append,St),tell(St),
   write('Time = '),write(T),
   write(': Obj = '),write(Obj),
   write(': Var = '),write(N),
   write(': Val = '),write(V),nl,tell(user_output),(is_stream(St)-> close(St);true),!.

getname(Va, In,Out):- !,string_concat(Va,'[',I1),string_concat(I1, In,I2),string_concat(I2,']',Out).
