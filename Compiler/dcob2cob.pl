%DCOB translator
%Authors: Bharat Jayaraman,Pallavi,Jinesh
:- module(dcob2cob, [dcob2cob/1,tcob2cob/1]).
:- use_module(library(lists)).
tcob2cob(File) :-
   atom_codes(File, FullCodes),
   atom_codes('.tcob',S),
   append(FileCodes, S ,FullCodes),
   atom_codes(Prefix,FileCodes),
   atom_concat(Prefix, '.cob', Output),
   dcob2cob(File,Output).

dcob2cob(File) :-
   atom_codes(File, FullCodes),
   atom_codes('.dcob',S),
   append(FileCodes, S ,FullCodes),
   atom_codes(Prefix,FileCodes),
   atom_concat(Prefix, '.cob', Output),
   dcob2cob(File,Output).

 tcob2cob(File, Output) :-
   open(File,read,Stream),
   process(Stream, Output).
 dcob2cob(File, Output) :-
   open(File,read,Stream),
   process(Stream, Output).


% tokenize, parse tokens to form parse tree,
% translate parse tree and write the translation to file
% along with a clause consulting a file called helper
% The helper file contains definitions of helper predicates
% like sizeof and conditional constraints.
%
%
%
process(Stream, Output) :-
   tell(Output),
   lex(Stream,Tokens), !, %print(Tokens), nl,
   parse(Tokens, ParseTree), !,
  % write('Parse successful. Printing parse tree...'), nl,write(ParseTree),!,nl,
    translatedcob(ParseTree),!,nl,write('$'),
   told.

translatedcob([time(X,Y)|T]):- translatedcobprog(T,T,[X,Y]).
%translate each class(X) of the program
translatedcobprog([X|T],ParseTree,Time) :-
   translatedcobclass(X,ParseTree,Time),!,nl,nl,
   translatedcobprog(T,ParseTree,Time).
translatedcobprog([],_,_).


getclassattr(_,[],[]).
getclassattr(N,[classdef(N,_,Attr,_,_,_,_,_,_,_)|_],Attr).
getclassattr(N,[classdef(abstract(N),_,Attr,_,_,_,_,_,_,_)|_],Attr).

getclassattr(N,[_|T],Attr):- getclassattr(N,T,Attr).

%%%%%%%translatedcobclass
translatedcobclass(classdef(Name,Superclass, Attributes, Constraints, _,
		   Predicates, _, _, Constructors, _),ParseTree,Time) :-
	            writeclass(Name),processuperclass(Superclass),
		    write('{'),nl,processattributes(Attributes),
		    %return mto constraints in Mtoconstraints
		    getclassattr(Superclass,ParseTree,CLB),!,append(Attributes,CLB,Attr),
		    processconstraints(ParseTree,Constraints,Attr,Mtoconstraints,Name,Time),
		    processpredicates(Predicates,Mtoconstraints,ParseTree,Attr),
		    processconstructor(Constructors),nl,write('}').

writeclass(abstract(Name)):- write('abstract class '),write(Name).
writeclass(Name) :- !,write('class '),write(Name).
%%%%Super class handling
processuperclass([]):- !.
processuperclass(""):- !.
processuperclass(Name) :- write(' extends '),!,write(Name).


%%- Attributes translation , processattributes1 for proper comma writing
processattributes([]):- !.
processattributes(X) :- write('attributes'),nl,!, processattributes1(X),nl.
processattributes1([]).
processattributes1([att(Type,var(Var))|T]) :-
		  tab(1),getdecl(Type,Decl),dumpprint(Decl),write(Var),
		  write(';'),nl,!,processattributes1(T).

%get attributes declaration
getdecl(primitive(Type),[Type,' ']).
getdecl(user(Type),[Type,' ']).
getdecl(series(primitive(Type)),[Type, ' []']).
getdecl(array(series(primitive(Type)),1),[Type, ' [][]']).
getdecl(array(series(user(Type)),1),[Type, ' [][]']).
getdecl(array(series(primitive(Type)),Size),[Type, ' [',Size,'][]']).
getdecl(array(series(user(Type)),Size),[Type, ' [',Size,'][]']).


getdecl(series(user(Type)),[Type, ' []']).
getdecl(array(primitive(Type),1),[Type,' []']).
getdecl(array(user(Type),1),[Type,' []']) .
%fix 30/8
getdecl(array(primitive(Type), [Size1,1]),[Type,'  [',Size1,'][]'] ) .
getdecl(array(primitive(Type), [Size1,Size2]),[Type,'  [',Size1,'][',Size2,']'] ) .
getdecl(array(user(Type), [Size1,Size2]),[Type,'  [',Size1,'][',Size2,']'] ) .
getdecl(array(primitive(Type), Size),[Type,' ',Size] ) .
getdecl(array(user(Type),Size),[Type,' ', Size]).

dumpprint([]).
%dumpprint([id(H)|T]) :-  write(H),dumpprint(T).
%fix 24/8
dumpprint([[H]]) :- integer(H),write('['),write(H),write(']').
dumpprint([H|T]) :- ppterm(H,[]),dumpprint(T).

%% Constraint translation
%
processconstraints(_,[],_,_,_,_):-!.

processconstraints(PT,C,A,P,N,T) :- write('constraints'),!,nl,processconstraints1(PT,C,A,P,N,T),nl.
processconstraints1(_,[],_,_,_,_) :-!.
%handle class with series variable add time loop
processconstraints1(ParseTree,Constraints,Attributes,Pred,Name,Time) :-
	           % seriescheck(Attributes),!,
	handletimeloop(ParseTree,Constraints,Attributes,Pred,Name,Time).
processconstraints1(ParseTree,Constraints,Attributes,_,_,_) :- ppconstraintlist(ParseTree,Constraints,Attributes,_),write(';'),nl.

handletimeloop(PT,Constraints,Attributes,Mtopredlist,Name,[X,Y]) :-
	            getstarttime(Y,Name,Start),!,( Start>0 ->  nl,tab(1),write('forall Time in '),write(Start),write('..'),write(X),write(':('),!,nl,tab(1),
		    getmtoconstraints(Constraints,Mtopred),delete(Mtopred,[],Mtopredlist),
		    ppconstraintlist(PT,Constraints,Attributes,Mtopredlist),nl,tab(1),write(');'),nl

						 ;  ppconstraintlist(PT,Constraints,_,_),write(';')).



getstarttime([],_,1).
getstarttime([[Name,Y]|_],Name,Y).
getstarttime([_|T],Name,Y) :- getstarttime(T,Name,Y).
%%%Check for series variable
seriescheck([att(series(_),_)|_]).
seriescheck([_|T]) :- seriescheck(T).
seriescheckvariable([att(series(_),var(V))|_],V).
seriescheckvariable([att(array(series(_),_),var(V))|_],V).

seriescheckvariable([_|T],V) :- seriescheckvariable(T,V).

processpredicates([],[],_,_).
processpredicates([class_predicates(X)],Y,PT,Attr) :-write('predicates'),nl, writepredicates1(X),writemtopredicates(Y,PT,Attr),nl.
processpredicates([],Y,PT,Attr) :-write('predicates'),nl, writemtopredicates(Y,PT,Attr),nl.
writepredicates1([]).
writepredicates1([X|T]) :- dumpprint(X),nl,writepredicates1(T).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
processconstructor([]).
processconstructor(X):- write('constructors'),nl,writeconstructor1(X).
writeconstructor1([]).
writeconstructor1([constructor(Name,AL,C)|T]) :-
	     write(Name),write('('),writeargument(AL),write('){'),
	     ppconstraintlist(_,C,_,_),addsemic(C),write('}'),nl,writeconstructor1(T).
addsemic([]).
addsemic(_):- write(';').

writeargument([]).
writeargument([att(_,var(X))|T]) :- write(X),writeargument2(T).
writeargument2([]).
writeargument2(T):- write(','),writeargument(T).

writeargumentlist([]).
writeargumentlist([X]) :- ppterm(X).
writeargumentlist([X|T]) :- ppterm(X),!,writeargumentlist2(T).
writeargumentlist(X):- ppterm(X).
writeargumentlist2([]).
writeargumentlist2(T):- !,write(','),writeargumentlist(T).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

getmtoconstraints([],_).
getmtoconstraints([X|T],[MtoC1|MtoC2]) :- getmtoconstraints(X,MtoC1),getmtoconstraints(T,MtoC2).
getmtoconstraints(mto('G',X,C),dcobG(N,X,C)) :-dcobgpred(N).
getmtoconstraints(mto('F',X,C),dcobF(N,X,C)) :-dcobfpred(N).
getmtoconstraints(condConstr(Con,_),Pred) :- getmtoconstraints(Con,Pred).
getmtoconstraints(_,[]).

dcobgpred(X) :- gensym(dcobg, X).
dcobfpred(X) :- gensym(dcobf, X).
dcobind(X) :- gensym('Dcobi',X).
dcobseries(X) :- gensym('Dcobser',X).
dcobindex(X) :- gensym('Dcobind',X).
dcobnp(X):- gensym('Dcobnp',X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
getvars(PT,Attr,compare(_,Term1,Term2),[H|T]) :- getvars(PT,Attr,Term1,H),getvars(PT,Attr,Term2,T).
getvars(PT,Attr,add(Term1,Term2),[H,T]) :-  getvars(PT,Attr,Term1,H),getvars(PT,Attr,Term2,T).
getvars(PT,Attr,sub(Term1,Term2),[H,T]) :-  getvars(PT,Attr,Term1,H),getvars(PT,Attr,Term2,T).
getvars(PT,Attr,mult(Term1,Term2),[H,T]) :-  getvars(PT,Attr,Term1,H),getvars(PT,Attr,Term2,T).
getvars(PT,Attr,divide(Term1,Term2),[H,T]) :-  getvars(PT,Attr,Term1,H),getvars(PT,Attr,Term2,T).
getvars(PT,Attr,enclosed(X),L) :- getvars(PT,Attr,X,L).

getvars(_,_,var(X),var(X)).
getvars(_,_,ser(X,Y),ser(X,Y)).
getvars(_,_,ind(X,Y),ind(X,Y)).
getvars(_,_,next(X),next(X)).
getvars(_,_,prev(X),prev(X)).
getvars(PT,Attr,ref(var(X),var(Y)),ref(var(X),ser(Y))):-getobjattr(PT,X,Attr,Attributes),append(Attributes,Attr,NewAttr),seriescheckvariable(NewAttr,Y).
getvars(_,_,ref(var(X),var(Y)),ref(var(X),var(Y))).
getvars(_,_,_,[]):- !.


ppconstraintlist(_,[],[],_). %not correct
ppconstraintlist(PT,[X],Attributes,Mto)   :- tabs(1), ppconstraint(PT,X,Attributes,Mto).
ppconstraintlist(PT,[X|T],Attributes,Mto) :-
		    ppconstraint2(PT,X,Attributes,Mto), !,tabs(1),ppconstraintlist(PT,T,Attributes,Mto).

ppconstraint2(_,compare('=', Term1, Term2),_,_) :-
	Term1 == Term2, !. % do nothing.
ppconstraint2(_,'true',_,_) :- write('true').
ppconstraint2(PT,X,Attr,Mto) :-  ppconstraint(PT,X,Attr,Mto), write(';'), nl.

pppredargument(_,[],_).
pppredargument(PT,[X],A):- ppconstraint(PT,X,A,_).
pppredargument(PT,[X|T],A):- ppconstraint(PT,X,A,_),write(','),pppredargument(PT,T,A).

ppconstraint(PT,compare(R, Term1, Term2),Attr,_) :-
	         ppconstraint(PT,Term1,Attr,_), write('  '), write(R), write('  '),ppconstraint(PT,Term2,Attr,_).
ppconstraint(PT,mult(Term1, Term2),Attr,_) :-
	         ppconstraint(PT,Term1,Attr,_), write('  '), write('*'), write('  '),ppconstraint(PT,Term2,Attr,_).
ppconstraint(PT,add( Term1, Term2),Attr,_) :-
	         ppconstraint(PT,Term1,Attr,_), write('  '), write('+'), write('  '),ppconstraint(PT,Term2,Attr,_).

ppconstraint(PT,sub(Term1, Term2),Attr,_) :-
	         ppconstraint(PT,Term1,Attr,_), write('  '), write('-'), write('  '),ppconstraint(PT,Term2,Attr,_).

ppconstraint(PT,divide(Term1, Term2),Attr,_) :-
	         ppconstraint(PT,Term1,Attr,_), write('  '), write('/'), write('  '),ppconstraint(PT,Term2,Attr,_).
ppconstraint(PT,enclosed(Term1),Attr,_) :-
	write(' ( '), ppconstraint(PT,Term1,Attr,_),write(')').
ppconstraint(PT,negative(Term1),Attr,_) :-
	write(' - '), ppconstraint(PT,Term1,Attr,_).
 ppconstraint(PT,function(Name, X),Attr,_) :-
   !, write(Name),  write('('), ppconstraint(PT,X,Attr,_), write(')').
   ppconstraint(PT,[X],Attr,_) :-  !, ppconstraint(PT,X,Attr,_).
    ppconstraint(PT,[X|T],Attr,_) :-
   !, ppconstraint(PT,X,Attr,_),write(','),ppconstraint(PT,T,Attr,_).

ppconstraint(PT,constraintPred(N,X),A,_) :- write(N),write('('),pppredargument(PT,X,A),write(')').
%ppconstraint(PT,pred(N,X),A,_) :- write(N),write('('),pppredargument(PT,X,A),write(')').
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%Extract mtoconstraints and print predicate name
%ppconstraint(PT,mto('G',time(Start,End),C),A,P) :-
%		getgpredicatename(P,C,N), write(N),
%		write('('),write(Start),write(','),write(End)
%	       ,write(',Time,['),getvars(PT,A,C,Vars),writeargumentlist(Vars),write('])').
%
ppconstraint(PT,mto('G',time(Start,End),C),A,P) :-
		getgpredicatename(P,C,N), write(N),
		write('('),ppconstraint(PT,Start,A,_),write(','),ppconstraint(PT,End,A,_)
	       ,write(',Time,['),getvars(PT,A,C,Vars),writeargumentlist(Vars),write('])').

ppconstraint(PT,mto('F',time(Start,End),C),A,P) :-
	        getfpredicatename(P,C,N),  write(N),
			write('('),ppconstraint(PT,Start,A,_),write(','),ppconstraint(PT,End,A,_)
		,write(',Time,['),getvars(PT,A,C,Vars),writeargumentlist(Vars),write('])').
ppconstraint(PT,mto('G',time(Start),C),A,P) :-
	        getgpredicatename(P,C,N),  write(N), write('('),ppconstraint(PT,Start,A,_),write(',Time1')
	       ,write(',Time,['),getvars(PT,A,C,Vars),writeargumentlist(Vars),write('])').
ppconstraint(PT,mto('F',time(Start),C),A,P) :-
	        getfpredicatename(P,C,N),  write(N), write('('),ppconstraint(PT,Start,A,_)
	       ,write(',Time,['),getvars(PT,A,C,Vars),writeargumentlist(Vars),write('])').

ppconstraint(PT,mto('F',[],C),A,P) :-
	        getfpredicatename(P,C,N),  write(N), write('(1,
	        ,0,[')
	       ,getvars(PT,A,C,Vars),writeargumentlist(Vars),write('])').
ppconstraint(PT,mto('G',[],C),A,P) :-
	        getgpredicatename(P,C,N),  write(N), write('(1,Time1,0,[')
	       ,getvars(PT,A,C,Vars),writeargumentlist(Vars),write('])').
ppconstraint(PT,ref(var(X),V),Attr,_):- getobjattr(PT,X,Attr,Attributes),append(Attributes,Attr,NewAttr),write(X),write(.),ppterm(V,NewAttr).
ppconstraint(PT,ref(ind(var(X),I),V),Attr,_):- getobjattr(PT,X,Attr,Attributes),append(Attributes,Attr,NewAttr),ppterm(ind(var(X),I),NewAttr),write(.),ppterm(V,NewAttr).
ppconstraint(PT,not(C),Attr,_) :- write('not('),ppconstraint(PT,C,Attr,_),write(')').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ppconstraint(PT,uquant(I,fromto(X,Y),C),Attr,Mto) :-
	        write('forall '),write(I),write(' in '), write(X),write('..'),
		write(Y),write(':('),ppconstraintlist(PT,C,Attr,Mto),write(')').
ppconstraint(PT,uquant(I,Obj,C),Attr,Mto) :-
	        write('forall '),write(I),write(' in '), write(Obj)
		,write(':('),getobjattr(PT,Obj,Attr,Attributes),append(Attributes,Attr,NewAttr),ppconstraintlist(PT,C,NewAttr,Mto),write(')').

ppconstraint(PT,enclosed(summation(In,Obj,C)),Attr,Mto) :-
	        write('sum '),write(In),write(' in '), write(Obj),
		write(':('),getobjattr(PT,Obj,Attr,Attributes),append(Attributes,Attr,NewAttr),ppconstraint(PT,C,NewAttr,Mto),write(')').

ppconstraint(PT,summation(In,Obj,C),Attr,Mto) :-
	        write('sum '),write(In),write(' in '), write(Obj),
		write(':('),getobjattr(PT,Obj,Attr,Attributes),append(Attributes,Attr,NewAttr),ppconstraint(PT,C,NewAttr,Mto),write(')').

ppconstraint(PT,condConstr(Constraint, Literals),Attr,Mto) :-
		ppconstraintlist(PT,[Constraint],Attr,Mto), write(':-'),
		ppliterals(Literals,PT,Attr).
ppconstraint(_,new(O,C,A),Attr,_) :-
	        ppterm(O), write(' = new ')
	       ,write(C),write('('),!, ppterms(A,Attr),write(')').



ppconstraint(PT,not(P),Attr,_) :-	write('not('), ppconstraint(PT,P,Attr,[]),write(')').
ppconstraint(PT,bool(P),Attr,_) :-  ppconstraint(PT,P,Attr,_).

ppconstraint(_,builtinclpr(X),_,_) :- write(X).
ppconstraint(_,T,A,_):- ppterm(T,A).
%%	%%%%%%%%%%%%%%%%%%%%%%%%%
%
gettype(V,[att(user(Type),var(V))|_],Type).
gettype(V,[att(array(user(Type),_),var(V))|_],Type).
gettype(_,[],[]).
gettype(V,[_|T],Type):- gettype(V,T,Type).

getobjattr(PT,Obj,Attr,NewAttr) :- gettype(Obj,Attr,Type),getclassattr(Type,PT,NewAttr).

ppterms([X]) :- ppterm(X).
ppterms([]).
ppterms([X|T]) :- ppterm(X),!, write(','), ppterms(T).
ppterms([],_).

ppterms([X],[]) :- ppterm(X).
ppterms([X|T],[]) :- ppterm(X),!, write(','), ppterms(T).
%ppterm(summation(X,Y,C),A) :- ppconstraint(summation(X,Y,C),A,_).
ppterms([X],A) :- ppterm(X,A).
ppterms([X|T],A) :- ppterm(X,A), !,write(','), ppterms(T,A).
ppterm((ref(prev(X),Y)),_) :-
       !, ppterm(ref(X,Y)), write('[Time-1]').
ppterm(ref(X,V),A):- ppterm(X),!,write('.'),ppterm(V,A).
ppterm(X,[]) :- ppterm(X).
ppterm(var(X),Attr) :- seriescheckvariable(Attr,X),!, write(X),write('[Time]').
ppterm(ser(_,Y),[1]):- write(Y).
ppterm(mult(X, Y),A) :-
   !, ppterm(X,A), write(' * '), ppterm(Y,A).
ppterm(add(X, Y),A) :-
   !, ppterm(X,A), write(' + '), ppterm(Y,A).
ppterm(divide(X, Y),A) :-
   !, ppterm(X,A), write(' / '), ppterm(Y,A).
ppterm(sub(X, Y),A) :-
   !, ppterm(X,A), write(' - '), ppterm(Y,A).
ppterm(pow(X, Y),A) :-
   !, ppterm(X,A), write(' ^ '), ppterm(Y,A).

   %fix 30/8
ppterm(ind(var(X),Y),A) :- seriescheckvariable(A,X),ppterm(X),write('['),ppterm(Y,A),write(',T]').
ppterm(ind(X,Y),A) :- !,ppterm(X),write('['),ppterm(Y,A),write(']').
ppterm(enclosed(T),A) :-
   !, write('('), ppterm(T,A), write(')').
 ppterm(negative(T)) :-
   !, write('-'), ppterm(T).
   ppterm(function(Name, X),A) :-
   !, write(Name),  write('('), ppterms(X,A), write(')').
%ppterms([X],A) :- ppterm(X,A).


ppterm(X,_) :- ppterm(X).
ppterm(ref(X,V)):- ppterm(X),!,write('.'),ppterm(V).
ppterm((ref(prev(X),Y))) :-
       !, ppterm(ref(X,Y)), write('[Time-1]').
ppterm(ser(X)):- !,write(X),write('[Time]').

ppterm(ind(ind(X,Y),Z)) :- !,ppterm(X),write('['),ppterm(Y),write(','),ppterm(Z),write(']').
ppterm(ind(X,Y)) :- !,ppterm(X),write('['),ppterm(Y),write(']').
ppterm(ser(X,_)) :- !,write(X).
ppterm(next(ind(X,Y))) :-
       !, ppterm(X), write('['),ppterm(Y),write(',Time+1]').
ppterm(prev(ind(X,Y))) :-
       !, ppterm(X), write('['),ppterm(Y),write(',Time-1]').


ppterm(mult(X, Y)) :-
   !, ppterm(X), write(' * '), ppterm(Y).
ppterm(add(X, Y)) :-
   !, ppterm(X), write(' + '), ppterm(Y).
ppterm(divide(X, Y)) :-
   !, ppterm(X), write(' / '), ppterm(Y).
ppterm(sub(X, Y)) :-
   !, ppterm(X), write(' - '), ppterm(Y).
ppterm(pow(X, Y)) :-
   !, ppterm(X), write(' ^ '), ppterm(Y).


ppterm(next(V)) :-
       !, ppterm(V), write('[Time+1]').


ppterm(prev(V)) :-
       !, ppterm(V), write('[Time-1]').

ppterm(prev(V)) :-
       !, ppterm(V), write('[Time-1]').

ppterm(at(V,I)) :- !,ppterm(V),write('['),write(I),write(']').

ppterm(const(C)) :-
   !, write(C).

ppterm(const(M,N,Z)) :-
   !, write(M), write('.'), writeleading0(Z),write(N).
ppterm(dnum(M,N,Z)) :-
   !, write(M), write('.'), writeleading0(Z),write(N).

ppterm(quotedString(S)) :-
   !, write('\''), write(S), write('\'').
ppterm(var(V)) :-
   !, write(V).
ppterm(enclosed(T)) :-
   !, write('('), ppterm(T), write(')').
ppterm(id(X)):- write(' '),write(X),write(' ').

%ppterm(id(X)):- !,write(X).
%next clause added on June 12 2003
ppterm(negative(T)) :-
   !, write('-'), ppterm(T).
ppterm(function(Name, X)) :-
   !, write(Name),  write('('), ppterms(X), write(')').
ppterm(tfunc(Name, X)) :-
   !, write(Name),  write('('), ppterms(X), write(')').
ppterm(functor(X)) :-
   extractvars(X, V),
   bagtoset(V, [], SV),
   removenumbers(SV, AV),
   ppterm(list(AV)).
%ppterm(functor(X)) :- !, write(X).
ppterm([X|T]) :- ppterms([X|T]).
   %write('['), ppterms([X|T]), write(']').%
ppterm([]).
ppterm(dangling(L)) :-
   write('['), ppterms(L), write('|'), write('_'), write(']').
%ppterm(summation(V, E, Out)) :- !, write(
%remaining cases ?
ppterm(list(L)) :-
   write('['), pplistterms(L), write(']').
ppterm(makelist(L)) :-
   write('['), ppmakelistterms(L), write(']').
ppterm(min(X,Y)) :-
   !, write('min'), write('('), ppterm(X), write(', '), ppterm(Y), write(')').
ppterm(max(X,Y)) :-
   !, write('max'), write('('), ppterm(X), write(', '), ppterm(Y), write(')').
ppterm(num(X)):- !,write(X).

ppterm(X) :-
   write(X).
pplistterms([X]) :-
   ppterm(X).
pplistterms([X|T]) :-
   ppterm(X), write(','), pplistterms(T).
pplistterms([]).

ppmakelistterms([X|T]) :-
   ppterm(X), ppmakelistterms(T).
ppmakelistterms([]).

ppliterals([X],PT,A) :-
   tabs(1),
   ppliteral(X,PT,A).
ppliterals([X|T],PT,A) :-
  tabs(1),
   ppliteral(X,PT,A), write(','), nl,
   ppliterals(T,PT,A).

ppliteral(X,PT,A):- ppconstraint(PT,X,A,_).


tabs(0) :- !.
tabs(N) :- write('   '), M is N-1, tabs(M).

%% MTO predicates
%% printing%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

getgpredicatename(dcobG(N,_,C),C,N):-!.
getgpredicatename([H|_],C,N) :- getgpredicatename(H,C,N).
getgpredicatename([_|T],C,N) :- getgpredicatename(T,C,N).

getfpredicatename(dcobF(N,_,C),C,N):-!.
getfpredicatename([H|_],C,N) :- getfpredicatename(H,C,N).
getfpredicatename([_|T],C,N) :- getfpredicatename(T,C,N).

writemtopredicates([],_,_).
writemtopredicates([X|T],PT,Attr) :-  writemtopredicates1(X,PT,Attr),writemtopredicates(T,PT,Attr).

writemtopredicates1(X,PT,A) :- ppmtopredicate(X,PT,A),write('.'),nl.
%Index for series variable
writeindex([],_).
writeindex([H|T],I) :-writeindex(H,I),writeindex(T,I).
writeindex(ser(X,Y),I):- write('index('),write(I),write(','),write(X),write(','),write(Y),write('),').
writeindex(_,_).
printgpredicate(N,C,PT,A) :- writedefaultG(N),nl,write(N),write('( Lo,Hi,T,['),
	pmtoargument(C,PT,A,R),getvars(PT,A,R,VarList),writeargumentlist(VarList),
	write(']) :- Hi1 is ceil(Hi),Lo<Hi1,'),dcobindex(I),write(I),write(' is Lo+T,'),writeindex(VarList,I),
	(checkquotedstring(C) -> ppconstraint(_,R,[1],_);write('{'),ppconstraint(_,R,[1],_),write('}')),write(',Lo1 is Lo+1,'),write(N),write('( Lo1,Hi1,T,['),
	writeargumentlist(VarList),write('])').
printfpredicate(N,C,PT,A) :- write(N),write('( Lo,Hi,T,['),pmtoargument(C,PT,A,R),getvars(PT,A,R,VarList),
	writeargumentlist(VarList),write(']) :- Hi1 is ceil(Hi),random(Lo,Hi1,S),'),dcobindex(I),write(I),write( ' is T+S,'),
	writeindex(VarList,I),(checkquotedstring(C) -> ppconstraint(_,R,[1],_);write('{'),ppconstraint(_,R,[1],_),write('}')).

ppmtopredicate(dcobG(N,time(_,_),C),PT,A) :- printgpredicate(N,C,PT,A).
ppmtopredicate(docobG(N,time(_),C),PT,A) :- printgpredicate(N,C,PT,A).
ppmtopredicate(dcobG(N,[],C),PT,A) :- printgpredicate(N,C,PT,A).
ppmtopredicate(dcobF(N,time(_,_),C),PT,A) :-printfpredicate(N,C,PT,A).
ppmtopredicate(dcobF(N,[],C),PT,A) :-printfpredicate(N,C,PT,A).
ppmtopredicate(dcobF(N,time(_),C),PT,A) :-write(N),write('( Lo,T,['),pmtoargument(C,PT,A,R),getvars(PT,A,R,VarList),
	writeargumentlist(VarList),write(']) :-'),dcobindex(I),write(I),write(' is round(T+Lo),'),
	writeindex(VarList,I),(checkquotedstring(C) -> ppconstraint(_,R,[1],_);write('{'),ppconstraint(_,R,[1],_),write('}')).

writedefaultG(N) :- write(N),write('(Lo,Lo,_,_):-!.'),nl,write(N),write('(Lo,Hi,_,_):-Lo>=Hi,!.'),nl.

pmtoargument(compare(R,Term1,Term2),PT,A,compare(R,RTerm1,RTerm2)):- pmtoterm(Term1,PT,A,RTerm1),pmtoterm(Term2,PT,A,RTerm2).

pmtoterm(compare(R,X,Y),PT,A,X) :- pmtoargument(compare(R,X,Y),PT,A,X).
pmtoterm(add(X,Y),PT,A,add(RX,RY)) :-pmtoterm(X,PT,A,RX),pmtoterm(Y,PT,A,RY).
pmtoterm(enclosed(X,Y),PT,A,enclosed(RX,RY)) :-pmtoterm(X,PT,A,RX),pmtoterm(Y,PT,A,RY).
pmtoterm(var(X),_,A,ser(X,N)) :- seriescheckvariable(A,X),dcobseries(N).
pmtoterm(var(X),_,_,var(X)).
pmtoterm(ind(_,_),_,_,var(R)):- dcobind(R).
pmtoterm(ref(var(X),var(Y)),PT,A,ser(X,N)):- getobjattr(PT,X,A,Attributes),append(Attributes,A,NewAttr),seriescheckvariable(NewAttr,Y),dcobseries(N).
pmtoterm(next(_),_,_,var(R)) :- dcobnp(R).
pmtoterm(prev(_),_,_,var(R)) :- dcobnp(R).
pmtoterm(ref(_,var(Y)),_,_,var(Y)).
pmtoterm(X,_,_,X).


checkquotedstring(compare(=,Term1,Term2)):- checkquotedstring(Term1);checkquotedstring(Term2).
checkquotedstring(add(Term1,Term2)) :- checkquotedstring(Term1);checkquotedstring(Term2).
checkquotedstring(mult(Term1,Term2)) :- checkquotedstring(Term1);checkquotedstring(Term2).
checkquotedstring(divide(Term1,Term2)) :- checkquotedstring(Term1);checkquotedstring(Term2).
checkquotedstring(sub(Term1,Term2)) :- checkquotedstring(Term1);checkquotedstring(Term2).
checkquotedstring(quotedString(_)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

writeleading0(0) :- !.
writeleading0(N) :- write('0'), M is N-1, writeleading0(M).

extractvars([], []).
extractvars([X|Tail], V) :-
   extractvars(X, Vx), extractvars(Tail, Vt), append(Vx, Vt, V).
extractvars(enclosed(Term), V) :-
   extractvars(Term, V).
%next clause added on June 12 2003
extractvars(negative(Term), V) :-
   extractvars(Term, V).
extractvars(pow(A, B), V) :-
   extractvars(A, Va), extractvars(B, Vb), append(Va, Vb, V).
extractvars(sub(A, B), V) :-
   extractvars(A, Va), extractvars(B, Vb), append(Va, Vb, V).
extractvars(add(A, B), V) :-
   extractvars(A, Va), extractvars(B, Vb), append(Va, Vb, V).
extractvars(mult(A, B), V) :-
   extractvars(A, Va), extractvars(B, Vb), append(Va, Vb, V).
extractvars(divide(A, B), V) :-
   extractvars(A, Va), extractvars(B, Vb), append(Va, Vb, V).
extractvars(function(_, Pars), V) :-
   extractvars(Pars, V).
extractvars(summation(X, E, Term), [X|V]) :-
   extractvars(E, Ve), extractvars(Term, Vt), append(Ve, Vt, V).
extractvars(ref(A, B), V) :-
   extractvars(A, Va), extractvars(B, Vb), append(Va, Vb, V).
extractvars(ind(C, N), V) :-
   extractvars(C, Vc), extractvars(N, Vn), append(Vc, Vn, V).
extractvars(fromto(M, N), V) :-
   extractvars(M, Vm), extractvars(N, Vn), append(Vm, Vn, V).
%extractvars([Par|Tail]
extractvars(list(L), V) :-
   extractvars(L, V).
extractvars(var(A), [A]).
extractvars(compare(_, Term1, Term2), V) :-
   extractvars(Term1, V1), extractvars(Term2, V2), append(V1, V2, V).
extractvars(uquant(X, E, Term), [X|V]) :-
   extractvars(E, Ve), extractvars(Term, Vt), append(Ve, Vt, V).
extractvars(equant(X, E, Term), [X|V]) :-
   extractvars(E, Ve), extractvars(Term, Vt), append(Ve, Vt, V).
extractvars(condConstr(A,B), V) :-
   extractvars(A, Va), extractvars(B, Vb), append(Va, Vb, V).
extractvars(new(X,_,Args),V) :-
   extractvars(X, Vx), extractvars(Args, Va), append(Vx, Va, V).
% dump not valid for sicstus, yet it can be present till the prettyprint stage.
% In pretty printing it is changed to print(...)
extractvars(constraintPred(dump,Args), V) :-
   extractvars(Args, V).
extractvars(constraintPred(print,Args), V) :-
   extractvars(Args, V). %  instead of dump for sicstus
extractvars(constraintPred(_Name,Args), V) :-
   extractvars(Args, V).
%next clause added to for Anand
extractvars(not(C), V) :- extractvars(C, V).
extractvars(A, [A]).

bagtoset([], L, L).
bagtoset([X|T], L, TL) :- member(X,L), bagtoset(T, L, TL).
bagtoset([X|T], L, TL) :- bagtoset(T, [X|L], TL). %  if \+ member(X,L).

removenumbers([], []).
removenumbers([const(_)|T], RT) :-
   removenumbers(T, RT).
removenumbers([const(_,_,_)|T], RT) :-
   removenumbers(T, RT).
removenumbers([X|T], RT) :-
   number(X), removenumbers(T, RT).
removenumbers([X|T], RT) :-
   atom(X),atom_chars(X, AX), number(AX), removenumbers(T, RT).
removenumbers([X|T], [X|RT]) :-
   removenumbers(T, RT). % if \+ number(X).




% ------------------ SYNTAX ANALYZER FOR TCOB ---------------------------

% This is the Cob to CLPR Translator, written by Pallavi Tambay
% with minor changes by Bharat Jayaraman.



parse(Tokens, ParseTree) :- resetclassnamecounter(_), resetkeywordcounter(_), time_program(ParseTree, Tokens, []), !.
% parse(_,[]) :- print('Syntax error in class '), %classnamecounter(C),
% print(C), nl,
   %            print('while parsing '), parsing(X), print(X),
  %            keywordclausecounter(Y), print(Y), nl, fail.

time_program([X|T]) --> time_info(X), {!}, program(T).
time_program([]) --> [].
time_info(time(X,Y)) --> ['{'],[id(simulation_time)],[=],[num(X)],[','],[id(start_time)],['('],time_start(Y),[')'],['}'].
time_info(time(X,[])) --> ['{'],[id(simulation_time)],[=],[num(X)],['}'].

time_start([[C,T]|Tail]) --> ['['],[id(C)],[','],[num(T)],[']'],[','],time_start(Tail).
time_start([[C,T]]) --> ['['],[id(C)],[','],[num(T)],[']'].
time_start([]) --> [].

program([X|T]) --> class_definition(X), {!}, program(T).
program([]) --> [].

class_definition(
   classdef(Name, Superclass, Attributes, Constraints, Functions,
   Predicates, Preferences, Methods, Constructors, Num_instances))
   --> [class], class_id(Name), ['{'], {!}, {setclassnamecounter(Name)},
   body(Attributes, Constraints, Functions, Predicates, Preferences,
   Methods, Constructors), ['}'], {Num_instances = 0, Superclass = ""}.
class_definition(
   classdef(abstract(Name), Superclass, Attributes, Constraints, Functions,
   Predicates, Preferences, Methods, Constructors, Num_instances))
   --> [abstract], [class], class_id(Name), ['{'], {!}, {setclassnamecounter(Name)},
   body(Attributes, Constraints, Functions, Predicates, Preferences,
   Methods, Constructors), ['}'], {Num_instances = 0, Superclass = "", Constructors = []}.
class_definition(
   classdef(Name, Superclass, Attributes, Constraints, Functions,
   Predicates, Preferences, Methods, Constructors, Num_instances))
   --> [class], class_id(Name), [extends], {!},
   class_id(Superclass), ['{'], {!}, {setclassnamecounter(Name)},
   body(Attributes, Constraints, Functions, Predicates, Preferences,
   Methods, Constructors), ['}'], {Num_instances = 0}.
class_definition(
   classdef(abstract(Name), Superclass, Attributes, Constraints, Functions,
   Predicates, Preferences, Methods, Constructors, Num_instances))
   --> [abstract], [class], class_id(Name), [extends], {!},
   class_id(Superclass), ['{'], {!}, {setclassnamecounter(Name)},
   body(Attributes, Constraints, Functions, Predicates, Preferences,
   Methods, Constructors), ['}'], {Num_instances = 0, Constructors = []}.

body(Attributes, Constraints, Functions, Predicates, Preferences, Methods, Constructors)
   --> attributes(Attributes), constraints(Constraints), functions(Functions),
   predicates(Predicates), preferences(Preferences), methods(Methods), constructors(Constructors).

attributes(Attributes) -->
   [attributes], {!},
   {seen('attributes: near declaration #'), resetkeywordclausecounter(_)},
   attribute_list(Attributes).
attributes(Attributes) -->
   [attribute], {!},
   {seen('attribute: near declaration #'), resetkeywordclausecounter(_)},
   attribute_list(Attributes),
   {Attributes = [_]}.
attributes([]) --> [].

attribute_list(Z) -->
   decl(X), [';'], {!},
   {upkeywordclausecounter(_)},
   attribute_list(Y),
   {append(X, Y, Z)}.
attribute_list([]) --> [].

decl(T) --> datatype(Type), id_list(Type, T).

datatype(Z) --> type(X), arraytype(X, 0, Z).
datatype(Z) --> [series],type(X), arraytype(series(X), 0, Z).

type(primitive(int)) --> [id('int')].
type(primitive(real)) --> [id('real')].
type(primitive(char)) --> [id('char')].
type(primitive(string)) --> [id('string')].
type(primitive(bool)) --> [id('bool')].
type(primitive(formula)) --> [id('formula')].
type(primitive(expr)) --> [id('expr')].
%type(primitive(enum)) --> [id('enum')].

type(user(Type)) --> class_id(Type). %% verification here will enforce order, hence verify later ?

arraytype(X, 0, X) --> [].
arraytype(X, N, array(X, N)) -->
   [], {number(N)}.

arraytype(X, N, Z) -->
   ['[]'], {N1 is N+1}, arraytype(X, N1, Z).
arraytype(X, 0, Z) -->
   ['['], [num(S)], [']'], arraytype(X, [S], Z).
arraytype(X, L, array(X, L)) -->
   [].
arraytype(X, L, Z) -->
   ['['], [num(S)], [']'], {append(L,[S],L1)}, arraytype(X, L1, Z).



arraytype(X, L, Z) -->
   ['['], [id(V)],  [']'], {append(L,[V],L1)}, arraytype(X, L1, Z).


id_list(Type, [att(Type, Id)|T]) -->
   attribute_id(Id), [','], {!}, id_list(Type, T).
id_list(Type, [att(Type, Id)]) -->
   attribute_id(Id).

constraints(Constraints) -->
   [constraints], {!},
   {seen('constraints: near constraint #'), resetkeywordclausecounter(_)},
   constraint_list(Constraints).
constraints(Constraints) -->
   [constraint], {!},
   {seen('constraint: near constraint #'), resetkeywordclausecounter(_)},
   constraint_list(Constraints), {Constraints = [_]}.
constraints([]) --> [].

constraint_list([X|T]) -->
   constraint(X), [';'],
   {upkeywordclausecounter(_)}, {!},
   constraint_list(T).
constraint_list([]) --> [].

constraint(X) --> creational_constraint(X), {!}.
constraint(X) --> quantified_constraint(X).
constraint(X) --> simple_constraint(X).

creational_constraint(new(T1, Name, Args)) -->
   term(T1), ['='],
   [id('new')], {!},
   class_id(Name),
   ['('], terms(Args), [')'].

creational_constraint(new(T1, Name, [])) -->
   term(T1), ['='],
   [id('new')], {!},
   class_id(Name),
   ['('], [')'].

%change name to uquant and equant...done
quantified_constraint(uquant(V, E, LX)) -->
   [forall], {!},
   variable(V), [in], {!},
   enum(E), [':'], {!},
   quantconstraint_list(LX).
quantified_constraint(equant(V, E, LX)) -->
   [exists], {!},
   variable(V), [in], {!},
   enum(E), [':'], {!},
   quantconstraint_list(LX).

quantconstraint_list([X]) -->
   constraint(X).
quantconstraint_list([X]) -->
   ['('], constraint(X), [')'].
quantconstraint_list([X|T]) -->
   ['('], {!}, constraint(X),
   qnt_constraint_lst(T), [')'].


qnt_constraint_lst([X|T]) -->
   [';'], constraint(X),
   qnt_constraint_lst(T).
qnt_constraint_lst([]) --> [].
simple_constraint(X) --> conditional_constraint(X).

simple_constraint(X) --> constraint_atom(X).


constraint_atom(mto('G',X,C)) -->
['G'],[<],time_interval(X),[>],constraint_atom(C).
constraint_atom(mto('F',X,C)) -->
['F'],[<],time_interval(X),[>],constraint_atom(C).
constraint_atom(mto('G',[],C)) -->
['G'],constraint_atom(C).
constraint_atom(mto('F',[],C)) -->
['F'],constraint_atom(C).


constraint_atom(C) -->
   ['('], constraint_atom(C), [')'].
constraint_atom(compare(R, Term1, Term2)) -->
   term(Term1),
   [R], {relop(R)},
   term(Term2).
%constraint_atom(compare(R, Term1, Term2)) --> term(Term1), [R], {relop(R)}, {!}, term(Term2).
constraint_atom(constraintPred(Name, X)) -->
   constraint_predicate_id(Name),
   ['('], terms(X), [')'].
   constraint_atom(pred(Name, X)) -->
   constraint_predicate_id(Name),
   ['('], terms(X), [')'].
constraint_atom(compare(=, Term1, Term2)) -->
   boolexpr(Term1),
   [=],
   boolexpr(Term2). %bool() around Term1 and Term2 in head ?
constraint_atom(bool(B)) -->
   boolexpr(B).

conditional_constraint(condConstr(Constraint, Literals)) -->
constraint_atom(Constraint) ,[:-], {!},
   literals(Literals).
conditional_constraint(condConstr(Constraint, Literals)) -->
literals(Literals),[-->], {!}, constraint_atom(Constraint).



time_interval(time(X,Y)) --> term(X),[','], term(Y).
time_interval(time(X)) --> term(X).
time_interval([]) --> [].


relop(<=).
relop(>=).
relop(=>).
relop(>).
relop(=<).
relop(<).
relop(=).
relop('!=').
relop('==').

%relop('=\\=').
% this is really the relop =\= but =\= wont be printed as
%is so we store it as the atom =\\=
%relop('=\=').
% not used due to =\\=
infixop(*).
infixop(^).
infixop(/).
infixop(-).
infixop(+).

%**********parser goes into infinite loop if there is unmatching ( in input!!!

term(Out) --> expr(_, Out).
%term(bool(B)) --> boolexpr(B). % should not allow comparisons other than = between bools.
                         % e.g. W >= A and B shouldn't be allowed.
%must be after first clause of term so that boolexpr does not catch pure expr
term(A) --> attribute(['$'], A).
term(list([])) --> ['[]'].
term(list(T)) --> ['['], terms(T), [']'].
%dcob
%term(next(T)) --> terms(T),['`'].

%term(list(T)) --> ['['], listmem(N), ['|'], listmem(M), [']']. %INCORRECT : T ? should have terms

expr(In, Out) --> sub1(In, Out1), sub2(Out1, Out).

sub2(In, Out) --> ['-'], {!}, sub1(In, Out1), sub2(sub(In,Out1), Out).
sub2(S, S) --> [].

sub1(In, Out) --> term1(In, Out1), term2(Out1, Out).

term1(In, Out) --> fact(In, Out1), fact2(Out1, Out).

term2(In, Out) --> ['+'], {!}, term1(In, Out1), term2(add(In,Out1), Out).
term2(S, S)    --> [].


div(_, const(C))   -->
   constant(C), {!}.
div(_, const(M,N,Z))   -->
   constant(M,N,Z), {!}.
div(_, function(Name, X)) -->
   function_id(Name),
   ['('], {!}, terms(X), [')']. %make sure this cut is correct
div(_, summation(V, E, Out)) -->
   [sum], {!},
   variable(V), [in],
   enum(E), [':'],
   term(Out).
div(_, minimum(V, E, Out)) --> [min], {!}, variable(V), [in], enum(E), [':'], term(Out).
div(_, maximum(V, E, Out)) --> [max], {!}, variable(V), [in], enum(E), [':'], term(Out).
div(_, enclosed(Out)) --> ['('], {!}, term(Out), [')'].
%next clause added on June 12 2003
div(_, negative(Out)) --> ['-'], term(Out). %should there be a cut here ?
%next clause added on June 12 2003
div(_, quotedString(S)) --> [quotedString(S)].
div(_, A) --> attribute(['$'], A).
div(_, var(V))   --> variable(V). %placed last so that coumpound terms may be parsed earlier.

div2(In, Out) --> ['/'], {!}, div(In, Out1), div2(divide(In,Out1), Out).
div2(S, S) --> [].

%fact(In, Out)   --> div(In, Out1), div2(Out1, Out).
fact(In, Out)   --> pow(In, Out1), pow2(Out1, Out).
pow(In, Out)   --> div(In, Out1), div2(Out1, Out).

fact2(In, Out) --> ['*'], {!}, fact(In, Out1), fact2(mult(In,Out1), Out).
fact2(S, S)    --> [].

pow2(In, Out) --> ['^'], {!}, pow(In, Out1), pow2(pow(In, Out1), Out).
pow2(S, S)    --> [].


function_id(Name) --> [id(Name)].
enum(Name) --> [id(Name)].
enum(fromto(N, M)) --> [fromto(N, M)].
%Following clauses added on 03.05.08
enum(A) --> attribute(['$'], A).

constant(N) --> [num(N)].
constant(N,M,Z) --> [dnum(N, M, Z)].

%add the following and more from different types.
%constant(C) --> [string(C)].
%constant(C) --> [bool(C)].
%constant(C) --> [char(C)].
%constant(C) --> [real(C)].

attribute(In, X) --> selector(In, X), { In = ['$']}.
attribute(In, Out) --> ['.'], {\+(In = ['$'])}, {!}, selector(In, Out).
attribute(In, Z) --> selector(In, X), ['.'], {!}, selector(X, Y), attribute(Y, Z), {In = ['$']}.
attribute(In, next(Z)) --> selector(In, X), ['.'], {!}, selector(X, Y), attribute(Y, Z),['`'], {In = ['$']}.
attribute(In, In) --> [], {\+(In = ['$'])}.

selector(In, X) --> attribute_id(X), {In = ['$']}.
%dcob
selector(In, next(X)) --> attribute_id(X),['`'], {In = ['$']}.
selector(In, prev(X)) --> ['`'],attribute_id(X), {In = ['$']}.

selector(In, at(X,T)) --> attribute_id(X),[<],[num(T)],[>],{In = ['$']}.
selector(In, next(Ind)) --> attribute_id(X), ['['], terms(T), [']'],['`'], {In = ['$']}, {!}, {make_ind(X, T, Ind)}.
selector(In, prev(Ind)) --> ['`'],attribute_id(X), ['['], terms(T), [']'], {In = ['$']}, {!}, {make_ind(X, T, Ind)}.



selector(In, Ind) --> attribute_id(X), ['['], terms(T), [']'], {In = ['$']}, {!}, {make_ind(X, T, Ind)}.


selector(In, sel(Name, X)) --> selector_id(Name), {!}, terms(X), {In = ['$']}.
       % Will this occur only at the
       % beginning of an attri ?
       % Is x.first(y) possible ?
       % If not then change grammar.
       % following clauses of selector will be used after occurance of a "."
selector(In, Ind) --> attribute_id(X), ['['], terms(T), [']'], {!},
			{make_ind(ref(In, X), T, Ind)}, {\+(In = ['$'])}.
selector(In, ref(In, next(X))) --> attribute_id(X), ['`'],{!}, {\+(In = ['$'])}.
selector(In, ref(In, at(X,T))) --> attribute_id(X), [<],[num(T)],[>],{!}, {\+(In = ['$'])}.

selector(In, ref(In, X)) --> attribute_id(X), {!}, {\+(In = ['$'])}.
%use of next clause is not known, hence may be incorrect
selector(In, sel(Name, X)) --> selector_id(Name), terms(X), {\+(In = ['$'])}.
	% Will this occur only at the
        % beginning of an attri ?
        % Is x.first(y) possible ?
        % if not then change grammar.

%attribute(In, ref(In, X)) --> ['.'], selector(X), {\+(In = ['$'])}.
%attribute(_, Z) --> selector(X), ['.'], selector(Y), attribute(ref(X,Y), Z).
%attribute(In, X) --> selector(X), {In = ['$']}.
%attribute(In, In) --> [], {\+(In = ['$'])}.

selector_id(fst) --> [first].
selector_id(nxt) --> [next].
selector_id(lst) --> [last].

terms([X]) --> term(X).
terms([]) -->[].
%terms([next(X)]) --> term(X),['`'].
terms([X|T]) --> term(X), [','], {!}, terms(T). %make sure this cut is correct

literals([L]) --> literal(L).
literals([L|T]) --> literal(L), [','], literals(T).
literals([L|T]) --> literal(L), ['&'], literals(T).

literal(builtinclpr('nl')) --> [id('nl')], {!}.
literal(not(A)) --> [not], atom(A).
literal(A) --> atom(A).
% Allowing for constraint-based info retrieval proj (sensors,
% targets)...Anand Ganesh%
%Disable  to check the new form of conditional constraint- Jinesh
%literal(C) --> constraint(C). literal(not(C))
% --> [not], constraint(C).

literal(mto('G',X,C)) --> ['G'],['<'],X,['>'],constraint(C).


%atom(pred(Name, X)) --> predicate_id(Name), ['('], terms(X), [')']. %incorrect
atom(X) --> constraint_atom(X).

class_id(X) --> [id(X)].
attribute_id(var(X)) --> [id(X)].
constraint_predicate_id(Name) --> [id(Name)]. %For now.
variable(X) --> [id(X)].
predicate_id(Name) --> [id(Name)].

functions(Functions) --> [functions], {seen(functions)}, func_clauses(Functions).
functions([]) --> [].

predicates([class_predicates(Predicates)]) --> [predicates], {seen(predicates)}, pred_clauses(Predicates).
predicates([]) --> [].

pred_clauses([P|Rest]) --> one_pred_clause(P), pred_clauses(Rest).
pred_clauses([]) --> [].

one_pred_clause(['.']) --> ['.'], {!}. % this will cause a problem if "." is present within the definition of a pred clause.
one_pred_clause([X|Rest]) --> [X], one_pred_clause(Rest).
%pred_clauses([pred_clause(H, B) | T]) --> head(H), [':-'], {!},predbody(B), ['.'], pred_clauses(T).
%pred_clauses([]) --> [].
%pred_clauses([pred_clause(H) | T]) --> head(H), ['.'], pred_clauses(T).

%head(pred(Name, T)) --> predicate_id(Name), ['('], terms(T), [')'].
%predbody(pred_body(L)) --> literals(L). % INCORRECT because this will allow constraint atoms in predicate literals.

preferences(Preferences) --> [preferences], {!}, {seen(preferences)}, pref_clauses(Preferences).
preferences([]) --> [].

methods(Methods) --> [methods], {!}, {seen(methods)}, method_definitions(Methods).
methods([]) --> [].

constructors(Constructors) --> [constructors], {!}, {seen('constructors: near constraint #'),
                               resetkeywordclausecounter(_)},  constructor_clauses(Constructors).
constructors(Constructors) --> [constructor], {!}, {seen('constructor: near constraint #'),
                               resetkeywordclausecounter(_)},  constructor_clauses(Constructors), {Constructors = [_]}.
constructors([]) --> [].


constructor_clauses([constructor(Name, Attributes, ConstructorConstraintList)|T]) -->
   class_id(Name), {seterrormessage(_), namematch(Name)}, ['('], id_list(_,Attributes), [')'], ['{'], %resetkeywordclausecounter(_)},
   constraint_list(ConstructorConstraintList), ['}'], constructor_clauses(T). %correct call to id_list
constructor_clauses([constructor(Name, [], ConstructorConstraintList)|T]) -->
   class_id(Name), {seterrormessage(_), namematch(Name)}, ['('], [')'], ['{'], %resetkeywordclausecounter(_)},
   constraint_list(ConstructorConstraintList), ['}'], constructor_clauses(T). %correct call to id_list
constructor_clauses([]) --> [].

seterrormessage(_) :- classnamecounter(_), seen('constructor, the constructor name does not match class name in constructor # '). %why are we counting classnamecounter ?

namematch(Name) :- classnamecounter(Name), seen('constructor : near constraint #').
namematch(_) :- classnamecounter(_), fail. %why are we counting classnamecounter ?

%__________________helper predicates______________________

make_ind(A, [], A).
make_ind(A, [T|Rest], Ind) :- make_ind(ind(A, T), Rest, Ind).

resetclassnamecounter(_) :- retractall(classnamecounter(_)), assert(classnamecounter(' # 1 near class name')).
setclassnamecounter(N) :- retractall(classnamecounter(_)), assert(classnamecounter(N)).

resetkeywordclausecounter(_) :- retractall(keywordclausecounter(_)), assert(keywordclausecounter(1)).
upkeywordclausecounter(_) :- retract(keywordclausecounter(C)), C1 is C+1, assert(keywordclausecounter(C1)).

resetkeywordcounter(_) :- retractall(parsing(_)).
seen(X) :- retractall(parsing(_)), assert(parsing(X)).

%_______________added to accomodate formulae

boolexpr(Y)     --> boolterm(X), boolterm2(X,Y).

boolterm2(X,X)  --> [].
boolterm2(X,Z)  --> [OP], {operator(OP, F)}, boolterm(Y), {T =.. [F,X,Y]}, boolterm2(T,Z).

boolterm(X) --> int(X).
boolterm(A) --> attribute(['$'], A).
boolterm(X) --> ['('], boolexpr(X), [')'].
boolterm(not(X)) --> [not], boolexpr(X). %%%%%%%%%%%????????????
%boolterm(Out) --> expr(_, Out).
boolterm(T) --> var(X), ['('], exprlist(L), [')'], {T =.. [X|L]}.
boolterm(X) --> var(X).
boolterm(compare(R, Term1, Term2)) --> term(Term1), [R], {relop(R)}, {!}, term(Term2).

var(X)  --> [id(X)].
var(next(X))  --> [id(X)],['`'].


int(X)  --> [num(X)].

exprlist([E])   --> boolexpr(E).
exprlist([E|L]) --> boolexpr(E), [','], exprlist(L).

%operator(+, plus).
%operator(-, minus).
%operator(/, div).
%operator(*, mult).
%operator(>=, ge).
%operator(=<, le).
%operator(=, eq).
%operator(>, gt).
%operator(<, lt).
operator(->, impl).
operator(and, and).
operator(or, or).


% ------------------ LEXICAL ANALYZER FOR COB ---------------------------

% This is the Cob to CLPR Translator, written by Pallavi Tambay
% with minor changes by Bharat Jayaraman.

lex(Stream,Tokens) :-
   get_chars(Stream,L), !,
   tokenize(L,Tokens),!.

get_chars(Str,L) :-
   get_code(Str,C),
   get_chars(Str,C,L).

%get_chars(_,46, [46]) :- !.				% 46 = period
get_chars(_,36, []) :- !.				% 36 = $
get_chars(Str,C,  [C|L1]) :-
   get_chars(Str,L1).

tokenize([], []) :- !.
tokenize([C|L], L3) :-
   white(C), !, skip_whites(L,L2),
   tokenize(L2,L3).
tokenize([C|L], [X|L3]) :-
   alpha(C), enumerated(X,[C|L],L2), !,
   tokenize(L2,L3).
tokenize([C|L], [X|L3]) :-
   d09(C), enumerated(X,[C|L],L2), !,
   tokenize(L2,L3).
tokenize([C|L], [X|L3]) :-
   d09(C), digits(X,[C|L],L2), !,
   tokenize(L2,L3).
% look for identifier after enumerated of type X..2 or X..Y
tokenize([C|L], [X|L3]) :-
   alpha(C), identifier(X,[C|L],L2), !,
   tokenize(L2,L3).
%next clause added on June 12 2003
tokenize(L, [quotedString(S)|L4]) :-
   special(quote,L,L2), !,
   quoted_string(StringAsListofAsciiCodes, L2, L3),
   name(S, StringAsListofAsciiCodes),
   tokenize(L3,L4).
tokenize(L, [X|L3]) :-
   special(X,L,L2), !,
   tokenize(L2,L3).
% ignore comments
tokenize([C|L], L3) :-
   comment(C), !, skip_till_eol(L,L2),
   tokenize(L2,L3).
tokenize([C|_L], _) :-
   print('Error: Cannot tokenize the character1: '),
   name(BadChar, [C]),
   print(BadChar),
   fail.

%this definition added on June 12 2003
%form string of characters till the next "quote"
quoted_string([], L, L2) :-
   special(quote, L, L2).
quoted_string([C|Rstring], [C|L], L2) :-
   quoted_string(Rstring, L, L2).

skip_whites([], []).
skip_whites([C|L], L2) :-
   (white(C) -> skip_whites(L,L2); L2 = [C|L]).

skip_till_eol([], []).
skip_till_eol([C|L], L2) :-
   (eol(C) -> L2 = L; skip_till_eol(L,L2)).

white(9).  % tab
white(32). % blank
white(10). % newline
white(13). %carriage return

eol(10).
comment(37). % % (percentage sign)

%next clause added on June 12 2003

special(quote, [39|L],L).
%special('!=',[33,61|L],L). not in sicstus
% special('==', [60,60|L],L). not working
special('=\\=',[61,92,61|L],L). % changed from =\= to =\\= for printing it correctly
%special('=\=',[61,92,61|L],L).
special('{',[123|L],L).
special('}',[125|L],L).
special(':-',[58,45|L],L).
special('=<',[60,61|L],L). %since <= not permitted in sicstus it is parsed into =<
special('>=',[61,62|L],L). %since => not permitted in sicstus it is parsed into >=
special('[]',[91,93|L],L).
special('[',[91|L],L).
special(']',[93|L],L).
special('$',[36|L],L).
special('&',[38|L],L).

special('->',[45,62|L],L). %order matters otherwise - and > will be made separate tokens
special('-->',[45,45,62|L],L).
special(_,[95|L],L). % in alpha also. is that ok ?
special(:=,[58,61|L],L).
special(=<,[61,60|L],L).
special(>=,[62,61|L],L).
special(>,[62|L],L).
special(=,[61|L],L).
special(<,[60|L],L).
special('(',[40|L],L).
special(')',[41|L],L).
special(;,[59|L],L).
special(',',[44|L],L).
special(*,[42|L],L).
special(^,[94|L],L).
special(+,[43|L],L).
special(-,[45|L],L).
special(.,[46|L],L).
special(/,[47|L],L).
special(:,[58|L],L).
special('|',[124|L],L).
special('!', [33|L],L).
special('`',[96|L],L).
specialdec(.,[46|L],L).
specialenum('..', [46,46|L],L).
%___________________________

identifier(X) --> ident(L), {name(N,L), (keyword(N) -> X=N; X=id(N))}.

ident([X|L]) --> letter(X), legits(L).
ident([X])   --> letter(X).

legits([X|W]) --> legit(X), legits(W).
legits([X])   --> legit(X).

legit(X) --> letter(X) ; digit(X).

letter(X) --> [X],  {alpha(X)}.

alpha(X) :-  X > 64,  X < 91.
alpha(X) :-  X > 96,  X < 123.
alpha(95). % ascii value of _

keyword(abstract).
keyword(class).
keyword(extends).
keyword(attributes).
keyword(attribute).
keyword(constraints).
keyword(constraint).
keyword(in).
keyword(predicates).
keyword(preferences).
keyword(functions).
keyword(methods).
keyword(constructors).
keyword(constructor).
keyword(sum).
keyword(forall).
keyword(exists).
keyword(min).
keyword(max).
keyword(belongs).

keyword(and).
keyword(or).
keyword(first).
keyword(next).
keyword(last).
keyword(not).
%Dcob
keyword(series).
keyword('G').
keyword('F').
%_________________________

digits(dnum(N,M,Z)) --> digs(K), specialdec(_), digs(L),
			{name(N, K),
			 leading0(L,L2,Z),
			 (L2 == [] -> M = 0 ; name(M, L2))
                        }.

digits(num(N)) --> digs(L), {name(N,L)}.

digs([X|L]) --> digit(X), digs(L).
digs([X]) --> digit(X).

digit(X) -->  [X],  {d09(X)}.

d09(X) :- X > 47,  X < 58.

leading0([48|L],L2,N) :- !, leading0(L,L2,M), N is M+1.
leading0([X|L],[X|L],0) :- notzero(X), !.
leading0([],[],0).

notzero(X) :- X > 48 ; X < 48.

enumerated(fromto(N, M)) --> digitsoridentifier(N), specialenum(_), digitsoridentifier(M), {!}.

digitsoridentifier(N) --> digits(num(N)).
digitsoridentifier(X) --> identifier(id(X)).
