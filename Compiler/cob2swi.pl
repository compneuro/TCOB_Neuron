%Change note
% Fix to accept creational constraint without parameter(search for %fix Jinesh).
:- module(cob2swi, [cob2swi/1]).
%Changes made on 03.04.30 not reflected in translate.plg
%03.04.30 : Commented two clauses of translateterm. Added 3rd clause containing !,fail to definition of typeof. Put cuts in translate, translateone, translateconstraints, translateterm.
%02.08.06 : porting to sicstus
%1. file contains modifications to translation of conditional constraints  needed for Bin Zhang's gis.cob file
%2. using full path name of helper file, so that user need not have it in the directory where clpr is running
%3. will translate constant sized array declarations (by creating the array).
%4. translation of coball and other quantified constraints has the clause for base case before the recursive clause.

:- use_module(library(lists)).

cob2swi(File) :-
   atom_codes(File, FullCodes),
   atom_codes('.cob',S),
   append(FileCodes, S ,FullCodes),
   atom_codes(Prefix,FileCodes),
   atom_concat(Prefix, '.pl', Output),
   cob2swi(File,Output).

 cob2swi(File, Output) :-
   open(File,read,Stream),
   process(Stream, Output).

% tokenize, parse tokens to form parse tree,
% translate parse tree and write the translation to file
% along with a clause consulting a file called helper
% The helper file contains definitions of helper predicates
% like sizeof and conditional constraints.
%
process(Stream, Output) :-
   tell(user_output),
   lex(Stream,Tokens), !, %print(Tokens), nl,
   parse(Tokens, ParseTree), !,
   %write('Parse successful. Printing parse tree...'), nl,write(ParseTree),!,nl,
   translateprog(ParseTree, ParseTree, P),
   append([pred_clause(pred("",_),
		       pred_body([call(consult, [helper_clpr]),
				  call(use_module, ['library(clpr)'])]))],
	   P, SWIprog),
% append([pred_clause(pred("",_), pred_body([call('use_module',
% ['library(clpr)'])])), pred_clause(pred("",_),
% pred_body([call('use_module', ['library(\'clpqr/expand\')'])])),
% pred_clause(pred("",_), pred_body([expand]))], CLPprog, SICStusprog1),
% append(SICStusprog1, [pred_clause(pred("",_), pred_body([noexpand]))],
% SICStusprog),
%nl, write('Translation successful. Printing translated program...'), nl, print(P), nl, !,
   tell(Output),
   prettyprint(SWIprog), told.

%translate each class(X) of the program
translateprog([X|T], O, P) :-
   translateclass(X, O, C),
   translateprog(T, O, Rest),
   append(C, Rest, P).
translateprog([], _, []).

% classdef(Name,Superclass,Attributes,Constraints,Functions,Predicates,Preferences,Methods,Constructors,Num_instances)
% translate a class using the original parse tree L.
% L may be used to get information about superclass/other classes/variable types in case of referencing
% predicates are translated as is.
translateclass(classdef(Name, Superclass, Attributes, Constraints, _,
               Predicates, _, _, Constructors, _), L, Class) :-
   translate(Name, Superclass, Attributes, Constraints, Constructors, L, C1),
   append(C1, Predicates, Class).
%eventually, this will also translate Functions, Methods, Preferences.

%translate each constructor
translate(Name, Superclass, Attributes, Constraints, [Constructor], L, C) :-
   !, translateone(Name, Superclass, Attributes, Constraints, Constructor, L, C).
translate(Name, Superclass, Attributes, Constraints, [Constructor|T], L, C) :-
   translateone(Name, Superclass, Attributes, Constraints, Constructor, L, C1),
   translate(Name, Superclass, Attributes, Constraints, T, L, CT), append(C1, CT, C).
% if constructor is absent (abstract class) still translate into a predicate so that
% subclasses' translation may call this predicate
translate(Name, Superclass, Attributes, Constraints, [], L, C) :-
   translateone(Name, Superclass, Attributes, Constraints, L, C).

% A "constructor" is translated into CLP Predicate with two arguments.
% The first argument is a list of constructor arguments. The second
% is a list of attributes of the class Name.
% "constructor body" is translated into CLP constraints
% "constraints of a class" are translated into CLP constraints
% ??????????????? Should we translate type declarations into predicate calls ?
% case when class Name is not a subclass
translateone(Name, "", Attributes, Constraints, constructor(Name, Att1, ConstructorConstraintList), L,
   [pred(Name, Att, AttList,  ConstraintList)|P]) :-
   translatetypedecl(Attributes, TD),
   removetypedecl(Attributes, AttList), removetypedecl(Att1, Att),
%   translateconstructorconstraints(ConstructorConstraintList, L, CC, _, Name), % predicates ?
   !, translateconstraints(ConstructorConstraintList, L, CC, P1, Name, []), % predicates ?
   translateconstraints(Constraints, L, C, P2, Name, []),
   append(TD, CC, C1),    append(C1, C, ConstraintList), append(P1, P2, P).
%   append(CC, C, ConstraintList), append(P1, P2, P).
% case when class Name is a subclass of Superclass.
% The attributes of Superclass (and its superclass and so on...) are appended to the
% head of the list of attributes of Name to form the second argument of the translated
% clpr predicate corresponding to this constructor.
translateone(Name, Superclass, Attributes, Constraints, constructor(Name, Att1, ConstructorConstraintList), L,
   [pred(Name, Att, AttList,  ConstraintList)|P]) :-
   translatetypedecl(Attributes, TD),
   attributesofclass(Superclass, L, SuperAttributes), append(SuperAttributes, Attributes, AttList1),
   removetypedecl(AttList1, AttList), removetypedecl(Att1, Att), removetypedecl(SuperAttributes, SAtt),
%   translateconstructorconstraints(ConstructorConstraintList, L, CC, _, Name), % predicates ?
   !, translateconstraints(ConstructorConstraintList, L, CC, P1, Name, []), % predicates ?
   translateconstraints(Constraints, L, C, P2, Name, []),
   append(TD, CC, C1),    append(C1, C, C2), append(P1, P2, P),
%   append(CC, C, C2), append(P1, P2, P),
   cobvar(X), append([call(Superclass,[X, SAtt])], C2, ConstraintList).
% case when class Name is an abstract class (constructor absent). In this case,
% the translated predicate's first argument is an empty list since this otherwise is
% a list of constructor arguments.
translateone(Name, "", Attributes, Constraints, L, [pred(Name, [], AttList,  ConstraintList)|P]) :-
   translatetypedecl(Attributes, TD),
   removetypedecl(Attributes, AttList),
%   translateconstructorconstraints(ConstructorConstraintList, L, CC, _, Name), % predicates ?
%   translateconstraints(ConstructorConstraintList, L, CC, P1, Name, []), % predicates ?
   !, translateconstraints(Constraints, L, ConstraintList1, P, Name, []),
   append(TD, ConstraintList1, ConstraintList).
%   append(CC, C, ConstraintList), append(P1, P2, P).
% translates an abstract class which is a subclass of another class.
translateone(Name, Superclass, Attributes, Constraints, L, [pred(Name, [], AttList,  ConstraintList)|P]) :-
   translatetypedecl(Attributes, TD),
   attributesofclass(Superclass, L, SuperAttributes), append(SuperAttributes, Attributes, AttList1),
   removetypedecl(AttList1, AttList), removetypedecl(SuperAttributes, SAtt),
%   translateconstructorconstraints(ConstructorConstraintList, L, CC, _, Name), % predicates ?
%   translateconstraints(ConstructorConstraintList, L, CC, P1, Name, []), % predicates ?
   !, translateconstraints(Constraints, L, C, P, Name, []),
   append(TD, C, C1),
%   append(CC, C, C2), append(P1, P2, P),
   cobvar(X), append([call(Superclass,[X, SAtt])], C1, ConstraintList).


% First argument is a list of tuples. Each tuple's second member is a variable and first
% member is the type of that variable.
% This predicate removes the type from each tuple in a list and returns just the list of variables.
removetypedecl([],[]).
%enum handling added on 3/12/2016
%removetypedecl([att(user(enum), _)|T], Y) :- removetypedecl(T, Y).
removetypedecl([att(_, X)|T], [X | Y]) :- removetypedecl(T, Y).

% Given class name Name, searches the parse tree L to return the list of attributes of Name
% appended at the front with the attributes of the superclass of Name (if it exists) and so on.
attributesofclass(Name, L, Att) :- attributesofclass(Name, L, L, Att).
attributesofclass("", _, _, []).
attributesofclass(Name, [X|_], _, Attributes) :-
   X = classdef(Name, "", Attributes, _, _, _, _, _, _, _).
attributesofclass(Name, [X|_], L, Attributes) :-
   X = classdef(Name, Superclass, Att, _, _, _, _, _, _, _),
   attributesofclass(Superclass, L, L, SuperAtt), append(SuperAtt, Att, Attributes).
attributesofclass(Name, [_|T], L, Attributes) :- attributesofclass(Name, T, L, Attributes).

%shouldn't EType be concatenated ?
translateconstraints([], _, [], [], _, _) :- !.
%translateconstraints([condConstr(Constraint, Literals)|Rest], L,
%                     [condConstr([Constraint], Literals)|TRest], P, Name, EType) :- %translate Constraints & Literals!!
%   translateconstraints(Rest, L, TRest, P, Name, EType).
% conditional constraint A:-B is translated into a call to a pre-defined predicate "conditional_constraint(TA,TB)"
% where TA and TB are respectively the translations of A and B.
%next clause modified from sunnyvale
translateconstraints([condConstr(Constraint, Literals)|Rest], L,
                     TRest1, P5, Name, EType) :- %translate Constraints & Literals!!
   !, translateconstraints1([Constraint], L, [FirstC|TC], P1, Name, EType),
   translateliterals(Literals, L, Callsbefore, TLiterals, P2, Name, EType),
   translateconstraints(Rest, L, TRest, P3, Name, EType), append(P1, P2, P4), append(P4, P3, P5),
append(TC, Callsbefore, TCL), append(TCL, [condConstr([FirstC], TLiterals)], TCLCC), append(TCLCC, TRest, TRest1).
%pprint this
translateconstraints([new(T1, Class, Pars)|Rest], L, Calls, P, Name, EType) :-
   !, % BJ: change the first argument as follows:
   % [compare(=,V1,Par1), ..., compare(=,Vn,Parn),
   %  new(T1, Class, [V1..Vn]) | Rest]
  % translatePars(Pars, VarList, ParEqnList),
   translateterm(T1, L, TT1, Cl1, P1, Name, Class, EType),
   translateterms(Pars, L, TPars, Cl2, P2, Name, _, _),
   translateconstraints(Rest, L, TRest, PRest, Name, _), %should this be _ ?
% Changed order of calls on May 16 2003 so that arguments of constructor are
% unified before call to constructor predicate.
   append(Cl1, Cl2, C13),
   append(ParEqnList, C13, CP13),
   append(CP13, [call(Class, [TPars, TT1])], Cl4), append(Cl4, TRest, Calls),
   append(P1, P2, P3), append(P3, PRest, P).

translateconstraints([uquant(V, fromto(N, M), LX)|Rest], L,
                     [call(makelistfromto, [N, M, NtoM]),
                      call(Forall, [NtoM, functor(SLX)])|TRest],
                     [pred_clause(pred(Forall, [makelist([]), functor(SLX)])),
                      pred_clause(pred(Forall, [makelist([W,'|',var('Tail')]),functor(SLX)]),pred_body(P5))|P],
                     Name, EType):-
   cobforallpred(Forall),
   cobvar(W), subst(V, W, LX, SLX),
   cobvar(NtoM),
   cobvar(U), subst(W, U, SLX, SSLX),
%   typeof(E, Name, L, EType, TypeofE), append([att(user(TypeofE), var(W))], EType, EType1),
   !,
   translateconstraints(SLX, L, C, P1, Name, EType),
   translateconstraints(Rest, L, TRest, P2, Name, EType),
   append(C, [ifthenelse([call('nonvar', [var('Tail')])],
                         [call(Forall, [var('Tail'), functor(SSLX)])],
                         ['true'])], P5),
   append(P1, P2, P).
%added this clause  on 03.05.08. If this is correct, then next one is redundant
translateconstraints([uquant(V, E, LX)|Rest], L,
                     Tultimate,
                     [pred_clause(pred(Forall, [makelist([]), functor(SLX)])),
                      pred_clause(pred(Forall, [makelist([W,'|',var('Tail')]),functor(SLX)]),pred_body(P5))|P],
                     Name, EType):-
   cobforallpred(Forall),
   cobvar(W), subst(V, W, LX, SLX),
   cobvar(U), subst(W, U, SLX, SSLX),
   !,
   translateterm(E, L, TE, Calls, Preds, Name, _, _),
   typeof(E, Name, L, EType, TypeofE), append([att(user(TypeofE), var(W))], EType, EType1),
   translateconstraints(SLX, L, C, P1, Name, EType1),
   translateconstraints(Rest, L, TRest, P2, Name, EType),
   append(C, [ifthenelse([call('nonvar', [var('Tail')])],
                         [call(Forall, [var('Tail'), functor(SSLX)])],
                         ['true'])], P5),
   append(P1, P2, P030508),
   append(P030508, Preds, P),
   append(Calls, [call(Forall, [TE, functor(SLX)])], TTTRest),
   append(TTTRest, TRest, Tultimate).
translateconstraints([uquant(V, E, LX)|Rest], L,
                     [call(Forall, [E, functor(SLX)])|TRest],
                     [pred_clause(pred(Forall, [makelist([]), functor(SLX)])),
                      pred_clause(pred(Forall, [makelist([W,'|',var('Tail')]),functor(SLX)]),pred_body(P5))|P],
                     Name, EType):-
   cobforallpred(Forall),
   cobvar(W), subst(V, W, LX, SLX),
   cobvar(U), subst(W, U, SLX, SSLX),
   !,
   typeof(E, Name, L, EType, TypeofE), append([att(user(TypeofE), var(W))], EType, EType1),
   translateconstraints(SLX, L, C, P1, Name, EType1),
   translateconstraints(Rest, L, TRest, P2, Name, EType),
   append(C, [ifthenelse([call('nonvar', [var('Tail')])],
             [call(Forall, [var('Tail'), functor(SSLX)])],
             ['true'])], P5),
   append(P1, P2, P).
% added next clause on June 6th 2003 for Anand
translateconstraints([equant(V, fromto(N, M), LX)|Rest], L,
                     [call(makelistfromto, [N, M, NtoM]),
                     call(Exists, [NtoM, functor(SLX)])|TRest],
                     [pred_clause(pred(Exists, [makelist([W,'|',var('Tail')]),functor(SLX)]),pred_body(P5))|P],
                     Name, EType):-
   cobexistspred(Exists),
   cobvar(W), subst(V, W, LX, SLX),
   cobvar(NtoM),
   cobvar(U), subst(W, U, SLX, SSLX),
%   typeof(E, Name, L, EType, TypeofE), append([att(user(TypeofE), var(W))], EType, EType1),
   !,
   translateconstraints(SLX, L, C, P1, Name, EType),
   translateconstraints(Rest, L, TRest, P2, Name, EType),
   P5 = [ifthenelse(C, ['true'],
                    [ifthenelse([call('nonvar', [var('Tail')])],
                                [call(Exists, [var('Tail'), functor(SSLX)])],
                                ['fail'])])],
   append(P1, P2, P).
%next clause is not similar to equivalent uquant clause...why??
translateconstraints([equant(V, E, LX)|Rest], L, [call(Exists, [E, functor(SLX)])|TRest],
   [pred_clause(pred(Exists, [makelist([W,'|', var('Tail')]), functor(SLX)]), pred_body(P5))|P], Name, EType):-
   cobexistspred(Exists), cobvar(W), subst(V, W, LX, SLX), cobvar(U), subst(W, U, SLX, SSLX),
   !, typeof(E, Name, L, EType, TypeofE), append([att(user(TypeofE), var(W))], EType, EType1),
   translateconstraints(SLX, L, C, P1, Name, EType1),
   translateconstraints(Rest, L, TRest, P2, Name, EType),
   P5 = [ifthenelse(C, ['true'],
                    [ifthenelse([call('nonvar', [var('Tail')])],
                                [call(Exists, [var('Tail'), functor(SSLX)])],
                                ['fail'])])],
   append(P1, P2, P).

 translateconstraints1([constraintPred(CPName,Terms)|Rest], L, CorrectedOrderofCalls, P, Name, EType) :-
   !, translateterms(Terms, L, TTerms, Cl1, P1, Name, _, EType),
   translateconstraints(Rest, L, TRest, P2, Name, EType), append(P1, P2, P),
append( [constraintPred(CPName, TTerms)],Cl1, Calls),
   append(Calls, TRest, CorrectedOrderofCalls).

translateconstraints1([not(constraintPred(CPName,Terms))|Rest], L, CorrectedOrderofCalls, P, Name, EType) :-
   !, translateterms(Terms, L, TTerms, Cl1, P1, Name, _, EType),
   translateconstraints(Rest, L, TRest, P2, Name, EType), append(P1, P2, P),
append( [not(constraintPred(CPName, TTerms))],Cl1, Calls),
   append(Calls, TRest, CorrectedOrderofCalls).
   translateconstraints1(Lit,L, P, P1,Name, EType) :-       translateconstraints(Lit, L, P, P1, Name, EType).

translateconstraints([constraintPred(CPName,Terms)|Rest], L, CorrectedOrderofCalls, P, Name, EType) :-
   !, translateterms(Terms, L, TTerms, Cl1, P1, Name, _, EType),
   translateconstraints(Rest, L, TRest, P2, Name, EType), append(P1, P2, P),
%  only change not in ./translate.plg
%  Calls appended in front of constraintPred(..,..) so that the arguments of dump/1 are unified to
%  their translated term before their values are printed.
%  changed order slightly on May 16th 2003.
%append([constraintPred(CPName, TTerms)],Cl1,Calls),
append(Cl1, [constraintPred(CPName, TTerms)], Calls),
   append(Calls, TRest, CorrectedOrderofCalls).



translateconstraints([compare(R, Term1, Term2)|Rest], L, [compare(R, T1, T2)|CTRest], P, Name, EType) :-
   !, translateterm(Term1, L, T1, Cl1, P1, Name, _, EType),
      translateterm(Term2, L, T2, Cl2, P2, Name, _, EType),
   translateconstraints(Rest, L, TRest, P3, Name, EType), append(Cl1, Cl2, Cl), append(Cl, TRest, CTRest),
   append(P1, P2, P4), append(P3, P4, P).
translateconstraints([bool(Term)|Rest], L, [bool(T)|CTRest], P, Name, EType) :-
   !, translateterm(Term, L, T, Cl, P1, Name, _, EType),
   translateconstraints(Rest, L, TRest, P2, Name, EType), append(Cl, TRest, CTRest),
   append(P1, P2, P).
 %Changed the order of translateconstraints
   translateconstraints([not(Constraint)|Rest], L, [not(FirstC)|Calls], P, Name, EType) :-
   !,
   translateconstraints([Constraint], L, [FirstC|TC], P1, Name, EType),
   translateconstraints(Rest, L, TRest, P2, Name, EType), append(TC, TRest, Calls), append(P1, P2, P).


% Added next clause on June 9th 2003 for Anand
translateconstraints([not(Constraint)|Rest], L, [FirstC, not(SecondC)|Calls], P, Name, EType) :-
   !,
   (Constraint =..[equant|_]; Constraint =..[uquant|_]),
   translateconstraints([Constraint], L, [FirstC, SecondC|TC], P1, Name, EType),
   FirstC =..[call, makelistfromto|_],
   translateconstraints(Rest, L, TRest, P2, Name, EType),
   append(TC, TRest, Calls),
   append(P1, P2, P).
%next 2 clauses from sunnyvale : % FirstC in next and above clauses assumes TC is empty ???

translateconstraints([pred(PName,Terms)|Rest], L, [pred(PName, TTerms)|Calls], P, Name, EType) :-
   !, translateterms(Terms, L, TTerms, Cl1, P1, Name, _, EType),
   translateconstraints(Rest, L, TRest, P2, Name, EType), append(Cl1, TRest, Calls), append(P1, P2, P).

translatePars([], [], []).
translatePars([Par|T], [V|VRest], [compare('=',V,Par)|PRest]) :-
	(atom(Par) -> V = Par
		   ;  cobvar(V)
	),
	translatePars(T,VRest,PRest).


%definition of translateliterals from sunnyvale
translateliterals([], _, [], [], [], _, _).
% next clause added on June 6th for Anand
translateliterals([Lit|RestL], L, Callsbefore, TLiterals, P3, Name, EType) :-
   Lit =..[EorUquant|_],
   (EorUquant = equant; EorUquant = uquant),
   translateconstraints([Lit], L, [FirstL, SecondL|TL], P1, Name, EType),
   FirstL =..[call, makelistfromto|_],
   translateliterals(RestL, L, Callsbefore1, TRestL, P2, Name, EType),
   append([SecondL], TRestL, TLiterals),
   append(P1, P2, P3),
   append([FirstL|TL], Callsbefore1, Callsbefore).
% next clause added on June 9th for Anand
translateliterals([Lit|RestL], L, Callsbefore, TLiterals, P3, Name, EType) :-
   Lit =..[not, EorUquantof|_],
   (EorUquantof =.. [equant|_]; EorUquantof =.. [uquant|_]),
   translateconstraints([Lit], L, [FirstL, SecondL|TL], P1, Name, EType),
   FirstL =..[call, makelistfromto|_],
   translateliterals(RestL, L, Callsbefore1, TRestL, P2, Name, EType),
   append([SecondL], TRestL, TLiterals),
   append(P1, P2, P3),
   append([FirstL|TL], Callsbefore1, Callsbefore).








%translateliterals([constraintPred(CPName,Terms)|Rest], L, Callsbefore, TLiterals, P3, Name, EType) :-
   % translateterms(Terms, L, TTerms, Cl1, P1, Name, _, EType),
   %translateliterals(RestL, L, Callsbefore1, TRestL, P2, Name, EType),
   %append( [constraintPred(CPName, TTerms)], TRestL, TLiterals), append(P1, P2, P3), append(Cl1, Callsbefore1, Callsbefore).

%translateliterals([not(constraintPred(CPName,Terms))|Rest], L, Callsbefore, TLiterals, P3, Name, EType) :-
    %translateterms(Terms, L, TTerms, Cl1, P1, Name, _, EType),
   %translateliterals(RestL, L, Callsbefore1, TRestL, P2, Name, EType),
   %append( [not(constraintPred(CPName, TTerms))], TRestL, TLiterals), append(P1, P2, P3), append(Cl1, Callsbefore1, Callsbefore).

translateliterals([Lit|RestL], L, Callsbefore, TLiterals, P3, Name, EType) :-
   translateconstraints1([Lit], L, [FirstL|TL], P1, Name, EType),
   translateliterals(RestL, L, Callsbefore1, TRestL, P2, Name, EType),
   append([FirstL], TRestL, TLiterals), append(P1, P2, P3), append(TL, Callsbefore1, Callsbefore).

%concatenate EType
%__________and, or, impl, not are added for formulae

%translateterm has 8 arguments.
%first argument is the input term to be translated
%second argument is the original parse tree (needed for attribute info of a class)
%third argument is the output translated term
%fourth argument is a list of calls that need to be made as a result of the translation
%fifth argument is a list of pedicate definitions that need to be made as a result of the translation
%sixth argument is the name of the current class which is being translated
%seventh argument is the type of the input term
%eighth argument is a list of tuples of quantifier variables and their types and
%the current translation is taking place within the scope of these quantifier variables.
translateterm(and(A, B), L, and(TA, TB), Calls, P, Name, 'Real', EType) :-
   !,
   translateterm(A, L, TA, Cl1, P1, Name, 'Real', EType),
   translateterm(B, L, TB, Cl2, P2, Name, 'Real', EType),
   append(P1, P2, P),
   append(Cl1, Cl2, Calls).
translateterm(or(A, B), L, or(TA, TB), Calls, P, Name, 'Real', EType) :-
   !,
   translateterm(A, L, TA, Cl1, P1, Name, 'Real', EType),
   translateterm(B, L, TB, Cl2, P2, Name, 'Real', EType),
   append(P1, P2, P),
   append(Cl1, Cl2, Calls).
translateterm(impl(A, B), L, impl(TA, TB), Calls, P, Name, 'Real', EType) :-
   !,
   translateterm(A, L, TA, Cl1, P1, Name, 'Real', EType),
   translateterm(B, L, TB, Cl2, P2, Name, 'Real', EType),
   append(P1, P2, P),
   append(Cl1, Cl2, Calls).
translateterm(not(A), L, not(TA), Calls, P, Name, 'Real', EType) :-
   !,
   translateterm(A, L, TA, Calls, P, Name, 'Real', EType).

translateterm(list([]), _, list([]), [], [], _, _, _).
translateterm(list(List), L, TList, Calls, P, Name, Type, EType) :-
   !,
   translateterms(List, L, TList, Calls, P, Name, Type, EType).
translateterm(enclosed(A), L, enclosed(TA), Calls, P, Name, Type, EType) :-
   !,
   translateterm(A, L, TA, Calls, P, Name, Type, EType).
%next clause added on June 12 2003
translateterm(negative(A), L, negative(TA), Calls, P, Name, Type, EType) :-
   !,
   translateterm(A, L, TA, Calls, P, Name, Type, EType).
translateterm(sub(A, B), L, sub(TA, TB), Calls, P, Name, 'Real', EType) :-
   !,
   translateterm(A, L, TA, Cl1, P1, Name, 'Real', EType),
   translateterm(B, L, TB, Cl2, P2, Name, 'Real', EType),
   append(P1, P2, P),
   append(Cl1, Cl2, Calls).
translateterm(add(A, B), L, add(TA, TB), Calls, P, Name, 'Real', EType) :-
   !,
   translateterm(A, L, TA, Cl1, P1, Name, 'Real', EType),
   translateterm(B, L, TB, Cl2, P2, Name, 'Real', EType),
   append(P1, P2, P), append(Cl1, Cl2, Calls).
translateterm(mult(A, B), L, mult(TA, TB), Calls, P, Name, 'Real', EType) :-
   !,
   translateterm(A, L, TA, Cl1, P1, Name, 'Real', EType),
   translateterm(B, L, TB, Cl2, P2, Name, 'Real', EType),
   append(P1, P2, P), append(Cl1, Cl2, Calls).
translateterm(divide(A, B), L, divide(TA, TB), Calls, P, Name, 'Real', EType) :-
   !,
   translateterm(A, L, TA, Cl1, P1, Name, 'Real', EType),
   translateterm(B, L, TB, Cl2, P2, Name, 'Real', EType),
   append(P1, P2, P),
   append(Cl1, Cl2, Calls).
translateterm(function(Fname, Pars), L, tfunc(Fname, TPars), Calls, P, Name, Type, EType) :-
   !,
   translateterms(Pars, L, TPars, Calls, P, Name, Type, EType).
translateterm(summation(V, fromto(N, M), Term), L, Y,
              [call(makelistfromto, [N, M, NtoM]),
               call(Sumover, [NtoM, functor(STerm), Y])],
              [pred_clause(pred(Sumover, [makelist([]), functor(STerm), const(0)])),
               pred_clause(pred(Sumover, [makelist([W, '|', var('Tail')]), functor(STerm),XXX]),
                           pred_body([compare(=,XXX,add(TTerm, var(Z))), ifthenelse([call('nonvar', [var('Tail')])],
                                                 [call(Sumover, [var('Tail'), functor(SSTerm), var(Z)])],
                                                                [compare(=, var(Z), const(0))])|Calls]))|P],
              Name, 'Real', EType) :-
	 cobvar(XXX),
   cobvar(Y), cobsumpred(Sumover), cobvar(W), subst(V, W, Term, STerm), cobvar(Z), cobvar(NtoM),
   cobvar(U), subst(W, U, STerm, SSTerm),
%   typeof(E, Name, L, EType, TypeofE), append([att(user(TypeofE), var(W))], EType, EType1),
   !, translateterm(STerm, L, TTerm, Calls, P, Name, 'Real', EType).
translateterm(summation(V, E, Term), L, Y, [call(Sumover, [E, functor(STerm), Y])],
              [pred_clause(pred(Sumover, [makelist([]), functor(STerm), const(0)])),
               pred_clause(pred(Sumover, [makelist([W, '|', var('Tail')]), functor(STerm),XXX]),  %var around Z might be redundant ?
	       pred_body([compare(=,XXX,add(TTerm, var(Z))),ifthenelse([call('nonvar', [var('Tail')])],
                                                 [call(Sumover, [var('Tail'), functor(SSTerm), var(Z)])],
                                                 [compare(=, var(Z), const(0))])|Calls]))|P],
              Name, 'Real', EType) :-
	      cobvar(XXX),
   cobvar(Y), cobsumpred(Sumover), cobvar(W), subst(V, W, Term, STerm), cobvar(Z),
   cobvar(U), subst(W, U, STerm, SSTerm),
   !, typeof(E, Name, L, EType, TypeofE), append([att(user(TypeofE), var(W))], EType, EType1),
   translateterm(STerm, L, TTerm, Calls, P, Name, 'Real', EType1).
%%%min max for fromto ?
translateterm(minimum(V, E, Term), L, Y, [call(Minover, [E, functor(STerm), Y])],
              [pred_clause(pred(Minover, [makelist([W]), functor(STerm), TTerm]), pred_body(Calls)),
               pred_clause(pred(Minover, [makelist([W, '|', var('Tail')]), functor(STerm), min(TTerm, var(Z))]),
                           pred_body([ifthenelse([call('nonvar', [var('Tail')])],
                                                 [call(Minover, [var('Tail'), functor(SSTerm), var(Z)])],
%else part is redundant since a non-ground Tail will match the first clause.
                                                 [compare(=, var(Z), TTerm)])|Calls]))
               |P], Name, 'Real', EType) :-
   cobvar(Y), cobminpred(Minover), cobvar(W), subst(V, W, Term, STerm), cobvar(Z),
   cobvar(U), subst(W, U, STerm, SSTerm),
   !, typeof(E, Name, L, EType, TypeofE), append([att(user(TypeofE), var(W))], EType, EType1),
   translateterm(STerm, L, TTerm, Calls, P, Name, 'Real', EType1).
translateterm(maximum(V, E, Term), L, Y, [call(Maxover, [E, functor(STerm), Y])],
              [pred_clause(pred(Maxover, [makelist([W]), functor(STerm), TTerm]), pred_body(Calls)),
               pred_clause(pred(Maxover, [makelist([W, '|', var('Tail')]), functor(STerm), max(TTerm, var(Z))]),
                           pred_body([ifthenelse([call('nonvar', [var('Tail')])],
                                                 [call(Maxover, [var('Tail'), functor(SSTerm), var(Z)])],
%else part is redundant since a non-ground Tail will match the first clause.
                                                 [compare(=, var(Z), TTerm)])|Calls]))
               |P], Name, 'Real', EType) :-
   cobvar(Y), cobmaxpred(Maxover), cobvar(W), subst(V, W, Term, STerm), cobvar(Z),
   cobvar(U), subst(W, U, STerm, SSTerm),
   !, typeof(E, Name, L, EType, TypeofE), append([att(user(TypeofE), var(W))], EType, EType1),
   translateterm(STerm, L, TTerm, Calls, P, Name, 'Real', EType1).
translateterm(ref(A, B), L, A_B, Calls, P, Name, TypeB, EType) :-
   !, translateterm(A, L, TA, Cl1, P1, Name, Type, EType), %typeof(TA, Name, L, AName),
   translateterm(B, L, TB, Cl2, P2, Type, TypeB, EType),
   append(P1, P2, P), append(Cl1, Cl2, Calls1),    %typeof(TA, Name, L, Type),
   attributesofclass(Type, L, Att), removetypedecl(Att, Att1),
   prefix(TA, Att1, TTAType), prefix(TA, TB, A_B),
%Calls1 is appended in front (in the next line) for efficiency reasons.
   append(Calls1, [compare('=', TA, dangling(TTAType))], Calls).
translateterm(ind(C, T), L, C_T, Calls, P, Name, TypeC, _) :-
   %typeof(C, _, L, _, TypeC), %isn't this incomplete ? e.g. A.I[5] will not be translated correctly
   !, translateterm(C, L, CT, Cl1, P1, Name, TypeC, _), % not sure if TypeC is correct
   cobvar(C_T), translateterm(T, L, TT, Cl2, P2, Name, int,_), append(P1, P2, P), append(Cl1, Cl2, Calls1),
%Calls1 is appended in front (in the next line) for efficiency reasons.
   translateindex(Calls1, CT,TT,C_T, Calls).

%translation of index is incomplete...write for each term.
% not sure why i wrote this case ??
%am commenting it on 4/30/03 and will remove it from here and ../CobDebug/translate.plg
%in due course of time if nothing goes wrong because of commenting it.
%translateterm(ind(C, N), _, C_N, [call('index', [C, N, C_N])], [], _, TypeC, _) :-
%   typeof(C, _, _, _, TypeC), %isn't this incomplete ? e.g. A.I[5] will not be translated correctly
%   prefix(C, N, C_N).
%commented next clause on 4/30/03 because I noticed it was being called for const(1,0) and such
%and I have just put a !, fail in typeof which together with the underlying clause on input where
%type of enumeration is not specified goes into infinite loop
translateterm(A, _L, A, [], [], _Name, real, _EType) :-
   A =..[const|_], !.
%Next clause needed for translating attribute names and getting their type for ref(A,B) kind of terms.
%Why dont I just call typeof(A,..) during translation of ref(A,B).
translateterm(A, L, A, [], [], Name, Type, EType) :-
   typeof(A, Name, L, EType, Type).
translateterm(A, _, A, [], [], _, _, _) :- !.

translateterms([], _, [], [], [], _, _, _).
translateterms([Par|Tail], L, [TPar|TTail], Calls, Preds, Name, Type, EType) :-
   translateterm(Par, L, TPar, C, P, Name, _, EType),
   translateterms(Tail, L, TTail, Cls, Pds, Name, Type, EType),
   append(C, Cls, Calls), append(P, Pds, Preds).

%for now only arrays are translated
%translatetypedecl(Attributes, TD).
translatetypedecl([att(array(_, N), var(V))|Rest],
                  [call('makearray', [N, V])|TRest]) :-
   translatetypedecl(Rest, TRest).
translatetypedecl([att(array(_, N), V)|Rest],
                  [call('makearray', [N, V])|TRest]) :-
   translatetypedecl(Rest, TRest).
translatetypedecl([_|Rest], TRest) :-
   translatetypedecl(Rest, TRest).
translatetypedecl([], []).


translateindex(Calls1,CT,TT,C_T, Calls) :-
       isCobvarorConst(TT), !,
      % append(Calls1, [call('index', [CT, TT, C_T])], Calls).
         append(Calls1, [call('index', [TT,CT, C_T])], Calls).
translateindex(Calls1,CT,TT,C_T, Calls) :-  % TT is some arithmetic expression
       cobvar(R), cobvar(I),
       append(Calls1, [compare('=',R, TT), r2i(R,I), call('index', [I,CT, C_T])], Calls).

      % append(Calls1, [compare('=',R, TT), r2i(R,I), call('index', [CT, I, C_T])], Calls).

isCobvarorConst(TT) :- atom(TT), !.
isCobvarorConst(const(N)) :- integer(N).


typeof(V, Name, L, _, Type) :-
   attributesofclass(Name, L, Att),
   revassoc(V, Att, Type).
typeof(V, _, _, EType, Type) :-
   revassoc(V, EType, Type).
%added next clause on 03.05.08
typeof(ind(_,_), _, _, _, array).
typeof(_V, _Name, _, _, _) :-
% had to comment because this message is displayed for each constructor argument
% since it doesn't have type declaration :-/
%   print('Error: Type for variable '), print(V),
%   print(' in class '), print(Name), print(' not specified.'),
   !, fail.

revassoc(V, [att(array(user(Type), _), var(V))|_], Type).
revassoc(V, [att(user(Type), var(V))|_], Type).
revassoc(V, [att(array(user(Type), _), V)|_], Type).
revassoc(V, [att(user(Type), V)|_], Type).
revassoc(V, [att(array(_, _), var(V))|_], _). % should this be array ?
revassoc(V, [att(array(_, _), V)|_], _). % should this be array ?
revassoc(V, [_|T], Type) :- revassoc(V, T, Type).

% prefix expects an Atom as the first argument, a list of Atoms as the second argument
prefix(_, [], []).
prefix([], _, []).
%change for sicstus : concat_atom changed to atom_concat
%prefix(V, [X|T], [V_X|PreT]) :-
%   concat_atom([V,'_'], V_), concat_atom([V_, X], V_X), (nonvar(T) -> prefix(V, T, PreT); PreT = T). % CORRECT ??
%prefix(V, X, V_X) :- concat_atom([V,'_'], V_), concat_atom([V_, X], V_X).
prefix(V, [X|T], [V_X|PreT]) :-
   atom_concat(V,'_', V_), atom_concat(V_, X, V_X), (nonvar(T) -> prefix(V, T, PreT); PreT = T). % CORRECT ??
prefix(V, X, V_X) :- atom_concat(V,'_', V_), atom_concat(V_, X, V_X).

subst(V, W, V, W).

%substitution in term
subst(V, W, enclosed(Term), enclosed(STerm)) :-
   subst(V, W, Term, STerm).
%next clause added on June 12 2003
subst(V, W, negative(Term), negative(STerm)) :-
   subst(V, W, Term, STerm).
subst(V, W, sub(A, B), sub(SA, SB)) :-
   subst(V, W, A, SA),
   subst(V, W, B, SB).
subst(V, W, add(A, B), add(SA, SB)) :-
   subst(V, W, A, SA),
   subst(V, W, B, SB).
subst(V, W, mult(A, B), mult(SA, SB)) :-
   subst(V, W, A, SA),
   subst(V, W, B, SB).
subst(V, W, divide(A, B), divide(SA, SB)) :-
   subst(V, W, A, SA),
   subst(V, W, B, SB).
subst(V, W, function(Fname, Pars), function(Fname, SPars)) :-
   subst(V, W, Pars, SPars).
subst(V, W, summation(V, E, Term), summation(V, E, STerm)) :-
   subst(V, W, Term, STerm).
subst(V, W, summation(X, E, Term), summation(X, E, STerm)) :-
   subst(V, W, Term, STerm). % shouldn't E be substituted ?
subst(V, W, ref(A, B), ref(SA, SB)) :-
   subst(V, W, A, SA),
   subst(V, W, B, SB).
subst(V, W, ind(C, N), ind(SC, SN)) :-
   subst(V, W, C, SC),
   subst(V, W, N, SN).
subst(V, W, [Par|Tail], [SPar|STail]) :-
   subst(V, W, Par, SPar),
   subst(V, W, Tail, STail).
subst(_, _, [], []).
subst(V, W, list(L), list(SL)) :-
   subst(V, W, L, SL).
subst(V, W, var(A), var(SA)) :-
   subst(V, W, A, SA).
%substitution in constraint : Incomplete
subst(V, W, compare(R, Term1, Term2), compare(R, STerm1, STerm2)) :-
   subst(V, W, Term1, STerm1),
   subst(V, W, Term2, STerm2).
subst(V, W, uquant(X, E, Term), uquant(X, SE, STerm)) :-
%next line added on 03.05.08
   subst(V, W, E, SE),
   subst(V, W, Term, STerm). % shouldn't X be substituted ?
subst(V, W, equant(X, E, Term), equant(X, SE, STerm)) :-
%next line added on 03.05.08
   subst(V, W, E, SE),
   subst(V, W, Term, STerm). % shouldn't X be substituted ?
subst(V, W, condConstr(A,B), condConstr(SA, SB)) :-
   subst(V, W, A, SA),
   subst(V, W, B, SB).
%Name and SName will be same...
subst(V, W, new(T1, Name, Args), new(ST1, SName, SArgs)) :-
   subst(V, W, T1, ST1),
   subst(V, W, Name, SName),
   subst(V, W, Args, SArgs).
%Name will not change
subst(V, W, constraintPred(Name, L), constraintPred(Name, SL)) :-
   subst(V, W, L, SL).
% Added next clause on June 9th 2003 for Anand
subst(V, W, not(C), not(SC)) :-
   subst(V, W, C, SC).
subst(_, _, A, A).

cobvar(X) :- gensym('Cob', X).
cobforallpred(X) :- gensym(coball, X).
cobexistspred(X) :- gensym(cobexists, X).
cobsumpred(X) :- gensym(cobsum, X).
cobminpred(X) :- gensym(cobmin, X).
cobmaxpred(X) :- gensym(cobmax, X).

%what about case when N and/or M are vars ??

% chaged for sicstus : gensym not provided in sicstus, hence giving
% defn of gensym which requires user_concat, inc_counter_for and asserting counters
% for internal variables/predicates
gensym(A, B) :-
   inc_counter_for(A, I),
   user_concat_atom_number(A, I, B).

user_concat_atom_number(Atom, N, AN) :-
   number_codes(N, Ncodes),
   atom_codes(Natom, Ncodes),
   atom_concat(Atom, Natom, AN).

inc_counter_for(A, I) :-
   counter(A, Current),
   I is Current + 1,
   retractall(counter(A,_)),
   assert(counter(A,I)), !.

% set counters of internal variables/predicates to 1
:- assert(counter(coball, 1)).
:- assert(counter('Cob', 1)).
:- assert(counter(cobexists, 1)).
:- assert(counter(cobsum, 1)).
:- assert(counter(cobmin, 1)).
:- assert(counter(cobmax, 1)).
% Feb 11 2002 - Feb 27 2002. This file has been modified to make the translation of sum,forall constraints result
% in more efficient CLPR code. This is done by passing a list of variables as the context rather than
% the functor containing the entire constraint.
% Aug 2002 - changes for porting to SICStus

prettyprint([pred(Name, [], AttList, [])|T]) :-
        write(Name), write('('), write([]), write(','), write(AttList), write(')'), write('.'), nl, nl,
        prettyprint(T).
prettyprint([pred(Name, Att, AttList, ConstraintList)|T]) :-
        write(Name), write('('), write(Att), write(','), write(AttList), write(')'), write(':-'), nl,
        ppconstraintlist(ConstraintList), write('.'), nl, nl,
        prettyprint(T).
%WRITE A BETTER CASE THAN THE NEXT (IMMEDIATELY FOLLOWING ONE)
prettyprint([pred_clause(Head, pred_body([]))|T]) :-
        pphead(Head), write('.'), nl, nl,
        prettyprint(T).
prettyprint([pred_clause(Head, Body)|T]) :-
        pphead(Head), write(':-'), nl,
        ppbody(Body), write('.'), nl, nl,
        prettyprint(T).
prettyprint([pred_clause(Head)|T]) :-
        pphead(Head), write('.'), nl, nl,
        prettyprint(T).
prettyprint([class_predicates(Predicates)|T]) :-
	ppclasspredicates(Predicates),
        prettyprint(T).
prettyprint([]).


ppconstraintlist([]). %not correct
ppconstraintlist([X])   :- tabs(1), ppconstraint(X).
ppconstraintlist([X|T]) :-  ppconstraint2(X), ppconstraintlist(T).


% For Swipl support Insert bracket in for constrinats-
ppconstraint2(compare('=', Term1, Term2)) :-
	Term1 == Term2, !. % do nothing.
ppconstraint2('true') :- write('true').
ppconstraint2(X) :- tabs(1), ppconstraint(X), write(','), nl.

ppconstraint('true') :- write('true').
ppconstraint(compare(R, Term1, Term2)) :-
amoper(Term1),write('{'), ppterm(Term1), write('  '), write(R), write('  '), ppterm(Term2), write('}') . % modified
ppconstraint(compare(R, Term1, Term2)) :-  amoper(Term2),write('{'), ppterm(Term1), write('  '), write(R), write('  '), ppterm(Term2), write('}'). % modified
ppconstraint(compare(R, Term1, Term2)) :- relop(R), R \== '=', write('{'), ppterm(Term1), write('  '), write(R), write('  '), ppterm(Term2),  write('}').
ppconstraint(compare(R, Term1, Term2)) :-  ppterm(Term1), write('  '), write(R), write('  '), ppterm(Term2).

% for sicstus, dump must be changed to print
ppconstraint(constraintPred(dump, [X]))  :- dump2print(X).
ppconstraint(constraintPred(Name, X))  :- write(Name), write('('), ppterms(X), write(')').
ppconstraint(call(Name, Att))          :- write(Name), write('('), ppterms(Att), write(')').
ppconstraint(condConstr(Constraint, Literals)) :-
       write('conditional_constraint'),
       write('('), write('('), ppconstraintlist(Constraint), write(')'), write(','),
                   write('('), ppliterals(Literals), write(')'), write(')').
ppconstraint(pred(Name, Att)) :- ppconstraint(call(Name, Att)).
%changed not to \+ for SICStus
%ppconstraint(not(P)) :- write('not('), ppconstraint(P), write(')').
%How was the next clause working?? probably wasn't but didn't notice it
%ppconstraint(not(P)) :- write('\+ '), ppconstraint(P).
ppconstraint(not(P)) :- write('\\+  '), ppconstraint(P).
ppconstraint(bool(P)) :- ppterm(P).
%expand write(Literals)
ppconstraint(ifthenelse(Bool, If, Else)) :-
   write('('), write('('), ppconstraintlist(Bool), write(')'), write('->'),
   write('('), ppconstraintlist(If), write(')'), write(';'),
   write('('), ppconstraintlist(Else), write(')'), write(')').
%ppconstraint(ifthenelse(Bool, If, Else)) :-
%   write('('), ppconstraint(Bool), write('->'), ppconstraintlist(If), write(';'), ppconstraintlist(Else), write(')').
ppconstraint(builtinclpr(X)) :- write(X).
ppconstraint(X):- write(X). %from sunnyvale


amoper(mult(_,_)). %added
amoper(divide(_,_)). %added
amoper(sub(_,_)). %added
amoper(add(_,_)). %added
amoper(pow(_,_)). %added


dump2print([]) :- print('true'). % dummy statement to end recursion without printing a comma
dump2print([X|T]) :- write('print(\''), ppterm(X), write(' =  \'), '),
		     ppterm('print('), ppterm(X), write('), '),
                     write('nl, '), dump2print(T).


ppterms([X]) :- ppterm(X).
ppterms([X|T]) :- ppterm(X), write(','), ppterms(T).

%ppterm(and(X,Y)) :- ppterm(X), write('^'), ppterm(Y).
%ppterm(or(X,Y)) :- ppterm(X), write('V'), ppterm(Y).
%ppterm(not(X)) :- write('~'), write('(')), ppterm(X), write(')').

ppterm(const(C)) :-
   !, write(C).
ppterm(const(M,N,Z)) :-
   !, write(M), write('.'), writeleading0(Z),write(N).
% next clause added on June 12 2003
ppterm(quotedString(S)) :-
   !, write('\''), write(S), write('\'').
ppterm(var(V)) :-
   !, write(V).
ppterm(enclosed(T)) :-
   !, write('('), ppterm(T), write(')').
%next clause added on June 12 2003
ppterm(negative(T)) :-
   !, write('-'), ppterm(T).
ppterm(function(Name, X)) :-
   !, write(Name),  write('('), ppterms(X), write(')').
ppterm(tfunc(Name, X)) :-
   !, write(Name),  write('('), ppterms(X), write(')').
% space before and after * is necessary for correct printing of unary - operator (June 2003)
ppterm(mult(X, Y)) :-
   !, ppterm(X), write(' * '), ppterm(Y).
% space before and after * is necessary for correct printing of unary - operator (June 2003)
ppterm(add(X, Y)) :-
   !, ppterm(X), write(' + '), ppterm(Y).
% space before and after * is necessary for correct printing of unary - operator (June 2003)
ppterm(divide(X, Y)) :-
   !, ppterm(X), write(' / '), ppterm(Y).
% space before and after * is necessary for correct printing of unary - operator (June 2003)
ppterm(sub(X, Y)) :-
   !, ppterm(X), write(' - '), ppterm(Y).
% space before and after * is necessary for correct printing of unary - operator (June 2003)
ppterm(pow(X, Y)) :-
   !, ppterm(X), write(' ^ '), ppterm(Y).
ppterm(functor(X)) :-
   extractvars(X, V),
   bagtoset(V, [], SV),
   removenumbers(SV, AV),
   ppterm(list(AV)).
%ppterm(functor(X)) :- !, write(X).
ppterm([X|T]) :-
   write('['), ppterms([X|T]), write(']').
%fix Jinesh
ppterm([]):-write('[]').
%ppterm([]).
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
%ppmakelistterms([X]) :- ppterm(X).
%ppterm("").

%pprin((A or B), N)  :-  !, pprin(A,N), write('  V'), nl, pprin(B,N).
%pprin((A and B), N) :-  !, pprin(A,N), write('  &'), nl, pprin(B,N).
%pprin((A --> B), N) :-  !, pprin(A,N), nl, tabs(N), write('-->'), nl,
%                           M is N+1,
%                           pprin(B,M).

%pprin(A, N) :-  tabs(N), write(A).

pphead(pred("",_)).
pphead(pred(Name, Terms)) :-
   write(Name),
   write('('), ppterms(Terms), write(')').
ppbody(pred_body(Literals)) :-
   ppliterals(Literals).

ppliterals([X]) :-
   tabs(1),
   ppliteral(X).
ppliterals([X|T]) :-
   tabs(1),
   ppliteral(X), write(','), nl,
   ppliterals(T).

ppliteral(X):- ppconstraint(X).

tabs(0) :- !.
tabs(N) :- write('   '), M is N-1, tabs(M).

pptokens(_, 0).
pptokens([X|T], N) :-
   print(X), print("   "),
   N1 is N-1,
   pptokens(T, N1).

ppclasspredicates([]).
ppclasspredicates([L|Rest]) :-
   pponeclasspredicate(L), nl,
   ppclasspredicates(Rest).

pponeclasspredicate([]).
pponeclasspredicate([id(X), '('|Rest]) :-
   write(X), write('('),
   pponeclasspredicate(Rest).
pponeclasspredicate([id(X)|Rest]) :-
   write(X), write('  '),
   pponeclasspredicate(Rest).
pponeclasspredicate(['('|Rest]) :-
   write('('),
   pponeclasspredicate(Rest). %from sunnyvale
pponeclasspredicate(['<','>'|Rest]) :-
   write('<>'), write('  '),
   pponeclasspredicate(Rest).
pponeclasspredicate([num(X)|Rest]) :-
   write(X), write('  '),
   pponeclasspredicate(Rest).
pponeclasspredicate([dnum(N,M,Z)|Rest]) :-
   write(N), write('.'), writeleading0(Z), write(M), write('  '),
   pponeclasspredicate(Rest).
pponeclasspredicate([const(N,M,Z)|Rest]) :-
   write(N), write('.'), writeleading0(Z), write(M), write('  '),
   pponeclasspredicate(Rest).
% next clause added on June 12 2003
pponeclasspredicate([quotedString(S)|Rest]) :-
   write('\''), write(S), write('\''), write('  '),
   pponeclasspredicate(Rest).
pponeclasspredicate([L|Rest]) :-
   write('  '), write(L),
   pponeclasspredicate(Rest). %from sunnyvale

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




% ------------------ SYNTAX ANALYZER FOR COB ---------------------------

% This is the Cob to CLPR Translator, written by Pallavi Tambay
% with minor changes by Bharat Jayaraman.



parse(Tokens, ParseTree) :- resetclassnamecounter(_), resetkeywordcounter(_), program(ParseTree, Tokens, []), !.
parse(_,[]) :- print('Syntax error in class '), classnamecounter(C), print(C), nl,
               print('while parsing '), parsing(X), print(X),
               keywordclausecounter(Y), print(Y), nl, fail.


program([X|T]) --> class_definition(X), {!}, program(T).
program([]) --> [].

class_definition(
   classdef(Name, Superclass, Attributes, Constraints, Functions,
   Predicates, Preferences, Methods, Constructors, Num_instances))
   --> [class], class_id(Name), ['{'], {!}, {setclassnamecounter(Name)},
   body(Attributes, Constraints, Functions, Predicates, Preferences,
   Methods, Constructors), ['}'], {Num_instances = 0, Superclass = ""}.
class_definition(
   classdef(Name, Superclass, Attributes, Constraints, Functions,
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
   classdef(Name, Superclass, Attributes, Constraints, Functions,
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

type(primitive(int)) --> [id('int')].
type(primitive(real)) --> [id('real')].
type(primitive(char)) --> [id('char')].
type(primitive(string)) --> [id('string')].
type(primitive(bool)) --> [id('bool')].
type(primitive(formula)) --> [id('formula')].
type(primitive(expr)) --> [id('expr')].

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
constraint_atom(compare(=, Term1, Term2)) -->
   boolexpr(Term1),
   [=],
   boolexpr(Term2). %bool() around Term1 and Term2 in head ?
constraint_atom(bool(B)) -->
   boolexpr(B).

conditional_constraint(condConstr(Constraint, Literals)) -->
   constraint_atom(Constraint),[:-], {!},
   literals(Literals).

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

listmem('_') --> ['_'].
listmem(X) --> [id(X)].

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
attribute(In, In) --> [], {\+(In = ['$'])}.

selector(In, X) --> attribute_id(X), {In = ['$']}.
selector(In, Ind) --> attribute_id(X), ['['], terms(T), [']'], {In = ['$']}, {!}, {make_ind(X, T, Ind)}.
selector(In, sel(Name, X)) --> selector_id(Name), {!}, terms(X), {In = ['$']}.
       % Will this occur only at the
       % beginning of an attri ?
       % Is x.first(y) possible ?
       % If not then change grammar.
       % following clauses of selector will be used after occurance of a "."
selector(In, Ind) --> attribute_id(X), ['['], terms(T), [']'], {!},
			{make_ind(ref(In, X), T, Ind)}, {\+(In = ['$'])}.
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

%fix Jinesh
terms([])  -->[].
terms([X]) --> term(X).
terms([X|T]) --> term(X), [','], {!}, terms(T). %make sure this cut is correct

literals([L]) --> literal(L).
literals([L|T]) --> literal(L), [','], literals(T).

literal(builtinclpr('nl')) --> [id('nl')], {!}.
literal(not(A)) --> [not], atom(A).
literal(A) --> atom(A).
%Allowing for constraint-based info retrieval proj (sensors, targets)...Anand Ganesh
literal(C) --> constraint(C).
literal(not(C)) --> [not], constraint(C).

%fix commented
%atom(pred(Name, X)) --> predicate_id(Name), ['('], terms(X), [')']. %incorrect
atom(X) --> constraint_atom(X).

class_id(X) --> [id(X)].
attribute_id(X) --> [id(X)].
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


constructor_clauses([constructor(Name, [], ConstructorConstraintList)|T]) -->
   class_id(Name), {seterrormessage(_), namematch(Name)}, ['('], [')'], ['{'], %resetkeywordclausecounter(_)},
   constraint_list(ConstructorConstraintList), ['}'], constructor_clauses(T). %correct call to id_list
constructor_clauses([]) --> [].
constructor_clauses([constructor(Name, Attributes, ConstructorConstraintList)|T]) -->
   class_id(Name), {seterrormessage(_), namematch(Name)}, ['('], id_list(_,Attributes), [')'], ['{'], %resetkeywordclausecounter(_)},
   constraint_list(ConstructorConstraintList), ['}'], constructor_clauses(T). %correct call to id_list


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

special('->',[45,62|L],L). %order matters otherwise - and > will be made separate tokens
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

%enumerated(fromto(N, M)) --> digits(num(N)), specialenum(_), digits(num(M)), {!}.
%This file has been modified to incorporate logic formulae (in order to accept Cob representation of flowcharts).
%This file has been modified to accept constant sized array declarations
%(in addition to accepting arrays of unspecified size as before)
%:- op(700, xfx, ':=').
%:- op(600, yfx, and).
%:- op(600, yfx, or).
%:- op(500, fx,  not).



