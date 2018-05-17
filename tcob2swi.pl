%TCOB translator
%Authors: Bharat Jayaraman,Pallavi,Jinesh

:- module(tcob2cob, [tcob2swi/2,cob2swi/1]).
:- use_module(library(lists)).

%--Main predicate-
%--Takes TCOB program and driver class as the input
tcob2swi(File,Driver) :- atom_codes(File, FullCodes),
   atom_codes('.tcob',S),
   append(FileCodes, S ,FullCodes),
   atom_codes(Prefix,FileCodes),
   atom_concat(Prefix, '.cob', CobOutput),
   tcob2cob(File,Driver),!,cob2swi1(CobOutput,1).
   
%---TCOB To COB translation
tcob2cob(File,Driver) :-
   atom_codes(File, FullCodes),
   atom_codes('.tcob',S),
   append(FileCodes, S ,FullCodes),
   atom_codes(Prefix,FileCodes),
   atom_concat(Prefix, '.cob', Output),
   tcob2cob(File,Output,Driver).
   
tcob2cob(File,Output,Driver) :-
   open(File,read,Stream),
   process(Stream, Output,Driver).

process(Stream, Output,Driver) :-
   tell(user_output),
   lex(Stream,Tokens), !,
   parsetcob(Tokens, ParseTree), !,
   tell(Output),
   translatetcob(ParseTree,Driver),!,nl,write('$'),
   told.
   
%---translate the program and write main class
translatetcob([time(X,Y,Z,D)|T],Driver):- 
   retractall(endsimtime(_)),retractall(debug_option(_)),
   assert(endsimtime(Y)),assert(debug_option(D)),translatetcobprog(T,T,Z),processmain(Driver,[X,Y]),
   retractall(endsimtime(_)).
   
%---translate each class(X) of the program
translatetcobprog([],_,_).
translatetcobprog([X|T],ParseTree,SkipList) :-
   translatetcobclass(X,ParseTree,SkipList),!,nl,nl,
   translatetcobprog(T,ParseTree,SkipList).
   
%---translate each TCOB Class
translatetcobclass(classdef(Name,Superclass, Attributes, Constraints,
  Predicates, Constructors,Debug, _),ParseTree,SkipList) :-
   writeclass(Name),processuperclass(Superclass),
   write('{'),nl,processtcobattr(Attributes),
   getclassattr(Superclass,ParseTree,CLB),!,append(Attributes,CLB,Attr),
   processtcobconstri(ParseTree,Constraints,Attr,Mtoconstraints,Name,SkipList,Debug),
   %processdebuginfo(,Attributes),
   processtcobpredicates(Predicates,Mtoconstraints,ParseTree,Attr),
   processtcobconstructor(Constructors),nl,write('}').
%---Write class name to COB program
writeclass(abstract(Name)):-
   write('abstract class '),write(Name).
writeclass(Name) :-
   !,write('class '),write(Name).

processuperclass([]):- !.
processuperclass(""):- !.
processuperclass(Name) :-
   write(' extends '),!,write(Name).
   
%---Write attributes to COB program, replace series variable with array representation
processtcobattr([]):- !.
processtcobattr(X) :-
   write('attributes'),nl,!, processtcobattr1(X),nl.
processtcobattr1([]).
processtcobattr1([att(Type,Var)|T]) :-
   tab(1),getdecl(Type,Decl),dumpprint(Decl),write(Var),
   write(';'),nl,!,processtcobattr1(T).
   
%---Extract attributes from parsetree
getclassattr(_,[],[]).
getclassattr(N,[classdef(N,_,Attr,_,_,_,_,_)|_],Attr).
getclassattr(N,[classdef(abstract(N),_,Attr,_,_,_,_,_)|_],Attr).
getclassattr(N,[_|T],Attr):-
   getclassattr(N,T,Attr).
   
%---Write constraints to COB program, 
processtcobconstri(_,[],_,_,_,_,[]):-!.
processtcobconstri(_,[],A,_,_,_,Debug):-
    write('constraints'),!,nl,processdebuginfo(Debug,A).
processtcobconstri(PT,C,A,P,N,T,Debug) :-
   write('constraints'),!,nl, processdebuginfo(Debug,A),processtcobconstri1(PT,C,A,P,N,T),nl.

%---Process debug information, if debug=yes
processdebuginfo([],_).
processdebuginfo([X|T],A) :- 
  debug_option(V),(V=yes -> tabs(1),write('logvar(Time,\''),write(X),write('\','),
  ptterm(X,A),write(',ObjN);'),nl,processdebuginfo(T,A);true).

%---Process constraints in TCOB class
processtcobconstri1(_,[],_,_,_,_) :-!.
processtcobconstri1(ParseTree,Constraints,Attributes,Pred,Name,SkipList) :-
   handletimeloop(ParseTree,Constraints,Attributes,Pred,Name,SkipList).
   
%---write predicates to TCOB class, which include mto predicates extracted from TCOB class
processtcobpredicates([],[],_,_).
processtcobpredicates([class_predicates(X)],Y,PT,Attr) :-
   write('predicates'),nl, writepredicates1(X),writemtopredicates(Y,PT,Attr),nl.
processtcobpredicates([],Y,PT,Attr) :-
   write('predicates'),nl, writemtopredicates(Y,PT,Attr),nl.

%-- write constructors to COB file
processtcobconstructor([]).
processtcobconstructor(X):-
   write('constructors'),nl,writeconstructor1(X).
   
%---Identify the type of attributes from parse tree
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
getdecl(array(primitive(Type), [Size1,1]),[Type,'  [',Size1,'][]'] ) .
getdecl(array(primitive(Type), [Size1,Size2]),[Type,'  [',Size1,'][',Size2,']'] ) .
getdecl(array(user(Type), [Size1,Size2]),[Type,'  [',Size1,'][',Size2,']'] ) .
getdecl(array(primitive(Type), Size),[Type,' ',Size] ) .
getdecl(array(user(Type),Size),[Type,' ', Size]).


%---Check time loop skip list, if not in the list, process with Time variable
handletimeloop(PT,Constraints,Attributes,Mtopredlist,Name,SkipList) :-
   ( checkSkipList(SkipList,Name) ->  pptcobconstraintlist(PT,Constraints,_,_);
   tab(1),nl,tab(1),getmtoconstraints(Constraints,Mtopred),
   delete(Mtopred,[],Mtopredlist),
   pptcobconstraintlist(PT,Constraints,Attributes,Mtopredlist)),write(';').

writepredicates1([]).
writepredicates1([X|T]) :-
   dumpprint(X),nl,writepredicates1(T).

%-- write constructor to COB file, add additional 'Time' argument as constructor parameter
writeconstructor1([]).
writeconstructor1([constructor(Name,AL,C)|T]) :-
   write(Name),write('( Time'),addcomma(AL),writeargument(AL),write('){'),
   pptcobconstraintlist(_,C,_,_),addsemic(C),write('}'),nl,writeconstructor1(T).
   
%----Write main class for time based simulation
processmain(Driver,[S,E]) :-
   atom_codes(Driver,X),tokenize(X,Out),
   getdriver(driver(N,Attr),Out,[]),write('class main'),nl,write('{'),
   nl,tabs(1),write('attributes' ), nl,tabs(2),write(N),write(' Main;'),
   nl, tabs(1),write('constructor main'),write('(' ),writeargument(Attr),
   write(')'),write('{'),nl,
   write('forall Time in '), write(S),write('..'),
   write(E),write(':('),nl,
   write('Main = new '),write(N),write('(Time'),  
   addcomma(Attr),writeargument(Attr),write(')'),
   write(');'),nl,write('}'),nl,write('}').

%---Process constraints list, contain Parse tree and attributes for series variable 
%---collect Mto  constraints
pptcobconstraintlist(_,[],[],_). 
pptcobconstraintlist(PT,[X],Attributes,Mto):-
   tabs(1), ptconstraint(PT,X,Attributes,Mto).
pptcobconstraintlist(PT,[X|T],Attributes,Mto) :-
   ptconstraint2(PT,X,Attributes,Mto), !,tabs(1),
   pptcobconstraintlist(PT,T,Attributes,Mto).
   
%---Get MTO constraints from constraints list
getmtoconstraints([],[]).
getmtoconstraints([X|T],MtoC) :-
   getmtoconstraints(X,MtoC1),getmtoconstraints(T,MtoC2),append(MtoC1,MtoC2,MtoC).
getmtoconstraints(mto('G',X,C),[dcobG(N,X,C)]) :-
   tcobgpred(N).
getmtoconstraints(mto('F',X,C),[dcobF(N,X,C)]) :-
   tcobfpred(N).
getmtoconstraints(condConstr(Con,Lit),MtoC) :-
   getmtoconstraints(Con,MtoC1), getmtoconstraints(Lit,MtoC2),append(MtoC1,MtoC2,MtoC).
getmtoconstraints(_,[]).

addcomma([]).
addcomma(_) :-
   write(', ').

 %--print for predicates in TCOB class  
dumpprint([]).
dumpprint([[H]]) :-
   integer(H),write('['),write(H),write(']').
dumpprint([H|T]) :-
   ptterm(H,[]),dumpprint(T).
   
%---Process argument list for constructor and predicates
writeargument([]).
writeargument([att(_,X)|T]) :-
   write(X),writeargument2(T).
writeargument2([]).
writeargument2(T):-
   write(','),writeargument(T).
writeargumentlist([]).
writeargumentlist([X]) :-
   ppterm(X).
writeargumentlist([X|T]) :-
   ppterm(X),!,writeargumentlist2(T).
writeargumentlist(X):-
   ppterm(X).
writeargumentlist2([]).
writeargumentlist2(T):-
   !,write(','),writeargumentlist(T).
   
%Check the class in time loop skip list
checkSkipList([Name|_],Name).
checkSkipList([_|T],Name) :-
   checkSkipList(T,Name).

%--add semicolon 
addsemic([]).
addsemic(_):- write(';').

%---Different types constraints in TCOB program
ptconstraint2(_,compare('=', Term1, Term2),_,_) :-
   Term1 == Term2, !.
ptconstraint2(_,'true',_,_) :-
   write('true').
ptconstraint2(PT,X,Attr,Mto) :-
   ptconstraint(PT,X,Attr,Mto), write(';'), nl.

ptconstraint(PT,compare(R, Term1, Term2),Attr,_) :-
   ptconstraint(PT,Term1,Attr,_), write('  '), write(R),
   write('  '),    ptconstraint(PT,Term2,Attr,_).
ptconstraint(PT,mult(Term1, Term2),Attr,_) :-
   ptconstraint(PT,Term1,Attr,_), write('  '), write('*'),
   write('  '),ptconstraint(PT,Term2,Attr,_).
ptconstraint(PT,add( Term1, Term2),Attr,_) :-
   ptconstraint(PT,Term1,Attr,_), write('  '), write('+'),
   write('  '),ptconstraint(PT,Term2,Attr,_).
ptconstraint(PT,sub(Term1, Term2),Attr,_) :-
   ptconstraint(PT,Term1,Attr,_), write('  '), write('-'),
   write('  '),ptconstraint(PT,Term2,Attr,_).
ptconstraint(PT,divide(Term1, Term2),Attr,_) :-
   ptconstraint(PT,Term1,Attr,_), write('  '), write('/'),
   write('  '),ptconstraint(PT,Term2,Attr,_).
ptconstraint(PT,enclosed(Term1),Attr,_) :-
   write(' ( '), ptconstraint(PT,Term1,Attr,_),write(')').
ptconstraint(PT,negative(Term1),Attr,_) :-
   write(' - '), ptconstraint(PT,Term1,Attr,_).
ptconstraint(PT,function(Name, X),Attr,_) :-
   !, write(Name),  write('('), ptconstraint(PT,X,Attr,_), write(')').
ptconstraint(PT,[X],Attr,_) :-
   !, ptconstraint(PT,X,Attr,_).
ptconstraint(PT,[X|T],Attr,_) :-
   !, ptconstraint(PT,X,Attr,_),write(','),ptconstraint(PT,T,Attr,_).
ptconstraint(PT,constraintPred(N,X),A,_) :-
   write(N),write('('),ptpredargument(PT,X,A),write(')').
   
%---Replace mto constraints with corresponding predicate call
ptconstraint(PT,mto('G',time(Start,End),C),A,P) :-
   getgpredicatename(P,C,N), write(N),write('('),
   ptconstraint(PT,Start,A,_),write(','),ptconstraint(PT,End,A,_),
   write(',Time,['),getvars(PT,A,C,Vars),writeargumentlist(Vars),write('])').
ptconstraint(PT,mto('F',time(Start,End),C),A,P) :-
   getfpredicatename(P,C,N),  write(N),
   write('('),ptconstraint(PT,Start,A,_),write(','),ptconstraint(PT,End,A,_),
   write(',Time,['),getvars(PT,A,C,Vars),writeargumentlist(Vars),write('])').
ptconstraint(PT,mto('G',time(Start),C),A,P) :-
   getgpredicatename(P,C,N),  write(N), write('('),ptconstraint(PT,Start,A,_),
   write(',Time1'),write(',Time,['),getvars(PT,A,C,Vars),writeargumentlist(Vars),
   write('])').
ptconstraint(PT,mto('F',time(Start),C),A,P) :-
   getfpredicatename(P,C,N),  write(N), write('('),ptconstraint(PT,Start,A,_),
   write(',Time,['),getvars(PT,A,C,Vars),writeargumentlist(Vars),write('])').
ptconstraint(PT,mto('F',[],C),A,P) :-
   getfpredicatename(P,C,N),  write(N),  endsimtime(Y),write('(Time,'),Y1 is Y+1,write(Y1),write(',0,['),
   getvars(PT,A,C,Vars),writeargumentlist(Vars),write('])').
ptconstraint(PT,mto('G',[],C),A,P) :-
   getgpredicatename(P,C,N),  write(N), endsimtime(Y),write('(Time,'),Y1 is Y+1,write(Y1),write(',0,['),
   getvars(PT,A,C,Vars),writeargumentlist(Vars),write('])').
   
%---To process series variable in reference,extract attributes from refered object 
ptconstraint(PT,ref(ind(X,I),V),Attr,_):-
   getobjattr(PT,X,Attr,Attributes),append(Attributes,Attr,NewAttr),
   ptterm(ind(X,I),NewAttr),write(.),ptterm(V,NewAttr).  
ptconstraint(_,ref(prev(X),V),Attr,_):-
   ptterm(ref(prev(X),V),Attr).
ptconstraint(_,ref(next(X),V),Attr,_):-
   ptterm(ref(next(X),V),Attr).
ptconstraint(PT,ref(X,V),Attr,_):-
   getobjattr(PT,X,Attr,Attributes),append(Attributes,Attr,NewAttr),
   ptterm(X,Attr),write(.),ptterm(V,NewAttr).
ptconstraint(PT,not(C),Attr,_) :-
   write('not('),ptconstraint(PT,C,Attr,_),write(')').
   
%---Quantified constraint handler
ptconstraint(PT,uquant(I,fromto(X,Y),C),Attr,Mto) :-
   write('forall '),write(I),write(' in '), write(X),write('..'),
   write(Y),write(':('),pptcobconstraintlist(PT,C,Attr,Mto),write(')').
ptconstraint(PT,uquant(I,Obj,C),Attr,Mto) :-
   write('forall '),write(I),write(' in '), ptterm(Obj,Attr),write(':('),
   getobjattr(PT,Obj,Attr,Attributes),append(Attributes,Attr,NewAttr),
   pptcobconstraintlist(PT,C,NewAttr,Mto),write(')').
ptconstraint(PT,equant(I,fromto(X,Y),C),Attr,Mto) :-
   write('exists '),write(I),write(' in '), write(X),write('..'),
   write(Y),write(':('),pptcobconstraintlist(PT,C,Attr,Mto),write(')').
ptconstraint(PT,equant(I,Obj,C),Attr,Mto) :-
   write('exists  '),write(I),write(' in '), ptterm(Obj,Attr),write(':('),
   getobjattr(PT,Obj,Attr,Attributes),append(Attributes,Attr,NewAttr),
   pptcobconstraintlist(PT,C,NewAttr,Mto),write(')').
ptconstraint(PT,enclosed(summation(In,Obj,C)),Attr,Mto) :-
   write('sum '),write(In),write(' in '),ptterm(Obj,Attr),write(':('),
   getobjattr(PT,Obj,Attr,Attributes),append(Attributes,Attr,NewAttr),
   ptconstraint(PT,C,NewAttr,Mto),write(')').
ptconstraint(PT,summation(In,Obj,C),Attr,Mto) :-
   write('sum '),write(In),write(' in '), ptterm(Obj,Attr),write(':('),
   getobjattr(PT,Obj,Attr,Attributes),append(Attributes,Attr,NewAttr),
   ptconstraint(PT,C,NewAttr,Mto),write(')').
ptconstraint(PT,minimum(In,Obj,C),Attr,Mto) :-
   write('min '),write(In),write(' in '), ptterm(Obj,Attr),write(':('),
   getobjattr(PT,Obj,Attr,Attributes),append(Attributes,Attr,NewAttr),
   ptconstraint(PT,C,NewAttr,Mto),write(')').
ptconstraint(PT,maximum(In,Obj,C),Attr,Mto) :-
   write('max '),write(In),write(' in '), ptterm(Obj,Attr),write(':('),
   getobjattr(PT,Obj,Attr,Attributes),append(Attributes,Attr,NewAttr),
   ptconstraint(PT,C,NewAttr,Mto),write(')').
ptconstraint(PT,condConstr([ConstraintX|T], Literals),Attr,Mto) :-
   pptcobconstraintlist(PT,[ConstraintX],Attr,Mto), write(':-'),
   ppliterals(Literals,PT,Attr,Mto),length(T,L),!,
   (L>0->(write(';'),nl,ptconstraint(PT,condConstr(T, Literals),Attr,Mto));!).
   
%---write constructor call with additional 'Time' variable
ptconstraint(_,new(O,C,A),Attr,_) :-
   ptterm(O,Attr), write(' = new '),write(C),
   write('( Time'),addcomma(A),!, ptterms(A,Attr),write(')').
ptconstraint(PT,not(P),Attr,_) :-
   write('not('), ptconstraint(PT,P,Attr,[]),write(')').
ptconstraint(PT,bool(P),Attr,_) :-
   ptconstraint(PT,P,Attr,_).
ptconstraint(_,builtinclpr(X),_,_) :-
   write(X).
ptconstraint(_,T,A,_):-
   ptterm(T,A).
   
%---Print tcob terms,Check variable is series or not. If series add Time index
ptterms([],_).
ptterms([X],[]) :-
   ppterm(X).
ptterms([X|T],[]) :-
   ppterm(X),!, write(','), ppterms(T).
%ppterm(summation(X,Y,C),A) :- ptconstraint(summation(X,Y,C),A,_).
ptterms([X],A) :-
   ptterm(X,A).
ptterms([X|T],A) :-
   ptterm(X,A), !,write(','), ptterms(T,A).
ptterm(ref(prev(X),Y),_) :-
   !, ppterm(ref(X,Y)), write('[Time-1]').
ptterm(ref(next(X),Y),_) :-
   !, ppterm(ref(X,Y)), write('[Time+1]').
ptterm(ref(X,V),A):-
   ptterm(X,A),!,write('.'),ptterm(V,A).
ptterm(X,[]) :-
   ppterm(X).
ptterm(X,Attr) :-
   seriescheckvariable(Attr,X),!, write(X),write('[Time]').
ptterm(ser(_,Y),[1]):-
   write(Y).
ptterm(repl(_,Y),[1]):-
   write(Y).
ptterm(mult(X, Y),A) :-
   !, ptterm(X,A), write(' * '), ptterm(Y,A).
ptterm(add(X, Y),A) :-
   !, ptterm(X,A), write(' + '), ptterm(Y,A).
ptterm(divide(X, Y),A) :-
   !, ptterm(X,A), write(' / '), ptterm(Y,A).
ptterm(sub(X, Y),A) :-
   !, ptterm(X,A), write(' - '), ptterm(Y,A).
ptterm(pow(X, Y),A) :-
   !, ptterm(X,A), write(' ^ '), ptterm(Y,A).

ptterm(ind(X,Y),A) :-
   seriescheckvariable(A,X),ppterm(X),write('['),ptterm(Y,A),write(']').
ptterm(ind(X,Y),A) :-
   !,ppterm(X),write('['),ptterm(Y,A),write(']').
ptterm(enclosed(T),A) :-
   !, write('('), ptterm(T,A), write(')').
%ppterms([X],A) :- ppterm(X,A).
ptterm(X,_) :-
   ppterm(X).
   
%---Check for series variable
seriescheckvariable([att(series(_),V)|_],V).
seriescheckvariable([att(array(series(_),_),V)|_],V).
seriescheckvariable([_|T],V) :-
   seriescheckvariable(T,V).
   
%---Add unique name for MTO predicates and print it in the predicate section of COB class
getgpredicatename(dcobG(N,_,C),C,N):-!.
getgpredicatename([H|_],C,N) :-
   getgpredicatename(H,C,N).
getgpredicatename([_|T],C,N) :-
   getgpredicatename(T,C,N).
getfpredicatename(dcobF(N,_,C),C,N):-!.
getfpredicatename([H|_],C,N) :-
   getfpredicatename(H,C,N).
getfpredicatename([_|T],C,N) :-
   getfpredicatename(T,C,N).

   writemtopredicates([],_,_).
writemtopredicates([X|T],PT,Attr) :-
   writemtopredicates1(X,PT,Attr),writemtopredicates(T,PT,Attr).

writemtopredicates1(X,PT,A) :-
   ppmtopredicate(X,PT,A),write('.'),nl.
   
%---Index for series variable in MTO constraints
writeindex([],_).
writeindex([H|T],I) :-writeindex(H,I),writeindex(T,I).
writeindex(ser(X,Y),I):- write('index('),write(I),write(','),write(X),write(','),write(Y),write('),').
writeindex(_,_).
printgpredicate(N,C,PT,A) :-
   writedefaultG(N),nl,write(N),write('( Lo,Hi,T,['),
   pmtoargument(C,PT,A,R),getvars(PT,A,R,VarList),writeargumentlist(VarList),
   write(']) :- Hi1 is ceil(Hi),Lo<Hi1,'),tcobindex(I),write(I),write(' is Lo+T,'),writeindex(VarList,I),
   %ptconstraint(_,R,[1],_),
   (checkquotedstring(C) -> ptconstraint(_,R,[1],_);write('catch({'),ptconstraint(_,R,[1],_),
   write('},Er,('),ptconstraint(_,R,[1],_),write('))')),
   write(',Lo1 is Lo+1,'),write(N),write('( Lo1,Hi1,T,['),	writeargumentlist(VarList),write('])').
printfpredicate(N,C,PT,A) :-
   write(N),write('( Lo,Hi,T,['),pmtoargument(C,PT,A,R),getvars(PT,A,R,VarList),
   writeargumentlist(VarList),write(']) :- Hi1 is ceil(Hi),Lo <Hi1,'),tcobindex(I),
   write(I),write( ' is T+Lo,'),writeindex(VarList,I),
   %ptconstraint(_,R,[1],_),write('.'),nl,
   (checkquotedstring(C) -> ptconstraint(_,R,[1],_);write('catch({'),ptconstraint(_,R,[1],_),
   write('},Er,('),ptconstraint(_,R,[1],_),write('))')),write('.'),nl,
   printfsecpredicate(N).
   
printfsecpredicate(N) :-
   write(N),write('( Lo,Hi,T,C):-'),write( ' Hi1 is ceil(Hi),TP is Lo+1,TP<Hi,!,'),write(N),write('(TP,Hi,T,C)').
   
ppmtopredicate(dcobG(N,time(_,_),C),PT,A) :-
   printgpredicate(N,C,PT,A).
ppmtopredicate(docobG(N,time(_),C),PT,A) :-
   printgpredicate(N,C,PT,A).
ppmtopredicate(dcobG(N,[],C),PT,A) :-
   printgpredicate(N,C,PT,A).
ppmtopredicate(dcobF(N,time(_,_),C),PT,A) :-
   printfpredicate(N,C,PT,A).
ppmtopredicate(dcobF(N,[],C),PT,A) :-
   printfpredicate(N,C,PT,A).
ppmtopredicate(dcobF(N,time(_),C),PT,A) :-
   write(N),write('( Lo,T,['),pmtoargument(C,PT,A,R),getvars(PT,A,R,VarList),
   writeargumentlist(VarList),write(']) :-'),tcobindex(I),write(I),write(' is round(T+Lo),'),
   writeindex(VarList,I),(checkquotedstring(C) -> ptconstraint(_,R,[1],_);write('{'),
   ptconstraint(_,R,[1],_),write('}')).

writedefaultG(N) :-
   write(N),write('(Lo,Lo,_,_):-!.'),nl,write(N),write('(Lo,Hi,_,_):-Lo>=Hi,!.'),nl.
   
%---Process MTO constraints argument
pmtoargument([],_,_,[]).
pmtoargument([H|T],PT,A,[AH|AT]) :- 
   pmtoargument(H,PT,A,AH),pmtoargument(T,PT,A,AT).
pmtoargument(compare(R,Term1,Term2),PT,A,compare(R,RTerm1,RTerm2)):-
   pmtoterm(Term1,PT,A,RTerm1),pmtoterm(Term2,PT,A,RTerm2).

pmtoterm(compare(R,X,Y),PT,A,X) :-
   pmtoargument(compare(R,X,Y),PT,A,X).
pmtoterm(add(X,Y),PT,A,add(RX,RY)) :-
   pmtoterm(X,PT,A,RX),pmtoterm(Y,PT,A,RY).
pmtoterm(enclosed(X,Y),PT,A,enclosed(RX,RY)) :-
   pmtoterm(X,PT,A,RX),pmtoterm(Y,PT,A,RY).
pmtoterm(X,_,A,ser(X,N)) :-
   seriescheckvariable(A,X),tcobseries(N).
%pmtoterm(X,_,_,X).
pmtoterm(ind(_,_),_,_,R):-
   tcobind(R).
pmtoterm(ref(X,Y),PT,A,ser(X,N)):-
   getobjattr(PT,X,A,Attributes),append(Attributes,A,NewAttr),seriescheckvariable(NewAttr,Y),tcobseries(N).
pmtoterm(next(_),_,_,R) :-
   tcobnp(R).
pmtoterm(prev(_),_,_,R) :-
   tcobnp(R).
pmtoterm(ref(_,_),_,_,R):-
   tcobref(R).
pmtoterm(X,_,_,X). 
%tcobvar(R).

%---Generate unique name for MTO predicates and variable
tcobgpred(X) :- 
   gensym(tcobg, X).
tcobfpred(X) :- 
   gensym(tcobf, X).
tcobind(X) :- 
   gensym('Tcobi',X).
tcobseries(X) :- 
   gensym('Tcobser',X).
tcobindex(X) :- 
   gensym('Tcobind',X).
tcobnp(X):- 
   gensym('Tcobnp',X).
tcobref(X):- 
   gensym('Tcobref',X).
tcobvar(X):- 
   gensym('Tcobvar',X).

%--Extract variable from a constraints for MTO predicates
getvars(_,_,[],[]).
getvars(PT,Attr,[HP|TP],Var) :- getvars(PT,Attr,HP,H),getvars(PT,Attr,TP,T),append(H,T,Var).
getvars(PT,Attr,compare(_,Term1,Term2),Var) :-
   getvars(PT,Attr,Term1,H),getvars(PT,Attr,Term2,T),append(H,T,Var).
getvars(PT,Attr,add(Term1,Term2),Var) :-
   getvars(PT,Attr,Term1,H),getvars(PT,Attr,Term2,T),append(H,T,Var).
getvars(PT,Attr,sub(Term1,Term2),Var) :-
   getvars(PT,Attr,Term1,H),getvars(PT,Attr,Term2,T),append(H,T,Var).
getvars(PT,Attr,mult(Term1,Term2),Var) :-
   getvars(PT,Attr,Term1,H),getvars(PT,Attr,Term2,T),append(H,T,Var).
getvars(PT,Attr,divide(Term1,Term2),Var) :-
   getvars(PT,Attr,Term1,H),getvars(PT,Attr,Term2,T),append(H,T,Var).
getvars(PT,Attr,enclosed(X),L) :-
   getvars(PT,Attr,X,L).

getvars(_,_,ser(X,Y),[ser(X,Y)]).
getvars(_,_,ind(X,Y),[ind(X,Y)]).
getvars(_,_,next(X),[next(X)]).
getvars(_,_,prev(X),[prev(X)]).
getvars(PT,Attr,ref(X,Y),[ref(X,Y)]):-
   getobjattr(PT,X,Attr,Attributes),append(Attributes,Attr,NewAttr),
   seriescheckvariable(NewAttr,Y).
getvars(_,_,ref(X,Y),[ref(X,Y)]).
getvars(_,_,const(_),[]).
getvars(_,_,quotedString(_),[]).
getvars(_,_,X,[X]).
%getvars(_,_,_,[]):- !.


ptpredargument(_,[],_).
ptpredargument(PT,[X],A):-
   ptconstraint(PT,X,A,_).
ptpredargument(PT,[X|T],A):-
   ptconstraint(PT,X,A,_),write(','),ptpredargument(PT,T,A).

gettype(V,[att(user(Type),V)|_],Type).
gettype(V,[att(array(user(Type),_),V)|_],Type).
gettype(_,[],[]).
gettype(V,[_|T],Type):- gettype(V,T,Type).

getobjattr(PT,ref(Obj,Ref),Attr,NewAttr) :-
   getobjattr(PT,Obj,Attr,Attr1), append(Attr,Attr1,Attr2), getobjattr(PT,Ref,Attr2,Attr3),
   append(Attr1,Attr3,NewAttr).
getobjattr(PT,Obj,Attr,NewAttr) :-
    gettype(Obj,Attr,Type),getclassattr(Type,PT,NewAttr).
    

%--Handle literals in TCOB constraints, ParseTree and Attribute list to get series variable
ppliterals([X],PT,A,M) :-
   ppliteral(X,PT,A,M).
ppliterals([X|T],PT,A,M) :-
   ppliteral(X,PT,A,M), write(','),
   ppliterals(T,PT,A,M).

ppliteral(X,PT,A,M):-
   ptconstraint(PT,X,A,M).
%---TCOB Specific Predicates ends here------------

%---COB Specific Predicates
%---Convert COB program to CLP
%---Source(So) added to handle native cob program
%---Otherwise add a Time variable in super class call
cob2swi(File) :-
   retractall(debug_option(_)),  assert(debug_option('no')),cob2swi1(File,2),
   cob2swi1(File,2).
cob2swi1(File,So) :-
   atom_codes(File, FullCodes),
   atom_codes('.cob',S),
   append(FileCodes, S ,FullCodes),
   atom_codes(Prefix,FileCodes),
   atom_concat(Prefix, '.pl', Output),
   cob2swi(File,Output,So).

cob2swi(File, Output,So) :-
   open(File,read,Stream),
   process1(Stream, Output,So).

process1(Stream, Output,So) :-
   tell(user_output),
   lex(Stream,Tokens), !, %print(Tokens), nl,
   parsecob(Tokens, ParseTree),
   translateprog(ParseTree, ParseTree, P,So),
   append([pred_clause(pred("",_),
   pred_body([call(consult, [helper_clpr]),
   call(use_module, ['library(clpr)'])]))],
   P, SWIprog),tell(Output),
   prettyprint(SWIprog), told.
%---Translate COB program
translateprog([X|T], O, P,So) :-
   (So=1 -> translateclass(X, O, C); 
   translateclass1(X, O, C)),
   translateprog(T, O, Rest,So),
   append(C, Rest, P).
translateprog([], _, [],_).
%---Take each class and transalte
translateclass(classdef(Name, Superclass, Attributes, Constraints,
               Predicates, Constructors, _), L, Class) :-
   translate(Name, Superclass, Attributes, Constraints, Constructors, L, C1),
   append(C1, Predicates, Class).

translate(Name, Superclass, Attributes, Constraints, [Constructor], L, C) :-
   !, translateone(Name, Superclass, Attributes, Constraints, Constructor, L, C).
translate(Name, Superclass, Attributes, Constraints, [Constructor|T], L, C) :-
   translateone(Name, Superclass, Attributes, Constraints, Constructor, L, C1),
   translate(Name, Superclass, Attributes, Constraints, T, L, CT), append(C1, CT, C).
% if constructor is absent (abstract class) still translate into a predicate so that
% subclasses' translation may call this predicate
translate(Name, Superclass, Attributes, Constraints, [], L, C) :-
   translateone(Name, Superclass, Attributes, Constraints, L, C).

translateone(Name, "", Attributes, Constraints, constructor(Name, Att1, ConstructorConstraintList), L,
   [pred(Name, Att, AttList,  ConstraintList)|P]) :-
   translatetypedecl(Attributes, TD),
   removetypedecl(Attributes, AttList), removetypedecl(Att1, Att),
   !, translateconstraints(ConstructorConstraintList, L, CC, P1, Name, []), % predicates ?
   translateconstraints(Constraints, L, C, P2, Name, []),
   append(TD, CC, C1),    append(C1, C, ConstraintList), append(P1, P2, P).

translateone(Name, Superclass, Attributes, Constraints, constructor(Name, Att1, ConstructorConstraintList), L,
   [pred(Name, Att, AttList,  ConstraintList)|P]) :-
   translatetypedecl(Attributes, TD),
   attributesofclass(Superclass, L, SuperAttributes), append(SuperAttributes, Attributes, AttList1),
   removetypedecl(AttList1, AttList), removetypedecl(Att1, Att), removetypedecl(SuperAttributes, SAtt),
   !, translateconstraints(ConstructorConstraintList, L, CC, P1, Name, []), % predicates ?
   translateconstraints(Constraints, L, C, P2, Name, []),
   append(TD, CC, C1),    append(C1, C, C2), append(P1, P2, P),
   %cobvar(X), 
  debug_option(V),(V=yes-> (append([call(Superclass,[['Time'], SAtt,'ObjN'])], C2, ConstraintList));
  append([call(Superclass,[['Time'], SAtt])], C2, ConstraintList)).
translateone(Name, "", Attributes, Constraints, L, [pred(Name, ['Time'], AttList,  ConstraintList)|P]) :-
   translatetypedecl(Attributes, TD),
   removetypedecl(Attributes, AttList),
   %cobvar(X),
   !, translateconstraints(Constraints, L, ConstraintList1, P, Name, []),
   append(TD, ConstraintList1, ConstraintList).
translateone(Name, Superclass, Attributes, Constraints, L, [pred(Name, [], AttList,  ConstraintList)|P]) :-
   translatetypedecl(Attributes, TD),
   attributesofclass(Superclass, L, SuperAttributes), append(SuperAttributes, Attributes, AttList1),
   removetypedecl(AttList1, AttList), removetypedecl(SuperAttributes, SAtt),
   !, translateconstraints(Constraints, L, C, P, Name, []),
   append(TD, C, C1),
   cobvar(X), append([call(Superclass,[X, SAtt])], C1, ConstraintList).
   
   
%--For pure COB
translateclass1(classdef(Name, Superclass, Attributes, Constraints,
               Predicates, Constructors, _), L, Class) :-
   translate1(Name, Superclass, Attributes, Constraints, Constructors, L, C1),
   append(C1, Predicates, Class).

translate1(Name, Superclass, Attributes, Constraints, [Constructor], L, C) :-
   !, translateone1(Name, Superclass, Attributes, Constraints, Constructor, L, C).
translate1(Name, Superclass, Attributes, Constraints, [Constructor|T], L, C) :-
   translateone1(Name, Superclass, Attributes, Constraints, Constructor, L, C1),
   translate1(Name, Superclass, Attributes, Constraints, T, L, CT), append(C1, CT, C).
% if constructor is absent (abstract class) still translate into a predicate so that
% subclasses' translation may call this predicate
translate1(Name, Superclass, Attributes, Constraints, [], L, C) :-
   translateone(Name, Superclass, Attributes, Constraints, L, C).

translateone1(Name, "", Attributes, Constraints, constructor(Name, Att1, ConstructorConstraintList), L,
   [pred(Name, Att, AttList,  ConstraintList)|P]) :-
   translatetypedecl(Attributes, TD),
   removetypedecl(Attributes, AttList), removetypedecl(Att1, Att),
   !, translateconstraints(ConstructorConstraintList, L, CC, P1, Name, []), % predicates ?
   translateconstraints(Constraints, L, C, P2, Name, []),
   append(TD, CC, C1),    append(C1, C, ConstraintList), append(P1, P2, P).

translateone1(Name, Superclass, Attributes, Constraints, constructor(Name, Att1, ConstructorConstraintList), L,
   [pred(Name, Att, AttList,  ConstraintList)|P]) :-
   translatetypedecl(Attributes, TD),
   attributesofclass(Superclass, L, SuperAttributes), append(SuperAttributes, Attributes, AttList1),
   removetypedecl(AttList1, AttList), removetypedecl(Att1, Att), removetypedecl(SuperAttributes, SAtt),
   !, translateconstraints(ConstructorConstraintList, L, CC, P1, Name, []), % predicates ?
   translateconstraints(Constraints, L, C, P2, Name, []),
   append(TD, CC, C1),    append(C1, C, C2), append(P1, P2, P),
   cobvar(X), 
    debug_option(V),(V=yes-> append([call(Superclass,[[X], SAtt,'ObjN'])], C2, ConstraintList);
    append([call(Superclass,[[X], SAtt])], C2, ConstraintList)).
translateone1(Name, "", Attributes, Constraints, L, [pred(Name, [X], AttList,  ConstraintList)|P]) :-
   translatetypedecl(Attributes, TD),
   removetypedecl(Attributes, AttList),
   cobvar(X),
   !, translateconstraints(Constraints, L, ConstraintList1, P, Name, []),
   append(TD, ConstraintList1, ConstraintList).
translateone1(Name, Superclass, Attributes, Constraints, L, [pred(Name, [], AttList,  ConstraintList)|P]) :-
   translatetypedecl(Attributes, TD),
   attributesofclass(Superclass, L, SuperAttributes), append(SuperAttributes, Attributes, AttList1),
   removetypedecl(AttList1, AttList), removetypedecl(SuperAttributes, SAtt),
   !, translateconstraints(Constraints, L, C, P, Name, []),
   append(TD, C, C1),
   cobvar(X), append([call(Superclass,[X, SAtt])], C1, ConstraintList).


removetypedecl([],[]).
removetypedecl([att(_, X)|T], [X | Y]) :- 
   removetypedecl(T, Y).

attributesofclass(Name, L, Att) :- 
   attributesofclass(Name, L, L, Att).
attributesofclass("", _, _, []).
attributesofclass(Name, [X|_], _, Attributes) :-
   X = classdef(Name, "", Attributes, _, _, _, _).
attributesofclass(Name, [X|_], L, Attributes) :-
   X = classdef(Name, Superclass, Att, _, _, _, _),
   attributesofclass(Superclass, L, L, SuperAtt), append(SuperAtt, Att, Attributes).
attributesofclass(Name, [_|T], L, Attributes) :- 
   attributesofclass(Name, T, L, Attributes).
%---Translate conditional constraints
translateconstraints([], _, [], [], _, _) :- !.
translateconstraints([condConstr(Constraint, Literals)|Rest], L,
                     TRest1, P5, Name, EType) :- %translate Constraints & Literals!!
   !, translateconstraints1([Constraint], L, [FirstC|TC], P1, Name, EType),
 % !, translateconstraints1([Constraint], L, FirstC, P1, Name, EType),
   translateliterals(Literals, L, Callsbefore, TLiterals, P2, Name, EType),
   translateconstraints(Rest, L, TRest, P3, Name, EType), append(P1, P2, P4), append(P4, P3, P5),
%append(TC, Callsbefore, TCL), 
%append(TCL, [condConstr([FirstC], TLiterals)], TCLCC), append(TCLCC, TRest, TRest1).
append(TC,[FirstC], TCL),
append(Callsbefore, [condConstr(TCL, TLiterals)], TCLCC), append(TCLCC, TRest, TRest1).
%---Translate creational constraint
translateconstraints([new(T1, Class, Pars)|Rest], L, Calls, P, Name, EType) :-
   translateterm(T1, L, TT1, Cl1, P1, Name, Class, EType),
   translateterms(Pars, L, TPars, Cl2, P2, Name, _, _),
   translateconstraints(Rest, L, TRest, PRest, Name, _), append(Cl1, Cl2, C13),debug_option(V),(V=yes-> 
   process_object(T1,TT1,Cl1,ONF,ClI),
 append(C13,ClI,C13I), append(C13I, [call(Class, [TPars, TT1,ONF])], Cl4);  append(C13, [call(Class, [TPars, TT1])],Cl4)),
 %  append(ParEqnList, C13, CP13), 
    append(Cl4, TRest, Calls),
   append(P1, P2, P3), append(P3, PRest, P).
   
     
%---Translate qunatified constraints
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
   !,translateconstraints(SLX, L, C, P1, Name, EType),
   translateconstraints(Rest, L, TRest, P2, Name, EType),
   append(C, [ifthenelse([call('nonvar', [var('Tail')])],
                         [call(Forall, [var('Tail'), functor(SSLX)])],
                         ['true'])], P5),
   append(P1, P2, P).
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
translateconstraints([equant(V, fromto(N, M), LX)|Rest], L,
                     [call(makelistfromto, [N, M, NtoM]),
                     call(Exists, [NtoM, functor(SLX)])|TRest],
                     [pred_clause(pred(Exists, [makelist([W,'|',var('Tail')]),functor(SLX)]),pred_body(P5))|P],
                     Name, EType):-
   cobexistspred(Exists),
   cobvar(W), subst(V, W, LX, SLX),
   cobvar(NtoM),
   cobvar(U), subst(W, U, SLX, SSLX),
   !,
   translateconstraints(SLX, L, C, P1, Name, EType),
   translateconstraints(Rest, L, TRest, P2, Name, EType),
   P5 = [ifthenelse(C, ['true'],
                    [ifthenelse([call('nonvar', [var('Tail')])],
                                [call(Exists, [var('Tail'), functor(SSLX)])],
                                ['fail'])])],
   append(P1, P2, P).
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


translateconstraints([constraintPred(CPName,Terms)|Rest], L, CorrectedOrderofCalls, P, Name, EType) :-
   !, translateterms(Terms, L, TTerms, Cl1, P1, Name, _, EType),
   translateconstraints(Rest, L, TRest, P2, Name, EType), append(P1, P2, P),
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
%---Translate predicate call in constriants  
translateconstraints1([constraintPred(CPName,Terms)|Rest], L, CorrectedOrderofCalls, P, Name, EType) :-
   !, translateterms(Terms, L, TTerms, Cl1, P1, Name, _, EType),
   translateconstraints(Rest, L, TRest, P2, Name, EType), append(P1, P2, P),
append( [constraintPred(CPName, TTerms)],Cl1, Calls),
   append(Calls, TRest, CorrectedOrderofCalls).

translateconstraints1([not(constraintPred(CPName,Terms))|Rest], L, 
                       CorrectedOrderofCalls, P, Name, EType) :-
   !, translateterms(Terms, L, TTerms, Cl1, P1, Name, _, EType),
   translateconstraints(Rest, L, TRest, P2, Name, EType), append(P1, P2, P),
   append( [not(constraintPred(CPName, TTerms))],Cl1, Calls),
   append(Calls, TRest, CorrectedOrderofCalls).
translateconstraints1(Lit,L, P, P1,Name, EType) :-       
   translateconstraints(Lit, L, P, P1, Name, EType).

translateliterals([], _, [], [], [], _, _).

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

translateliterals([Lit|RestL], L, Callsbefore, TLiterals, P3, Name, EType) :-
   translateconstraints1([Lit], L, [FirstL|TL], P1, Name, EType),
   translateliterals(RestL, L, Callsbefore1, TRestL, P2, Name, EType),
   append([FirstL], TRestL, TLiterals), append(P1, P2, P3), append(TL, Callsbefore1, Callsbefore).
%---Term handling
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

translateterm(minimum(V, E, Term), L, Y, Pred_call,
              [pred_clause(pred(Minover, [makelist([W]), functor(STerm), TTerm]), pred_body(Calls)),
               pred_clause(pred(Minover, [makelist([W, '|', var('Tail')]), functor(STerm), min(TTerm, var(Z))]),
                           pred_body([ifthenelse([call('nonvar', [var('Tail')])],
                                                 [call(Minover, [var('Tail'), functor(SSTerm), var(Z)])],
%else part is redundant since a non-ground Tail will match the first clause.
                                                 [compare(=, var(Z), TTerm)])|Calls]))
               |P], Name, 'Real', EType) :-   translateterm(E, L, TTerm1, Calls1, _, Name, 'Real', EType), append(Calls1,
               [call(Minover, [TTerm1, functor(STerm), Y])],Pred_call),
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
   append(Calls1, [compare('=', TA, dangling(TTAType))], Calls).
translateterm(ind(C, T), L, C_T, Calls, P, Name, TypeC,EType) :-
   %typeof(C, _, L, _, TypeC), %isn't this incomplete ? e.g. A.I[5] will not be translated correctly
   !, translateterm(C, L, CT, Cl1, P1, Name, TypeC, EType), % not sure if TypeC is correct edithere3/11/17
   cobvar(C_T), translateterm(T, L, TT, Cl2, P2, Name, int,_), append(P1, P2, P), append(Cl1, Cl2, Calls1),
%Calls1 is appended in front (in the next line) for efficiency reasons.
   translateindex(Calls1, CT,TT,C_T, Calls).

translateterm(A, _L, A, [], [], _Name, real, _EType) :-
   A =..[const|_], !.
translateterm(A, L, A, [], [], Name, Type, EType) :-
   typeof(A, Name, L, EType, Type).
translateterm(A, _, A, [], [], _, _, _) :- !.

translateterms([], _, [], [], [], _, _, _).
translateterms([Par|Tail], L, [TPar|TTail], Calls, Preds, Name, Type, EType) :-
   translateterm(Par, L, TPar, C, P, Name, _, EType),
   translateterms(Tail, L, TTail, Cls, Pds, Name, Type, EType),
   append(C, Cls, Calls), append(P, Pds, Preds).

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
   append(Calls1, [call('index', [TT,CT, C_T])], Calls).
translateindex(Calls1,CT,TT,C_T, Calls) :-  % TT is some arithmetic expression
   cobvar(R), cobvar(I),
   append(Calls1, [compare('is',R, TT), r2i(R,I), call('index', [I,CT, C_T])], Calls).

isCobvarorConst(TT) :- 
   atom(TT), !.
isCobvarorConst(const(N)) :- 
   integer(N).

process_object(Ori,Trans,Index,ONF,Cl4):- 
     (Ori = Trans -> get_simple_name(Ori,ONF,Cl4) ; get_index_name(Index,ONF,Cl4)).
   
get_index_name(Index,IN,[ call(getname,[ONF,In,IN])]):-  
     get_index(Index,Va,In),string_concat('\'', Va,ON),
     string_concat(ON,'\'',ONF) ,cobvar(IN).
   
get_simple_name(Ori, ONF,[]):-
     string_concat('\'', Ori,ON),string_concat(ON,'\'',ONF).
get_index([call(index,[X,V,_])],V,X).
get_index(_,0,0).

typeof(V, Name, L, _, Type) :-
   attributesofclass(Name, L, Att),
   revassoc(V, Att, Type).
typeof(V, _, _, EType, Type) :-
   revassoc(V, EType, Type).
%added next clause on 03.05.08
typeof(ind(_,_), _, _, _, array).
typeof(_V, _Name, _, _, _) :-
   !, fail.

revassoc(V, [att(array(user(Type), _), var(V))|_], Type).
revassoc(V, [att(user(Type), var(V))|_], Type).
revassoc(V, [att(array(user(Type), _), V)|_], Type).
revassoc(V, [att(user(Type), V)|_], Type).
revassoc(V, [att(array(_, _), var(V))|_], _). % should this be array ?
revassoc(V, [att(array(_, _), V)|_], _). % should this be array ?
revassoc(V, [_|T], Type) :- revassoc(V, T, Type).

prefix(_, [], []).
prefix([], _, []).
prefix(V, [X|T], [V_X|PreT]) :-
   atom_concat(V,'_', V_), atom_concat(V_, X, V_X), (nonvar(T) -> prefix(V, T, PreT); PreT = T). % CORRECT ??
   prefix(V, X, V_X) :- atom_concat(V,'_', V_), atom_concat(V_, X, V_X).
prefix(V, X, V_X) :- 
   atom_concat(V,'_', V_), atom_concat(V_, X, V_X).

subst(V, W, V, W).
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
%--Generate unique name for variable
cobvar(X) :- 
   gensym('Cob', X).
cobforallpred(X) :- 
   gensym(coball, X).
cobexistspred(X) :- 
   gensym(cobexists, X).
cobsumpred(X) :- 
   gensym(cobsum, X).
cobminpred(X) :- 
   gensym(cobmin, X).
cobmaxpred(X) :- 
   gensym(cobmax, X).

%--Print predicates to CLP file
prettyprint([pred(Name, [], AttList, [])|T]) :-
   write(Name), write('('), write([]), write(','), write(AttList), debug_option(D),(D=yes-> write(','),
   write('ObjN');true),write(')'), write('.'), nl, nl,   prettyprint(T).
prettyprint([pred(Name, Att, AttList, [])|T]) :-
   write(Name), write('('), write(Att), write(','), write(AttList),debug_option(D),(D=yes-> write(','), 
   write('ObjN');true),write(')'), write('.'), nl,prettyprint(T).
prettyprint([pred(Name, Att, AttList, ConstraintList)|T]) :-
   write(Name), write('('), write(Att), write(','), write(AttList),debug_option(D),(D=yes-> write(','), 
   write('ObjN');true),write(')'), write(':-'), nl,
   ppconstraintlist(ConstraintList), write('.'), nl, nl,
   prettyprint(T).
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

%--Print formating for constraints
ppconstraintlist([]). %not correct
ppconstraintlist([X])   :- 
   tabs(1), ppconstraint(X).
ppconstraintlist([X|T]) :- 
   ppconstraint2(X), ppconstraintlist(T).

% For Swipl support Insert bracket in for constrinats-
ppconstraint2(compare('=', Term1, Term2)) :-
	Term1 == Term2, !. % do nothing.
ppconstraint2('true') :- write('true').
ppconstraint2(X) :- tabs(1), ppconstraint(X), write(','), nl.

ppconstraint('true') :- write('true').
ppconstraint(compare('is', Term1, Term2)) :-  
   ppterm(Term1), write('  '), write('is'), write('  '), ppterm(Term2) .
ppconstraint(compare(R, Term1, Term2)) :-
   amoper(Term1),write('{'), ppterm(Term1), write('  '), write(R), write('  '), 
   ppterm(Term2), write('}') . % modified
ppconstraint(compare(R, Term1, Term2)) :-  
   amoper(Term2),write('{'), ppterm(Term1), write('  '), write(R), write('  '), 
   ppterm(Term2), write('}'). % modified
ppconstraint(compare(R, Term1, Term2)) :- 
   relop(R), R \== '=', write('{'), ppterm(Term1), write('  '), write(R),
   write('  '), ppterm(Term2),  write('}').
ppconstraint(compare(R, Term1, Term2)) :- 
   var(Term1),var(Term2), write('{'), ppterm(Term1), write('  '), write(R), 
   write('  '), ppterm(Term2),  write('}').
ppconstraint(compare(R, Term1, Term2)) :-  
   ppterm(Term1), write('  '), write(R), write('  '), ppterm(Term2).

% for sicstus, dump must be changed to print
ppconstraint(constraintPred(dump, [X]))  :- 
   dump2print(X).
ppconstraint(constraintPred(Name, X))  :- 
   write(Name), write('('), ppterms(X), write(')').
ppconstraint(call(Name, Att))          :- 
   write(Name), write('('), ppterms(Att), write(')').
ppconstraint(condConstr(Constraint, Literals)) :-
   write('conditional_constraint'),
   write('('), write('('), ppconstraintlist(Constraint), write(')'), write(','),
   write('('), ppliterals(Literals), write(')'), write(')').
ppconstraint(pred(Name, Att)) :- 
   ppconstraint(call(Name, Att)).
ppconstraint(not(P)) :- 
   write('\\+  '), ppconstraint(P).
ppconstraint(bool(P)) :- 
   ppterm(P).
%expand write(Literals)
ppconstraint(ifthenelse(Bool, If, Else)) :-
   write('('), write('('), ppconstraintlist(Bool), write(')'), write('->'),
   write('('), ppconstraintlist(If), write(')'), write(';'),
   write('('), ppconstraintlist(Else), write(')'), write(')').
ppconstraint(builtinclpr(X)) :- 
   write(X).
ppconstraint(X):- 
   write(X). %from sunnyvale

amoper(mult(_,_)). %added
amoper(divide(_,_)). %added
amoper(sub(_,_)). %added
amoper(add(_,_)). %added
amoper(pow(_,_)). %added
amoper(negative(mult(_,_))). %added
amoper(negative(divide(_,_))). %added
amoper(negative(sub(_,_))). %added
amoper(negative(add(_,_))). %added
amoper(negative(pow(_,_))). %added
amoper(negative(_,_)).

dump2print([]) :- 
   print('true'). % dummy statement to end recursion without printing a comma
dump2print([X|T]) :- 
   write('write(\''), ppterm(X), write(' =  \'), '),
   ppterm('write('), ppterm(X), write('), '),
   write('nl, '), dump2print(T).

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
ppliteral(X):- 
  ppconstraint(X).

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
   pponeclasspredicate(['=','='|Rest]) :-
   write('=='), write('  '),
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
%---COB Specifc predicates end here
   
%---Common printterms for COB&TCOB
ppterms([X]) :-
   ppterm(X).
ppterms([]).
ppterms([X|T]) :-
   ppterm(X),!, write(','), ppterms(T).

ppterm(ref(X,V)):-
   ppterm(X),!,write('.'),ppterm(V).
ppterm((ref(prev(X),Y))) :-
   !, ppterm(ref(X,Y)), write('[Time-1]').
ppterm(ser(X)):-
   !,write(X),write('[Time]').
ppterm(ind(ind(X,Y),Z)) :-
   !,ppterm(X),write('['),ppterm(Y),write(','),ppterm(Z),write(']').
ppterm(ind(X,Y)) :-
   !,ppterm(X),write('['),ppterm(Y),write(']').
ppterm(ser(X,_)) :-
   !,write(X).
ppterm(rep(_,Y)) :-
   !,write(Y).
ppterm(next(ind(X,Y))) :-
   !, ppterm(X), write('['),ppterm(Y),write(',Time+1]').
ppterm(prev(ind(X,Y))) :-
   !, ppterm(X), write('['),ppterm(Y),write(',Time-1]').
 ppterm(negative(T)) :-
   !, write('-'), ppterm(T).
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
ppterm(at(V,I)) :-
   !,ppterm(V),write('['),write(I),write(']').
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
ppterm([X|T]) :- %ppterms([X|T]).
   write('['), ppterms([X|T]), write(']').
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

tabs(0) :- !.
tabs(N) :-
   write('   '), M is N-1, tabs(M).
checkquotedstring([H|T]):- 
   checkquotedstring(H);checkquotedstring(T).
checkquotedstring(compare(=,Term1,Term2)):- 
   checkquotedstring(Term1);checkquotedstring(Term2).
checkquotedstring(add(Term1,Term2)) :- 
   checkquotedstring(Term1);checkquotedstring(Term2).
checkquotedstring(mult(Term1,Term2)) :- 
   checkquotedstring(Term1);checkquotedstring(Term2).
checkquotedstring(divide(Term1,Term2)) :- 
   checkquotedstring(Term1);checkquotedstring(Term2).
checkquotedstring(sub(Term1,Term2)) :- 
   checkquotedstring(Term1);checkquotedstring(Term2).
checkquotedstring(quotedString(_)).

writeleading0(0) :- !.
writeleading0(N) :- write('0'), M is N-1, writeleading0(M).

extractvars([], []).
extractvars([X|Tail], V) :-
   extractvars(X, Vx), extractvars(Tail, Vt), append(Vx, Vt, V).
extractvars(enclosed(Term), V) :-
   extractvars(Term, V).
%next clause added on 
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
extractvars(constraintPred(dump,Args), V) :-
   extractvars(Args, V).
extractvars(constraintPred(print,Args), V) :-
   extractvars(Args, V). 
extractvars(constraintPred(_Name,Args), V) :-
   extractvars(Args, V).

extractvars(not(C), V) :- 
   extractvars(C, V).
extractvars(A, [A]).

bagtoset([], L, L).
bagtoset([X|T], L, TL) :- 
   member(X,L), bagtoset(T, L, TL).
bagtoset([X|T], L, TL) :- 
   bagtoset(T, [X|L], TL). %  if \+ member(X,L).

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
   
% ------------------ SYNTAX ANALYZER FOR TCOB&COB ---------------------------
%---TCOB parser
parsetcob(Tokens, ParseTree) :-
   resetclassnamecounter(_), resetkeywordcounter(_),
   time_program(ParseTree, Tokens, []), !.
parsetcob(_,[]) :-
   write('Syntax error in TCOB class '), classnamecounter(C),write(C),nl,
   write('while parsing '), parsing(X), write(X),
   keywordclausecounter(Y), write(Y), nl, fail.
%To read driver class name from argument
getdriver(driver(Name,Attr))-->
   [id(Name)],['('], id_list(_,Attr),[')'].
getdriver(driver(Name,[])) -->
    [id(Name)],['('],[')'].

time_program([X|T]) -->
    time_info(X), {!}, tcob_program(T).
time_program([]) --> [].

time_info(time(X,Y,Z,no)) -->
   ['{'],[id(simulation_start)],[=],[num(X)],[','],[id(simulation_end)],
      [=],[num(Y)],[','],[id(skip_time_loop)],['='],['['],class_list(Z),[']'],['}'].
time_info(time(X,Y,[],no)) -->
   ['{'],[id(simulation_start)],[=],[num(X)],[','],[id(simulation_end)],[=],
      [num(Y)],['}'].
time_info(time(1,Y,[],no)) -->
   ['{'],[id(simulation_end)],[=],[num(Y)],['}'].
time_info(time(1,10,[],no)) -->
   ['{'],['}'].
   
time_info(time(X,Y,Z,D)) -->
   ['{'],[id(simulation_start)],[=],[num(X)],[','],[id(simulation_end)],
   [=],[num(Y)],[','],[id(skip_time_loop)],['='],['['],class_list(Z),[']'],
    [','],
    [id(debug)],['='],[id(D)],['}'].
time_info(time(X,Y,[],D)) -->
   ['{'],[id(simulation_start)],[=],[num(X)],[','],[id(simulation_end)],[=],
      [num(Y)],[','],[id(debug)],['='],[id(D)],['}'].
time_info(time(1,Y,[],D)) -->
   ['{'],[id(simulation_end)],[=],[num(Y)],[','],[id(debug)],['='],[id(D)],['}'].
time_info(time(1,10,[],D)) -->
   ['{'],[id(debug)],['='],[id(D)],['}'].

class_list([C|Tail]) -->
   [id(C)],[','],class_list(Tail).
class_list([C]) -->
   [id(C)].
class_list([]) -->
   [].
%---COB parser
parsecob(Tokens, ParseTree) :- 
   resetclassnamecounter(_), resetkeywordcounter(_), program(ParseTree, Tokens, []), !.
parsecob(_,[]) :- 
   write('Syntax error in COB class '), classnamecounter(C), write(C), nl,
               write('while parsing '), parsing(X), write(X),
               keywordclausecounter(Y), write(Y), nl, fail.

tcob_program([X|T]) -->
   tcob_class_definition(X), {!}, tcob_program(T).
tcob_program([]) -->
   [].
%---Common
program([X|T]) -->
   class_definition(X), {!}, program(T).
program([]) -->
   [].

   %-- TCOB class parsing
tcob_class_definition(
  classdef(Name, Superclass, Attributes, Constraints,
  Predicates, Constructors, Debug, Num_instances)) -->
   [class], class_id(Name), ['{'], {!}, {setclassnamecounter(Name)},
   body(Attributes, Constraints, Predicates,Constructors), debug(Debug),['}'],
   {Num_instances = 0, Superclass = ""}.
tcob_class_definition(
  classdef(abstract(Name), Superclass, Attributes,Constraints,
  Predicates, Constructors, Debug, Num_instances)) -->
   [abstract], [class], class_id(Name), ['{'], {!}, {setclassnamecounter(Name)},
   body(Attributes, Constraints,Predicates, Constructors), debug(Debug),['}'],
   {Num_instances = 0, Superclass = "", Constructors = []}.
tcob_class_definition(
  classdef(Name, Superclass, Attributes, Constraints,
  Predicates,Constructors, Debug, Num_instances)) -->
   [class], class_id(Name), [extends], {!},
   class_id(Superclass), ['{'], {!}, {setclassnamecounter(Name)},
   body(Attributes, Constraints,  Predicates,Constructors), debug(Debug),['}'],
   {Num_instances = 0}.
tcob_class_definition(
  classdef(abstract(Name), Superclass, Attributes, Constraints,
  Predicates,   Constructors, Debug, Num_instances)) -->
   [abstract], [class], class_id(Name), [extends], {!},
   class_id(Superclass), ['{'], {!}, {setclassnamecounter(Name)},
   body(Attributes, Constraints,  Predicates, Constructors),debug(Debug), ['}'],
   {Num_instances = 0, Constructors = []}.

class_definition(
  classdef(Name, Superclass, Attributes, Constraints,
  Predicates, Constructors, Num_instances)) -->
   [class], class_id(Name), ['{'], {!}, {setclassnamecounter(Name)},
   body(Attributes, Constraints, Predicates,Constructors), ['}'],
   {Num_instances = 0, Superclass = ""}.
class_definition(
  classdef(Name, Superclass, Attributes,Constraints,
  Predicates, Constructors, Num_instances)) -->
   [abstract], [class], class_id(Name), ['{'], {!}, {setclassnamecounter(Name)},
   body(Attributes, Constraints,Predicates, Constructors), ['}'],
   {Num_instances = 0, Superclass = "", Constructors = []}.
class_definition(
  classdef(Name, Superclass, Attributes, Constraints,
  Predicates,Constructors, Num_instances)) -->
   [class], class_id(Name), [extends], {!},
   class_id(Superclass), ['{'], {!}, {setclassnamecounter(Name)},
   body(Attributes, Constraints,  Predicates,Constructors), ['}'],
   {Num_instances = 0}.
class_definition(
  classdef(abstract(Name), Superclass, Attributes, Constraints,
  Predicates, Constructors, Num_instances)) -->
   [abstract], [class], class_id(Name), [extends], {!},
   class_id(Superclass), ['{'], {!}, {setclassnamecounter(Name)},
   body(Attributes, Constraints,  Predicates, Constructors), ['}'],
   {Num_instances = 0, Constructors = []}.


body(Attributes, Constraints,  Predicates,Constructors) -->
   attributes(Attributes), constraints(Constraints),
   predicates(Predicates), constructors(Constructors).

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

decl(T) -->
   datatype(Type), id_list(Type, T).

datatype(Z) -->
   type(X), arraytype(X, 0, Z).
datatype(Z) -->
   [series],type(X), arraytype(series(X), 0, Z).

type(primitive(int)) --> [id('int')].
type(primitive(real)) --> [id('real')].
type(primitive(char)) --> [id('char')].
type(primitive(string)) --> [id('string')].
type(primitive(bool)) --> [id('bool')].
type(primitive(formula)) --> [id('formula')].
type(primitive(expr)) --> [id('expr')].
%type(primitive(enum)) --> [id('enum')].

type(user(Type)) --> class_id(Type). %

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

constraint(X) --> creational_constraint(X). % removed cut{!}.
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

%--Parsing MTO constraints
constraint_atom(mto('G',X,C)) -->
   ['G'],[<],time_interval(X),[>],['('],literals(C),[')'].
constraint_atom(mto('G',X,[C])) -->
   ['G'],[<],time_interval(X),[>],constraint_atom(C).
constraint_atom(mto('F',X,C)) -->
   ['F'],[<],time_interval(X),[>],['('],literals(C),[')'].
constraint_atom(mto('F',X,[C])) -->
   ['F'],[<],time_interval(X),[>],constraint_atom(C).
constraint_atom(mto('G',[],[C])) -->
   ['G'],['('],literals(C),[')'].
constraint_atom(mto('G',[],[C])) -->
   ['G'],constraint_atom(C).
constraint_atom(mto('F',[],[C])) -->
   ['F'],['('],literals(C),[')'].
constraint_atom(mto('F',[],[C])) -->
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
   boolexpr(Term2).
%change 
constraint_atom(B) -->
   boolexpr(B).

conditional_constraint(condConstr(Constraint, Literals)) -->
   constraint_creation_atom(Constraint) ,[:-], {!},
   literals(Literals).
conditional_constraint(condConstr(Constraint, Literals)) -->
   literals(Literals),[-->],{!}, literals(Constraint).
constraint_creation_atom(Constraint) -->
   creational_constraint(Constraint).
constraint_creation_atom(Constraint) -->
   constraint_atom(Constraint).

time_interval(time(X,Y)) -->
   term(X),[','], term(Y).
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

term(Out) -->
   expr(_, Out).
term(A) -->
   attribute(['$'], A).
term(list([])) --> ['[]'].
term(list(T)) -->
   ['['], terms(T), [']'].

expr(In, Out) -->
   sub1(In, Out1), sub2(Out1, Out).
sub2(In, Out) -->
   ['-'], {!}, sub1(In, Out1), sub2(sub(In,Out1), Out).
sub2(S, S) --> [].

sub1(In, Out) -->
  term1(In, Out1), term2(Out1, Out).

term1(In, Out) -->
  fact(In, Out1), fact2(Out1, Out).

term2(In, Out) -->
   ['+'], {!}, term1(In, Out1), term2(add(In,Out1), Out).
term2(S, S)    --> [].

div(_, const(C))  -->
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
div(_, minimum(V, E, Out)) -->
   [min], {!}, variable(V), [in], enum(E), [':'], term(Out).
div(_, maximum(V, E, Out)) -->
   [max], {!}, variable(V), [in], enum(E), [':'], term(Out).
div(_, enclosed(Out)) -->
   ['('], {!}, term(Out), [')'].

div(_, negative(Out)) -->
   ['-'], term(Out). %should there be a cut here ?
div(_, quotedString(S)) -->
   [quotedString(S)].
div(_, A) -->
   attribute(['$'], A).
div(_, var(V))   -->
   variable(V). %placed last so that coumpound terms may be parsed earlier.

div2(In, Out) -->
   ['/'], {!}, div(In, Out1), div2(divide(In,Out1), Out).
div2(S, S) --> [].

fact(In, Out) -->
   pow(In, Out1), pow2(Out1, Out).
pow(In, Out)  -->
   div(In, Out1), div2(Out1, Out).

fact2(In, Out) -->
   ['*'], {!}, fact(In, Out1), fact2(mult(In,Out1), Out).
fact2(S, S)    --> [].

pow2(In, Out) -->
   ['^'], {!}, pow(In, Out1), pow2(pow(In, Out1), Out).
pow2(S, S)    --> [].

function_id(Name) --> [id(Name)].

enum(Name) --> [id(Name)].
enum(fromto(N, M)) --> [fromto(N, M)].
enum(A) --> attribute(['$'], A).

constant(N) --> [num(N)].
constant(N,M,Z) --> [dnum(N, M, Z)].

attribute(In, X) -->
   selector(In, X), { In = ['$']}.
attribute(In, Out) -->
   ['.'], {\+(In = ['$'])}, {!}, selector(In, Out).
attribute(In, Z) -->
   selector(In, X), ['.'], {!}, selector(X, Y), attribute(Y, Z), {In = ['$']}.
attribute(In, next(Z)) -->
   selector(In, X), ['.'], {!}, selector(X, Y), attribute(Y, Z),['`'], {In = ['$']}.
attribute(In, In) -->
   [], {\+(In = ['$'])}.

selector(In, X) -->
   attribute_id(X), {In = ['$']}.
%tcob
selector(In, next(X)) -->
   attribute_id(X),['`'], {In = ['$']}.
selector(In, prev(X)) -->
   ['`'],attribute_id(X), {In = ['$']}.
selector(In, at(X,T)) -->
   attribute_id(X),[<],[num(T)],[>],{In = ['$']}.
selector(In, next(Ind)) -->
   attribute_id(X), ['['], terms(T), [']'],['`'], {In = ['$']}, {!}, {make_ind(X, T, Ind)}.
selector(In, prev(Ind)) -->
   ['`'],attribute_id(X), ['['], terms(T), [']'], {In = ['$']}, {!}, {make_ind(X, T, Ind)}.
selector(In, Ind) -->
   attribute_id(X), ['['], terms(T), [']'], {In = ['$']}, {!}, {make_ind(X, T, Ind)}.
selector(In, sel(Name, X)) -->
   selector_id(Name), {!}, terms(X), {In = ['$']}.
selector(In, Ind) -->
   attribute_id(X), ['['], terms(T), [']'], {!},
   {make_ind(ref(In, X), T, Ind)}, {\+(In = ['$'])}.
selector(In, ref(In, next(X))) -->
   attribute_id(X), ['`'],{!}, {\+(In = ['$'])}.
selector(In, ref(In, at(X,T))) -->
   attribute_id(X), [<],[num(T)],[>],{!}, {\+(In = ['$'])}.
selector(In, ref(In, X)) -->
   attribute_id(X), {!}, {\+(In = ['$'])}.
selector(In, sel(Name, X)) -->
   selector_id(Name), terms(X), {\+(In = ['$'])}.

selector_id(fst) --> [first].
selector_id(nxt) --> [next].
selector_id(lst) --> [last].

terms([X]) --> term(X).
terms([]) -->[].
%terms([next(X)]) --> term(X),['`'].
terms([X|T]) -->
   term(X), [','], {!}, terms(T). %make sure this cut is correct
literals([L]) --> literal(L).
literals([L|T]) -->
   literal(L), ['&'],{!}, literals(T).
   

literals([L|T]) -->
   literal(L), [','], literals(T).


literal(builtinclpr('nl')) -->
   [id('nl')], {!}.
literal(not(A)) -->
   [not], atom(A).
literal(A) --> atom(A).

 atom(X) --> 
   creational_constraint(X).  
atom(X) --> 
   constraint_atom(X).
  

class_id(X) --> [id(X)].
attribute_id(X) --> [id(X)].
constraint_predicate_id(Name) --> [id(Name)]. %For now.
variable(X) --> [id(X)].

predicates([class_predicates(Predicates)]) -->
   [predicates], {seen(predicates)}, pred_clauses(Predicates).
predicates([]) --> [].

pred_clauses([P|Rest]) -->
   one_pred_clause(P), pred_clauses(Rest).
pred_clauses([]) --> [].

one_pred_clause(['.']) -->
   ['.'], {!}. % this will cause a problem if "." is present within the definition of a pred clause.
one_pred_clause([X|Rest]) -->
   [X], one_pred_clause(Rest).

%--Parsing constructor section in class definition
constructors(Constructors) -->
   [constructors], {!}, {seen('constructors: near constraint #'),
   resetkeywordclausecounter(_)},  constructor_clauses(Constructors).
constructors(Constructors) -->
   [constructor], {!}, {seen('constructor: near constraint #'),
   resetkeywordclausecounter(_)},  constructor_clauses(Constructors), {Constructors = [_]}.
constructors([]) --> [].

%--Parsing constructors
constructor_clauses([constructor(Name, Attributes, ConstructorConstraintList)|T]) -->
   class_id(Name), {seterrormessage(_), namematch(Name)}, ['('], id_list(_,Attributes), [')'], ['{'], %resetkeywordclausecounter(_)},
   constraint_list(ConstructorConstraintList), ['}'], constructor_clauses(T). %correct call to id_list
constructor_clauses([constructor(Name, [], ConstructorConstraintList)|T]) -->
   class_id(Name), {seterrormessage(_), namematch(Name)}, ['('], [')'], ['{'], %resetkeywordclausecounter(_)},
   constraint_list(ConstructorConstraintList), ['}'], constructor_clauses(T). %correct call to id_list
constructor_clauses([]) --> [].

seterrormessage(_) :-
   classnamecounter(_), seen('constructor, the constructor name does not match class name in constructor # ').

namematch(Name) :-
   classnamecounter(Name), seen('constructor : near constraint #').
namematch(_) :-
   classnamecounter(_), fail.

make_ind(A, [], A).
make_ind(A, [T|Rest], Ind) :- 
   make_ind(ind(A, T), Rest, Ind).

resetclassnamecounter(_) :- 
   retractall(classnamecounter(_)), assert(classnamecounter(' # 1 near class name')).
setclassnamecounter(N) :- 
   retractall(classnamecounter(_)), assert(classnamecounter(N)).

resetkeywordclausecounter(_) :- 
   retractall(keywordclausecounter(_)), assert(keywordclausecounter(1)).
upkeywordclausecounter(_) :- 
   retract(keywordclausecounter(C)), C1 is C+1, assert(keywordclausecounter(C1)).

resetkeywordcounter(_) :- 
   retractall(parsing(_)).
seen(X) :- 
   retractall(parsing(_)), assert(parsing(X)).

%---added to accomodate formulae
boolexpr(Y)     --> 
   boolterm(X), boolterm2(X,Y).

boolterm2(X,X)  --> 
   [].
boolterm2(X,Z)  --> 
   [OP], {operator(OP, F)}, boolterm(Y), {T =.. [F,X,Y]}, boolterm2(T,Z).

boolterm(X) --> 
   int(X).
boolterm(A) --> 
   attribute(['$'], A).
boolterm(X) --> 
   ['('], boolexpr(X), [')'].
boolterm(not(X)) --> 
   [not], boolexpr(X). %%%%%%%%%%%????????????
%boolterm(Out) --> expr(_, Out).
boolterm(T) --> 
   var(X), ['('], exprlist(L), [')'], {T =.. [X|L]}.
boolterm(X) --> 
   var(X).
boolterm(compare(R, Term1, Term2)) --> 
   term(Term1), [R], {relop(R)}, {!}, term(Term2).

var(X)  --> 
   [id(X)].
var(next(X))  --> 
   [id(X)],['`'].

int(X)  --> 
   [num(X)].

exprlist([E])   --> 
   boolexpr(E).
exprlist([E|L]) --> 
   boolexpr(E), [','], exprlist(L).

operator(->, impl).
operator(and, and).
operator(or, or).

%--monitor clause parsing
debug(Debug) -->
   [monitor], {!},
   debug_list(Debug).
debug([]) --> [].

%--Get list of attributes to be monitored
debug_list(X) -->
   debug_attr(X), [';'].
debug_list([]) --> [].

debug_attr([X|T]) -->
   attribute_id(X), [','], {!}, debug_attr(T).
debug_attr([X]) -->
   attribute_id(X).

% ------------------ LEXICAL ANALYZER FOR TCOB&COB---------------------------

lex(Stream,Tokens) :-
   get_chars(Stream,L), !,
   tokenize(L,Tokens),!.

get_chars(Str,L) :-
   get_code(Str,C),
   get_chars(Str,C,L).
get_chars(_,36, []) :- !. %termination
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
tokenize([C|L], [X|L3]) :-
   alpha(C), identifier(X,[C|L],L2), !,
   tokenize(L2,L3).
tokenize(L, [quotedString(S)|L4]) :-
   special(quote,L,L2), !,
   quoted_string(StringAsListofAsciiCodes, L2, L3),
   name(S, StringAsListofAsciiCodes),
   tokenize(L3,L4).
tokenize(L, [X|L3]) :-
   special(X,L,L2), !,
   tokenize(L2,L3).
tokenize([C|L], L3) :-
   comment(C), !, skip_till_eol(L,L2),
   tokenize(L2,L3).
tokenize([C|_L], _) :-
   print('Error: Cannot tokenize the character1: '),
   name(BadChar, [C]),
   print(BadChar),
   fail.

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

special(quote, [39|L],L).
special('=\\=',[61,92,61|L],L).
special('{',[123|L],L).
special('}',[125|L],L).
special(':-',[58,45|L],L).
special('=<',[60,61|L],L).
special('>=',[61,62|L],L).
special('[]',[91,93|L],L).
special('[',[91|L],L).
special(']',[93|L],L).
special('$',[36|L],L).
special('&',[38|L],L).
special('->',[45,62|L],L).
special('-->',[45,45,62|L],L). %for new conditional condConstr
special(_,[95|L],L). % in alpha also.
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

identifier(X) --> 
   ident(L), {name(N,L),(keyword(N) -> X=N; X=id(N))}.

ident([X|L]) --> 
   letter(X), legits(L).
ident([X])   --> 
   letter(X).

legits([X|W]) --> 
   legit(X), legits(W).
legits([X])   --> 
   legit(X).

legit(X) --> 
   letter(X) ; digit(X).

letter(X) --> 
   [X],  {alpha(X)}.

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
keyword(belongs).
keyword(and).
keyword(or).
keyword(first).
keyword(next).
%keyword(last).
keyword(not).
keyword(min).
keyword(max).
%--Specific for tcob
keyword(series).
keyword('G').
keyword('F').
%--Debug 
keyword(monitor).

digits(dnum(N,M,Z)) --> 
   digs(K), specialdec(_), digs(L),
   {name(N, K),
   leading0(L,L2,Z),
   (L2 == [] -> M = 0 ; name(M, L2))
   }.

digits(num(N)) --> 
   digs(L), {name(N,L)}.

digs([X|L]) --> 
   digit(X), digs(L).
digs([X]) -->  
   digit(X).

digit(X) -->  
   [X],  {d09(X)}.

d09(X) :- 
   X > 47,  X < 58.

leading0([48|L],L2,N) :- 
   !, leading0(L,L2,M), N is M+1.
leading0([X|L],[X|L],0) :- 
   notzero(X), !.
leading0([],[],0).

notzero(X) :- 
   X > 48 ; X < 48.

enumerated(fromto(N, M)) --> 
   digitsoridentifier(N), specialenum(_), 
   digitsoridentifier(M), {!}.

digitsoridentifier(N) --> 
   digits(num(N)).
digitsoridentifier(X) --> 
   identifier(id(X)).

%--LEX ANALYZER END----------
   
