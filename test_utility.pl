/*  File:    test_utility.pl
    Author:  Carlo,,,
    Created: Mar 27 2020
    Purpose:
*/

:- module(test_utility,
          [parse_test/2
          ,parse_show/1
          ,parse_show/2
          ,group_codes_in_content/2
          ,stringify/2
          ,stringized_show/2
          ,deb/1
          ,deb/2
          ,deb/3
          ,indent/3
          ]).

%:- use_module(library(yall)).
:- use_module(library(debug)).
:- use_module(path_utility).

parse_test(F,R) :-
  module_directory(plunit_tex2swi,D),
  atom_concat(D,'/test',T),
  parse_file(T,F,R).

parse_show(R) :-
  (   debugging(tex2swi(parse_show))
  ->  with_output_to(string(S),parse_show(R,0)),
      debug(tex2swi(parse_show),'~s',[S])
  ;   true
  ).
parse_show([H|T],L) :-
  (   maplist(integer,[H|T])
  ->  indent(L,`~s`,[[H|T]])
  ;   group_codes_in_content([H|T],G),
      G\=[H|T]
  ->  parse_show(G,L)
  ;   parse_show(H,L),
      parse_show(T,L)
  ).
parse_show([],_).

parse_show(A+B,L) :-
  parse_show(A,L),
  parse_show(B,L).
parse_show(c(C,As,In),L) :-
  indent(L,`c ~w(`,[C]),
  parse_show(As,L+2),
  indent(L,`) {`,[]),
  parse_show(In,L+2),
  indent(L,`}`,[]).
parse_show(S,L) :-
  S =.. [F|As],
  indent(L,`~w :`,[F]),
  parse_show(As,L+1).

indent(Indent,Spec,Args) :-
  %debug(tex2swi(indent),'~w',[indent(Indent,Spec,Args)]),
  N is Indent,
  append(`~n~t~s~*|`,Spec,S),
  format(S,['',N|Args]).

%!  group_codes_in_content(+Content,-Grouped) is det
%
%   just groups char codes in a level
%
group_codes_in_content(Content,L+c(X,Y,Z)+Rg) :-
    append(L,[c(X,Y,Z)|R],Content),
    !,
    group_codes_in_content(R,Rg).
group_codes_in_content(C,C).

stringify(Content,Stringized) :-
  maplist(integer,Content),
  !,
  string_codes(Stringized,Content).
stringify(Content,Stringized) :-
  append(L,[c(X,Y,Z)|R],Content),
  maplist(stringify_args,Y,Ys),
  stringify(Z,Zs),
  (   L==[],R==[]
  ->  Stringized=c(X,Ys,Zs)
  ;   L\==[],R==[]
  ->  string_codes(Ls,L),
      Stringized=[Ls,c(X,Ys,Zs)]
  ;   L==[],R\==[]
  ->  stringify(R,Rs),
      (   is_list(Rs)
      ->  Stringized=[c(X,Ys,Zs)|Rs]
      ;   Stringized=[c(X,Ys,Zs),Rs]
      )
  ;   string_codes(Ls,L),
      stringify(R,Rs),
      (   is_list(Rs)
      ->  Stringized=[Ls,c(X,Ys,Zs)|Rs]
      ;   Stringized=[Ls,c(X,Ys,Zs),Rs]
      )
  ),
  !.
stringify(Content,Content) :-
  throw(cannot_stringify).

stringify_args(arg(A),arg(S)) :-
  stringify(A,S).
stringify_args(opt(A),opt(S)) :-
  stringify(A,S).
stringify_args(A,A) :-
  atomic(A).

/*
  is_list(Content)
  ->
  maplist(stringify,Content,Stringized).

    group_codes_in_content(Content,Group0),
    group_expr(Group0,Stringized).
*/
/*
group_expr(L0+[],R) :-
    !, group_expr(L0,R).
group_expr(L0+R,L:S) :-
    string_codes(S,R),
    !, group_expr(L0,L).
group_expr(T,R) :-
    group_term(T,R).

group_term(c(X,Ys,Z),c(X,Gs,G)) :-
    !,
    maplist(group_arg,Ys,Gs),
    group_content(Z,G).
group_term(Cs,S) :-
    string_codes(S,Cs).

group_arg(arg(C),arg(G)) :-
    !, group_content(C,G).
group_arg(opt(C),opt(G)) :-
    !, group_content(C,G).
group_arg(_,_) :-
    assertion(false).
*/

stringized_show(L,S) :-
  (   string(S)
  ->  indent(L,`~s`,[S])
  ;   is_list(S)
  ->  maplist(stringized_show(L),S)
  ;   S = c(C,As,In)
  ->  indent(L,`c ~w(`,[C]),
      stringized_show(L+2,As),
      indent(L,`) {`,[]),
      stringized_show(L+2,In),
      indent(L,`}`,[])
  ;   S =.. [F|As]
  ->  indent(L,`~w :`,[F]),
      stringized_show(L+1,As)
  ).

deb(As) :- deb('~w',As).
deb(Spec,As) :-
  context_module(Mod),
  deb(tex2swi(Mod),Spec,As).
deb(Selector,Spec,As) :-
  ( is_list(As) -> Args=As ; Args=[As] ),
  debug(Selector,Spec,Args).
