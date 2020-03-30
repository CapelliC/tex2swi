/*  File:    test_utility.pl
    Author:  Carlo,,,
    Created: Mar 27 2020
    Purpose:
*/

:- module(test_utility,
          [parse_test/2
          ,parse_show/1
          ,group_codes_in_content/2
          ]).

%:- use_module(library(yall)).
:- use_module(library(debug)).
:- use_module(path_utility).

parse_test(F,R) :-
  module_directory(plunit_latex2swish,D),
  atom_concat(D,'/test',T),
  parse_file(T,F,R).

parse_show(R) :-
  (   debugging(tex2swi(parse_show))
  ->  with_output_to(string(S),parse_show(R,0)),
      debug(tex2swi(parse_show),'~s',[S])
  ;   true
  ).
/*
parse_show(R,L) :-
  %debug(tex2swi(parse_show),'in parse_show ~d,~q',[L,R]),
  (   maplist(integer,R)
  ->  indent(L,`~s`,[R])
  ;   R=A+B
  ->  parse_show(A,L),
      parse_show(B,L)
  ;   maplist(
        {L}/[E]>>
          (  %debug(tex2swi(parse_show),'in maplist',[]),
             is_list(E)
          -> parse_show(E,L+2)
          ;  E=c(C,As,InC)
          -> indent(L,`~w: (`,[C]),
             parse_show(As,L+2),
             group_codes_in_content(InC,G),
             parse_show(G,L+2),
             indent(L,`)`,[])
          ;  indent(L,`~w`,[E])
          ),R)
  ).
*/
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

/*
group_codes_in_content(C,G) :-
    append([L0|L1],[c(X,Y,Z)|R],C),
    !,
    group_codes_in_content(Z,ZG),
    group_codes_in_content(R,RG),
    append([[[L0|L1]],[c(X,Y,ZG)],RG],G).
group_codes_in_content(C,[C]).
*/

%!  group_codes_in_content(+Content,-Grouped) is det
%
%   just groups char codes in a level
%
group_codes_in_content(Content,L+c(X,Y,Z)+Rg) :-
    append(L,[c(X,Y,Z)|R],Content),
    !,
    group_codes_in_content(R,Rg).
group_codes_in_content(C,C).
