/*  File:    tex2swi.plt
    Author:  Carlo,,,
    Created: Feb 29 2020
    Purpose: test the parser and generator
*/

:- use_module(library(plunit)).

:- use_module(parse_tex).
:- use_module(test_utility).

:- begin_tests(tex2swi).

test(test_1,R==C) :-
  parse_test(test_1,R),
  parse_show(R),
  test_1_content(C).

test(test_2,C==[c(begin,[arg(`x`)],`abc\n`)]) :-
  phrase(parse_tex(C),`
   \\begin{x}
    abc
   \\end{x}`),
  parse_show(C).

test(test_3,[
       G==`abc\n`+c(begin,[arg(`y`)],`123\n`)+`def\n`
     ]) :-
  phrase(parse_tex(C),`
   \\begin{x}
    abc
    \\begin{y}
     123
    \\end{y}
    def
   \\end{x}`),
  C=[c(begin,[arg(`x`)],N)],
  group_codes_in_content(N,G)
  .

test(test_4,[T==U]) :-
  parse_test(test_4,R),
  R=[ c(documentclass,[arg(`article`)],[]),
      c(begin,[arg(`document`)],N)
    ],
  group_codes_in_content(N,G),
  G=`test_4, before test_1 !\n`+
    c(input,[arg(`test_1`)],U)+
    `test_4, after test_1 !\n`,
  test_1_content(T),
  deb('U:~w~nT:~w',[U,T])
  .

test(test_5,[]) :-
  parse_test(test_5,R),
  parse_show(R).

test(test_6,[
       S==[c(documentclass,[arg("article")],""),
           c(begin,[arg("document")],"Hello world!\n")]
     ]) :-
  test_1_content(C),
  stringify(C,S),
  stringized_show(0,S).

test(test_7,[S==c(hspace,[*,arg("4em")],"")]) :-
  stringify([c(hspace, [*, arg([52, 101, 109])], [])],S),
  stringized_show(0,S).

test(test_8,[]) :-
  parse_test(test_8,R),
  parse_show(R).
/*
test(test_9,[R==[c(documentclass,[arg("memoir")],"")]]) :-
  parse_test(test_9,R),
  parse_show(R).
*/
test(test_9,[]) :-
  writeln(test_9),
  parse_test(test_9,R),
  stringify(R,S),
  parse_show(R),
  writeln(r:R),
  writeln(s:S),
  phrase(gen_swinb:structured_text(S),L),
  writeln(l:L).

  % service

test_1_content(
    [ c(documentclass,[arg(`article`)],[]),
      c(begin,[arg(`document`)],
        `Hello world!\n`
       )
    ]).

:- end_tests(tex2swi).
