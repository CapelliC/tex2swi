/*  File:    show_tree.pl
    Author:  Carlo,,,
    Created: Apr  7 2020
    Purpose:
*/

:- module(show_tree,
          [show_tree/2
          ]).

:- use_module(library(pce)).

% from https://rosettacode.org/wiki/Visualize_a_tree#XPCE
%
/*
show_tree(Data) :-
    % direction may be horizontal/vertical/list
    Direction = vertical,%list,
    sformat(A, 'Display tree ~w', [Direction]),
    new(D, window(A)),
    send(D, size, size(350,200)),
    new(T, tree(text('Root'))),
    send(T, neighbour_gap, 2),
    show_tree(T,Data),
    send(T, direction, Direction),
    send(D, display, T),
    send(D, open).

show_tree(T,_Data) :-
    new(S1, node(text('Child1'))),
    new(S2, node(text('Child2'))),
    send_list(T, son,[S1,S2]),
    new(S11, node(text('Grandchild1'))),
    new(S12, node(text('Grandchild2'))),
    send_list(S1, son, [S11, S12]),
    new(S21, node(text('Grandchild3'))),
    new(S22, node(text('Grandchild4'))),
    send_list(S2, son, [S21, S22]).

*/
show_tree(Data,Title) :-
    % direction may be horizontal/vertical/list
    new(D, picture(Title)),
    send(D, size, size(600,600)),
    Data=..[F|As],
    new(T, tree(text(F))),
    (   is_list(Data)
    ->  maplist(show_data(T),Data)
    ;   maplist(show_data(T),As)
    ),
    %send(T, neighbour_gap, 2),
    send(T, direction, list),
    send(D, display, T),
    send(D, open).

show_data(T,Data) :-
    (   var(Data)
    ->  format(atom(Label),'VAR ~w',[Data]),
        As=[]
    ;   is_list(Data)
    ->  Label='[|]',
        As=Data
    ;   Data=..[Label|As]
    ),
    new(S, node(text(Label))),
    send(T, son, S),
    maplist(show_data(S),As).


