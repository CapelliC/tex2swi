/*  File:    show_tex2swi.pl
    Author:  Carlo,,,
    Created: Sep  6 2020
    Purpose:
*/

:- module(show_tex2swi,
          [start/0, stop/0]).

:- use_module(library(http/http_server)).

%:- initialization
start :-
    http_server([port(8080)]).
stop :-
    http_stop_server(8080, []).

:- http_handler(root(.),
                http_redirect(moved, location_by_id(home_page)),
                []).
:- http_handler(root(home), home_page, []).

home_page(_Request) :-
    reply_html_page(
        [ title('Demo server'),
          script('https://cdnjs.cloudflare.com/ajax/libs/cytoscape/3.15.2/cytoscape.min.js')
        ],
        [ h1('Hello world!'),
          div(['with a check', input([type=checkbox,checked])]),
          div('host cy')
        ]).
