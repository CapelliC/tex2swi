/*  File:    gen_swinb.pl
    Author:  Carlo,,,
    Created: Mar 20 2020
    Purpose:
*/

:- module(gen_swinb, [
              generate_swinb/5
          ]).

:- use_module(path_utility).

:- use_module(library(debug)).
:- use_module(library(filesex)).

:- use_module(library(http/html_write)).

:- dynamic gen_params/2.


/*
*/
generate_swinb(TargetFolder,Title,Name,ImagesFolder,Structure) :-
    swish_folder(SwishFolder),
    path_list(StructFolder,[SwishFolder,TargetFolder]),
    (   exists_directory(StructFolder)
    ->  true
    ;   make_directory_path(StructFolder)
    ),

    retractall(gen_params(_,_)),
    assertz(gen_params(StructFolder,ImagesFolder)),

    generate_structure(Title,Name,Structure).

swish_folder(SwishFolder) :-

    % adjust this path to your SWISH installation directory
    SwishRoot = '~/swish',

    expand_file_name(SwishRoot,[SwishFolder]),
    assertion(exists_directory(SwishFolder)).

generate_structure(Title,Name,Structure) :-
    phrase(overall_structure(Title,Structure),HtmlTokens),
    gen_params(Folder,_),
    swinb_file(Folder,Name,Swinb),
    open(Swinb,write,S),
    print_html(S,HtmlTokens),
    close(S),
    true.

overall_structure(Title,_Structure) -->
    html([
        \page_title(Title)
    ]).

page_title(Title) -->
    html([h1(Title), p([],`hello`)]).
