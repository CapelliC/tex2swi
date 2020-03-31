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

:- use_module(library(dcg/high_order)).
:- use_module(library(http/html_write)).

:- dynamic gen_params/2.

generate_swinb(TargetFolder,Title,Name,ImagesFolder,Structure) :-
    swish_folder(SwishFolder),
    path_list(StructFolder,[SwishFolder,TargetFolder]),
    (   exists_directory(StructFolder)
    ->  true
    ;   make_directory_path(StructFolder)
    ),

    retractall(gen_params(_,_)),
    assertz(gen_params(StructFolder,ImagesFolder)),

    debug(tex2swi(gen_swinb),'~w',[gen_params(StructFolder,ImagesFolder)]),
    generate_structure(Title,Name,Structure),

    publish_entry_point(SwishFolder).


swish_folder(SwishFolder) :-

    % adjust this path to your SWISH installation directory
    SwishRoot = '~/swish',

    expand_file_name(SwishRoot,[SwishFolder]),
    assertion(exists_directory(SwishFolder)).

generate_structure_(_Title,_Name,Structure) :-
    forall(member(c(begin,[arg(X)],Doc),Structure),
           (   format(' ~s~n',[X]),
               forall(member(c(input,[arg(TexFile)],_),Doc),
                      format(' ~s~n',[TexFile]))
           )).

generate_structure(Title,Name,Structure) :-
    phrase(overall_structure(Title,Structure),HtmlTokens),
    html_swinb(Name,HtmlTokens).

html_swinb(Name,HtmlTokens) :-
    gen_params(Folder,_),
    swinb_file(Folder,Name,Swinb),
    debug(tex2swi(gen_swinb),'saving ~w',[html_swinb(Name)]),
    open(Swinb,write,S),
    print_html(S,HtmlTokens),
    close(S),
    debug(tex2swi(gen_swinb),'saved ~w',[html_swinb(Name)]).

overall_structure(Title,Structure) -->
    {memberchk(c(begin,[arg(`document`)],Doc),Structure)},
    html([
        \page_css,
        \page_title(Title),
        \chapters(Doc)
    ]).

class(link_to_chapter, 'background-color:red').
page_css -->
    html(
        style(\foreach(class(C,R), html(\['.~s {~s}~n'-[C,R]])))
    ).

chapters(Doc) -->
    foreach(
        (member(c(input,[arg(TexFile)],Content),Doc),
         memberchk(c(chapter,[arg(Name)],_),Content),
         chapter(Name,Content)
        ),
        html([
            div([a([class(link_to_chapter),
                    href('books/tabled_prolog/~s.swinb'-[TexFile])],
                   '~s'-[Name])])
        ])
    ).

page_title(Title) -->
    html([h1(Title), p([],`hello`)]).

chapter(Name,Content) :-
    phrase(content(Content),HtmlTokens),
    html_swinb(Name,HtmlTokens).

content(Content) -->
    {length(Content,Length)},
    html([
        \page_css,
        div('content length ~w'-[Length])
    ]).

publish_entry_point(SwishFolder) :-
    phrase(entry_point,HtmlTokens),
    with_output_to(string(ToAdd),print_html(HtmlTokens)),
    add_to_swinb_if_needed(SwishFolder,'examples/examples',ToAdd).

add_to_swinb_if_needed(Folder,BaseWinb,ToAdd) :-
    swinb_file(Folder,BaseWinb,Swinb),
    read_file_to_string(Swinb,Current,[]),
    (   sub_string(Current,_,_,_,ToAdd)
    ->  true
    ;   open(Swinb,append,S),
        write(S,ToAdd),
        close(S)
    ).

entry_point -->
    {gen_params(StructFolder,_ImagesFolder)},
    html([hr,
          div(class(notebook),
              div(class('nb-cell'),
                  a(href('~s/book.swinb'-[StructFolder]),'Tabled Prolog Book')))
         ]).
