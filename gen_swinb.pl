/*  File:    gen_swinb.pl
    Author:  Carlo,,,
    Created: Mar 20 2020
    Purpose:
*/

:- module(gen_swinb, [
              generate_swinb/5
          ]).

:- use_module(path_utility).
:- use_module(test_utility).

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

debug(tex2swi(_),'~w',[gen_params(StructFolder,ImagesFolder)]),
    generate_structure(Title,Name,Structure),

    publish_entry_point(SwishFolder).

swish_folder(SwishFolder) :-

    % adjust this path to your SWISH installation directory
    SwishRoot = '~/swish',

    expand_file_name(SwishRoot,[SwishFolder]),
    assertion(exists_directory(SwishFolder)).

generate_structure(Title,Name,Structure) :-
    phrase(overall_structure(Title,Structure),HtmlTokens),
    html_swinb(Name,HtmlTokens).

html_swinb(Name,HtmlTokens) :-
    gen_params(Folder,_),
    swinb_file(Folder,Name,Swinb),
debug(tex2swi(_),'saving ~w',[html_swinb(Name)]),
    open(Swinb,write,S),
    print_html(S,HtmlTokens),
    close(S),
debug(tex2swi(_),'saved ~w',[html_swinb(Name)]).

overall_structure(Title,Structure) -->
    {memberchk(c(begin,[arg(`document`)],Book),Structure)},
    html([
        \page_css,
        div(class(notebook),
            div(class('nb-cell html'),
                [
                    \page_title(Title),
                    \chapters(Book)
                ]))
    ]).

class(to_next_chapter, 'background-color:green').
class(to_prev_chapter, 'background-color:gold').
class(to_chapter, 'background-color:cyan').
page_css -->
    html(
        style(\foreach(class(C,R), html(\['.~s {~s}~n'-[C,R]])))
    ).

chapters(Book) -->
    {findall(Input-Content,member(c(input,[arg(Input)],Content),Book),Inputs),
debug(tex2swi(_),'chapters',[]),
     %NInputs=1
     length(Inputs,NInputs)
    },
    foreach(
        (between(1,NInputs,I),
         gen_chapter_swinb(I,Inputs,Name,Title),
debug(tex2swi(_),'gen_chapter_swinb ~s ~s',[Name,Title])
        ),
        html([
            div([a([class(to_chapter),
                    href('books/tabled_prolog/~s.swinb'-[Name])],
                   '~s'-[Title])])
        ])
    ).

page_title(Title) -->
    html(h3('~s'-[Title])).

gen_chapter_swinb(I,Inputs,Name,Title) :-
    nth1(I,Inputs,Name-Content),
debug(tex2swi(_),'I ~d ~s',[I,Name]),
%parse_show(Content,0),
    optlink(I-1,Inputs,PrevCh),
    optlink(I+1,Inputs,NextCh),
    content_title(Content,Title),
debug(tex2swi(_),'before phrase',[]),
    phrase(content(Content,Title,PrevCh,NextCh),HtmlTokens),
debug(tex2swi(_),'after phrase',[]),
    html_swinb(Name,HtmlTokens),
    !.

optlink(E,Inputs,Link) :-
    I is E,
    nth1(I,Inputs,Link),
    !.
optlink(_,_,_).

content_title(Content,Title) :-
    memberchk(c(chapter,[arg(Title)],_),Content) -> true ; Title = ? .

navigation(Title,Prev,Next) -->
{debug(tex2swi(_),'navigation ~s',[Title])},
    content_link(to_prev_chapter,Prev),
    page_title(Title),
    content_link(to_next_chapter,Next).

content_link(Class,Name-Content) -->
    {var(Name)} -> html(div(?))
    ;
    {content_title(Content,Title)},
    html(a([class(Class),href('~s.swinb'-[Name])],'~s'-[Title])).

content(Content,Title,PrevCh,NextCh) -->
    {length(Content,Length)},
    html([
        \page_css,
        div(class(notebook),[
                \navigation(Title,PrevCh,NextCh),
                div(class('nb-cell html'),
                    div('content length ~w'-[Length])
                   )
            ])
    ]).

publish_entry_point(SwishFolder) :-
    /*
    phrase(entry_point,HtmlTokens),
    with_output_to(string(ToAdd),print_html(HtmlTokens)),
    */
    format(string(ToAdd),"
### Books
  - [Tabled Prolog Book](~w/tabled_prolog/book.swinb) by D.Warren
    ",SwishFolder),
    add_to_swinb_if_needed(SwishFolder,'examples/examples',ToAdd).

add_to_swinb_if_needed(Folder,BaseWinb,ToAdd) :-
    swinb_file(Folder,BaseWinb,Swinb),
    read_file_to_string(Swinb,Current,[]),
    (   sub_string(Current,_,_,_,ToAdd)
    ->  true
    ;   sub_string(Current,S,_,E,"</div>"),
        sub_string(Current,0,S,_,L),
        sub_string(Current,E,_,0,R),
        open(Swinb,write,Stream),
        format(Stream,'~s~n~s~n~s',[L,ToAdd,R]),
        close(Stream)
    ).

entry_point -->
    {gen_params(StructFolder,_ImagesFolder)},
    html([%hr, not working...
          div(class(notebook),
              div(class('nb-cell'),
                  a(href('~s/book.swinb'-[StructFolder]),'Tabled Prolog Book')))
         ]).
