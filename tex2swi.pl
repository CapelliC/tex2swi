/*  File:    tex2swi.pl
    Author:  Carlo,,,
    Created: Feb 27 2020
    Purpose: publish on SWISH D.Warren' book about Prolog tabling.
      - LaTeX explained:
        https://en.wikibooks.org/wiki/LaTeX
      - LaTeX quick reference:
        http://www.emerson.emory.edu/services/latex/latex_toc.html
*/

:- module(tex2swi,
          [tex2swi/0
          ,git_show_status/0
          ]).

:- use_module(library(debug)).
:- use_module(library(git)).

:- use_module(parse_tex).
:- use_module(gen_swinb).
:- use_module(path_utility).

tex2swi :-
    parse_book(Book),
    generate_book(Book).

parse_book(Book) :-
    folder_book(Folder),
    parse_file(Folder,book,Book).

generate_book(Structure) :-
    folder_book(Folder),
    format(atom(ImagesFolder),'~s/~w',[Folder,figures]),
    assertion(exists_directory(ImagesFolder)),
    generate_swinb('examples/books/tabled_prolog',
                   'Tabled Prolog',book,ImagesFolder,Structure).

folder_book(LatexFolder) :-
    module_property(tex2swi,file(ModuleFile)),
    path_list(ModuleFile,List),
    append(FolderList,[_,_],List),
    path_list(LatexFolder,FolderList).
/*
x :-
    parse_book(Book),
    parse_show(Book).
*/
git_show_status :-
    directory(Here),
    is_git_directory(Here),
    git_shortlog(Here,ShortLog,[]),
    maplist(writeln,ShortLog).
/*
    git([commit,'-m','test commit'],[directory(Here)]),
    git([push],[directory(Here)]).
*/
