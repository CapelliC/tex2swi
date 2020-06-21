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
          ,parse_print/1
          ]).

:- use_module(library(debug)).
%:- use_module(library(git)).

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
    %format(atom(ImagesFolder),'~s/~w',[Folder,figures]),
    %assertion(exists_directory(ImagesFolder)),
    TargetFolder = 'examples/books/tabled_prolog',
    prepare_images(Folder,TargetFolder,ImagesFolder),
    generate_swinb(
        TargetFolder, %'examples/books/tabled_prolog',
        'example/books/tabled_prolog',
        'Tabled Prolog',
        book,
        ImagesFolder,
        Structure).
/*
generate_book(Structure) :-
    folder_book(Folder),
    Config = {
        target_folder:'examples/books/tabled_prolog',
        target_urlbase:'example/books/tabled_prolog',
        book_title:'Tabled Prolog',
        basename:book
    },
    prepare_images(Config),
    generate_swinb(
        Config.target_folder, %'examples/books/tabled_prolog',
        Config.target_urlbase,%'example/books/tabled_prolog',
        Config.book_title, %'Tabled Prolog',
        Config.basename,%book,
        Config.images_folder,
        Structure).
*/

folder_book(LatexFolder) :-
    module_directory(tex2swi,ModuleDir),
    path_list(ModuleDir,List),
    append(DirList,[_],List),
    path_list(LatexFolder,DirList).
/*
folder_book(LatexFolder) :-
    module_property(tex2swi,file(ModuleFile)),
    path_list(ModuleFile,List),
    append(FolderList,[_,_],List),
    path_list(LatexFolder,FolderList).
*/

%!  prepare_images(+Folder,-ImagesFolder) is det
%
%   I'd like to keep the declarative graphics in native format,
%   which could be done with https://logand.com/sw/wps/, but to keep
%   things simple, right now translate .eps to .svg. Requires an unix
%   setup, since invokes ps2pdf and pdf2svg.
%
prepare_images(SourceFolder,TargetFolder,TargetImagesFolder) :-
    images_folder(TargetFolder,TargetImagesFolder),
    path_list(SourceImagesFolder,[SourceFolder,figures]),
    assertion(exists_directory(SourceImagesFolder)),
    path_list(GlobEPSs,[SourceImagesFolder,'*.eps']),
    expand_file_name(GlobEPSs,EPSs),
    maplist(eps2svg(TargetImagesFolder),EPSs,_SVGs).

eps2svg(SourceImagesFolder,Eps,Svg) :-
    directory_file_path(_D,F,Eps),
    re_replace('\\.eps$','.svg',F,S),
    re_replace('\\.eps$','.pdf',Eps,Pdf),
    path_list(Svg,[SourceImagesFolder,S]),
    (   exists_file(Svg) ->
        true
    ;   format(atom(X),'ps2pdf -dEPSCrop ~s ~s',[Eps,Pdf]),
        shell(X),
        assertion(exists_file(Pdf)),
        format(atom(Y),'pdf2svg ~s ~s',[Pdf,Svg]),
        shell(Y),
        delete_file(Pdf)
    ).

/*
git_show_status :-
    directory(Here),
    is_git_directory(Here),
    git_shortlog(Here,ShortLog,[]),
    maplist(writeln,ShortLog).
*/
parse_print(Name) :-
    folder_book(Folder),
    parse_file(Folder,Name,Parsed),
    %gen_swinb:content_title(Parsed,Title),
    format('title: ~s~n',[_Title]),
    parse_show(Parsed,0),
    %phrase(gen_swinb:content(Parsed,Title,_,_),HtmlTokens),
    html_write:print_html(_HtmlTokens).
