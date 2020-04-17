/*  File:    gen_swinb.pl
    Author:  Carlo,,,
    Created: Mar 20 2020
    Purpose:
*/

:- module(gen_swinb, [
              generate_swinb/6
          ]).

:- use_module(path_utility).
:- use_module(test_utility).
%:- use_module(show_tree).

:- use_module(library(debug)).
%:- use_module(library(error)).
:- use_module(library(filesex)).

:- use_module(library(dcg/high_order)).
:- use_module(library(http/html_write)).

:- dynamic gen_params/3.

generate_swinb(TargetFolder,TargetBaseUrl,Title,Name,ImagesFolder,Structure) :-

    swish_folder(SwishFolder),
    path_list(StructFolder,[SwishFolder,TargetFolder]),
    (   exists_directory(StructFolder)
    ->  true
    ;   make_directory_path(StructFolder)
    ),

    retractall(gen_params(_,_,_)),
    assertz(gen_params(StructFolder,ImagesFolder,TargetBaseUrl)),

    generate_structure(Title,Name,Structure),
    publish_entry_point(SwishFolder).

swish_folder(SwishFolder) :-

    % adjust this path to your SWISH installation directory
    SwishRoot = '~/swish',

    expand_file_name(SwishRoot,[SwishFolder]),
    assertion(exists_directory(SwishFolder)).

generate_structure(Title,Name,Structure) :-
    stringify(Structure,Stringized),
    phrase(overall_structure(Title,Stringized),HtmlTokens),
    html_swinb(Name,HtmlTokens).

html_swinb(Name,HtmlTokens) :-
    gen_params(Folder,_,_),
    swinb_file(Folder,Name,Swinb),
    open(Swinb,write,S),
    print_html(S,HtmlTokens),
    close(S),
    deb(saved(Name)).

overall_structure(Title,Structure) -->
    {memberchk(c(begin,[arg("document")],Book),Structure)},
    html([
        div(class(notebook),
        [ \page_css,
          div(class('nb-cell html'),
              [   \page_title(Title),
                  \chapters(Book)
              ])
        ])]).

% not working in .swinb
% but keep it anyway...
page_css --> html({|html||
<style>
.to_next_chapter { background-color:green }
.to_prev_chapter { background-color:gold }
.to_chapter      { background-color:cyan }
ol { counter-reset: item }
li { display: block }
li:before {
  content: counters(item, ".") " ";
  counter-increment: item
}
</style>
|}).

chapter_props(Props,Content,
              _{name:Name,
                title:Title,
                href:Href,
                content:Content}) :-
    memberchk(input(Name),Props),
    memberchk(arg(Title),Props),
    format(string(Href), '~s.swinb', [Name]).

chapters(Book) -->
    {apply_inputs(Book,Whole),
     recover_structure(Whole,Structure),
     findall(ChProps,
             (   member(chapter(P,C),Structure),
                 chapter_props(P,C,ChProps)
             ),Valids),
     findall(N-C, nth1(N,Valids,C), Chapters)
    },
    html(ol(\chapters_items(Chapters))).

chapters_items(Chapters) -->
    foreach(
        member(Numbered,Chapters),
        html(li(\chapter_item(Numbered,Chapters)))).
chapter_item(Number-Chapter,Chapters) -->
    {deb(chapters,x(Number,Chapter.name)),
     optlink(Number-1,Chapters,PrevCh),
     optlink(Number+1,Chapters,NextCh),
     phrase(content(Chapter,PrevCh,NextCh),HtmlTokens),
     html_swinb(Chapter.name,HtmlTokens)
    },
    html([\link_title(Chapter), \sections_items(Chapter)]).

sections_items(Chapter) -->
    {memberchk(section(_,_),Chapter.content)}
    -> html(ol(\foreach(
                    with_title(section(Args,Sec),Chapter.content),
                    html(\section_item(Args,Sec)))))
    ; [].
section_item(Args,Section) -->
    html(li([\args_title(Args), \subsections_items(Section)])).

subsections_items(Section) -->
    {memberchk(subsection(_,_),Section)}
    -> html(ol(\foreach(
                    with_title(subsection(Args,Sub),Section),
                    html(\subsection_item(Args,Sub)))))
    ; [].
subsection_item(Args,SubSection) -->
    html(li([\args_title(Args), \subsubsections_items(SubSection)])).

subsubsections_items(SubSection) -->
    {memberchk(subsubsection(_,_),SubSection)}
    -> html(ol(\foreach(
                    with_title(subsubsection(Args,SubSubSection),SubSection),
                    html(\subsubsection_item(Args,SubSubSection)))))
    ; [].
subsubsection_item(Args,_Content) -->
    html(li(\args_title(Args))).

link_title(P) -->
    html(a(href(P.href), '~s'-[P.title])).

args_title(Args) -->
    {memberchk(arg(Title),Args) ; Title = '???'},
    !, html('~s'-[Title]).
with_title(Elem,Contents) :-
    member(Elem,Contents),
    arg(1,Elem,Args),
    memberchk(arg(_X),Args).

apply_inputs([c(input,[arg(TexName)],Content0)|R],C) :-
    !,
    deb(apply_inputs,TexName),
    apply_inputs(R,T),
    (   nth0(P,Content0,c(chapter,Args0,Chapter),Content1),
        Args1=[input(TexName)|Args0]
    ->  deb(apply_inputs,found(Args1)),
        nth0(P,Content2,c(chapter,Args1,Chapter),Content1),
        deb(apply_inputs,update(Args1))
    ;   Content2=Content0,
        deb(apply_inputs,skip(TexName))
    ),
    append(Content2,T,C).
apply_inputs([H|T],[H|R]) :-
    apply_inputs(T,R).
apply_inputs([],[]).

recover_structure(Whole,Structure) :-
    %dump_whole_structure(Whole),
    group_sequence_or_keep(Whole,chapter,Chapters),
    recover_sections(Chapters,Structure).
/*
dump_whole_structure(Whole) :-
    forall(nth1(I,Whole,S),
           (   S=c(chapter,X,_)
           ->  indent(0,`~d:~w~n`,[I,X])
           ;   S=c(section,X,_)
           ->  indent(2,`~d:~w~n`,[I,X])
           ;   S=c(subsection,X,_)
           ->  indent(4,`~d:~w~n`,[I,X])
           ;   S=c(subsubsection,X,_)
           ->  indent(6,`~d:~w~n`,[I,X])
           ;   true
           )).
*/
recover_sections(Chapters,Sections) :-
    maplist([chapter(C,Ls),chapter(C,Subs)]>>
                (   has_marker(section,Ls)
                ->  group_sequence(Ls,section,[],G),
                    recover_subsections(G,Subs)
                ;   Subs=Ls
                )
            , Chapters,Sections).
recover_subsections(Sections,SubSections) :-
    maplist([section(S,Ls),section(S,Subs)]>>
            (   has_marker(subsection,Ls)
            ->  group_sequence(Ls,subsection,[],G),
                recover_subsubsections(G,Subs)
            ;   Subs=Ls
            ),Sections,SubSections).
recover_subsubsections(SubSections,SubSubSections) :-
    maplist([subsection(T,Ls),subsection(T,SubSubs)]>>
            group_sequence_or_keep(Ls,subsubsection,SubSubs)
           ,SubSections,SubSubSections).

has_marker(Marker,Sequence) :-
    memberchk(c(Marker,_,_),Sequence).

group_sequence_or_keep(Sequence,Marker,Grouped) :-
    (   has_marker(Marker,Sequence)
    ->  group_sequence(Sequence,Marker,[],Grouped)
    ;   Grouped=Sequence
    ).

group_sequence(Sequence,Marker,Args0,[Block|Blocks]) :-
    append(S0,[c(Marker,Args,Empty)|Rest],Sequence),
    Block=..[Marker,Args0,S0],
    assertion(Empty==""),
    !, group_sequence(Rest,Marker,Args,Blocks).
group_sequence(S,Marker,Args,[Last]) :-
    Last=..[Marker,Args,S].

page_title(Title) -->
    html(h1('~s'-[Title])).
chapter_title(Title) -->
    html(h2(Title)).
section_title(Title) -->
    html(h3(Title)).
subsection_title(Title) -->
    html(h4(Title)).
subsubsection_title(Title) -->
    html(h5(Title)).

optlink(E,Chapters,Linked) :-
    I is E,
    memberchk(I-Linked,Chapters),
    !.
optlink(_,_,_).

navigation(Chapter,Prev,Next) -->
    {deb(navigation(Chapter.title))},
    html(div(class('nb-cell html'),
             table(tr([
                   td(\content_link(to_prev_chapter,Prev)),
                   td(\chapter_title(Chapter.title)),
                   td(\content_link(to_next_chapter,Next))
               ])))).

content_link(Class,Chapter) -->
    {var(Chapter)} -> []
    ;
    html(a([class(Class),href(Chapter.href)],Chapter.title)).

content(Chapter,PrevCh,NextCh) -->
    html(div(class(notebook),
             [\page_css,
              \navigation(Chapter,PrevCh,NextCh),
              \foreach(member(E,Chapter.content),
                       html(div(class('nb-cell html'),
                                \section_cell(E))))
             ])).

structured_text(T) -->
    (   {string(T)}
    ->  html('~s'-[T])
    ;   {is_list(T)}
    ->  html('~s'-[list])
    ;   html('~s'-[unknown])
    ).

section_cell(SecOrText) -->
    {SecOrText=section(Title,Content)}
    -> html([\section_title(Title),
          \foreach(member(C,Content),
                   html(\subsection_cell(C)))])
    ;  structured_text(SecOrText).
subsection_cell(SubOrText) -->
    {SubOrText=subsection(Title,Content)}
    -> html([\subsection_title(Title),
          \foreach(member(C,Content),
                   html(\subsubsection_cell(C)))])
    ;  structured_text(SubOrText).
subsubsection_cell(SubSubOrText) -->
    {SubSubOrText=subsubsection(Title,Content)}
    -> html([\subsubsection_title(Title),\structured_text(Content)])
    ;  structured_text(SubSubOrText).


publish_entry_point(SwishFolder) :-
    gen_params(_,_,Base),
    format(string(ToAdd),
"### Books
  - [Tabled Prolog Book](~w/book.swinb) by D.Warren
",
           Base),
    add_to_swinb_if_needed(SwishFolder,'examples/examples',ToAdd).

add_to_swinb_if_needed(Folder,BaseWinb,ToAdd) :-
    swinb_file(Folder,BaseWinb,Swinb),
    read_file_to_string(Swinb,Current,[]),
    (   sub_string(Current,_,_,_,ToAdd)
    ->  true
    ;   sub_string(Current,S,_,E,"</div>"),
        sub_string(Current,0,S,_,L),
        E2 is E+6, % length of </div>
        sub_string(Current,_,E2,0,R),
        open(Swinb,write,Stream),
        format(Stream,'~s~n~s~n~s',[L,ToAdd,R]),
        close(Stream)
    ).
