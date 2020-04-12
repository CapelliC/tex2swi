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
:- use_module(show_tree).

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
deb('saving ~w',html_swinb(Name)),
    open(Swinb,write,S),
    print_html(S,HtmlTokens),
    close(S),
deb('saved ~w',html_swinb(Name)).

overall_structure(Title,Structure) -->
    {memberchk(c(begin,[arg("document")],Book),Structure)},
    html([
        div(class(notebook),
        [ \page_css,
          div(class('nb-cell html'),
              [
                  \page_title(Title),
                  \chapters(Book)
              ])
        ])]).

/*
class(to_next_chapter, 'background-color:green').
class(to_prev_chapter, 'background-color:gold').
class(to_chapter, 'background-color:cyan').

page_css -->
    html(
        style([\foreach(class(C,R),
                       html(\['.~s {~s}~n'-[C,R]]))])).
*/
page_css --> html({|html||
<style>

.to_next_chapter {background-color:green}
.to_prev_chapter {background-color:gold}
.to_chapter      {background-color:cyan}

ol {
  counter-reset: item
}
li {
  display: block
}
li:before {
  content: counters(item, ".") " ";
  counter-increment: item
}
</style>
                  |}).
/*
,
`ol {
  counter-reset: item
}
li {
  display: block
}
li:before {
  content: counters(item, ".") " ";
  counter-increment: item
}`
              ])
    ).
*/

chapters(Book) -->
    {apply_inputs(Book,Whole),
     recover_structure(Whole,Structure)
    },
    html(ol(\chapters_items(Structure))).

chapters_items(Chapters) -->
    foreach(
        %member(chapter(Args,Chapter),Chapters),
        with_title(chapter(Args,Chapter),Chapters),
        html(li(\chapter_item(Args,Chapter)))).
chapter_item(Args,Chapter) -->
    html([\args_title(Args), \sections_items(Chapter)]).

sections_items(Chapter) -->
    {memberchk(section(_,_),Chapter)}
    -> html(ol(\foreach(
                    with_title(section(Args,Sec),Chapter),
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

args_title(Args) -->
    {memberchk(arg(Title),Args) ; Title = '???'},
    !, html('~s'-[Title]).
with_title(Elem,Contents) :-
    member(Elem,Contents),
    arg(1,Elem,Args),
    memberchk(arg(_X),Args),
    true.%format('X:~w~n',[_X]).

/*
chapter(Title,Chapter) -->
    html(['~s'-[Title],
          \foreach(member(subsection([arg(SubTitle)],Sub),Chapter),
                   html(li(\subsection(SubTitle,Sub))))
         ]).
subsection(SubTitle,SubSection) -->
    html(['~s'-[SubTitle],
          \foreach(member(subsubsection([arg(SubSubTitle)],SubSection),SubSection),
                   html(['~s'-SubSubTitle]))
         ]).
subsubsection(Title,_Content) -->
    html(['~s'-[Title],
          \foreach(false,html([]))]).
*/
apply_inputs([c(input,[arg(_)],Content)|R],C) :-
    !, apply_inputs(R,T),
    append(Content,T,C).
apply_inputs([H|T],[H|R]) :-
    apply_inputs(T,R).
apply_inputs([],[]).

recover_structure(Whole,Structure) :-
    dump_whole_structure(Whole),
    deb(recover_structure),
    group_sequence_or_keep(Whole,chapter,Chapters),
    recover_sections(Chapters,Structure),
    %show_tree(Structure,s),
    /*with_output_to(string(X), dump_structure(Structure)),
    write(X),*/
    true.
    %deb(structure(Structure)).

/*
subsections(Chapters,Subsections) :-
    maplist([chapter(C,Ls),chapter(C,SubSubs)]>>
            (  group_sequence_or_keep(Ls,subsection,Gs),
               maplist([subsection(S,Xs),subsection(S,Subs)]>>
                       group_sequence_or_keep(Xs,subsubsection,Subs),Gs,SubSubs)
            ),
            Chapters,Subsections).
*/
recover_sections(Chapters,Subsections) :-
    findall(chapter(C,Subs),
            (   member(chapter(C,Ls),Chapters),
                group_sequence_or_keep(Ls,section,G),
                recover_subsections(G,Subs)
            ),Subsections).
recover_subsections(Sections,Subsections) :-
    findall(section(S,Subs),
            (   member(section(S,Ls),Sections),
                group_sequence_or_keep(Ls,subsection,G),
                recover_subsubsections(G,Subs)
            ),Subsections).
recover_subsubsections(Subsections,SubSubSections) :-
    findall(subsection(T,SubSubs),
            (   member(subsection(T,Ls),Subsections),
                group_sequence_or_keep(Ls,subsubsection,SubSubs)
            ),SubSubSections).

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

/*
subsubsections(Subsections,Subsubsections) :-
    findall(chapter(C,Ls),member(chapter(C,Cs)),
    maplist([C,S]>>
            group_sequence(C,subsubsection,S,[]),
            Subsections,Subsubsections).
*/
group_sequence_or_keep(Sequence,Marker,Grouped) :-
    (   memberchk(c(Marker,_,_),Sequence)
    ->  group_sequence(Sequence,Marker,[],Grouped),
        true /*length(Grouped,L),
        debug(tex2swi(group_sequence_or_keep),'~s ~w ~d~n',[Marker,X,L])*/
    ;   Grouped=Sequence
    ).

group_sequence(Sequence,Marker,Args0,[Block|Blocks]) :-
    append(S0,[c(Marker,Args,Empty)|Rest],Sequence),
    Block=..[Marker,Args0,S0],
    assertion(Empty==""),
    !, group_sequence(Rest,Marker,Args,Blocks).
group_sequence(S,Marker,Args,[Last]) :-
    Last=..[Marker,Args,S].
/*
chapters_(Book) -->
    {findall(Input-Content,member(c(input,[arg(Input)],Content),Book),Inputs),
deb('chapters'),
     %NInputs=1
     length(Inputs,NInputs)
    },
    foreach(
        (between(1,NInputs,I),
         gen_chapter_swinb(I,Inputs,Name,Title),
deb('gen_chapter_swinb ~s ~s',[Name,Title])
        ),
        html([
            div([a([class(to_chapter),
                    href('~s.swinb'-[Name])],
                   '~s'-[Title])])
        ])
    ).
*/
page_title(Title) -->
    html(h3('~s'-[Title])).

gen_chapter_swinb(I,Inputs,Name,Title) :-
    nth1(I,Inputs,Name-Content),
deb('I ~d ~s',[I,Name]),
%parse_show(Content,0),
    optlink(I-1,Inputs,PrevCh),
    optlink(I+1,Inputs,NextCh),
    content_title(Content,Title),
deb('before phrase'),
    phrase(content(Content,Title,PrevCh,NextCh),HtmlTokens),
deb('after phrase'),
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
{deb('navigation ~s',Title)},
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
    gen_params(_,_,Base),
    %swinb_target_base_url(Base),
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
