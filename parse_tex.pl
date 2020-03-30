/*  File:    parse_tex.pl
    Author:  Carlo,,,
    Created: Feb 27 2020
    Purpose:
*/

:- module(parse_tex,
          [parse_file/3
          ,parse_tex//1
          ,report_tex_commands/1
          ,report_tex_files_commands/1
          ]).

:- use_module(library(debug)).
:- use_module(library(readutil)).
:- use_module(library(aggregate)).
:- use_module(library(dcg/basics)).
:- use_module(library(portray_text)).
:- use_module(library(dcg/high_order)).

:- use_module(path_utility).

:- dynamic
    input_folder/1,
    current_tex/3,
    tex_command_seen/2.

%!  parse_file(+Folder,+Name,-Parsed) is det
%
%   Folder used for \input{tex_included}
%   Name must omit the .tex extension
%
parse_file(Folder,Basename,Parsed) :-
    parse_cached(Folder,Basename,Parsed),
    !.
parse_file(Folder,Basename,Parsed) :-
    % initialize status
    retractall(input_folder(_)),
    assertz(input_folder(Folder)),

    tex_codes(Basename,Codes,Len,NLs),

    retractall(current_tex(_,_,_)),
    assertz(current_tex(Basename,Len,NLs)),

    retractall(tex_command_seen(_,_)),

    phrase(parse_tex(Parsed),Codes),

    retract(current_tex(Basename,_,_)),
    cache_parsed_tex(Basename,Parsed).

parse_tex(Content) -->
    spc,
    content([],Content).

report_tex_commands(Report) :-
    setof(C-N,
          aggregate(count,T^tex_command_seen(T,C),N),
          Report).
report_tex_files_commands(Report) :-
    setof(File-Commands,
          setof(C-N,
                aggregate(count,tex_command_seen(File,C),N),
                Commands),
          Report).

%!  content(+Stops,-Content) is det
%
%   mirror content from input list, just handle nesting
%
content(Stops,[c(begin,[arg(`verbatim`)],As)|Cs]) -->
    "\\begin{verbatim}",
    string(As),
    "\\end",
    !,
    content(Stops,Cs).

content(Stops,Content) -->
    count_codes_to_eof(D),
    command(C,As),
    !,
    cursor(D,Cursor),
    {log_command(C,Cursor)},
    spc,
    (   {open_close(C,E)}
    ->  content([E/As|Stops],Inner),
        exec(c(C,As,Inner),Cursor,Result),
        content(Stops,Follow),
        {Content=[c(C,As,Result)|Follow]}
    ;   {open_close(_,C)}
    ->  {As=[F|_],
         debug(parse_tex(content),
               'at ~d,~s~nAs:~w~nStops:~w~nF:~w',
               [D,C,As,Stops,F]),
         % assume LaTeX source is properly nested
         assertion(Stops=[C/[F|_]|_]),
         Content=[]}
    ;   exec(c(C,As),Cursor,Result),
        content(Stops,Rest),
        {Content=[c(C,As,Result)|Rest]}
    ).

content(Stops,[$(S)|R]) -->
    "$", string(S), "$",
    !,
    content(Stops,R).
content(Stops,[C|Cs]) -->
    spc(C),
    !,
    content(Stops,Cs).

% these are better hardcoded instead of listing in open_close/2
content(['{}'],[]) -->
    "}",
    !.
content(['[]'],[]) -->
    "]",
    !.

content(Stops,[C|Cs]) -->
    [C],
    !,
    content(Stops,Cs).
content(Stops,[]) -->
    [],
    {assertion(Stops==[])}.

%!  open_close(?Open,?Close) is semidet
%
%   declare commands acting as open/close brackets
%
open_close(begin,end).

%!  command(-S,-As)// is det
%
%   parse a command (macro?) with arguments
%
command(S,As) -->
    "\\",
    command_name(S),
    sequence(argument,As),
    {debug(parse_tex(command),'~w As ~w',[S,As])},
    blanks.

argument(*) -->
    blanks,
    "*".
argument(opt(A)) -->
    blanks,
    "[",
    content(['[]'],A).
argument(arg(A)) -->
    blanks,
    "{",
    content(['{}'],A).

%!  exec(++Command,+Cursor,-Result)// is det
%
%   commands execution, just \input{tex_file} by now
%
exec(c(input,[arg(Tex)]),Cursor,Content) -->
    !,
    {debug(parse_tex(input),'input open ~s at ~w',[Tex,Cursor]),
     parse_input(Tex,Content),
     debug(parse_tex(input),'input done ~s',[Tex])
    }.
exec(c(_C,_As,Inner),_Cur,Inner) -->
    {true %log_command(C,Cur)
    }.
exec(c(_C,_As),_Cur,[]) -->
    {true %log_command(C,Cur)
    }.

log_command(C,cursor(Tex,Line,Off)) :-
    assertz(tex_command_seen(Tex,C)),
    debug(parse_tex(log_command),
          '~|~`0t~d~5+ ~|~`0t~d~3+ seen command ~w in ~w',
          [Line,Off,C,Tex]).

tex_codes(Basename,Codes,Len,NLs) :-
    input_folder(Folder),
    tex_file(Folder,Basename,TexFile),
    read_file_to_codes(TexFile,Codes,[]),
    length(Codes,Len),
    findall(P,nth1(P,Codes,0'\n),NLs).

parse_input(TexCs,Parsed) :-
    atom_codes(Tex,TexCs),
    input_folder(Folder),
    (   parse_cached(Folder,Tex,Parsed)
    ->  true
    ;   tex_codes(Tex,Codes,Len,NLs),
        retract(current_tex(TexC,LenC,NLsC)),
        assertz(current_tex(Tex,Len,NLs)),
        phrase(parse_tex(Parsed),Codes),
        retract(current_tex(Tex,Len,NLs)),
        assertz(current_tex(TexC,LenC,NLsC)),
        cache_parsed_tex(Tex,Parsed)
    ).

% tokenization utilities

char(Type,C) --> [C], { code_type(C,Type) }.
chars(Type,Cs) --> sequence(char(Type),Cs).

command_name(A) -->
    char(alpha,C),
    chars(alpha,Cs),
    {atom_codes(A,[C|Cs])}.

spc --> blank, !, blanks, spc.
spc --> "%", string_without("\n", _), !, spc.
spc --> [].

spc(C) --> char(space,C), !, blanks, spc.

count_codes_to_eof(D,L,L) :-
    length(L,D).

cursor(CountCodesToEof,cursor(Tex,Line,Off)) -->
    {current_tex(Tex,Len,NLs),
     FromStart is Len - CountCodesToEof,
     NLs=[F|_],
     (   FromStart < F
     ->  Line is 1,
         Off is FromStart+1
     ;   append(_,[A,B|_],NLs), A=<FromStart, B>FromStart,
         nth1(LineP,NLs,A),
         Line is LineP+1,
         Off is FromStart-A+1
     ),
     debug(parse_tex(cursor),
           '~d cursor:~w NLs:~w',
           [CountCodesToEof,cursor(Tex,Line,Off),NLs])
    },
    !.
cursor(CountCodesToEof,cursor(?,0,0)) -->
    {debug(parse_tex(cursor), '? ~d', [CountCodesToEof])}.

%--- caching parsed files

parse_cached(Folder,Name,Parsed) :-
    tex_cached_info(Folder,Name,
                    info(_TexPath,TexTS),
                    info(CachePath,CacheTS)),
    %assertion(float(TexTS)), % assume Tex file exists
    (   (TexTS = ? ; CacheTS = ? ; CacheTS < TexTS)
    ->  fail
    ;   % cache exists and has timestamp after tex
        portray_text(false),
        read_file_to_terms(CachePath,[Parsed],[])
    ).
cache_parsed_tex(Tex,Parsed) :-
    cached_file(Tex,Path),
    open(Path,write,S),
    portray_text(false),
    write_term(S,Parsed,[fullstop(true),quoted(true),portray(false)]),
    close(S).
