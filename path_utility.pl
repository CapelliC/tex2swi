/*  File:    path_utility.pl
    Author:  Carlo,,,
    Created: Mar  1 2020
    Purpose:
*/

:- module(path_utility,
          [module_directory/2
          ,path_list/2
          ,tex_file/3
          ,swinb_file/3
          ,tex_cached_info/4
          ,cached_file/2
          ,cache_clear/0
          ]).

:- use_module(library(debug)).
:- use_module(library(filesex)).

module_directory(Module,Directory) :-
    module_property(Module,file(ModuleFile)),
    directory_file_path(Directory,_,ModuleFile).

% file name utility
path_list(Path,List) :-
    atomic_list_concat(List,/,Path).

tex_file(Folder,Basename,TexFile) :-
    ext_file(tex,Folder,Basename,TexFile).
swinb_file(Folder,Basename,TexFile) :-
    ext_file(swinb,Folder,Basename,TexFile).

ext_file(Ext,Folder,Basename,TexFile) :-
    format(atom(TexFile),'~s/~s.~s',[Folder,Basename,Ext]).

cache_directory(Cache_directory) :-
    module_directory(path_utility,ThisDirectory),
    format(atom(Cache_directory),'~s/~s',[ThisDirectory,cache]),
    (   exists_directory(Cache_directory)
    ->  true
    ;   make_directory(Cache_directory)
    ).

cached_file(Basename,Path) :-
    cache_directory(Cache_directory),
    ext_file(plc,Cache_directory,Basename,Path).

tex_cached_info(Folder,Basename,TexInfo,CachedInfo) :-
    tex_file(Folder,Basename,TexPath),
    info_path(TexPath,TexInfo),
    cached_file(Basename,CachedPath),
    info_path(CachedPath,CachedInfo),
    debug(path_utility(cache), '~w',
          [tex_cached_info(Folder,Basename,TexInfo,CachedInfo)]).

info_path(Path,info(Path,Timestamp)) :-
    (   exists_file(Path),
        time_file(Path,Timestamp)
    ->  true
    ;   Timestamp = ?
    ).

cache_clear :-
    cache_directory(D),
    directory_files(D,Files),
    maplist({D}/[F]>>(
                    path_list(P,[D,F]),
                    debug(path_utility(cache),'clear ~w',[P]),
                    (   exists_file(P)
                    ->  delete_file(P)
                    ;   true
                    )
                ), Files).
