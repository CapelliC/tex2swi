/*  File:    presentest.pl
    Author:  Carlo,,,
    Created: Sep  6 2020
    Purpose:
*/

:- module(presentest,
          [start/0, stop/0]).

:- use_module(library(http/http_server)).
:- use_module(library(http/http_log)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_parameters)).
:- use_module(library(debug)).

:- use_module(test_utility).

:- initialization (catch(stop,_,true),start).
start :- http_server([port(8080)]).
stop :- http_stop_server(8080,[]).

:- http_handler(root(.),
                http_redirect(moved,location_by_id(home_page)),
                []).
:- http_handler(root(home),home_page,[]).

:- http_handler(root(select_test),select_test,[]).

home_page(_Request) :-
    reply_html_page(
        [ title('swi2tex test presentation'),
          script([src='https://cdnjs.cloudflare.com/ajax/libs/cytoscape/3.15.2/cytoscape.min.js'],'')
        ],
        [ h1('presentation of tex fragments parsing and rendering to html'),
          div([
            \expose_tests,
            'show parse by cytoscape',
            input([id=show_parse_by_cy,type=checkbox,checked])
          ]),
          \css,
          \js,
          div([class=container],[
            div([id=host_cy,class=column],'host cy'),
            div([id=host_tex,class=column],'host tex')
          ])
        ]).

expose_tests -->
  html(select([id=select_test], [
    option([value=0], '[ Select a test ]'),
    option([value=1], 'test_1'),
    option([value=4], 'test_4'),
    option([value=5], 'test_5'),
    option([value=8], 'test_8')
  ])).

css --> html(style('

#host_cy {
  height: 400px;
  background-color: gold;
}
#host_tex {
  height:400px;
  background-color:cyan;
}

/* from https://stackoverflow.com/a/34486579/874024 */
.container { display: flex; }
.container div:nth-child(1) { flex: 0 0 50%; }
.container div:nth-child(2) { flex: 1; }

')).

js --> html(script("

window.onload = () => {
  const request = (url,jsondata) => fetch(url,{
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },    
    body: JSON.stringify(jsondata)
  })
    
  select_test.onchange = async (ev) => {
    let s = await request('/select_test', {option:select_test.value})
    let d = await s.json()
    host_tex.innerHTML = d.tex.split('\\n').map(l => `<p>${l}</p>`).join('\\n')
  }
}

")).

select_test(Request) :-
  http_read_json_dict(Request,DictIn),
  format(atom(Tex),'test/test_~w.tex',[DictIn.option]),
  read_file_to_string(Tex,TexStr,[]),
  reply_json_dict(_{tex:TexStr}).
