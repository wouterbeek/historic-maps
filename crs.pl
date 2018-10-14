:- module(crs, [download/0]).

:- use_module(library(xpath)).

:- use_module(library(file_ext)).
:- use_module(library(http/http_client2)).
:- use_module(library(xml_ext)).

:- curl.

download :-
  write_to_file('crs.pl.dat', run).

run(Out) :-
  run1(Out, 'http://www.opengis.net/def/crs/').

run1(Out, Uri) :-
  http_call(Uri, run_stream1(Out), [accept(xml)]).

run_stream1(Out, In) :-
  load_xml(In, [Dom]),
  forall(
    xpath(Dom, //identifier(normalize_space), Uri),
    run2(Out, Uri)
  ).

run2(Out, Uri) :-
  http_call(Uri, run_stream2(Out), [accept(xml)]).

run_stream2(Out, In) :-
  load_xml(In, [Dom]),
  forall(
    xpath(Dom, //identifier(normalize_space), Uri),
    run3(Out, Uri)
  ).

run3(Out, Uri) :-
  http_call(Uri, run_stream3(Out), [accept(xml)]).

run_stream3(Out, In) :-
  load_xml(In, [Dom]),
  forall(
    xpath(Dom, //identifier(normalize_space), Uri),
    format(Out, "crs('~a').\n", Uri)
  ).
