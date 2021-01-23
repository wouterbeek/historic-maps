#!/usr/bin/env swipl

:- use_module(library(persistency)).
:- use_module(library(xpath)).

:- use_module(library(file_ext)).
:- use_module(library(http_client2)).
:- use_module(library(xml_ext)).

:- initialization(main, main).

:- persistent
   crs(iri:atom).

main :-
  db_attach('data.pl', []),
  run1('http://www.opengis.net/def/crs/').

run1(Uri) :-
  http_call(Uri, run_stream1, [accept(xml)]).

run_stream1(In) :-
  load_xml(In, [Dom]),
  forall(
    xpath(Dom, //identifier(normalize_space), Uri),
    run2(Uri)
  ).

run2(Uri) :-
  http_call(Uri, run_stream2, [accept(xml)]).

run_stream2(In) :-
  load_xml(In, [Dom]),
  forall(
    xpath(Dom, //identifier(normalize_space), Uri),
    run3(Uri)
  ).

run3(Uri) :-
  http_call(Uri, run_stream3, [accept(xml)]), !.
run3(Uri) :-
  format(user_error, "Unable to download <~a>.", [Uri]).

run_stream3(In) :-
  load_xml(In, [Dom]),
  forall(
    xpath(Dom, //identifier(normalize_space), Uri),
    assert_crs(Uri)
  ).
