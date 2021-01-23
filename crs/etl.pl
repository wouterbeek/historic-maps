#!/usr/bin/env swipl

:- use_module(library(apply)).
:- use_module(library(persistency)).

:- use_module(library(rdf_api)).
:- use_module(library(rdf_mem)).
:- use_module(library(rdf_prefix)).
:- use_module(library(rdf_term)).

:- initialization(main, main).

:- persistent
   crs(iri:atom).

:- maplist(rdf_register_prefix, [
     graph-'https://triplydb.com/Triply/crs/graphs/',
     wms-'https://triply.cc/ogc/wms/def/'
   ]).

main :-
  db_attach('data.pl', []),
  rdf_create_iri(graph, instances, GraphName),
  rdf_create_graph(GraphName),
  dataset(GraphName, Dataset),
  forall(
    crs(Crs),
    assert_instance(Crs, crs:'Crs', Dataset)
  ),
  rdf_save_file('crs.nq.gz').
