#!/usr/bin/env swipl

/* MapWarper scraper

@author Wouter Beek
@version 2018-10-14, 2021-01
*/

:- use_module(library(apply)).
:- use_module(library(error)).
:- use_module(library(pprint)).
:- use_module(library(sgml)).
:- use_module(library(xpath)).

:- use_module(library(dcg)).
:- use_module(library(http_client2)).
:- use_module(library(rdf_api)).
:- use_module(library(rdf_mem)).
:- use_module(library(rdf_prefix)).
:- use_module(library(rdf_term)).
:- use_module(library(uri_ext)).

:- load_files('../wms/wms.pl').

:- initialization(main, main).

:- maplist(rdf_register_prefix, [
     address-'https://druid.datalegend.net/IISG/historic-maps/id/address/',
     bbox-'https://druid.datalegend.net/IISG/historic-maps/id/bbox/',
     catalog-'https://druid.datalegend.net/IISG/historic-maps/id/catalog/',
     contact-'https://druid.datalegend.net/IISG/historic-maps/id/contact/',
     dcat,
     dct,
     def-'https://druid.datalegend.net/IISG/historic-maps/def/',
     graph-'https://druid.datalegend.net/IISG/historic-maps/graphs/',
     instant-'https://druid.datalegend.net/IISG/historic-maps/id/instant/',
     interval-'https://druid.datalegend.net/IISG/historic-maps/id/interval/',
     layer-'https://druid.datalegend.net/IISG/historic-maps/id/layer/',
     person-'https://druid.datalegend.net/IISG/historic-maps/id/person/',
     rdf,
     service-'https://druid.datalegend.net/IISG/historic-maps/id/service/',
     style-'https://druid.datalegend.net/IISG/historic-maps/id/style/',
     time,
     version-'https://druid.datalegend.net/IISG/historic-maps/id/version/'
   ]).

main :-
  forall(
    site(Name, Uri),
    run(Name, Uri)
  ).


%! run(+Name:atom, +Uri:atom) is nondet.

run(Name, Uri) :-
  uri_comps(Uri, uri(Scheme,Auth,Segments1,_,_)),
  append_segments(Segments1, [maps], Segments2),
  between(1, inf, N),
  uri_comps(BaseUri, uri(Scheme,Auth,Segments2,[page(N),per_page(100)],_)),
  rdf_create_iri(graph, Name, GraphName),
  rdf_create_graph(GraphName),
  dataset(GraphName, Dataset),
  rdf_create_iri(catalog, Name, Catalog),
  assert_instance(Catalog, dcat:'Catalog', Dataset),
  assert_label(Catalog, string(Name), Dataset),
  http_call(BaseUri, assert_maps(Dataset, Catalog, BaseUri), [accept(html)]),
  atomic_list_concat([Name,nq,gz], '.', File),
  rdf_save_file(File),
  rdf_retract_graphs.



%! site(+Name:atom, +Uri:uri) is semidet.
%! site(+Name:atom, -Uri:uri) is det.
%! site(-Name:atom, -Uri:uri) is multi.

site(erfgoedleiden, 'http://warper.erfgoedleiden.nl/').
site(mapwarper, 'https://mapwarper.net/').
site(nypl, 'http://maps.nypl.org/warper/').



assert_maps(Dataset, Catalog, BaseUri, In) :-
  load_html(In, Dom, [space(remove)]),
  xpath_chk(Dom, //div(@class=maplist_title), _),
  forall(
    xpath(Dom, //div(@class=maplist_title)/a(@href), RelUri),
    (
      uri_resolve(RelUri, BaseUri, Map),
      uri_comp_set(fragment, Map, 'Exporteer_tab', ExportUri),
      http_call(ExportUri, assert_map(Dataset, Catalog, Map), [accept(html)])
    )
  ).

assert_map(Dataset, Catalog, Map, In) :-
  load_html(In, Dom, [space(remove)]),
  xpath_chk(Dom, //span(@class=maplist_title,normalize_space), Title),
  xpath_chk(Dom, //span(@class=map_description,normalize_space), Description),
  xpath_chk(Dom, //span(@class=map_summary)/span(@class=map_year,content), [Atom]),
  (atom_phrase(year(Year), Atom) -> true ; domain_error(year, Atom)),
  once((
    xpath(Dom, //ul/li/a(@href), RequestUri),
    sub_string(RequestUri, _, 3, _, wms)
  )),
  assert_triple(Catalog, dcat:dataset, Map, Dataset),
  assert_instance(Map, def:'Map', Dataset),
  (   Description \== ''
  ->  assert_triple(Map, rdfs:comment, string(Description), Dataset)
  ;   true
  ),
  assert_triple(Map, dct:title, string(Title), Dataset),
  assert_label(Map, string(Title), Dataset),
  assert_triple(Map, rdfs:seeAlso, uri(Map), Dataset),
  assert_temporal_year(Dataset, Map, Year),
  (assert_capabilities(Dataset, RequestUri, Service) -> true ; throw(error(RequestUri))),
  assert_triple(Service, dcat:servesDataset, Map, Dataset).

assert_temporal_year(Dataset, Map, Year) :-
  assert_interval(date(Year,1,1), date(Year,12,31), Interval, Dataset),
  assert_instance(Interval, dct:'PeriodOfTime', Dataset),
  assert_triple(Map, dct:temporal, Interval, Dataset).

year(N) -->
  keyword, ":", whites,
  integer(N).

keyword --> "Depicts".
keyword --> "Jaar".
keyword --> "Year".
