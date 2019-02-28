:- module(
  map_warper,
  [
    assert_maps/1 % +Uri
  ]
).

/** <module> MapWarper scraper

The following URIs can be used for testing:

  - http://maps.nypl.org/warper/
  - http://warper.erfgoedleiden.nl/

---

@author Wouter Beek
@version 2018-10-11
*/

:- use_module(library(apply)).
:- use_module(library(error)).
:- use_module(library(pprint)).
:- use_module(library(sgml)).
:- use_module(library(xpath)).

:- use_module(library(dcg)).
:- use_module(library(http/http_client2)).
:- use_module(library(semweb/rdf_api)).
:- use_module(library(semweb/rdf_mem)).
:- use_module(library(semweb/rdf_prefix)).
:- use_module(library(semweb/rdf_term)).
:- use_module(library(uri_ext)).

:- use_module(wms).

:- curl.

:- maplist(rdf_register_prefix, [
     def-'https://triply.cc/open-geo-metadata/def/',
     rdf
   ]).





%! assert_maps(+Uri:atom) is nondet.

assert_maps(Uri) :-
  uri_comps(Uri, uri(Scheme,Auth,Segments1,_,_)),
  append_segments(Segments1, [maps], Segments2),
  between(1, inf, N),
  uri_comps(BaseUri, uri(Scheme,Auth,Segments2,[page(N),per_page(100)],_)),
  rdf_default_graph(G),
  B = mem(G),
  http_call(BaseUri, assert_maps(B, BaseUri), [accept(html)]).

assert_maps(B, BaseUri, In) :-
  load_html(In, Dom, [space(remove)]),
  xpath_chk(Dom, //div(@class=maplist_title), _), !,
  xpath(Dom, //div(@class=maplist_title)/a(@href), RelUri),
  uri_resolve(RelUri, BaseUri, Map),
  uri_comp_set(fragment, Map, 'Exporteer_tab', ExportUri),
  http_call(ExportUri, assert_map(B, Map), [accept(html)]).

assert_map(B, Map, In) :-
  load_html(In, Dom, [space(remove)]),
  xpath_chk(Dom, //span(@class=maplist_title,normalize_space), Title),
  xpath_chk(Dom, //span(@class=map_description,normalize_space), Description),
  xpath_chk(Dom, //span(@class=map_summary)/span(@class=map_year,content), [Atom]),
  (atom_phrase(year(Year), Atom) -> true ; domain_error(year, Atom)),
  once((
    xpath(Dom, //ul/li/a(@href), RequestUri),
    sub_string(RequestUri, _, 3, _, wms)
  )),
  assert_triple(B, Map, rdf:type, def:'Map'),
  assert_triple(B, Map, rdfs:comment, string(Description)),
  assert_triple(B, Map, rdfs:label, string(Title)),
  assert_triple(B, Map, rdfs:seeAlso, uri(Map)),
  assert_triple(B, Map, schema:dateCreated, year(Year)),
  (assert_capabilities(B, RequestUri, Service) -> true ; throw(error(RequestUri))),
  assert_triple(B, Map, wms:serviceDescription, Service).

year(N) -->
  keyword, ":", whites,
  integer(N).

keyword --> "Depicts".
keyword --> "Jaar".
keyword --> "Year".
