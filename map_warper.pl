/* MapWarper scraper

@author Wouter Beek
@version 2018-10-11, 2021-01-09
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

:- use_module(wms).

:- curl.

:- maplist(rdf_register_prefix, [
     def-'https://druid.datalegend.net/IISG/historic-maps/def/',
     rdf
   ]).





%! run is det.

run :-
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
  rdf_create_dataset(
    ['https://druid.datalegend.net/IISG/historic-maps/graphs/instances'],
    Dataset
  ),
  http_call(BaseUri, assert_maps(Dataset, BaseUri), [accept(html)]),
  atomic_list_concat([Name,nq,gz], '.', File),
  rdf_save_file(File).



%! site(+Name:atom, +Uri:uri) is semidet.
%! site(+Name:atom, -Uri:uri) is det.
%! site(-Name:atom, -Uri:uri) is multi.

site(erfgoedleiden, 'http://warper.erfgoedleiden.nl/').
site(mapwarper, 'https://mapwarper.net/').
site(nypl, 'http://maps.nypl.org/warper/').



assert_maps(Dataset, BaseUri, In) :-
  load_html(In, Dom, [space(remove)]),
  xpath_chk(Dom, //div(@class=maplist_title), _),
  forall(
    xpath(Dom, //div(@class=maplist_title)/a(@href), RelUri),
    (
      uri_resolve(RelUri, BaseUri, Map),
      uri_comp_set(fragment, Map, 'Exporteer_tab', ExportUri),
      http_call(ExportUri, assert_map(Dataset, Map), [accept(html)])
    )
  ).

assert_map(Dataset, Map, In) :-
  load_html(In, Dom, [space(remove)]),
  xpath_chk(Dom, //span(@class=maplist_title,normalize_space), Title),
  xpath_chk(Dom, //span(@class=map_description,normalize_space), Description),
  xpath_chk(Dom, //span(@class=map_summary)/span(@class=map_year,content), [Atom]),
  (atom_phrase(year(Year), Atom) -> true ; domain_error(year, Atom)),
  once((
    xpath(Dom, //ul/li/a(@href), RequestUri),
    sub_string(RequestUri, _, 3, _, wms)
  )),
  assert_instance(Map, def:'Map', Dataset),
  assert_triple(Map, rdfs:comment, string(Description), Dataset),
  assert_label(Map, string(Title), Dataset),
  assert_triple(Map, rdfs:seeAlso, uri(Map), Dataset),
  assert_triple(Map, sdo:dateCreated, year(Year), Dataset),
  (assert_capabilities(Dataset, RequestUri, Service) -> true ; throw(error(RequestUri))),
  assert_triple(Map, wms:serviceDescription, Service, Dataset).

year(N) -->
  keyword, ":", whites,
  integer(N).

keyword --> "Depicts".
keyword --> "Jaar".
keyword --> "Year".
