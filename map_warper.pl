:- module(
  map_warper,
  [
    run/1 % +Uri
  ]
).

/** <module> MapWarper scraper

The following URI can be used for testing:
http://warper.erfgoedleiden.nl/

---

@author Wouter Beek
@version 2018-10-11
*/

:- use_module(library(error)).
:- use_module(library(pprint)).
:- use_module(library(sgml)).
:- use_module(library(xpath)).

:- use_module(library(http/http_client2)).
:- use_module(library(uri_ext)).

:- use_module(wms).

:- curl.





%! run(+Uri:atom) is nondet.

run(Uri) :-
  uri_comps(Uri, uri(Scheme,Auth,Segments1,_,_)),
  append(Segments1, [maps], Segments2),
  between(1, inf, N),
  uri_comps(BaseUri, uri(Scheme,Auth,Segments2,[page(N),per_page(100)],_)),
  http_call(BaseUri, run(BaseUri), [accept(html)]).

run(BaseUri, In) :-
  load_html(In, Dom, [space(remove)]),
  xpath_chk(Dom, //div(@class=maplist_title), _), !,
  xpath(Dom, //div(@class=maplist_title)/a(@href), RelUri),
  uri_resolve(RelUri, BaseUri, Uri1),
  uri_comps(Uri1, uri(Scheme,Auth,Segments,_,_)),
  uri_comps(Uri2, uri(Scheme,Auth,Segments,_,'Exporteer_tab')),
  http_call(Uri2, map(Title, Description, Year, Uri3), [accept(html)]),
  get_map(Uri3).

map(Title, Description, Year, Uri, In) :-
  load_html(In, Dom, [space(remove)]),
  xpath_chk(Dom, //span(@class=maplist_title,normalize_space), Title),
  xpath_chk(Dom, //span(@class=map_description,normalize_space), Description),
  xpath_chk(Dom, //span(@class=map_summary)/span(@class=map_year,content), [Year0]),
  year_number(Year0, Year),
  once((
    xpath(Dom, //div(@id=export_window_content,content)//li(content)//a(@href), Uri),
    sub_string(Uri, _, 3, _, wms)
  )).

year_number(Atom1, Number) :-
  atom_concat('Jaar: ', Atom2, Atom1),
  atom_number(Atom2, Number), !.
year_number(Atom, _) :-
  domain_error(year, Atom).

get_map(Uri) :-
  get_capabilities(Uri, Dict),
  print_term(Dict, []), nl,%DEB
  wms_option(exception_formats, Dict, ExceptionFormat),
  wms_option(formats, Dict, Format),
  member(Layer, Dict.layers),
  wms_option(crses, Layer, CRS),
  get_map(
    Uri,
    Layer.ex_bbox,
    CRS,
    ExceptionFormat,
    Format,
    [Layer.name],
    size(250,250),
    [default],
    Dict.version
  ).

wms_option(Key, Dict, Value) :-
  get_dict(Key, Dict, Values),
  call(Key, Value),
  memberchk(Value, Values), !.





%  PREFERENCES %

crses(label('EPSG','3857')).
crses(label('EPSG','4269')).
crses(label('EPSG','4326')).
crses(label('EPSG','900913')).

exception_formats(media(application/'vnd.ogc.se_xml',_)).
exception_formats(media(application/'vnd.ogc.se_inimage',_)).
exception_formats(media(application/'vnd.ogc.se_blank',_)).

formats(media(image/'svg+xml',_)).
formats(media(image/png,_)).
formats(media(image/jpeg,_)).
formats(media(image/gif,_)).
formats(media(image/tiff,_)).
formats(media(application/'x-pdf',_)).
formats(media(application/'vnd.google-earth.kmz',_)).
formats(media(application/'vnd.google-earth.kml+xml',_)).
