:- module(
  map_warper,
  [
    map/2,     % +MapUri, -Data
    scrape/1,  % -Data
    site/1,    % -SiteUri
    site_map/2 % +SiteUri, -MapUri
  ]
).

/** <module> MapWarper scraper

@author Wouter Beek
@version 2018
*/

:- use_module(library(error)).
:- use_module(library(pprint)).
:- use_module(library(sgml)).
:- use_module(library(xpath)).

:- use_module(library(http/http_client2)).
:- use_module(library(uri_ext)).

:- use_module(wms).

:- curl.





%! map(+MapUri:atom, -Data:dict) is det.

map(Uri1, Data) :-
  uri_comps(Uri1, uri(Scheme,Auth,Segments,_,_)),
  uri_comps(Uri2, uri(Scheme,Auth,Segments,_,'Exporteer_tab')),
  http_open2(Uri2, In, [accept(html)]),
  call_cleanup(
    map_stream(In, Data),
    close(In)
  ).

map_stream(In, Data) :-
  load_html(In, Dom, [space(remove)]),
  xpath_chk(Dom, //span(@class=maplist_title,normalize_space), Title),
  xpath_chk(Dom, //span(@class=map_description,normalize_space), Description),
  xpath_chk(Dom, //span(@class=map_summary)/span(@class=map_year,content), [Year0]),
  year_number(Year0, Year),
  once((
    xpath(Dom, //div(@id=export_window_content,content)//li(content)//a(@href), Uri0),
    sub_string(Uri0, _, 3, _, wms)
  )),
  uri_comps(Uri0, uri(Scheme,Auth,Segments,_,_)),
  uri_comps(Uri, uri(Scheme,Auth,Segments,_,_)),
  Data = data{
    description: Description,
    title: Title,
    wms: Uri,
    year: Year
  }.

year_number(Atom1, Number) :-
  atom_concat('Jaar: ', Atom2, Atom1),
  atom_number(Atom2, Number), !.
year_number(Atom, _) :-
  domain_error(year, Atom).



%! scrape(-Data:dict) is nondet.

scrape(Data) :-
  site(SiteUri),
  site_map(SiteUri, MapUri),
  map(MapUri, MapData),
  get_capabilities(MapData.wms, Dict),
  print_term(Dict, []), nl,
  wms_option(exception_formats, Dict, ExceptionFormat),
  wms_option(formats, Dict, Format),
  member(Layer, Dict.layers),
  wms_option(crses, Layer, CRS),
  get_map(
    MapData.wms,
    Layer.ex_bbox,
    CRS,
    ExceptionFormat,
    Format,
    [Layer.name],
    size(250,250),
    [default],
    Dict.version
  ),
  true.

wms_option(Key, Dict, Value) :-
  get_dict(Key, Dict, Values),
  call(Key, Value),
  memberchk(Value, Values), !.

crses(label('EPSG','3857')).
crses(label('EPSG','4269')).
crses(label('EPSG','4326')).
crses(label('EPSG','900913')).

exception_formats('application/vnd.ogc.se_xml').
exception_formats('application/vnd.ogc.se_inimage').
exception_formats('application/vnd.ogc.se_blank').

formats('image/svg+xml').
formats('image/png').
formats('image/jpeg').
formats('image/gif').
formats('image/tiff').
formats('application/x-pdf').
formats('application/vnd.google-earth.kmz').
formats('application/vnd.google-earth.kml+xml').



%! site(-Uri:atom) is multi.

site('http://warper.erfgoedleiden.nl/').



%! site_map(+SiteUri:atom, -MapUri:dict) is nondet.

site_map(Uri, AbsUri) :-
  uri_comps(Uri, uri(Scheme,Auth,Segments1,_,_)),
  append(Segments1, [maps], Segments2),
  between(1, inf, N),
  uri_comps(BaseUri, uri(Scheme,Auth,Segments2,[page(N),per_page(100)],_)),
  http_open2(BaseUri, In, [accept(html)]),
  call_cleanup(
    site_map_stream(BaseUri, In, AbsUri),
    close(In)
  ).

site_map_stream(BaseUri, In, AbsUri) :-
  load_html(In, Dom, [space(remove)]),
  xpath_chk(Dom, //div(@class=maplist_title), _), !,
  xpath(Dom, //div(@class=maplist_title)/a(@href), RelUri),
  uri_resolve(RelUri, BaseUri, AbsUri).
