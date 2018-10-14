:- module(gis_server, []).

/** <module> GIS server

Leaflet supports the following CRSes: CRS:3857, CRS:3395, and
CRS:4326.  I don't think that these exist; maybe they are EPSG:3857,
EPSG:3395, and EPSG:4326.

---

@author Wouter Beek
@version 2018-10-13
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(http/http_dispatch)).

:- use_module(library(gis/wkt)).
:- use_module(library(html/html_ext)).
:- use_module(library(http/http_server)).
:- use_module(library(http/js_write)).
:- use_module(library(semweb/rdf_mem)).
:- use_module(library(semweb/rdf_prefix)).
:- use_module(library(semweb/rdf_term)).

:- http_handler(/, home_handler, [methods([get,head,options])]).

http:media_types(home_handler, [media(text/html,[])]).

:- initialization
   rdf_load_file('wms.nt.gz').

:- maplist(rdf_register_prefix, [
     'EPSG'-'http://www.opengis.net/def/crs/EPSG/0/',
     geo,
     rdfs,
     wms-'https://demo.triply.cc/ogc/wms/def/'
   ]).

home_handler(Request) :-
  rest_method(Request, home_method).

home_method(Method, MediaTypes) :-
  http_is_get(Method),
  rest_media_type(MediaTypes, home_media_type).

home_media_type(media(text/html,_)) :-
  html_page([\home_head], [\home_body]).

home_body -->
  {map_layers('http://warper.erfgoedleiden.nl/maps/629', Service, Layers)},
  html([
    h1("Welcome to the GIS test server"),
    div([id(mapid),style('width: 1800px; height: 800px;')], []),
    \layers(Service, Layers)
  ]).

layers(Service, [layer(Name1,Title1,point(X,Y))]) --> !,
  js_script({|javascript(Name1,Service,Title1,X,Y)||
    var map = L.map(mapid, {center: [Y,X], zoom: 15});
    var maps = {
      Title1: L.tileLayer.wms(Service, {layers: Name1})
    };
    L.control.layers(maps).addTo(map);
  |}).
layers(Service, [layer(Name1,Title1,point(_X,_Y)),layer(Name2,Title2,point(X,Y))]) -->
  js_script({|javascript(Name1,Name2,Service,Title1,Title2,X,Y)||
    var map = L.map(mapid, {center: [Y,X], zoom: 15});
    var maps = {
      Title1: L.tileLayer.wms(Service, {layers: Name1}),
      Title2: L.tileLayer.wms(Service, {layers: Name2})
    };
    L.control.layers(maps).addTo(map);
  |}).

home_head -->
  html([
    title("GIS test server"),
    link([
      rel(stylesheet),
      href('https://unpkg.com/leaflet@1.3.4/dist/leaflet.css'),
      integrity('sha512-puBpdR0798OZvTTbP4A8Ix/l+A4dHDD0DGqYW6RQ+9jxkRFclaxxQb/SJAWZfWAkuyeQUytO7+7N4QKrDh+drA=='),
      crossorigin('')
    ], []),
    script([
      src('https://unpkg.com/leaflet@1.3.4/dist/leaflet.js'),
      integrity('sha512-nMMmRyTVoLYqjP9hrbed9S+FzjZHW5gY1TWCHA5ckwXZBadntCNs8kEqAWdrb9O7rxbCaA4lKTIWjDXZxflOcA=='),
      crossorigin('')
    ], [])
  ]).

map_layers(Map, Service, Layers) :-
  rdf_triple(Map, wms:serviceDescription, Service),
  aggregate_all(set(Layer), map_layer(Service, Layer), Layers).

map_layer(Service, layer(LayerName,LayerTitle,point(X,Y))) :-
  rdf_triple(Service, wms:layer, Layer),
  rdf_triple(Layer, wms:name, LayerName0),
  rdf_literal_value(LayerName0, LayerName),
  LayerName \== "MapWarper",
  rdf_triple(Layer, rdfs:label, LayerTitle0),
  rdf_literal_value(LayerTitle0, LayerTitle),
  rdf_triple(Layer, geo:hasGeometry, Geo),
  rdf_triple(Geo, wms:crs, 'EPSG':'4326'),
  rdf_triple(Geo, geo:asWKT, literal(type(geo:wktLiteral,Lex))),
  wkt_shape_atom(shape(_,_,_,Term), Lex),
  Term = 'Polygon'(['LineString'(['Point'([XMax,YMax]),_,'Point'([XMin,YMin]),_,_])]),
  X is (XMin+XMax)/2,
  Y is (YMin+YMax)/2.
