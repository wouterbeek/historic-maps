:- encoding(utf8).
:- module(
  wms,
  [
    assert_capabilities/3, % +Dataset, +RequestUri, -Service
    get_map/9              % +Uri,
                           % +BBox,
                           % +Crs,
                           % +ExceptionFormat,
                           % +Format,
                           % +Layers,
                           % +Size,
                           % +Styles,
                           % +Version
  ]
).

/** <module> WMS Client

The following URI can be used for testing:
http://metaspatial.net/cgi-bin/ogc-wms.xml

---

@author Wouter Beek
@version 2018-10-14, 2021-01
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(error)).
:- use_module(library(lists)).
:- use_module(library(ordsets)).
:- use_module(library(solution_sequences)).
:- use_module(library(xpath)).

:- use_module(library(call_ext)).
:- use_module(library(dcg)).
:- use_module(library(dict)).
:- use_module(library(http_client2)).
:- use_module(library(media_type)).
:- use_module(library(os_ext)).
:- use_module(library(pair_ext)).
:- use_module(library(rdf_api)).
:- use_module(library(rdf_mem)).
:- use_module(library(rdf_prefix)).
:- use_module(library(rdf_term)).
:- use_module(library(uri_ext)).
:- use_module(library(xml_ext)).

:- maplist(rdf_register_prefix, [
     'AUTO2'-'http://www.opengis.net/def/crs/OGC/1.3/AUTO',
     'CRS'-'http://www.opengis.net/def/crs/OGC/1.3/CRS',
     dcat,
     'EPSG'-'http://www.opengis.net/def/crs/EPSG/0/',
     geo,
     mediaType-'https://triplydb.com/Triply/tmt/id/',
     rdfs,
     sdo,
     tg-'https://triplydb.com/Triply/tg/def/',
     tv-'https://triplydb.com/Triply/tv/def/',
     wms-'https://triplydb.com/Triply/wms/def/'
   ]).





%! assert_capabilities(+Dataset:dataset, +RequestUri:atom, -Service:iri) is det.
%
% Returns the service metadata, using the highest version that is
% supported by the server.

assert_capabilities(Dataset, RequestUri1, Service) :-
  uri_local_name(RequestUri1, Local),
  rdf_create_iri(service, Local, Service),
  assert_instance(Service, wms:'Service', Dataset),
  assert_triple(Service, rdfs:seeAlso, uri(RequestUri1), Dataset),
  uri_strip(RequestUri1, ServiceUrl),
  uri_comp_set(
    query,
    ServiceUrl,
    [request('GetCapabilities'),service('WMS')],
    RequestUri2
  ),
  http_call(
    RequestUri2,
    assert_capabilities_stream(Dataset, Service, RequestUri2),
    [accept(media(application/'vnd.ogc.wms_xml',[charset('UTF-8')]))]
  ).



%! get_map(+Uri:atom,
%!         +BBox:compound,
%!         +Crs:compound,
%!         +ExceptionFormat:compound,
%!         +Format:compound,
%!         +Layers:list(atom),
%!         +Size:compound,
%!         +Styles:list(atom),
%!         +Version:compound) is det.
%
% @tbd transparent(boolean)
% @tbd bgcolor(atom)
% @tbd time(?)
% @tbd elevation(?)

get_map(
  Uri1,
  bbox(min(XMin,YMin),max(XMax,YMax)),
  Crs,
  ExceptionFormat,
  Format,
  Layers,
  size(Height,Width),
  Styles,
  Version
) :-
  atomic_list_concat([XMin,YMin,XMax,YMax], ',', BBoxAtom),
  wms_crs_term(Crs, CrsAtom),
  (   Version == version(1,1,1)
  ->  CrsQuery = srs(CrsAtom)
  ;   Version == version(1,3,0)
  ->  CrsQuery = crs(CrsAtom)
  ),
  atom_phrase(media_type(ExceptionFormat), ExceptionFormatAtom),
  atom_phrase(media_type(Format), FormatAtom),
  atomic_list_concat(Layers, ',', LayersAtom),
  atomic_list_concat(Styles, ',', StylesAtom),
  wms_version_term(Version, VersionAtom),
  uri_comps(Uri1, uri(Scheme,Auth,Segments,_,_)),
  Query = [
    bbox(BBoxAtom),
    CrsQuery,
    exception(ExceptionFormatAtom),
    format(FormatAtom),
    height(Height),
    layers(LayersAtom),
    request('GetMap'),
    service('WMS'),
    styles(StylesAtom),
    version(VersionAtom),
    width(Width)
  ],
  uri_comps(Uri2, uri(Scheme,Auth,Segments,Query,_)),
  http_download(
    Uri2,
    File1,
    [
      accept(media(application/'vnd.ogc.wms_xml',[charset('UTF-8')])),
      metadata(Metas)
    ]
  ),
  Metas = [Meta|_],
  dict_get(headers, Meta, Headers),
  dict_get('content-type', Headers, [Atom]),
  atom_phrase(media_type(MediaType), Atom),
  media_type_extension(MediaType, Ext),
  atomic_list_concat(Layers, -, Base),
  file_name_extension(Base, Ext, File2),
  rename_file(File1, File2),
  writeln(File2),
  open_file(File2).





% ASSERT %

%! assert_bboxes(+Dataset:dataset,
%!               +Version:compound,
%!               +LayerDom:compound,
%!               +Parent:dict,
%!               +Layer:iri,
%!               -Pairs:list(pair(compound,list(integer)))) is det.
%
% A Layer may have multiple BoundingBox elements, but each one shall
% state a different CRS.  A Layer inherits any BoundingBox values
% defined by its parents.  A BoundingBox inherited from the parent
% Layer for a particular CRS is replaced by any declaration for the
% same CRS in the child Layer.  A BoundingBox in the child for a new
% CRS not already declared by the parent is added to the list of
% bounding boxes for the child Layer.  A single Layer element shall
% not contain more than one BoundingBox for the same CRS.

assert_bboxes(Dataset, Version, LayerDom, Parent, Layer, Pairs) :-
  dict_get(bboxes, Parent, [], ParentPairs),
  aggregate_all(set(SelfPair), wms_bbox(Version, LayerDom, SelfPair), SelfPairs),
  merge_pairs(SelfPairs, ParentPairs, Pairs),
  maplist(assert_bbox(Dataset, Layer), Pairs).

assert_bbox(Dataset, Layer, CrsTerm-[XMax,YMax,XMin,YMin,XRes,YRes]) :-
  include(ground, [XMax,YMax,XMin,YMin,XRes,YRes], L),
  rdf_hash_iri(bbox, CrsTerm-L, BBox),
  assert_triple(Layer, geo:hasGeometry, BBox, Dataset),
  assert_instance(BBox, tg:'BoundingBox', Dataset),
  rdf_equal('CRS':'84', Crs),
  Shape = shape(
    false,
    false,
    Crs,
    'Polygon'([
      'LineString'([
        'Point'([XMax,YMax]),
        'Point'([XMax,YMin]),
        'Point'([XMin,YMin]),
        'Point'([XMin,YMax]),
        'Point'([XMax,YMax])
      ])
    ])
  ),
  (   skip_polygon(XMax, YMax, XMax, YMin, XMin, YMin, XMin, YMax, XMax, YMax)
  ->  true
  ;   (abs(YMax - YMin) > 50 -> rdf_object_dwim(Shape, Term), writeln(Term) ; true),
      assert_triple(BBox, geo:asWKT, Shape, Dataset)
  ),
  (var(XRes) -> true ; assert_triple(BBox, tg:resolutionX, string(XRes), Dataset)),
  (var(YRes) -> true ; assert_triple(BBox, tg:resolutionY, string(YRes), Dataset)),
  assert_crs_term(Dataset, BBox, CrsTerm).

skip_polygon(-75.9923, 3347.04, -75.9923, 25.592, -7261.93, 25.592, -7261.93, 3347.04, -75.9923, 3347.04).
skip_polygon(0.78581, 112.57, 0.78581, -18.2331, -208.262, -18.2331, -208.262, 112.57, 0.78581, 112.57).
skip_polygon(144.888, 105.14, 144.888, -98.8381, -174.992, -98.8381, -174.992, 105.14, 144.888, 105.14).
skip_polygon(158.99, 102.254, 158.99, -102.104, -271.622, -102.104, -271.622, 102.254, 158.99, 102.254).
skip_polygon(162.564, 93.9454, 162.564, -99.6109, -164.214, -99.6109, -164.214, 93.9454, 162.564, 93.9454).
skip_polygon(180, 90, 180, -90, -180, -90, -180, 90, 180, 90).
skip_polygon(199.385, 126.574, 199.385, -84.2338, -194.016, -84.2338, -194.016, 126.574, 199.385, 126.574).
skip_polygon(214.616, 113.254, 214.616, -150.897, -199.801, -150.897, -199.801, 113.254, 214.616, 113.254).
skip_polygon(233.267, 127.447, 233.267, -88.6602, -39.7652, -88.6602, -39.7652, 127.447, 233.267, 127.447).
skip_polygon(238.159, 164.4, 238.159, -362.562, -394.629, -362.562, -394.629, 164.4, 238.159, 164.4).
skip_polygon(85.3485, 348.84, 85.3485, -54.4906, -149.221, -54.4906, -149.221, 348.84, 85.3485, 348.84).
skip_polygon(92.0387, 61.3577, 92.0387, 28.9605, -398.402, 28.9605, -398.402, 61.3577, 92.0387, 61.3577).

%! wms_bbox(+Version:compound,
%!          +LayerDom:compound,
%!          -Pair:pair(compound,list(integer))) is nondet.
%
% WMS service metadata shall declare one or more bounding boxes for
% each Layer.  A Bounding Box metadata element may either be stated
% explicitly or may be inherited from a parent Layer.  In XML, the
% <BoundingBox> metadata element includes the following attributes:
%
%   - CRS indicates the Layer CRS that applies to this bounding box.
%
%   - minx, miny, maxx, maxy indicate the limits of the bounding box
%     using the axis units and order of the specified CRS.
%
%   - resx and resy (optional) indicate the spatial resolution of the
%   data comprising the layer in those same units.

wms_bbox(Version, LayerDom, CrsTerm-[XMax,YMax,XMin,YMin,XRes,YRes]) :-
  wms_bbox_crs(Version, LayerDom, CrsTerm),
  % X maximum, X minimum, Y maximum, Y minimum
  xpath_chk(
    LayerDom,
    //'BoundingBox'(@maxx(number)=XMax,
                    @maxy(number)=YMax,
                    @minx(number)=XMin,
                    @miny(number)=YMin),
    _
  ),
  % X resolution
  ignore(xpath_chk(LayerDom, 'BoundingBox'(@resx(number)), XRes)),
  % Y resolution
  ignore(xpath_chk(LayerDom, 'BoundingBox'(@resy(number)), YRes)).

wms_bbox_crs(Version, LayerDom, CrsTerm) :-
  wms_bbox_crs_string(Version, LayerDom, Atom),
  wms_crs_term(CrsTerm, Atom).

wms_bbox_crs_string(version(1,1,1), LayerDom, Atom) :- !,
  xpath_chk(LayerDom, //'BoundingBox'(@'SRS'(string)=Atom), _).
wms_bbox_crs_string(version(1,3,0), LayerDom, Atom) :- !,
  xpath_chk(LayerDom, //'BoundingBox'(@'CRS'(string)=Atom), _).
wms_bbox_crs_string(Version, _, _) :-
  domain_error(wms_version, Version).



%! assert_capability(+Dataset:dataset,
%!                   +Version:compound,
%!                   +CapabilityDom:compound,
%!                   +Service:iri) is det.

assert_capability(Dataset, Version, CapabilityDom, Service) :-
  % /Exception
  xpath_chk(CapabilityDom, //'Exception'(content), ExceptionDom),
  assert_exceptions(Dataset, ExceptionDom, Service),
  % /Layer
  %
  % Each available map is advertised by a <Layer> element in the
  % service metadata.  Conceptually, each Layer is a distinct entity.
  % However, as a means of classifying and organizing layers, and as a
  % means of reducing the size of the service metadata, a single
  % parent Layer may enclose any number of additional layers, which
  % may be hierarchically nested as desired.  Some properties defined
  % in a parent layer are inherited by the children it encloses.
  % These inherited properties may be either redefined or added to by
  % the child.
  %
  % A server shall include at least one <Layer> element for each map
  % layer offered.  If desired, layers may be repeated in different
  % categories (i.e. enclosed in more than one parent <Layer>) when
  % relevant.
  forall(
    assert_layer(Dataset, Version, CapabilityDom, Service),
    true
  ),
  % /Request
  xpath_chk(CapabilityDom, //'Request'(content), RequestDom),
  assert_request(Dataset, RequestDom, Service).



%! assert_capabilities_stream(+Dataset:dataset,
%!                            +Service:iri,
%!                            +RequestUri:uri,
%!                            +In:stream) is det.

assert_capabilities_stream(Dataset, Service, RequestUri, In) :-
  assert_triple(Service, dcat:endpointURL, uri(RequestUri), Dataset),
  load_xml(In, [Dom]),
  assert_version(Dataset, Dom, Service, Version),
  % /Service
  %
  % The first part of the service metadata is a <Service> element
  % providing general metadata for the server as a whole.  It shall
  % include a Name, Title, and Online Resource URL.  Optional service
  % metadata includes Abstract, Keyword List, Contact Information,
  % Fees, Access Constraints, and limits on the number of layers in a
  % request or the output size of maps.
  xpath_chk(Dom, //'Service'(content), ServiceDom),
  assert_service(Dataset, ServiceDom, Service),
  % /Capability
  %
  % The <Capability> element of the service metadata names the actual
  % operations that are supported by the server, the output formats
  % offered for those operations, and the URL prefix for each
  % operation.  The XML schema includes placeholders for Distributed
  % Computing Platforms other than HTTP, but currently only the HTTP
  % platform is defined.
  xpath_chk(Dom, //'Capability'(content), CapabilityDom),
  assert_capability(Dataset, Version, CapabilityDom, Service).



%! assert_contact_information(+Dataset:dataset,
%!                            +ContactInformationDom:compound,
%!                            +Service:iri) is semidet.

assert_contact_information(Dataset, ContactInformationDom, Service) :-
  rdf_hash_iri(contact, ContactInformationDom, Contact),
  assert_triple(Service, dcat:contactPoint, Contact, Dataset),
  assert_instance(Contact, wms:'Contact', Dataset),
  % /ContactAddress
  (   xpath_chk(ContactInformationDom, //'ContactAddress'(content), ContactAddressDom)
  ->  rdf_hash_iri(address, ContactAddressDom, Address),
      assert_triple(Contact, wms:address, Address, Dataset),
      assert_instance(Address, wms:'Address', Dataset),
      xpath_chk(ContactAddressDom, //'AddressType'(normalize_space), Type),
      assert_triple(Address, wms:addressType, string(Type), Dataset),
      xpath_chk(ContactAddressDom, //'Address'(normalize_space), AddressString),
      assert_triple(Address, sdo:address, string(AddressString), Dataset),
      xpath_chk(ContactAddressDom, //'City'(normalize_space), City),
      assert_triple(Address, wms:city, string(City), Dataset),
      xpath_chk(ContactAddressDom, //'StateOrProvince'(normalize_space), StateOrProvince),
      assert_triple(Address, wms:stateOrProvince, string(StateOrProvince), Dataset),
      xpath_chk(ContactAddressDom, //'PostCode'(normalize_space), PostCode),
      assert_triple(Address, sdo:postalCode, string(PostCode), Dataset),
      xpath_chk(ContactAddressDom, //'Country'(normalize_space), Country),
      assert_triple(Address, wms:country, string(Country), Dataset)
  ;   true
  ),
  % /ContactElectronicMailAddress
  (   xpath_chk(ContactInformationDom, //'ContactElectronicMailAddress'(normalize_space), EMail)
  ->  assert_triple(Contact, sdo:email, uri(EMail), Dataset)
  ;   true
  ),
  % /ContactPersonPrimary
  (   xpath_chk(ContactInformationDom, //'ContactPersonPrimary'(content), ContactPersonDom)
  ->  gtrace,
      rdf_hash_iri(person, ContactPersonDom, Person),
      assert_instance(Person, sdo:'Person', Dataset),
      assert_triple(Contact, wms:primaryPerson, Person, Dataset),
      xpath_chk(ContactPersonDom, //'ContactPerson'(normalize_space), Name),
      assert_triple(Person, sdo:name, string(Name), Dataset),
      xpath_chk(ContactPersonDom, //'ContactOrganization'(normalize_space), Organization),
      assert_triple(Person, sdo:organization, string(Organization), Dataset)
  ;   true
  ),
  % /ContactPosition
  (   xpath_chk(ContactInformationDom, //'ContactPosition'(normalize_space), Position)
  ->  assert_triple(Contact, wms:position, string(Position), Dataset)
  ;   true
  ).



%! assert_crs(+Dataset:dataset,
%!            +Version:compound,
%!            +Dom:compound,
%!            +Iri:iri,
%!            -CrsTerm:compound) is det.

assert_crs(Dataset, Version, Dom, Iri, Term) :-
  wms_crs_string(Version, Dom, Atom),
  wms_crs_term(Term, Atom),
  assert_crs_term(Dataset, Iri, Term).

wms_crs_string(version(1,1,1), Dom, Atom) :- !,
  xpath(Dom, //'SRS'(normalize_space), Atom).
wms_crs_string(version(1,3,0), Dom, Atom) :- !,
  xpath(Dom, //'CRS'(normalize_space), Atom).
wms_crs_string(Version, _, _) :-
  domain_error(wms_version, Version).

wms_crs_term(Term, Atom1) :-
  ground(Atom1), !,
  (   atomic_list_concat([Namespace,Atom2], :, Atom1)
  ->  (   atomic_list_concat([Code,Params], ',', Atom2)
      ->  Term = label(Namespace,Code,Params)
      ;   Term = label(Namespace,Atom2)
      )
  ;   Term = uri(Atom1)
  ).
wms_crs_term(label(Namespace,Code), Atom) :- !,
  format(atom(Atom), "~a:~a", [Namespace,Code]).
wms_crs_term(label(Namespace,Code,Params), Atom) :- !,
  format(atom(Atom), "~a:~a,~a", [Namespace,Code,Params]).
wms_crs_term(uri(Atom), Atom).



%! assert_crs_term(+Dataset:dataset, +Iri:iri, +CrsTerm:compound) is det.

assert_crs_term(Dataset, Iri, label(Namespace,Code)) :- !,
  rdf_prefix_iri(Namespace, Code, Crs),
  assert_crs_term(Dataset, Iri, uri(Crs)).
% @tbs What to do with parameters?
assert_crs_term(Dataset, Iri, label(Namespace,Code,_)) :- !,
  assert_crs_term(Dataset, Iri, label(Namespace,Code)).
assert_crs_term(Dataset, Iri, uri(Crs)) :-
  assert_triple(Iri, tg:crs, Crs, Dataset).



%! assert_ex_bbox(+Dataset:dataset,
%!                +Version:compound,
%!                +LayerDom:compound,
%!                +Parent:dict,
%!                +Layer:iri,
%!                -ExBBox:list(integer)) is det.

assert_ex_bbox(Dataset, Version, LayerDom, Parent, Layer, [E,N,S,W]) :-
  'EX_GeographicBoundingBox'(Version, LayerDom, Parent, [E,N,S,W]),%ex_bbox
  atomic_list_concat([E,N,S,W], '_', ExBBoxLocal),
  rdf_prefix_iri(bbox, ExBBoxLocal, ExBBox),
  assert_instance(ExBBox, tg:'BoundingBox', Dataset),
  format(string(ExBBoxLabel), "East: ~f, North: ~f, South: ~f, West: ~f", [E,N,S,W]),
  assert_label(ExBBox, string(ExBBoxLabel), Dataset),
  assert_triple(Layer, tg:boundingBox, ExBBox, Dataset),
  assert_triple(ExBBox, wms:eastBoundLongitude, E, Dataset),
  assert_triple(ExBBox, wms:northBoundLongitude, N, Dataset),
  assert_triple(ExBBox, wms:westBoundLongitude, W, Dataset),
  assert_triple(ExBBox, wms:southBoundLongitude, S, Dataset).



%! assert_exceptions(+Dataset:dataset,
%!                   +ExceptionDom:compound,
%!                   +Service:iri) is det.

assert_exceptions(Dataset, ExceptionDom, Service) :-
  aggregate_all(
    set(Format),
    xpath(ExceptionDom, //'Format'(normalize_space), Format),
    Formats1
  ),
  (Formats1 == [] -> Formats2 = ['XML'] ; Formats2 = Formats1),
  maplist(assert_exception_format(Dataset, Service), Formats2).

assert_exception_format(Dataset, Service, Format) :-
  rdf_create_iri(mediaType, Format, MediaType),
  assert_triple(Service, wms:exceptionFormat, MediaType, Dataset).



%! assert_formats(+Datasrt:dataset, +Dom:compound, +Iri:iri) is det.

assert_formats(Dataset, Dom, Iri) :-
  forall(
    xpath(Dom, //'Format'(normalize_space), Atom),
    (
      atom_phrase(media_type(MediaType), Atom),
      (   known_format(MediaType)
      ->  assert_media_type(Dataset, Iri, dct:format, MediaType)
      ;   domain_error(exception_format, MediaType)
      )
    )
  ).

assert_media_type(Dataset, S, P, media(Supertype/Subtype,_)) :-
  atomic_list_concat([Supertype,Subtype], /, Local),
  rdf_prefix_iri(mediaType, Local, MediaType),
  assert_triple(S, P, MediaType, Dataset).



%! assert_keywords(+Dataset:dataset, +Dom:compound, +Iri:iri) is det.
%
% Each keyword may be accompanied by a “vocabulary” attribute to
% indicate the defining authority for that keyword.  One “vocabulary”
% attribute value is defined by this International Standard: the value
% “ISO 19115:2003” refers to the metadata topic category codes defined
% in ISO 19115:2003, B.5.27; information communities may define other
% “vocabulary” attribute values.  No particular vocabulary is mandated
% by this International Standard.

assert_keywords(Dataset, Dom, Iri) :-
  forall(
    xpath(Dom, //'KeywordList'//'Keyword'(normalize_space), Keyword),
    assert_triple(Iri, dcat:keyword, string(Keyword), Dataset)
  ).



%! assert_layer(+Dataset:dataset,
%!              +Version:compound,
%!              +CapabilityDom:compound,
%!              +Service:iri) is nondet.
%
% @tbd AuthorityURL
% @tbd cascaded
% @tbd DataURL
% @tbd Dimension
% @tbd FeatureListURL
% @tbd fixedHeight
% @tbd fixedWidth
% @tbd Identifier
% @tbd noSubsets
% @tbd opaque
% @tbd queryable

assert_layer(Dataset, Version, Dom, Service) :-
  assert_layer(Dataset, Version, Dom, layer{}, Service).


assert_layer(Dataset, Version, Dom, Parent, Service) :-
  xpath_direct_indirect(Dom, 'Layer', LayerDom, IndirectDom),
  % /Title
  %
  % A <Title> is mandatory for all layers; it is a human-readable
  % string for presentation in a menu.  The Title is not inherited by
  % child Layers.
  xpath_chk(LayerDom, //'Title'(normalize_space), LayerTitle),
  split_string(LayerTitle, " \n\t", " \n\t", Atoms),
  atomic_list_concat(Atoms, -, LayerLocal),
  rdf_prefix_iri(layer, LayerLocal, Layer),
  assert_instance(Layer, tg:'Layer', Dataset),
  assert_label(Layer, string(LayerTitle), Dataset),
  assert_triple(Service, tg:layer, Layer, Dataset),
  % /KeywordList (optional, not inherited)
  %
  % A list of keywords or keyword phrases describing each layer should
  % be included to help catalogue searching.  Each keyword may be
  % accompanied by a “vocabulary” attribute to indicate the defining
  % authority for the keyword.
  %
  % KeywordList contains zero or more <Keyword> elements to aid in
  % catalogue searches.
  assert_keywords(Dataset, LayerDom, Layer),
  % /Name (optional)
  %
  % If, and only if, a layer has a <Name>, then it is a map layer that
  % can be requested by using that Name in the LAYERS parameter of a
  % GetMap request.  A Layer that contains a <Name> element is
  % referred to as a “named layer” in this International Standard.  If
  % the layer has a Title but no Name, then that layer is only a
  % category title for all the layers nested within.  A server that
  % advertises a Layer containing a Name element shall be able to
  % accept that Name as the value of LAYERS argument in a GetMap
  % request and return the corresponding map.  A client shall not
  % attempt to request a layer that has a Title but no Name.
  %
  % A containing category itself may include a Name by which a map
  % portraying all of the nested layers can be requested at once.  For
  % example, a parent layer "Roads" may have children “Interstates”
  % and “State Highways” and allow the user to request either child
  % individually or both together.
  %
  % The Name is not inherited by child Layers.
  (   xpath_chk(LayerDom, //'Name'(normalize_space), Name)
  ->  assert_triple(Layer, sdo:name, string(Name), Dataset)
  ;   true
  ),
  % /Abstract (optional, not inherited)
  %
  % Abstract is a narrative description of the map layer.
  (   xpath_chk(LayerDom, //'Abstract'(normalize_space), Abstract),
      Abstract \== ''
  ->  assert_triple(Layer, rdfs:comment, string(Abstract), Dataset)
  ;   true
  ),
  % /Style (optional, default value, additive inheritance)
  %
  % Zero or more Styles may be advertised for a Layer or collection of
  % layers using <Style> elements
  %
  % @tbd If only a single style is available, that style is known as
  % the “default” style and need not be advertised by the server.
  dict_get(styles, Parent, [], ParentStyles),
  aggregate_all(
    set(Style),
    (
      xpath(LayerDom, /'Style', StyleDom),
      assert_style(Dataset, StyleDom, Layer, Style)
    ),
    SelfStyles
  ),
  ord_union(ParentStyles, SelfStyles, Styles),
  % /EX_GeographicBoudningBox (default inheritance)
  %
  % Every named Layer shall have exactly one
  % <EX_GeographicBoundingBox> element that is either stated
  % explicitly or inherited from a parent Layer.
  % EX_GeographicBoundingBox states, via the elements
  % westBoundLongitude, eastBoundLongitude, southBoundLatitude, and
  % northBoundLatitude, the minimum bounding rectangle in decimal
  % degrees of the area covered by the Layer.
  % EX_GeographicBoundingBox shall be supplied regardless of what CRS
  % the map server may support, but it may be approximate if the data
  % are not natively in geographic coordinates.  The purpose of
  % EX_GeographicBoundingBox is to facilitate geographic searches
  % without requiring coordinate transformations by the search engine.
  assert_ex_bbox(Dataset, Version, LayerDom, Parent, Layer, ExBBox),
  % bounding boxes (inheritance:replace)
  assert_bboxes(Dataset, Version, LayerDom, Parent, Layer, Pairs),
  % CRSes (additive inheritance)
  %
  % Every Layer is available in one or more layer coordinate reference
  % systems. 6.7.3 discusses the Layer CRS.  In order to indicate
  % which Layer CRSs are available, every named Layer shall have at
  % least one <CRS> element that is either stated explicitly or
  % inherited from a parent Layer.  The root <Layer> element shall
  % include a sequence of zero or more CRS elements listing all CRSs
  % that are common to all subsidiary layers.  A child layer may
  % optionally add to the list inherited from a parent layer.  Any
  % duplication shall be ignored by clients.
  %
  % When a Layer is available in several coordinate reference systems,
  % the list of available CRS values shall be represented as a
  % sequence of <CRS> elements, each of which contains only a single
  % CRS name.
  dict_get(crses, Parent, [], ParentCrsTerms),
  aggregate_all(
    set(CrsTerm),
    assert_crs(Dataset, Version, LayerDom, Layer, CrsTerm),
    SelfCrsTerms
  ),
  ord_union(ParentCrsTerms, SelfCrsTerms, CrsTerms),
  % keywords (optional)
  assert_keywords(Dataset, LayerDom, Layer),
  % /MetadataURL (optional, no inheritance)
  %
  % A server should use one or more <MetadataURL> elements to offer
  % detailed, standardized metadata about the data corresponding to a
  % particular layer.  The “type” attribute indicates the standard to
  % which the metadata complies.  Two “type” attribute values are
  % defined by this International Standard: the value “ISO 19115:2003”
  % refers to ISO 19115:2003; the value “FGDC:1998” refers to
  % FGDC-STD-001-1998 [1].  An information community may define
  % meanings for other “type” attribute values.  The enclosed <Format>
  % element indicates the file format MIME type of the metadata
  % record.
  %
  % MetadataURL elements are not inherited by child Layers.
  forall(
    xpath(LayerDom, //'MetadataURL'(@type(string)=MetadataType,content), MetadataDom),
    assert_metadata_url(Dataset, Layer, MetadataType, MetadataDom)
  ),
  % /MaxScaleDenominator (optional, default inheritance)
  % /MinScaleDenominator (optional, default inheritance)
  %
  % The <MinScaleDenominator> and <MaxScaleDenominator> elements
  % define the range of scales for which it is appropriate to generate
  % a map of a Layer.  Because maps from a WMS may be viewed on
  % arbitrary displays rather than on paper, the values used are
  % actually the scale denominators relative to a common display pixel
  % size.  The intent of scale denominators is not that the
  % translation between “actual” and “standard” scales always be
  % completely accurate.  Rather, the intent is to reduce the amount
  % of clutter or crowding of features portrayed on the map.  The
  % scale denominator values are guidelines for clients, not firm
  % limits. Upon receiving a request for a map that is not within the
  % scale denominator range, the server may return a blank map, or may
  % return a portrayal of the Layer that is crowded with features or
  % otherwise poorly suited for display; the server shall not respond
  % with a service exception.
  (   'MaxScaleDenominator'(LayerDom, Parent, MaxScaleDenominator)
  ->  gtrace,
      assert_triple(Layer, wms:maxScaleDenominator, integer(MaxScaleDenominator), Dataset)
  ;   true
  ),
  (   'MinScaleDenominator'(LayerDom, Parent, MinScaleDenominator)
  ->  gtrace,
      assert_triple(Layer, wms:minScaleDenominator, integer(MinScaleDenominator), Dataset)
  ;   true
  ),
  Self = layer{
    bboxes: Pairs,
    crses: CrsTerms,
    ex_bbox: ExBBox,
    iri: Layer,
    styles: Styles
  },
  % /Attribution (optional, default inheritance)
  %
  % The optional <Attribution> element provides a way to identify the
  % source of the geographic information used in a Layer or collection
  % of Layers.  Attribution encloses several optional elements:
  % <OnlineResource> states the data provider’s URL; <Title> is a
  % human-readable string naming the data provider; <LogoURL> is the
  % URL of a logo image.  Client applications may choose to display
  % one or more of these items.  A <Format> element in LogoURL
  % indicates the MIME type of the logo image, and the attributes
  % width and height state the size of the image in pixels.
  %
  % The Attribution element is inherited by child layers.  Any
  % redefinition by a child replaces the inherited value.
  %
  % @tbd LogoURL
  (   xpath_chk(LayerDom, //'Attribution'(content), AttributionDom)
  ->  gtrace,
      % /OnlineResource
      (   xpath_chk(
            AttributionDom,
            //'OnlineResource'(normalize_space),
            AttributionUri
          )
      ->  assert_triple(Layer, wms:attributionURL, uri(AttributionUri), Dataset)
      ;   true
      ),
      % /Title
      xpath_chk(AttributionDom, //'Title'(normalize_space), AttributionTitle),
      assert_triple(Layer, wms:attribution, string(AttributionTitle), Dataset)
  ;   true
  ),
  (   dict_get(iri, Parent, ParentLayer)
  ->  assert_triple(ParentLayer, geo:sfContains, Layer, Dataset)
  ;   true
  ),
  (   % If, and only if, a layer has a <Name>, then it is a map layer
      % that can be requested by using that Name in the LAYERS
      % parameter of a GetMap request.  A Layer that contains a <Name>
      % element is referred to as a “named layer” in this
      % International Standard.  If the layer has a Title but no Name,
      % then that layer is only a category title for all the layers
      % nested within.
      ground(Name)
  ;   % Explore children.
      assert_layer(Dataset, Version, IndirectDom, Self, Service)
  ).

xpath_direct_indirect(Dom, Tag, DirectDom, IndirectDom) :-
  member(element(Tag,_,Doms), Dom),
  partition(indirect_tag(Tag), Doms, IndirectDom, DirectDom).

indirect_tag(Tag, element(Tag,_,_)).



%! assert_legend(+Dataset:dataset, +LegendDom:compound, +Style:iri) is det.

assert_legend(Dataset, LegendDom, Style) :-
  % /OnlineResource
  xpath_chk(LegendDom, /'OnlineResource'('xlink:href'(normalize_space)), Legend),
  assert_instance(Legend, wms:'Legend', Dataset),
  assert_triple(Style, wms:legend, Legend, Dataset),
  assert_triple(Legend, rdfs:seeAlso, uri(Legend), Dataset),
  % A <Format> element in LegendURL indicates the MIME type of the
  % legend image, and the optional attributes width and height state
  % the size of the image in pixels.  Servers should provide the width
  % and height attributes if known at the time of processing the
  % GetCapabilities request.  The legend image should clearly
  % represent the symbols, lines and colours used in the map
  % portrayal.  The legend image should not contain text that
  % duplicates the Title of the layer, because that information is
  % known to the client and may be shown to the user by other means.
  assert_formats(Dataset, LegendDom, Legend),
  (   xpath_chk(LegendDom, /self(@height(number)), Height)
  ->  assert_triple(Legend, wms:height, positive_integer(Height), Dataset)
  ;   true
  ),
  (   xpath_chk(LegendDom, /self(@width(number)), Width)
  ->  assert_triple(Legend, wms:width, positive_integer(Width), Dataset)
  ;   true
  ).



%! assert_metadata_url(+Dataset:dataset, +Layer:iri, +MetadataType:atom, +MetadataDom:compound) is det.

assert_metadata_url(Dataset, Layer, MetadataType, MetadataDom) :-
  xpath_chk(MetadataDom, /'OnlineResource'(@'xlink:href'), MetadataUrl),
  assert_triple(Layer, wms:metadataURL, MetadataUrl, Dataset),
  assert_instance(MetadataUrl, wms:'MetadataURL', Dataset),
  assert_formats(Dataset, MetadataDom, MetadataUrl),
  assert_triple(MetadataUrl, wms:type, string(MetadataType), Dataset).



%! assert_request(+Dataset:dataset, +RequestDom:compound, +Service:iri) is det.

assert_request(Dataset, RequestDom, Service) :-
  % /GetMap
  xpath_chk(RequestDom, //'GetMap'(content), GetMapDom),
  assert_request_GetMap(Dataset, GetMapDom, Service).



%! assert_request_GetMap(+Dataset:dataset, +GetMapDom:compound, +Service:iri) is det.

assert_request_GetMap(Dataset, GetMapDom, Service) :-
  assert_formats(Dataset, GetMapDom, Service).



%! assert_service(+Dataset:dataset, +ServiceDom:compound, +Service:iri) is det.
%
% @tbd DescribeLayer
% @tbd GetCapabilities
% @tbd GetFeatureInfo
% @tbd GetLegendGraphic
% @tbd GetStyles
assert_service(Dataset, ServiceDom, Service) :-
  % /Name
  %
  % The Service Name shall be “WMS” in the case of a WMS.
  xpath_chk(ServiceDom, //'Name'(normalize_space), ServiceName),
  (   memberchk(ServiceName, ['OGC:WMS','WMS'])
  ->  true
  ;   print_message(warning, format("Incorrect service name ‘~a’.", [ServiceName]))
  ),
  % /Title
  %
  % The Service Title is at the discretion of the provider, and should
  % be brief yet descriptive enough to identify this server in a menu
  % with other servers.
  xpath_chk(ServiceDom, //'Title'(normalize_space), ServiceTitle),
  assert_label(Service, string(ServiceTitle), Dataset),
  % /Abstract
  %
  % The Abstract element allows a descriptive narrative providing more
  % information about the enclosing object.
  (   xpath_chk(ServiceDom, //'Abstract'(normalize_space), ServiceAbstract),
      ServiceAbstract \== ''
  ->  assert_triple(Service, rdfs:comment, string(ServiceAbstract), Dataset)
  ;   true
  ),
  % /OnlineResource
  %
  % The OnlineResource element within the Service element may be used
  % to refer to the web site of the service provider.  There are other
  % OnlineResource elements used for the URL prefix of each supported
  % operation (see below).
  (   xpath_chk(ServiceDom, //'OnlineResource'(normalize_space), OnlineResource),
      OnlineResource \== ''
  ->  assert_triple(Service, rdfs:seeAlso, uri(OnlineResource), Dataset)
  ;   true
  ),
  % /KeywordList/Keyword
  %
  % A list of keywords or keyword phrases describing the server as a
  % whole should be included to help catalogue searching.
  assert_keywords(Dataset, ServiceDom, Service),
  % /ContactInformation
  %
  % Contact Information should be included.
  xpath_chk(ServiceDom, //'ContactInformation'(content), ContactInformationDom),
  (   ContactInformationDom == []
  ->  true
  ;   gtrace,
      assert_contact_information(Dataset, ContactInformationDom, Service)
  ),
  % /LayerLimit
  %
  % The optional <LayerLimit> element in the service metadata is a
  % positive integer indicating the maximum number of layers a client
  % is permitted to include in a single GetMap request.  If this
  % element is absent, the server imposes no limit.
  (   xpath_chk(ServiceDom, //'LayerLimit'(number), MaxLayers)
  ->  gtrace,
      assert_triple(Service, wms:layerLimit, positive_integer(MaxLayers), Dataset)
  ;   true
  ),
  % /MaxHeight
  % /MaxWidth
  %
  % The optional <MaxWidth> and <MaxHeight> elements in the service
  % metadata are positive integers indicating the maximum width and
  % height values that a client is permitted to include in a single
  % GetMap request.  If either element is absent the server imposes no
  % limit on the corresponding parameter.
  (   xpath_chk(ServiceDom, //'MaxHeight'(number), MaxHeight)
  ->  gtrace,
      assert_triple(Service, wms:maxHeight, positive_integer(MaxHeight), Dataset)
  ;   true
  ),
  (   xpath_chk(ServiceDom, //'MaxWidth'(number), MaxWidth)
  ->  gtrace,
      assert_triple(Service, wms:maxWidth, positive_integer(MaxWidth), Dataset)
  ;   true
  ),
  % /AccessConstraints
  % /Fees
  %
  % The optional elements <Fees> and <AccessConstraints> may be
  % omitted if they do not apply to the server.  If either of those
  % elements is present, the reserved word “none” (case-insensitive)
  % shall be used if there are no fees or access constraints, as
  % follows: <Fees>none</Fees>,
  % <AccessConstraints>none</AccessConstraints>.  When constraints are
  % imposed, no precise syntax has been defined for the text content
  % of these elements, but client applications may display the content
  % for user information and action.
  (   xpath_chk(ServiceDom, //'AccessConstraints'(normalize_space), AccessConstraints)
  ->  gtrace,
      assert_triple(Service, wms:accessConstraints, string(AccessConstraints), Dataset)
  ;   true
  ),
  (   xpath_chk(ServiceDom, //'Fees'(normalize_space), ServiceFees)
  ->  gtrace,
      assert_triple(Service, wms:fees, string(ServiceFees), Dataset)
  ;   true
  ).



%! assert_style(+Dataset:dataset,
%!              +StyleDom:compound,
%!              +Layer:iri,
%!              -Style:iri) is semidet.

assert_style(Dataset, StyleDom, Layer, Style) :-
  gtrace,
  % /Name
  % /Title
  %
  % Each <Style> element shall have <Name> and <Title> elements.  The
  % style's Name is used in the Map request STYLES parameter.  The
  % Title is a human-readable string.
  xpath_chk(StyleDom, /'Name'(normalize_space), StyleName),
  % Style declarations are inherited by child Layers.  A child shall
  % not redefine a Style with the same Name as one inherited from a
  % parent.  A child may define a new Style with a new Name that is
  % not available for the parent Layer.
  (   tp(Parent, wms:child, Layer, Dataset),
      tp(Parent, wms:name, string(StyleName), Dataset)
  ->  throw(error(wms_style, StyleName))
  ;   true
  ),
  rdf_prefix_iri(style, StyleName, Style),
  (instance(Style, wms:'Style', Dataset) -> gtrace ; true),%DEB
  assert_triple(Layer, wms:style, Style, Dataset),
  assert_instance(Style, wms:'Style', Dataset),
  assert_triple(Style, sdo:name, string(StyleName), Dataset),
  xpath_chk(StyleDom, /'Title'(normalize_space), StyleTitle),
  assert_label(Style, string(StyleTitle), Dataset),
  % /Abstract
  %
  % <Abstract> provides a narrative description.
  (   xpath_chk(StyleDom, /'Abstract'(normalize_space), StyleAbstract),
      StyleAbstract \== ''
  ->  assert_triple(Style, rdfs:comment, string(StyleAbstract), Dataset)
  ;   true
  ),
  % /LegendURL
  %
  % <LegendURL> contains the location of an image of a map legend
  % appropriate to the enclosing style.
  (   xpath_chk(StyleDom, /'LegendURL', LegendDom)
  ->  gtrace,
      assert_legend(Dataset, LegendDom, Style)
  ;   true
  ).



%! assert_version(+Dataset:dataset,
%!                +VersionDom:compound,
%!                +Service:iri,
%!                -Version:compound) is det.

assert_version(Dataset, Dom, Service, Term) :-
  wms_version_string(Dom, Atom),
  wms_version_term(Term, Atom),
  call_must_be(known_version, Term),
  Term = version(Major,Minor,Patch),
  rdf_prefix_iri(version, Atom, Version),
  assert_triple(Service, tv:version, Version, Dataset),
  assert_instance(Version, tv:'SemanticVersion', Dataset),
  assert_label(Version, string(Atom), Dataset),
  assert_triple(Version, tv:major, Major, Dataset),
  assert_triple(Version, tv:minor, Minor, Dataset),
  assert_triple(Version, tv:patch, Patch, Dataset).

% WMS 1.1.1
wms_version_string(Dom, Atom) :-
  xpath_chk(Dom, /'WMT_MS_Capabilities'(@version), Atom), !.
% WMS 1.3.0
wms_version_string(Dom, Atom) :-
  xpath_chk(Dom, /'WMS_Capabilities'(@version), Atom).

wms_version_term(version(Major,Minor,Patch), Atom) :-
  ground(Atom), !,
  atomic_list_concat([Major0,Minor0,Patch0], ., Atom),
  maplist(atom_number, [Major0,Minor0,Patch0], [Major,Minor,Patch]).
wms_version_term(version(Major,Minor,Patch), Atom) :-
  format(atom(Atom), "~d.~d.~d", [Major,Minor,Patch]).



%! 'EX_GeographicBoundingBox'(+Version:compound,
%!                            +LayerDom:compound,
%!                            +Parent:dict,
%!                            -BBox:list(integer)) is det.

'EX_GeographicBoundingBox'(version(1,1,1), LayerDom, _, [E,N,S,W]) :-
  xpath_chk(
    LayerDom,
    //'LatLonBoundingBox'(@maxx(number)=E,
                          @maxy(number)=N,
                          @minx(number)=W,
                          @miny(number)=S),
    _
  ), !.
'EX_GeographicBoundingBox'(version(1,3,0), LayerDom, _, [E,N,S,W]) :-
  xpath_chk(LayerDom, //'EX_GeographicBoundingBox'(content), ExBBoxDom), !,
  xpath_chk(ExBBoxDom, //eastBoundLongitude(number), E),
  xpath_chk(ExBBoxDom, //northBoundLatitude(number), N),
  xpath_chk(ExBBoxDom, //southBoundLatitude(number), S),
  xpath_chk(ExBBoxDom, //westBoundLongitude(number), W).
'EX_GeographicBoundingBox'(_, _, Parent, BBox) :-
  dict_get(ex_bbox, Parent, BBox), !.
'EX_GeographicBoundingBox'(_, LayerDom, _, _) :-
  existence_error('EX_GeographicBoundingBox', LayerDom).



%! 'MaxScaleDenominator'(+LayerDom:compound,
%!                       +Parent:dict,
%!                       -MaxScaleDenominator:number) is semidet.

% WMS 1.1.1
'MaxScaleDenominator'(Dom, _, N) :-
  xpath_chk(Dom, //'ScaleHint'(@max(number)), N), !.
% WMS 1.3.0
'MaxScaleDenominator'(Dom, _, N) :-
  xpath_chk(Dom, //'MaxScaleDenominator'(number), N), !.
'MaxScaleDenominator'(_, Parent, N) :-
  dict_get(max_scale_denominator, Parent, N).



%! 'MinScaleDenominator'(+LayerDom:compound,
%!                       +Parent:dict,
%!                       -MinScaleDenominator:number) is semidet.

% WMS 1.1.1
'MinScaleDenominator'(Dom, _, N) :-
  xpath_chk(Dom, //'ScaleHint'(@min(number)), N), !.
% WMS 1.3.0
'MinScaleDenominator'(Dom, _, N) :-
  xpath_chk(Dom, //'MinScaleDenominator'(number), N), !.
'MinScaleDenominator'(_, Parent, N) :-
  dict_get(min_scale_denominator, Parent, N).





% KNOWN VALUES %

known_crs('AUTO2', '42001').
known_crs('AUTO2', '42002').
known_crs('AUTO2', '42003').
known_crs('AUTO2', '42004').
known_crs('AUTO2', '42005').
known_crs('CRS', '1').
known_crs('CRS', '27').
known_crs('CRS', '83').
known_crs('CRS', '84').
known_crs('CRS', '88').
known_crs('EPSG', '3857').
known_crs('EPSG', '4269').
known_crs('EPSG', '4326').
known_crs('EPSG', '900913').

known_crs_namespace(Namespace) :-
  distinct(Namespace, known_crs(Namespace, _)).

known_exception_format(media(application/'vnd.ogc.se_blank',[])).
known_exception_format(media(application/'vnd.ogc.se_inimage',[])).
known_exception_format(media(application/'vnd.ogc.se_xml',[])).

known_format(media(application/'atom+xml',[])).
known_format(media(application/json,[type(geojson)])).
known_format(media(application/json,[type(topojson)])).
known_format(media(application/json,[type(utfgrid)])).
known_format(media(application/pdf,[])).
known_format(media(application/'rss+xml',[])).
known_format(media(application/'vnd.google-earth.kml+xml',[])).
known_format(media(application/'vnd.google-earth.kml+xml',[mode(networklink)])).
known_format(media(application/'vnd.google-earth.kmz',[])).
known_format(media(application/'x-gpkg',[])).
known_format(media(application/'x-pdf',[])).
known_format(media(application/'x-protobuf',[type('mapbox-vector')])).
known_format(media(application/'x-sqlite3',[])).
known_format(media(image/geotiff,[])).
known_format(media(image/geotiff8,[])).
known_format(media(image/gif,[])).
known_format(media(image/jpeg,[])).
known_format(media(image/png,[])).
known_format(media(image/png,[mode('8bit')])).
known_format(media(image/'svg+xml',[])).
known_format(media(image/tiff,[])).
known_format(media(image/tiff8,[])).
known_format(media(image/'vnd.jpeg-png',[])).
known_format(media(text/html,[subtype(openlayers)])).

known_version(version(1,1,1)).
known_version(version(1,3,0)).
