:- module(
  wms,
  [
    get_capabilities/2, % +Uri, -Dict
    get_map/9,          % +Uri,
                        % +BBox,
                        % +CRS,
                        % +ExceptionFormat,
                        % +Format,
                        % +Layers,
                        % +Size,
                        % +Styles,
                        % +Version
    example/1           % -Uri
  ]
).

/** <module> WMS Client

'http://maps.nypl.org/warper/maps/wms/7847?request=GetMap&service=WMS&version=1.3.0&bbox=-74.0117,40.7009,-73.9883,40.7176&crs=EPSG:4326&height=250&width=250&layers=image&styles=default'

@author Wouter Beek
@version 2018-10-10
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(error)).
:- use_module(library(lists)).
:- use_module(library(ordsets)).
:- use_module(library(pairs)).
:- use_module(library(pprint)).
:- use_module(library(xpath)).

:- use_module(library(http/http_client2)).
:- use_module(library(uri_ext)).
:- use_module(library(xml_ext)).





%! get_capabilities(+Uri:atom, -Capabilities:dict) is det.
%
% Returns the service metadata, using the highest version that is
% supported by the server.

get_capabilities(Uri1, Dict) :-
  uri_comps(Uri1, uri(Scheme,Auth,Segments,_,_)),
  uri_comps(Uri2, uri(Scheme,Auth,Segments,[request('GetCapabilities'),service('WMS')],_)),
  http_call(Uri2, get_capabilities_stream(Dict), [accept(media(application/'vnd.ogc.wms_xml',[]))]).

get_capabilities_stream(Dict, In) :-
  load_xml(In, [Dom]),
  wms_version(Dom, Version),
  wms_service(Dom, Service),
  wms_capability(Version, Dom, ExceptionFormats, Formats, Layers),
  Dict = endpoint{
    exception_formats: ExceptionFormats,
    formats: Formats,
    layers: Layers,
    service: Service,
    version: Version
  }.



%! get_map(
%!   +Uri:atom,
%!   +BBox:compound,
%!   +CRS:compound,
%!   +ExceptionFormat:atom,
%!   +Format:atom,
%!   +Layers:list(atom),
%!   +Size:compound,
%!   +Styles:list(atom),
%!   +Version:compound
%! ) is det.
%
% @tbd transparent(boolean)
% @tbd bgcolor(atom)
% @tbd time(?)
% @tbd elevation(?)

get_map(
  Uri1,
  bbox(min(XMin,YMin),max(XMax,YMax)),
  CRS,
  ExceptionFormat,
  Format,
  Layers,
  size(Height,Width),
  Styles,
  version(Major,Minor,Patch)
) :-
  atomic_list_concat([XMin,YMin,XMax,YMax], ',', BBoxString),
  crs_string(CRS, CRSString),
  atomic_list_concat(Layers, ',', LayersString),
  atomic_list_concat(Styles, ',', StylesString),
  atomic_list_concat([Major,Minor,Patch], ., VersionString),
  uri_comps(Uri1, uri(Scheme,Auth,Segments,_,_)),
  Query = [
    bbox(BBoxString),
    crs(CRSString),
    exception(ExceptionFormat),
    format(Format),
    height(Height),
    layers(LayersString),
    request('GetMap'),
    service('WMS'),
    styles(StylesString),
    version(VersionString),
    width(Width)
  ],
  uri_comps(Uri2, uri(Scheme,Auth,Segments,Query,_)),
  http_download(Uri2, test, [accept(media(application/'vnd.ogc.wms_xml',[]))]).

crs_string(label(Namespace,Code), String) :-
  format(atom(String), "~a:~a", [Namespace,Code]).





% HELPERS %

%! get_dict(+Key:atom, +Dict:dict, -Value, +Default) is det.

get_dict(Key, Dict, Value, _) :-
  get_dict(Key, Dict, Value), !.
get_dict(_, _, Default, Default).



%! wms_bboxes(+Version:compound, +Dom:compound, +Parent:dict, -BBoxes:list(pair(compound,dict))) is det.

wms_bboxes(Version, Dom, Parent, Pairs) :-
  get_dict(bboxes, Parent, ParentPairs, []),
  aggregate_all(set(SelfPair), wms_bbox(Version, Dom, SelfPair), SelfPairs),
  wms_bboxes_inheritance(ParentPairs, SelfPairs, Pairs).

wms_bbox(Version, Dom, CRS-Dict) :-
  wms_bbox_crs(Version, Dom, CRS),
  % X maximum, X minimum, Y maximum, Y minimum
  xpath_chk(
    Dom,
    'BoundingBox'(@maxx(number)=XMax,@maxy(number)=YMax,@minx(number)=XMin,@miny(number)=YMin),
    _
  ),
  % X resolution
  ignore(xpath_chk(Dom, 'BoundingBox'(@resx(number)), XRes)),
  % Y resolution
  ignore(xpath_chk(Dom, 'BoundingBox'(@resy(number)), YRes)),
  include(
    ground,
    [xmax-XMax,xmin-XMin,xres-XRes,ymax-YMax,ymin-YMin,yres-YRes],
    Pairs
  ),
  dict_pairs(Dict, bbox, Pairs).

wms_bbox_crs(Version, Dom, CRS) :-
  wms_bbox_crs_string(Version, Dom, String),
  wms_crs_term(String, CRS).

wms_bbox_crs_string(version(1,1,1), Dom, String) :- !,
  xpath_chk(Dom, //'BoundingBox'(@'SRS',string), String).
wms_bbox_crs_string(version(1,3,0), Dom, String) :- !,
  xpath_chk(Dom, //'BoundingBox'(@'CRS',string), String).
wms_bbox_crs_string(Version, _, _) :-
  domain_error(wms_version, Version).

wms_bboxes_inheritance([], Pairs, Pairs) :- !.
wms_bboxes_inheritance(Pairs, [], Pairs) :- !.
% The child's bounding box replaces the parent's bounding box for the
% same CRS.
wms_bboxes_inheritance([CRS-_|T1], [CRS-BBox|T2], [CRS-BBox|T3]) :- !,
  wms_bboxes_inheritance(T1, T2, T3).
% A bounding box for a CRS that is only defined by the parent.
wms_bboxes_inheritance([CRS1-BBox1|T1], [CRS2-BBox2|T2], [CRS1-BBox1|T3]) :-
  CRS1 @< CRS2, !,
  wms_bboxes_inheritance(T1, [CRS2-BBox2|T2], T3).
% A bounding box for a CRS that is only defined by the child.
wms_bboxes_inheritance([CRS1-BBox1|T1], [CRS2-BBox2|T2], [CRS2-BBox2|T3]) :-
  CRS2 @< CRS1, !,
  wms_bboxes_inheritance([CRS1-BBox1|T1], T2, T3).



%! wms_capability(+Version:compound, +Dom:compound, -ExceptionFormats:list(atom),
%!                -Formats:list(atom), -Layers:list(dict)) is det.

wms_capability(Version, Dom1, ExceptionFormats, Formats, Layers) :-
  xpath_chk(Dom1, //'Capability'(content), Dom2),
  wms_request(Dom2, Formats),
  wms_exception(Dom2, ExceptionFormats),
  wms_named_layers(Version, Dom2, Layers).



%! wms_contact_address(+Dom:compound, -Address:dict) is det.

wms_contact_address(Dom, Dict) :-
  xpath_chk(Dom, //'AddressType'(normalize_space), Type),
  xpath_chk(Dom, //'Address'(normalize_space), Address),
  xpath_chk(Dom, //'City'(normalize_space), City),
  xpath_chk(Dom, //'StateOrProvince'(normalize_Space), StateOrProvince),
  xpath_chk(Dom, //'PostCode'(normalize_space), PostCode),
  xpath_chk(Dom, //'Country'(normalize_space), Country),
  Dict = address{
    address: Address,
    city: City,
    country: Country,
    postcode: PostCode,
    state_or_province: StateOrProvince,
    type: Type
  }.



%! wms_contact_information(+Dom:compound, -ContactInformation:dict) is semidet.
%
% Fails in case no contact information is present.

wms_contact_information(Dom1, Dict) :-
  xpath_chk(Dom1, //'ContactInformation'(content), Dom2),
  xpath_chk(Dom2, //'ContactAddress'(content), Dom3),
  wms_contact_address(Dom3, Address),
  xpath_chk(Dom2, //'ContactElectronicMailAddress'(normalize_space), EMail),
  xpath_chk(Dom2, //'ContactPersonPrimary'(content), Dom4),
  wms_contact_person(Dom4, Person),
  xpath_chk(Dom2, //'ContactPosition'(normalize_space), Position),
  Dict = contact_information{
    address: Address,
    email: EMail,
    person: Person,
    position: Position
  }.



%! wms_contact_person(+Dom:compound, -Person:dict) is det.

wms_contact_person(Dom, Dict) :-
  xpath_chk(Dom, //'ContactPerson'(normalize_space), Name),
  xpath_chk(Dom, //'ContactOrganization'(normalize_space), Organization),
  Dict = person{name: Name, organization: Organization}.



%! wms_crses(+Version:compound, +Dom:compound, -CRSes:list(atom)) is det.

wms_crses(Version, Dom, CRSes) :-
  aggregate_all(set(CRS), wms_crs(Version, Dom, CRS), CRSes).

wms_crs(Version, Dom, CRS) :-
  wms_crs_string(Version, Dom, String),
  wms_crs_term(String, CRS).

wms_crs_string(version(1,1,1), Dom, String) :- !,
  xpath(Dom, //'SRS'(normalize_space), String).
wms_crs_string(version(1,3,0), Dom, String) :- !,
  xpath(Dom, //'CRS'(normalize_space), String).
wms_crs_string(Version, _, _) :-
  domain_error(wms_version, Version).

wms_crs_term(String1, Label) :-
  atomic_list_concat([Namespace,String2], :, String1),
  wms_crs_term_namespace(Namespace), !,
  (   atomic_list_concat([Code,Params], ',', String2)
  ->  Label = label(Namespace,Code,Params)
  ;   Label = label(Namespace,String2)
  ).
wms_crs_term(Uri, uri(Uri)).

wms_crs_term_namespace('AUTO2').
wms_crs_term_namespace('CRS').
wms_crs_term_namespace('EPSG').



%! wms_exception(+Dom:compound, -Exception:dict) is det.

wms_exception(Dom1, Formats) :-
  xpath_chk(Dom1, //'Exception'(content), Dom2),
  wms_formats(Dom2, Formats).



%! wms_formats(+Dom:compound, -Formats:list(atom)) is det.

wms_formats(Dom, Formats) :-
  aggregate_all(set(Format), xpath(Dom, //'Format'(normalize_space), Format), Formats).



%! wms_keywords(+Dom:compound, -Keywords:list(atom)) is det.

wms_keywords(Dom, Keywords) :-
  aggregate_all(set(Keyword), wms_keyword(Dom, Keyword), Keywords).

wms_keyword(Dom, Keyword) :-
  xpath(Dom, //'KeywordList'//'Keyword'(normalize_space), Keyword).



%! wms_layer_attribution(+Dom:compound, -Attribution:dict) is semidet.

wms_layer_attribution(Dom1, Dict) :-
  xpath_chk(Dom1, //'Attribution'(content), Dom2),
  % logo URL
  ignore(wms_layer_attribution_logo(Dom2, Logo)),
  % online resource
  ignore(xpath_chk(Dom2, //'OnlineResource'(normalize_space), OnlineResource)),
  % title
  xpath_chk(Dom2, //'Title'(normalize_space), Title),
  include(
    ground,
    [logo-Logo,online_resource-OnlineResource,title-Title],
    Pairs
  ),
  dict_pairs(Dict, attribution, Pairs).



%! wms_layer_attribution_logo(+Dom:compound, -Logo:dict) is semidet.

wms_layer_attribution_logo(Dom1, Dict) :-
  xpath_chk(Dom1, //'LogoURL'(@height(number)=Height,@width(number)=Width,content), Dom2),
  % format
  xpath_chk(Dom2, //'Format'(normalize_space), Format),
  Dict = logo{format: Format, size: size(Height,Width)}.



%! wms_layer_ex_bbox(+Version:compound, +Dom:compound, +Parent:dict, -BBox:compound) is det.

wms_layer_ex_bbox(version(1,1,1), Dom1, _, bbox(min(W,S),max(E,N))) :-
  xpath_chk(Dom1, //'LatLonBoundingBox'(@maxx(number)=E,@maxy(number)=N,@minx(number)=W,@miny(number)=S), _), !.
wms_layer_ex_bbox(version(1,3,0), Dom1, _, bbox(min(W,S),max(E,N))) :-
  xpath_chk(Dom1, //'EX_GeographicBoundingBox'(content), Dom2), !,
  xpath_chk(Dom2, //eastBoundLongitude(number), E),
  xpath_chk(Dom2, //northBoundLatitude(number), N),
  xpath_chk(Dom2, //southBoundLatitude(number), S),
  xpath_chk(Dom2, //westBoundLongitude(number), W).
wms_layer_ex_bbox(_, _, Parent, BBox) :-
  get_dict(ex_bbox, Parent, BBox), !.
wms_layer_ex_bbox(_, Dom, _, _) :-
  existence_error(ex_bbox, Dom).



%! wms_layer_max_scale_denominator(+Dom:compound, -MaxScaleDenominator:number) is semidet.

% WMS 1.1.1
wms_layer_max_scale_denominator(Dom, N) :-
  xpath_chk(Dom, //'ScaleHint'(@max(number)), N).
% WMS 1.3.0
wms_layer_max_scale_denominator(Dom, N) :-
  xpath_chk(Dom, //'MaxScaleDenominator'(number), N).



%! wms_layer_metadata_uris(+Dom:compound, -Uris:list(dict)) is det.

wms_layer_metadata_urls(Dom, Dicts) :-
  aggregate_all(set(Dict), wms_layer_metadata_url(Dom, Dict), Dicts).

% Two “type” attribute values are defined by this International
% Standard: the value “ISO 19115:2003” refers to ISO 19115:2003; the
% value “FGDC:1998” refers to FGDC-STD-001-1998.
wms_layer_metadata_url(Dom1, Dict) :-
  xpath(Dom1, //'MetadataURL'(@type(string)=Type,content), Dom2),
  xpath_chk(Dom2, //'Format'(normalize_space), Format),
  Dict = metadata_url{format: Format, type: Type}.



%! wms_layer_min_scale_denominator(+Dom:compound, -MinScaleDenominator:number) is semidet.

% WMS 1.1.1
wms_layer_min_scale_denominator(Dom, N) :-
  xpath_chk(Dom, //'ScaleHint'(@min(number)), N).
% WMS 1.3.0
wms_layer_min_scale_denominator(Dom, N) :-
  xpath_chk(Dom, //'MinScaleDenominator'(number), N).



%! wms_named_layer(+Version:compound, +Dom:compound, -Layers:list(dict)) is nondet.
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

wms_named_layers(Version, Dom, Layers) :-
  aggregate_all(set(Layer), wms_named_layer(Version, Dom, layer{}, Layer), Layers).

wms_named_layer(Version, Dom1, Parent, Child) :-
  xpath_direct_indirect(Dom1, 'Layer', Dom2, Indirect),
  % abstract (optional)
  ignore(xpath_chk(Dom2, //'Abstract'(normalize_space), Abstract)),
  % attribution (optional,inheritance:replace)
  ignore(
    (   wms_layer_attribution(Dom2, Attribution)
    ->  true
    ;   get_dict(attribution, Parent, Attribution)
    )
  ),
  % bounding boxes (inheritance:replace)
  wms_bboxes(Version, Dom2, Parent, BBoxes),
  % CRSes (inheritance:add)
  get_dict(crses, Parent, ParentCRSes, []),
  wms_crses(Version, Dom2, SelfCRSes),
  ord_union(ParentCRSes, SelfCRSes, CRSes),
  % ex bounding box (inheritance:replace)
  wms_layer_ex_bbox(Version, Dom2, Parent, ExBBox),
  % keywords (optional)
  wms_keywords(Dom2, Keywords),
  % metadata URLs
  wms_layer_metadata_urls(Dom2, MetadataURLs),
  % name (optional)
  ignore(xpath_chk(Dom2, //'Name'(normalize_space), Name)),
  % scale denominators (optional,inheritance:replace)
  ignore(
    (   wms_layer_max_scale_denominator(Dom2, MaxScaleDenominator)
    ->  true
    ;   get_dict(max_scale_denominator, Parent, MaxScaleDenominator)
    )
  ),
  ignore(
    (   wms_layer_min_scale_denominator(Dom2, MinScaleDenominator)
    ->  true
    ;   get_dict(min_scale_denominator, Parent, MinScaleDenominator)
    )
  ),
  % styles (inheritance:add)
  get_dict(styles, Parent, ParentStyles, []),
  wms_styles(Dom2, SelfStyles),
  ord_union(ParentStyles, SelfStyles, Styles),
  % title
  xpath_chk(Dom2, //'Title'(normalize_space), Title),
  ignore(get_dict(name, Parent, ParentName)),
  include(
    ground,
    [
      abstract-Abstract,
      attribution-Attribution,
      bboxes-BBoxes,
      crses-CRSes,
      ex_bbox-ExBBox,
      keywords-Keywords,
      max_scale_denominator-MaxScaleDenominator,
      metadata_urls-MetadataURLs,
      min_scale_denominator-MinScaleDenominator,
      name-Name,
      parent-ParentName,
      styles-Styles,
      title-Title
    ],
    Pairs
  ),
  dict_pairs(Self, layer, Pairs),
  (   % If, and only if, a layer has a <Name>, then it is a map layer
      % that can be requested by using that Name in the LAYERS
      % parameter of a GetMap request.  A Layer that contains a <Name>
      % element is referred to as a “named layer” in this
      % International Standard.  If the layer has a Title but no Name,
      % then that layer is only a category title for all the layers
      % nested within.
      ground(Name),
      Child = Self
  ;   % Explore children.
      wms_named_layer(Version, Indirect, Self, Child)
  ).



%! wms_request(+Dom:compound, -Formats:list(atom)) is det.
%
% @tbd DescribeLayer
% @tbd GetCapabilities
% @tbd GetFeatureInfo
% @tbd GetLegendGraphic
% @tbd GetStyles

wms_request(Dom1, Formats) :-
  xpath_chk(Dom1, //'Request'(content), Dom2),
  wms_request_GetMap(Dom2, Formats).



%! wms_request_GetMap(+Dom:compound, -Formats:list(atom)) is det.

wms_request_GetMap(Dom1, Formats) :-
  xpath_chk(Dom1, //'GetMap'(content), Dom2),
  wms_formats(Dom2, Formats).



%! wms_service(+Dom:compound, -Service:dict) is det.

wms_service(Dom1, Dict) :-
  xpath_chk(Dom1, //'Service'(content), Dom2),
  ignore(xpath_chk(Dom2, //'Abstract'(normalize_space), Abstract)),
  ignore(xpath_chk(Dom2, //'AccessConstraints'(normalize_space), AccessConstraints)),
  ignore(wms_contact_information(Dom2, ContactInformation)),
  ignore(xpath_chk(Dom2, //'Fees'(normalize_space), Fees)),
  wms_keywords(Dom2, Keywords),
  ignore(xpath_chk(Dom2, //'LayerLimit'(number), MaxLayers)),
  ignore(xpath_chk(Dom2, //'MaxHeight'(number), MaxHeight)),
  ignore(xpath_chk(Dom2, //'MaxWidth'(number), MaxWidth)),
  xpath_chk(Dom2, //'Name'(normalize_space), Name),
  xpath_chk(Dom2, //'OnlineResource'(normalize_space), Uri),
  xpath_chk(Dom2, //'Title'(normalize_space), Title),
  include(
    ground,
    [
      abstract-Abstract,
      access_constraints-AccessConstraints,
      contact_inormation-ContactInformation,
      fees-Fees,
      keywords-Keywords,
      max_layers-MaxLayers,
      max_size-size(MaxWidth,MaxHeight),
      name-Name,
      title-Title,
      uri-Uri
    ],
    Pairs
  ),
  dict_pairs(Dict, service, Pairs).



%! wms_styles(+Dom:compound, -Styles:list(dict)) is det.

wms_styles(Dom, Dicts) :-
  aggregate_all(set(Dict), wms_style(Dom, Dict), Dicts).

wms_style(Dom1, Dict) :-
  xpath(Dom1, /'Style', Dom2),
  ignore(xpath_chk(Dom2, /'Abstract'(normalize_space), Abstract)),
  ignore(wms_style_legend(Dom2, Legend)),
  xpath_chk(Dom2, /'Name'(normalize_space), Name),
  xpath_chk(Dom2, /'Title'(normalize_space), Title),
  include(
    ground,
    [abstract-Abstract, legend-Legend, name-Name, title-Title],
    Pairs
  ),
  dict_pairs(Dict, style, Pairs).



%! wms_style_legend(+Dom:compound, -Legend:dict) is semidet.

wms_style_legend(Dom1, Dict) :-
  xpath_chk(Dom1, /'LegendURL'(@height(number)=Height,@width(number)=Width), Dom2),
  xpath_chk(Dom2, /'Format'(normalize_space), Format),
  Dict = legend_url{format: Format, size: size(Height,Width)}.



%! wms_version(+Dom:compound, -Version:compound) is det.

wms_version(Dom, Version) :-
  wms_version_string(Dom, String),
  atomic_list_concat([Major0,Minor0,Patch0], ., String),
  maplist(atom_number, [Major0,Minor0,Patch0], [Major,Minor,Patch]),
  Version = version(Major,Minor,Patch).

% WMS 1.1.1
wms_version_string(Dom, Version) :-
  xpath_chk(Dom, /'WMT_MS_Capabilities'(@version(string)), Version), !.
% WMS 1.3.0
wms_version_string(Dom, Version) :-
  xpath_chk(Dom, /'WMS_Capabilities'(@version(string)), Version).



%! xpath_direct_indirect(+Dom1:compound, +Tag:atom, -Direct:compound, -Indirect:compound) is nondet.

xpath_direct_indirect(Dom, Tag, Direct, Indirect) :-
  member(element(Tag,_,Doms), Dom),
  partition(indirect_tag(Tag), Doms, Indirect, Direct).

indirect_tag(Tag, element(Tag,_,_)).





% TESTS %

%example('http://metaspatial.net/cgi-bin/ogc-wms.xml').
example('http://warper.erfgoedleiden.nl/maps/wms/1313').
