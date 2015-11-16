:- module(ahm_convert_people,
	  [ convert_file/1,		% +XMLFile
	    convert_dir/1,		% +Dir
	    rewrite/0,
	    rewrite/1,
	    number_speeches/0
	  ]).

% Setup a search path for finding the data.  Of course this can also
% use relative or absolute paths, but this is a bit easier to addapt
% to the environment.  The search path 'metadata' is predefined as
% one of $RDF_METADATA_DIR or ~/RDF/metadata

user:file_search_path(data, src).

% Get core libraries

:- use_module(library(semweb/rdf_db)).		% Core RDF
:- use_module(library(semweb/rdf_cache)).
:- use_module(library(semweb/rdf_library)).	% Load RDF from library
:- use_module(library(xmlrdf/xmlrdf)).		% XML --> RDF conversion
:- use_module(library(semweb/rdf_turtle_write)).% Save results
:- use_module(library(semweb/rdf_cache)).	% Cache control
:- use_module(library(semweb/rdf_persistency)).	% Persistency control

% Configure the environment:
%
%  - Make prefixes (namespaces) known
%  - Enable caching (speedup reloading large RDF files)
%  - Disable persistency for our rewrite graph(s).

:- rdf_register_ns(lpv,	 'http://purl.org/linkedpolitics/vocabulary/').
:- rdf_persistency(data, false).

% Load our dataset specific rewrite rules

:- use_module(rewrite_data).

number_speeches :-
	findall(S, rdf(S, rdf:type, lpv:'Speech'), Speeches),
	forall(nth1(I, Speeches, S),
	       (   atom_number(A, I),
		   rdf_assert(S, lpv:speechNo, literal(A)))).

convert_dir(Dir) :-
        atom_concat(Dir, '/*.xml', Pattern),
        expand_file_name(Pattern, Files),
        forall(member(File, Files),
               convert_file(File)).

%%	load_xml(+File)
%
%	Load the XML.  Relevant options:
%
%	  - The file is a plain XML file (not using XML namespaces)
%	  - Database entries are mapped to the XML element =record=.
%	    we process the file record-by-record and ignore all data
%	    outside the record elements (e.g., header)
%	  - Set the RDF prefix to the =ahm= prefix.
%	  - Dump all data into the graph =people=
%
%	After running this, we have `raw RDF'.

load_xml(File) :-
	rdf_current_ns(lpv, Prefix),
	load_xml_as_rdf(File,
			[ dialect(xml),
%			  unit(record),
			  prefix(Prefix),
			  graph(people)
			]).

%%	convert_file(+XMLFile)
%%	convert_file(+XMLFile, +TurtleFile)
%
%	Perform the entire conversion:
%
%	  1. Load XMLFile
%	  2. Rewrite the RDF graph
%	  3. Save the result.

convert_file(XMLFile) :-
	file_name_extension(Base, xml, XMLFile),
	file_name_extension(Base, ttl, TurtleFile),
	convert_file(XMLFile, TurtleFile).

convert_file(XMLFile, TurtleFile) :-
	%rdf_retractall(_,_,_,people),
	load_xml(XMLFile),
	%rewrite,
	rdf_save_turtle(TurtleFile, [graph(people)]).



