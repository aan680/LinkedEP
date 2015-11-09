:- module(ahm_rewrite_people,
	  [ rewrite/0,
	    rewrite/1,
	    rewrite/2,
	    list_rules/0
	  ]).
:- use_module(library(regex)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(xmlrdf/rdf_convert_util)).
:- use_module(library(xmlrdf/cvt_vocabulary)).
:- use_module(library(xmlrdf/rdf_rewrite)).

:- debug(rdf_rewrite).

%%	rewrite
%
%	Apply all rules on the graph =people=

rewrite :-
	rdf_rewrite(people).

%%	rewrite(+Rule)
%
%	Apply the given rule on the graph =people=

rewrite(Rule) :-
	rdf_rewrite(people, Rule).

%%	rewrite(+Graph, +Rule)
%
%	Apply the given rule on the given graph.

rewrite(Graph, Rule) :-
	rdf_rewrite(Graph, Rule).

%%	list_rules
%
%	List the available rules to the console.

list_rules :-
	rdf_rewrite_rules.

:- discontiguous
	rdf_mapping_rule/5.


clean_space @@
{ _, _, "\240\\240\" }
	<=>
	true.

merge_text @@
{ S, rdf:type, lpv:'Speech' }
	==>
     findall(T, p_of(S, T), Text),
     atomic_list_concat(Text, '\n', AllText),
     { S, lpv:text, literal(AllText) }.

p_of(S, T) :-
	rdf(S, lpv:p, BN),
	rdf(BN, rdf:value, literal(T)).

define_lang @@
      { S, lpv:p, BN},
      { BN, lpv:stageDirection, literal(SD)}
	==>
      contains_one_language(SD),
      language_from_metadata(SD,Lang),
     { S, dc:language, literal(Lang) }.


speech_language(S,L):-
	rdf(S, lpv:p, BN),
	rdf(BN, lpv:stageDirection, literal(SD)),
	contains_one_language(SD),
	language_from_metadata(SD, L).

contains_one_language(SD):-
	aggregate_all(count, (language_from_metadata(SD, _L)), Count), Count<2.

english_default_lang @@
      {S, rdf:type, lpv:'Speech'}
       <=>
      \+ rdf(S, dc:language,_),
      {S, dc:language, literal(type('http://www.w3.org/2001/XMLSchema','en'))}.



:- use_module(library(dcg/basics)).

language_from_metadata(Atom, Lang) :-
    atom_codes(Atom, Codes),
    phrase(contains_lang(Lang), Codes).

contains_lang(Lang) -->
    string(_), "(", [C1,C2], ")", string(_),
    { atom_codes(Lang, [C1,C2]),
      valid_lang(Lang)
    }.

valid_lang(Lang) :-
    downcase_atom(Lang, Lower),
    iso_639_2(Lower, _Lang).

%aggregate_all(count, (contains_a_language('(FR), (EN)', L)), Count), Count <2.

%%%%% give the structural parts each a correct URI%%%%%%

speech_uri @@
{ S, rdf:type, lpv:'Speech' },
{ S, lpv:speechNo, literal(No) }
	\ {S}
	<=>
	speech_root(S, Root),
	rdf(Root, lpv:meta, Meta),
	rdf(Meta, lpv:dcDate, literal(Date)),
	atomic_list_concat(['http://purl.org/linkedpolitics/', Date,
			    '_', 'Speech', '_', No], N),
	{N}.


agendaitem_uri_OLD @@
{ S, rdf:type, lpv:'Topic' },
{ S, lpv:docno, literal(Docno) }
	\ {S}
	<=>
	split_string(Docno,'.', '.', SubStrings),
	nth1(2, SubStrings, Date),
	nth1(3, SubStrings, No),
	atomic_list_concat(['http://purl.org/linkedpolitics/', Date,
			    '_', 'AgendaItem', '_', No], N),
	{N}.

agendaitem_uri @@
{ S, rdf:type, lpv:'Topic' },
{ S, lpv:docno, literal(Docno) }
	\ {S}
	<=>
	split_string(Docno,'.', '.', SubStrings),
	%nth1(2, SubStrings, Date),
	nth1(3, SubStrings, No),
	speech_to_meta(_,S,_,_,Meta),
	meta_date(Meta,Date),
	atomic_list_concat(['http://purl.org/linkedpolitics/', Date,
			    '_', 'AgendaItem', '_', No], N),
	{N}.


sessionday_uri @@
{ S, rdf:type, lpv:'Proceedings' },
{ S, lpv:docno, literal(Docno) }
	\ {S}
	<=>
	speech_to_meta(_,_,S,_,Meta),
	meta_date(Meta, Date),
	atomic_list_concat(['http://purl.org/linkedpolitics/', Date,
			    '_', 'SessionDay'], N),
	{N}.


meta_date(Meta, Date):-
	rdf(Meta, lpv:dcDate, literal(Date)).


speech_to_meta(S, Topic, Proceedings, Root, Meta) :-
	rdf(S, rdf:type, lpv:'Speech'),
	rdf(Topic, lpv:speech, S),
	rdf(Proceedings, lpv:topic, Topic),
	rdf(Root, lpv:proceedings, Proceedings),
        rdf(Root, lpv:meta, Meta).


%%%%%assign the structural parts to the right class (name)%%%%%%%
agendaitem_type @@
{S, rdf:type, lpv:'Topic' }
	<=>
    atomic_list_concat(['http://purl.org/linkedpolitics/vocabulary/', 'eu/plenary/','AgendaItem'], N),    {S, rdf:type, N}.


sessionday_type @@
{S, rdf:type, lpv:'Proceedings'}
	<=>
    atomic_list_concat(['http://purl.org/linkedpolitics/vocabulary/', 'eu/plenary/','SessionDay'], N),    {S, rdf:type, N}.


speech_type @@
{S, rdf:type, lpv:'Speech'}
	<=>
    atomic_list_concat(['http://purl.org/linkedpolitics/vocabulary/', 'eu/plenary/','Speech'], N),    {S, rdf:type, N}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%correct the properties%%%%%%

%agendaitem-title
agendaitem_title_langtag @@
       { S, lpv:title, literal(Title) },
       \  { S, rdf:type, lpv:'Topic '}
	<=>
	agendaitem_meta(S, Meta),
        meta_language(Meta, Lang),
	{S, dc:title, literal(lang(Lang, Title))}.

title @@
{A, lpv:title, B}
	<=>
{A, dc:title, B}.



meta_language(Meta, Lang):-
	rdf(Meta, lpv:dcLanguage, literal(Lang)).


date_and_number(Docno, Date, No):-
	split_string(Docno,'.', '.', SubStrings),
	nth1(2, SubStrings, Date),
	nth1(3, SubStrings, No).

agendaitem_meta(Topic, Meta) :-
	rdf(Proceedings, lpv:topic, Topic),
	rdf(Root, lpv:proceedings, Proceedings),
        rdf(Root, lpv:meta, Meta).

speech_root(S, Root) :-
	rdf(S, rdf:type, lpv:'Speech'),
	rdf(Topic, lpv:speech, S),
	rdf(Proceedings, lpv:topic, Topic),
	rdf(Root, lpv:proceedings, Proceedings).

%%%%%speech-speaker

mep @@
{ S, lpv:mpid, literal(MEP) },
{ S, lpv:speaker, literal(_Speaker) }
   <=>
   %{ S, lpv:'MEP_ID', literal(MEP)},
   %{ S, lpv:speaker, N},
   atomic_list_concat(['http://purl.org/linkedpolitics/', 'EUmember','_', MEP], N),
   {S, lpv:speaker, N},
   {N, lpv:'MEP_ID', literal(MEP)},
   {N, rdf:type, 'http://purl.org/linkedpolitics/vocabulary/Speaker'},
   {N, rdf:type, 'http://purl.org/linkedpolitics/vocabulary/MemberOfParliament'}.


non-mep-speaker @@ %execute after mep!!!!
{ S, lpv:speaker, literal(_Speaker) }
   <=>
   %{ S, lpv:'MEP_ID', literal(MEP)},
   %{ S, lpv:speaker, N},
   atomic_list_concat(['http://purl.org/linkedpolitics/', 'Speaker','_', MEP], N),
   {N, lpv:'MEP_ID', literal(MEP)},
   {S, lpv:speaker, N},
   {N, rdf:type, 'http://purl.org/linkedpolitics/vocabulary/Speaker'}.

%%%%%%%%hasPart

haspart_ai-speech @@
{A, lpv:speech, B}
	<=>
{A, dcterms:hasPart, B}.

haspart_sessionday-topic @@
{A, lpv:topic, B}
	<=>
{A, dcterms:hasPart, B}.


metadata@@
{A, lpv:stageDirection, B}
	<=>
{A, lpv:unclassifiedMetadata, B}.

metadata_reify @@ %metadata is now attached to a paragraph
{S, lpv:p, A},

{A, lpv:unclassifiedMetadata, B}
	<=>
{S, lpv:unclassifiedMetadata, B}.

remove_p @@
{S, lpv:p,  _}
        <=> true.


remove_pmentity @@ %use this one as a test
{S, rdf:type,  lpv:'PmEntity'}
        <=> true.



%VIC: removed this as it forces skos, for now we go with rda
% {A,skos:inScheme,http://purl.org/collections/nl/am/AM_PeopleScheme'}.
/*							%

people_uris @@
{ A, ahm:name, _Name } \ {A} <=>
	rdf_is_bnode(A),
	rdf(A, ahm:priref, literal(Priref)),
	rdf_current_ns(ahm, S1),
	concat_atom([S1, 'p-', Priref], S),
	{S}.

*/










