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
%1
merge_text @@
{ S, rdf:type, lpv:'Speech' }
	==>
     findall(T, p_of(S, T), Text),
     atomic_list_concat(Text, '\n', AllText),
     { S, lpv:text, literal(AllText) }.

p_of(S, T) :-
	rdf(S, lpv:p, BN),
	rdf(BN, rdf:value, literal(T)).

%2
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


english_default_lang_old @@
      {S, rdf:type, lpv:'Speech'}
       ==>
      no_language_defined(S),
      {S, dc:language, literal(type('http://www.w3.org/2001/XMLSchema','en'))}.

%2b
english_default_lang @@
      {S, rdf:type, lpv:'Speech'}
       <=>
      \+ rdf(S, dc:language,_),
      {S, dc:language, literal(type('http://www.w3.org/2001/XMLSchema','en'))},
      {S, rdf:type, lpv:'Speech'}.


no_language_defined(S):-
    \+ rdf(S, dc:language,_).

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

speech_uri_test(S,Date):-
   speech_root(S,Root),
   rdf(Root, lpv:meta, Meta),
   rdf(Meta, lpv:dcDate, literal(Date)).

%3
speech_uri_old @@
{ S, rdf:type, lpv:'Speech' },
{ S, lpv:speechNo, literal(No) }
	\ {S}
	<=>
	speech_root(S, Root),
	rdf(Root, lpv:meta, Meta),
	rdf(Meta, lpv:dcDate, literal(Date)),
	%rdf(S, lpv:speechNo, literal(No)),
	%split_string(No,'-', '-', SubStrings),
	%nth1(3, SubStrings, No),
	atomic_list_concat(['http://purl.org/linkedpolitics/', Date,
			    '_', 'Speech', '_', No], N),
	{N}.

speech_uri @@
{ S, rdf:type, lpv:'Speech' },
{ S, lpv:docno, literal(No) }
	\ {S}
	<=>
        speech_to_meta(S, Topic, Proceedings, Root, Meta),
	meta_date(Meta, Date),
	split_string(No,'-', '-', SubStrings),
	nth1(3, SubStrings, No),
	atomic_list_concat(['http://purl.org/linkedpolitics/', Date,
			    '_', 'Speech', '_', No], N),
	{N}.

%4
agendaitem_uri @@
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


%5
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

%6
agendaitem_type @@
{S, rdf:type, lpv:'Topic' }
	<=>
    atomic_list_concat(['http://purl.org/linkedpolitics/vocabulary/', 'eu/plenary/','AgendaItem'], N),    {S, rdf:type, N}.


%7
sessionday_type @@
{S, rdf:type, lpv:'Proceedings'}
	<=>
    atomic_list_concat(['http://purl.org/linkedpolitics/vocabulary/', 'eu/plenary/','SessionDay'], N),    {S, rdf:type, N}.

%8
speech_type @@
{S, rdf:type, lpv:'Speech'}
	<=>
    atomic_list_concat(['http://purl.org/linkedpolitics/vocabulary/', 'eu/plenary/','Speech'], N),    {S, rdf:type, N}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%correct the properties%%%%%%

%9
title @@
{A, lpv:title, B}
	<=>
{A, dc:title, B}.

%10
agendaitem_title_langtag @@
       { S, dc:title, literal(Title) }
	<=>
	agendaitem_meta(S, Meta),
        meta_language(Meta, Lang),
	{S, dc:title, literal(lang(Lang, Title))}.


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

%11
mep @@
{ S, lpv:mpid, literal(MEP) },
{ S, lpv:speaker, literal(Whatever) }
   <=>
   %{ S, lpv:'MEP_ID', literal(MEP)},
   %{ S, lpv:speaker, N},
   atomic_list_concat(['http://purl.org/linkedpolitics/', 'EUmember','_', MEP], N),
   {S, lpv:speaker, N},
   {N, lpv:'MEP_ID', literal(MEP)},
   {N, rdf:type, 'http://purl.org/linkedpolitics/vocabulary/Speaker'},
   {N, rdf:type, 'http://purl.org/linkedpolitics/vocabulary/MemberOfParliament'}.

%12
nonmepspeaker @@ %execute after mep!!!!
{ S, lpv:speaker, literal(Speaker) }
   <=>
   %{ S, lpv:'MEP_ID', literal(MEP)},
   %{ S, lpv:speaker, N},
   atomic_list_concat(['http://purl.org/linkedpolitics/', 'Speaker','_', Speaker], N),
   {S, lpv:speaker, N},
   {N, foaf:'name', literal(Speaker)},
   {N, rdf:type, 'http://purl.org/linkedpolitics/vocabulary/Speaker'}.

%%%%%%%%hasPart
%13
haspart_ai_speech @@
{A, lpv:speech, B}
	<=>
{A, dcterms:hasPart, B}.

%14
haspart_sessionday_topic @@
{A, lpv:topic, B}
	<=>
{A, dcterms:hasPart, B}.

%15
metadata@@
{A, lpv:stageDirection, B}
	<=>
{A, lpv:unclassifiedMetadata, B}.

%16
metadata_reify @@ %metadata is now attached to a paragraph
{S, lpv:p, A},

{A, lpv:unclassifiedMetadata, B}
	<=>
{S, lpv:unclassifiedMetadata, B}.

%17
number @@
{S, lpv:speechNo, B}
	<=>
{S, lpv:number, B}.

%18
remove_p @@
{S, lpv:p,  _}
        <=> true.



