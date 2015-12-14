infer(SourceG, TargetG):-
	forall(has_subsequent(S1, S2, SourceG), rdf_assert(S1, lpv:hasSubsequent, S2, TargetG)),
	forall(session_has_part(S, SD, SourceG), rdf_assert(S, dcterms:hasPart, SD, TargetG)),
  forall(is_part_of(A, B, SourceG), rdf_assert(A, dcterms:isPartOf, B, TargetG)),
	forall(spoken_during_affiliation(S, A, SourceG), rdf_assert(S, lpv:spokenAs, A, TargetG)).

has_subsequent(S1, S2,G) :-
	rdf(S1, dc:date, D1,G),
	rdf(S1, lpv:number, N1, G),
	rdf(S1, rdf:type, T1, G),
	rdf(S2, dc:date, D1, G),
	rdf(S2, lpv:number, N2, G),
	rdf(S2, rdf:type, T2),
	rdf_next_int(N1, N2),
	T1 == T2.

is_part_of(S1,A1, G) :-
	rdf(A1, dcterms:hasPart, S1, G).

spoken_during_affiliation(S,A, G) :-
	rdf(S, lpv:speaker, M, G),
	rdf(S, dc:date, literal(type(xsd:date, D))),
    rdf(M, lpv:politicalFunction, A),
    %rdf(A, rdf:type, lpv:'PoliticalFunction'),
	rdf(A, lpv:beginning, literal(type(xsd:date, B))),
	(rdf(A, lpv:end, literal(type(xsd:date, E))) ->   D @< E),
	%rdf(S, rdf:type, lpv_eu:'Speech'),
 	B @< D.

session_has_part(S, SD, G) :-
	rdf(S, rdf:type, lpv_eu:'Session', G),
	rdf(SD, rdf:type, lpv_eu:'SessionDay', G),
	rdf(S, lpv:month, literal(type(xsd:gMonth, M)), G),
	rdf(SD, lpv:month, literal(type(xsd:gMonth, M)), G),
	rdf(S, lpv:year, literal(type(xsd:gYear, Y)), G),
	rdf(SD, lpv:year, literal(type(xsd:gYear, Y)), G).

:- rdf_meta
	rdf_next_int(o,o).


rdf_next_int(literal(type(xsd:integer, S1)),
	     literal(type(xsd:integer, S2))) :-
	atom_number(S1, N1),
	atom_number(S2, N2),
	N2 =:= N1+1.

