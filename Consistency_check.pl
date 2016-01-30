/** <examples>
?- distinct([A,B], (double_membership(A,B))). 
?- prop_with_multiple_datatypes(Prop,DT1,DT2).
?- undefined_class_or_prop(C).
?- unannotated_class_or_prop(C).
*/


%only allowed for LP classes Speakers and MEPs.
double_membership(A,B):-
	rdf(S, rdf:type, A),
	rdf(S, rdf:type, B),
    \+ rdf_equal(A,B).

prop_with_multiple_datatypes(Prop,DT1,DT2):-
    rdf(_,Prop, literal(type(DT1, _))),
    rdf(_,Prop, literal(type(DT2, _))),
    \+ rdf_equal(DT1, DT2).

undefined_class_or_prop(C):-
    rdf(_, rdf:type, C),
    \+ rdf(C, rdf:type, rdfs:'Class'); 
    \+ rdf(C, rdf:type, rdf:'Property').

unannotated_class_or_prop(C):- %only defined as
%class or prop but e.g. no label
    rdf(_, rdf:type, C),
    \+ rdf(C, P, _),
    \+ rdf_equal(P, rdf:type).

mixedup_class_or_prop:-
    rdf(_, rdf:type, Class),
    rdf(_, Prop, _),
    rdf_equal(Prop,Class).

one_day_of_debates(Date, Title, Url):-
    rdf(SessionDay, dc:date, Date),
    rdf(SessionDay, dcterms:hasPart, AgendaItem),
    rdf(SessionDay, lpv:textURI, literal(Url)),
    rdf(AgendaItem, dc:title, literal(Title)),
    rdf(AgendaItem, lpv:number, literal(Number)).

rdf(AgendaItem, dcterms:hasPart, Speech),
    rdf(SessionDay, dcterms:hasPart, AgendaItem),
    rdf(SessionDay, dc:date, Date).
    
    
  
