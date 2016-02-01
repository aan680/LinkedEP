/** <examples>
?- distinct([A,B], (double_membership(A,B))). 
?- prop_with_multiple_datatypes(Prop,DT1,DT2).
?- undefined_class_or_prop(C).
?- unannotated_class_or_prop(C).
*/

%entities as members of disjoint classes.
%only allowed for LP classes Speakers and MEPs.
double_membership(A,B):-
	rdf(S, rdf:type, A),
	rdf(S, rdf:type, B),
    \+ rdf_equal(A,B).

%usage of homogeneous datatypes 
prop_with_multiple_datatypes(Prop,DT1,DT2):-
    rdf(_,Prop, literal(type(DT1, _))),
    rdf(_,Prop, literal(type(DT2, _))),
    \+ rdf_equal(DT1, DT2).

%no stating of inconsistent property values for entities
%interpreted as: multiple values. excluded properties 'text' and 'translatedText' and 'politicalFunction' 
%(all have doubles) from this search.
%ontology hijacking is assessed at the same time: as the external vocabularies have been loaded into ClioPatria, 
%redefinitions pop up for the nonliteral rule.

%10 inconsistent dates of birth for MEPs for two versions of ADEP. 
prop_with_inconsistent_values_literals(S,Prop,L1,L2,G1, G2):-
    rdf(S,Prop, literal(type(DT1, L1)), G1),
    rdf(S,Prop, literal(type(DT2, L2)), G2),
    \+ rdf_equal(L1,L2),
    \+ rdf_equal(Prop, 'lpv:text'),
    \+ rdf_equal(Prop, 'lpv:translatedText').
  
prop_with_inconsistent_values_nonliterals(S,Prop,O1,O2, G1, G2):-
    rdf(S,Prop, O1, G1),
    rdf(S,Prop, O2, G2),
    \+ rdf_equal(O1,O2),
    \+ rdf_equal(Prop, 'lpv:text'),
    \+ rdf_equal(Prop, 'lpv:translatedText'),
    \+ rdf_equal(Prop, 'lpv:politicalFunction').
    
%ambiguous annotation
%doubles are due to (manually verified) links to Italian parliament
ambiguous_annotation(S,O1,O2):-
    rdf(S, owl:sameAs, O1),
    rdf(S, owl:sameAs, O2),
    \+ rdf_equal(O1, O2).

%invalid usage of undefined classes and properties
undefined_class_or_prop(C):-
    rdf(_, rdf:type, C),
    \+ rdf(C, rdf:type, rdfs:'Class'); 
    \+ rdf(C, rdf:type, rdf:'Property').

%invalid usage of undefined classes and properties
unannotated_class_or_prop(C):- %only defined as
%class or prop but e.g. no label
    rdf(_, rdf:type, C),
    \+ rdf(C, P, _),
    \+ rdf_equal(P, rdf:type).
    
%misplaced classes or properties
mixedup_class_or_prop:-
    rdf(_, rdf:type, Class),
    rdf(_, Prop, _),
    rdf_equal(Prop,Class).


