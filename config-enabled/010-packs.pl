/* Generated file
   This file defines the search-path for added packs
*/

:- module(conf_packs, []).

:- multifile user:file_search_path/2.
:- dynamic user:file_search_path/2.
:- multifile cpack:registered_cpack/2.

:- cpack_register(foaf,'cpack/foaf',[home_url('http://cliopatria.swi-prolog.org/packs/foaf'),requires([])]).
:- cpack_register(skos,'cpack/skos',[home_url('http://cliopatria.swi-prolog.org/packs/skos'),requires([])]).
:- cpack_register(xmlrdf,'cpack/xmlrdf',[home_url('http://cliopatria.swi-prolog.org/packs/xmlrdf'),requires([foaf,skos])]).
:- cpack_register(swish,'cpack/swish',[home_url('http://cliopatria.swi-prolog.org/packs/swish'),requires([])]).
