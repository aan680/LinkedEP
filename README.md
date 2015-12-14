# LinkedEP
LinkedEP RDF generation from XML, written in SWI-Prolog. The scripts rely on a toolkit made by Jan Wielemaker: 
http://semanticweb.cs.vu.nl/xmlrdf/ 

In a nutshell, this is how it works:

0. Install ClioPatria. See http://www.swi-prolog.org/web/ClioPatria/.

1. Store the XML files with web-scraped Europarl data in folder src in the ClioPatria folder.

2. Start the server with run.pl (with optional --port argument)

3. Run [run-data.pl] to load script run-data.pl.

4. Run convert_dir(src). This loads the XML files stored in folder 'src', converts the XML trees to triples, and stores all of these in a single RDF graph. The conversion is a literal one: every node in the XML becomes an entity carrying the same name as in the XML file. The only (initial) step towards 'linkedpolitics' data is in the namespace, which is already set to http://purl.org/linkedpolitics/vocabulary. Further rewriting is done using rewrite_data.pl. If desired, the 'rewrite' statement in convert_file can be uncommented to run all rules in one go.

5. Run all rewrite rules in rewrite_data.pl in the order indicated in comments. See the link to the rewrite package above for details about the rewrite syntax. Running the rewrite functions one at a time allows to carefully check the results.

6. Infer some relations missing in the source data (notably the auxiliary ones) with Infer_auxiliary_relations.pl. 
