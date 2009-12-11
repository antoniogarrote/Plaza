/*
 * Implementation of the generic Adapter interface for a Sesame repository.
 */

package plaza.repositories.sesame;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangBitstr;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import org.openrdf.model.Resource;
import org.openrdf.model.Statement;
import org.openrdf.model.impl.URIImpl;
import org.openrdf.query.BindingSet;
import org.openrdf.query.GraphQuery;
import org.openrdf.query.GraphQueryResult;
import org.openrdf.repository.Repository;
import org.openrdf.repository.RepositoryConnection;
import org.openrdf.rio.RDFFormat;
import org.openrdf.sail.memory.MemoryStore;
import org.openrdf.repository.sail.SailRepository;
import org.openrdf.query.QueryLanguage;
import org.openrdf.query.TupleQuery;
import org.openrdf.query.TupleQueryResult;
import org.openrdf.repository.RepositoryResult;


/**
 *
 * @author Antonio Garrote Hern‡ndez
 */
public class Adapter implements plaza.repositories.interfaces.Adapter {

    private Repository repository;

    // Possible options for the connection:
    public static final String TYPE = "type";
    public static final String PERSISTENT = "persistent";
    public static final String LOCATION = "location";

    // Supported formats
    public static final String RDFXML = "rdfxml";
    public static final String N3 = "n3";
    public static final String TURTLE = "turtle";


    /**
     * Starts the connection to a certain adapter.
     * 
     * @param options 
     * The options to the Sesame repositories. Possible values are
     * - type : (memory, native, http)
     * - persistent : true, false
     * - location : file path or URL
     * - inference : true, false
     */
    public void connect(HashMap<String, String> options) throws Exception {
        String type = options.get(TYPE);

        if(type.equalsIgnoreCase("memory")) {
            memoryConnect(options);
        } else if(type.equalsIgnoreCase("native")) {
            throw new Exception("not implemented yet");
        } else if(type.equalsIgnoreCase("http")) {
            throw new Exception("not implemented yet");
        } else {
            throw new Exception("Unsupported kind of Sesame repository " + type);
        }
    }

    /**
     * Creates a new memory repository.
     *
     * @param options
     */
    private void memoryConnect(HashMap<String, String> options) throws Exception {
        String persistent = options.get(PERSISTENT);

        if(persistent == null || persistent.equalsIgnoreCase("false")) {
            repository=  new SailRepository(new MemoryStore());
            repository.initialize();
        } else {
            File dataDir = new File(options.get(LOCATION));
            repository=  new SailRepository(new MemoryStore(dataDir));
            repository.initialize();
        }
    }

    /**
     * Adds a set of triplets with a certain encoded to the graph stored in the
     * repository identified by baseUri
     *
     * @param baseUri
     * The URI of the graph where the triplet must be inserted.
     * @param triplets
     * The encoded triplets to be inserted.
     * @param format
     * The format of the triplets: (RDFXML, N3, TURTLE)
     * @throws Exception
     */
    public void addTriples(String baseUri, String triplets, String[] contexts, String format) throws Exception {
        RepositoryConnection con = repository.getConnection();

        URIImpl[] contextUris = new URIImpl[contexts.length];
        for(int i=0; i<contexts.length; i++) {
            contextUris[i] = new URIImpl(contexts[i]);
        }

        try {
            if(format.equalsIgnoreCase(RDFXML)) {
                con.add(new ByteArrayInputStream(triplets.getBytes()), baseUri, RDFFormat.RDFXML,contextUris);
            } else if(format.equalsIgnoreCase(N3)) {
                con.add(new ByteArrayInputStream(triplets.getBytes()), baseUri, RDFFormat.N3, contextUris);
            } else if(format.equalsIgnoreCase(TURTLE)) {
                con.add(new ByteArrayInputStream(triplets.getBytes()), baseUri, RDFFormat.TURTLE, contextUris);
            } else {
                throw new Exception("Unsupported format " + format);
            }
        } finally {
            con.close();
        }
    }

    /**
     * Receives a SPARQL query string and returns the result of evaluating that
     * query in the repository.
     *
     * @param sparqlQuery
     * @return List of ErlangTriplet objects
     * @throws Exception
     */
    public OtpErlangList query(String sparqlQuery) throws Exception {
        RepositoryConnection con = repository.getConnection();
        try {

            TupleQuery tupleQuery = con.prepareTupleQuery(QueryLanguage.SPARQL, sparqlQuery);
            TupleQueryResult result = tupleQuery.evaluate();
            
            List<String> names = result.getBindingNames();
            ArrayList<OtpErlangObject> bindings = new ArrayList();

            while(result.hasNext()) {
                BindingSet binding = result.next();
                Iterator<String> it = names.iterator();
                OtpErlangTuple[] singleResultTuples = new OtpErlangTuple[names.size()];
                int counter = 0;

                while(it.hasNext()) {
                    String name = it.next();
                    String value = binding.getBinding(name).getValue().stringValue();
                    OtpErlangObject[] tupleValues = new OtpErlangObject[2];
                    tupleValues[0] = new OtpErlangAtom(name);
                    tupleValues[1] = new OtpErlangBitstr(value.getBytes());

                    OtpErlangTuple prop = new OtpErlangTuple(tupleValues);

                    singleResultTuples[counter] = prop;
                    counter++;
                }
                bindings.add(new OtpErlangList(singleResultTuples));
            }
            
            return new OtpErlangList(bindings.toArray(new OtpErlangObject[bindings.size()]));
        }finally {
            con.close();
        }
    }

    public void delete(String graphUri, String[] contexts) throws Exception {
        RepositoryConnection con = repository.getConnection();
        try {

            RepositoryResult<Statement> sts = con.getStatements(null, null, null, false, new URIImpl(graphUri));


            org.openrdf.model.Resource[] contextsDeleteRes = new Resource[contexts.length] ;
            for(int i=0; i<contexts.length; i++) {
                contextsDeleteRes[i] = new URIImpl(contexts[i]);
            }

            con.remove(sts, contextsDeleteRes);
            con.clear(new URIImpl(graphUri));
        }finally {
            con.close();
        }
    }

    public void updateTriples(String baseUri, String triplesToDeleteQuery, String triplesToAdd, String[] contextsToDelete, String[] contextsToInsert, String format) throws Exception {
        // This starts a transaction
        RepositoryConnection con = repository.getConnection();
        try {

            // First we remove the delete query graph
            GraphQuery tupleQuery = con.prepareGraphQuery(QueryLanguage.SPARQL, triplesToDeleteQuery);
            GraphQueryResult result = tupleQuery.evaluate();

            org.openrdf.model.Resource[] contextsDeleteRes = new Resource[contextsToDelete.length] ;
            for(int i=0; i<contextsToDelete.length; i++) {
                contextsDeleteRes[i] = new URIImpl(contextsToDelete[i]);
            }

            con.remove(result, contextsDeleteRes);
            result.close();

            // Then we insert the new triples
            org.openrdf.model.Resource[] contextsInsertRes = new Resource[contextsToInsert.length] ;
            for(int i=0; i<contextsInsertRes.length; i++) {
                contextsInsertRes[i] = new URIImpl(contextsToInsert[i]);
            }


            if (format.equalsIgnoreCase(RDFXML)) {
                con.add(new ByteArrayInputStream(triplesToAdd.getBytes()), baseUri, RDFFormat.RDFXML,contextsInsertRes);
            } else if(format.equalsIgnoreCase(N3)) {
                con.add(new ByteArrayInputStream(triplesToAdd.getBytes()), baseUri, RDFFormat.N3, contextsInsertRes);
            } else if(format.equalsIgnoreCase(TURTLE)) {
                con.add(new ByteArrayInputStream(triplesToAdd.getBytes()), baseUri, RDFFormat.TURTLE, contextsInsertRes);
            } else {
                throw new Exception("Unsupported format " + format);
            }
        }finally {
            // End of the transaction
            con.close();
        }
    }

}
