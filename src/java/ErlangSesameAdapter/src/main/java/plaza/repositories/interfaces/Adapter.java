/*
 * A common interface for every triple repository adapter.
 */

package plaza.repositories.interfaces;

import java.util.HashMap;
import com.ericsson.otp.erlang.OtpErlangList;

/**
 *
 * @author Antonio Garrote Hern‡ndez
 */
public interface Adapter {

    public final static String CONNECT = "connect";
    public final static String ADD_TRIPLES = "add_triples";
    public final static String QUERY = "query";
    public final static String DELETE_GRAPH = "delete_graph";
    public final static String UPDATE_GRAPH = "update_graph";

    public void connect(HashMap<String,String> options) throws Exception;
    public void addTriples(String baseUri, String triples, String[] contexts, String format) throws Exception;
    public OtpErlangList query(String sparqlQuery) throws Exception;
    public void updateTriples(String baseUri, String triplesToDeleteQuery, String triplesToAdd, String[] contextsToDelete, String[] contextsToInsert, String format) throws Exception;
    public void delete(String graphUri, String[] contexts) throws Exception;
}
