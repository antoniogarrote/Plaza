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

    public void connect(HashMap<String,String> options) throws Exception;
    public void addTriples(String baseUri, String triplets, String format) throws Exception;
    public OtpErlangList query(String sparqlQuery) throws Exception;
}
