/*
 * An implementation of a Erlang node with JInterface offering
 * the repository functionality.
 */

package plaza.node;

import plaza.repositories.sesame.Adapter;
import com.ericsson.otp.erlang.OtpNode;
import com.ericsson.otp.erlang.OtpMbox;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangString;
import java.io.IOException;
import java.util.HashMap;
import java.util.logging.Level;
import java.util.logging.Logger;


/**
 *
 * @author Antonio Garrote Hern‡ndez
 */
public class RepositoryNode {

    private static void doConnect(OtpErlangPid from, OtpErlangTuple msg, Adapter repository, OtpMbox mbox) {
        try {
            OtpErlangList propList = (OtpErlangList) msg.elementAt(2);
            HashMap<String, String> map = OtpMapperUtils.otpStringPropsList(propList);
            repository.connect(map);

            mbox.send(from, new OtpErlangAtom("ok"));
        } catch (Exception ex) {
            OtpErlangObject[] reply = new OtpErlangObject[2];
            reply[0] = new OtpErlangAtom("error");
            reply[1] = new OtpErlangString(ex.getMessage());

            mbox.send(from, new OtpErlangTuple(reply));
        }
    }

    private static void doAddTriplets(OtpErlangPid from, OtpErlangTuple msg, Adapter repository, OtpMbox mbox) {
        try {
            OtpErlangList propList = (OtpErlangList) msg.elementAt(2);
            HashMap<String, String> map = OtpMapperUtils.otpStringPropsList(propList);
            repository.addTriples(map.get("base"), map.get("triples"), map.get("format"));

            mbox.send(from, new OtpErlangAtom("ok"));
        } catch (Exception ex) {
            OtpErlangObject[] reply = new OtpErlangObject[2];
            reply[0] = new OtpErlangAtom("error");
            reply[1] = new OtpErlangString(ex.getMessage());

            mbox.send(from, new OtpErlangTuple(reply));
        }
    }

    private static void doQuery(OtpErlangPid from, OtpErlangTuple msg, Adapter repository, OtpMbox mbox) {
        try{
            OtpErlangList propList = (OtpErlangList) msg.elementAt(2);
            HashMap<String, String> map = OtpMapperUtils.otpStringPropsList(propList);
            OtpErlangList result = repository.query(map.get("query"));

            

            OtpErlangObject[] reply = new OtpErlangObject[2];
            reply[0] = new OtpErlangAtom("ok");
            reply[1] = result;
            
            mbox.send(from, new OtpErlangTuple(reply));

        } catch (Exception ex) {
            OtpErlangObject[] reply = new OtpErlangObject[2];
            reply[0] = new OtpErlangAtom("error");
            reply[1] = new OtpErlangString(ex.getMessage());

            mbox.send(from, new OtpErlangTuple(reply));
        }
    }

    // IMPORTANT!!!!
    // Must have the same value that plaza_repository.erl REPOSITORY_JAVA_MBOX
    public String MBOX_IDENTIFIER = "triples_repository";

    private OtpNode node;
    private OtpMbox mbox;

    public RepositoryNode(String name) throws IOException {
        node = new OtpNode(name);
        mbox = node.createMbox(MBOX_IDENTIFIER);

        process();
    }

    private void process() {
        final Adapter repository = new Adapter();
        final OtpMbox _mbox = mbox;

        while(true) {
            try {
                OtpErlangObject reply = mbox.receive();
                if (reply instanceof OtpErlangTuple) {
                    final OtpErlangTuple msg = (OtpErlangTuple) reply;

                    new Thread(new Runnable() {

                        public void run() {
                            OtpErlangPid from = (OtpErlangPid) (msg.elementAt(0));
                            String operation = ((OtpErlangAtom) msg.elementAt(1)).toString();

                            if(operation.equalsIgnoreCase(Adapter.CONNECT)) {
                                RepositoryNode.doConnect(from,msg,repository, _mbox);
                            } else if(operation.equalsIgnoreCase(Adapter.ADD_TRIPLES)) {
                                RepositoryNode.doAddTriplets(from,msg,repository, _mbox);
                            } else if(operation.equalsIgnoreCase(Adapter.QUERY)) {
                                RepositoryNode.doQuery(from,msg,repository, _mbox);
                            } else {
                                // not implemented yet
                            }
                        }

                    }).start();
                }
            } catch (Exception ex) {
                System.out.println(ex.getMessage());
            }
        }
    }

    public static void main(String[] args) {
        if(args.length == 0) {
            System.out.println("You must provide a name for the java OTP node as the first parameter");
            Logger.getLogger(RepositoryNode.class.getName()).log(Level.SEVERE, "No node given for the Java OTP node");
            System.exit(1);
        }
        try {
            new RepositoryNode(args[0]).process();
        } catch (IOException ex) {
            Logger.getLogger(RepositoryNode.class.getName()).log(Level.SEVERE, "Exception in repository node:"+ex.getMessage(), ex);
        }
    }
}
