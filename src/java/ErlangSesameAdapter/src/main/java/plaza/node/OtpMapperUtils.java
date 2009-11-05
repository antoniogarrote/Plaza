/*
 * Utility functions for translating between OTP types and Java objects.
 */

package plaza.node;

import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.ericsson.otp.erlang.OtpErlangBitstr;
import com.ericsson.otp.erlang.OtpErlangBinary;
import com.ericsson.otp.erlang.OtpErlangAtom;
import java.util.HashMap;
import java.util.Iterator;


/**
 *
 * @author Antonio Garrote Hern‡ndez
 */
public class OtpMapperUtils {

    public static HashMap<String,String> otpStringPropsList(OtpErlangList list) {
        HashMap<String,String> map = new HashMap<String, String>();

        Iterator it = list.iterator();

        while(it.hasNext()) {
            OtpErlangTuple tuple = (OtpErlangTuple) it.next();

            OtpErlangAtom fst = (OtpErlangAtom) tuple.elementAt(0);
            String propValue = null;
            if(tuple.elementAt(1) instanceof OtpErlangString) {
                propValue = ((OtpErlangString) tuple.elementAt(1)).stringValue();
            } else if(tuple.elementAt(1) instanceof OtpErlangAtom) {
                propValue = ((OtpErlangAtom) tuple.elementAt(1)).atomValue();
            } else if(tuple.elementAt(1) instanceof OtpErlangBitstr) {
                propValue = new String(((OtpErlangBitstr) tuple.elementAt(1)).binaryValue());
            } else if(tuple.elementAt(1) instanceof OtpErlangBinary) {
                propValue = new String(((OtpErlangBinary) tuple.elementAt(1)).binaryValue());
            }
            map.put(fst.atomValue(), propValue);
        }

        return map;
    }

    public static HashMap<String,Object> otpStringObjPropsList(OtpErlangList list) {
        HashMap<String,Object> map = new HashMap<String, Object>();

        Iterator it = list.iterator();

        while(it.hasNext()) {
            OtpErlangTuple tuple = (OtpErlangTuple) it.next();

            OtpErlangAtom fst = (OtpErlangAtom) tuple.elementAt(0);
            Object propValue = null;
            if(tuple.elementAt(1) instanceof OtpErlangString) {
                propValue = ((OtpErlangString) tuple.elementAt(1)).stringValue();
            } else if(tuple.elementAt(1) instanceof OtpErlangAtom) {
                propValue = ((OtpErlangAtom) tuple.elementAt(1)).atomValue();
            } else if(tuple.elementAt(1) instanceof OtpErlangBitstr) {
                propValue = new String(((OtpErlangBitstr) tuple.elementAt(1)).binaryValue());
            } else if(tuple.elementAt(1) instanceof OtpErlangBinary) {
                propValue = new String(((OtpErlangBinary) tuple.elementAt(1)).binaryValue());
            } else {
                propValue = tuple.elementAt(1);
            }
            map.put(fst.atomValue(), propValue);
        }

        return map;
    }

    public static String otpBitString(OtpErlangBitstr bstring) {
        return new String(bstring.binaryValue());
    }
}
