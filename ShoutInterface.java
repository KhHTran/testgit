//package comp2207.shout;

import java.rmi.Remote;
import java.rmi.RemoteException;

/**
 * The remote persona of an RMI Shout Service.
 * <p> Any implementation of this interface must implement at least
 * these methods.
 * @author Tim Norman, University of Southampton
 * @version 3.0
 */

public interface ShoutInterface extends Remote
{
    public String shout( String s )
	throws RemoteException;
}
