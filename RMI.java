import java.rmi.*;
import java.util.*;

public interface RMI extends Remote{
	public UUID receiveID (String x) throws RemoteException;
	public int getPrime () throws RemoteException;
	public int getRoot () throws RemoteException;
	public int getValue (UUID uuid) throws RemoteException;
	public String getcipher (UUID uuid) throws RemoteException;
	public void passValue(UUID uuid, int k) throws RemoteException;
}