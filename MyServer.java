import java.rmi.server.*;
import java.rmi.*;
import java.rmi.registry.*;
import java.util.*;
import java.math.BigInteger;
import java.util.concurrent.*;

public class MyServer extends UnicastRemoteObject implements RMI {
	String address;
	CiphertextInterface ctstub;
	Registry reg;
	public static int prime = 97;
	public static int root = 23;
	boolean taken[];
	ConcurrentHashMap<UUID,Client> mapUser; // store unique id for each client and client info
	
	public MyServer() throws RemoteException { // initialize a MyServer object
		this.mapUser = new ConcurrentHashMap<UUID,Client>(); // create a map of <UUID, client> 
		this.taken = new boolean[500];
		try {
			System.setProperty("java.security.policy","mypolicy");
			if (System.getSecurityManager() == null) {
				System.setSecurityManager(new SecurityManager());
			}
			// get registry from VPN server
			Registry ctreg = LocateRegistry.getRegistry("svm-tjn1f15-comp2207.ecs.soton.ac.uk", 12345);
			this.ctstub = (CiphertextInterface) ctreg.lookup("CiphertextProvider");
		}
		catch (Exception e) {
		    e.printStackTrace();
		}
		try {
			//get local registry and bind to MyServer object
			reg = LocateRegistry.getRegistry();
			reg.rebind("rmiServer", this);
		}
		catch(RemoteException e) {
			System.out.println("remote exception"+ e);
		}
	}
	
	public int getPrime() throws RemoteException { // pass the constant prime p value to client 
		return prime;
	}
	
	public int getRoot() throws RemoteException { // pass the constant root of p value to client 
		return root;
	}
	
	public synchronized int getValue(UUID uuid) { // pass the random value generated to client
		Random a = new Random();
		int x = a.nextInt(500);
		BigInteger bigint = BigInteger.valueOf(root);
		BigInteger pow = bigint.modPow(BigInteger.valueOf(x), BigInteger.valueOf(prime));
		int value = Integer.valueOf(pow.toString()).intValue();
		Client temp = this.mapUser.get(uuid);
		temp.value = value;
		return x;
	}

	public synchronized UUID receiveID(String x) throws RemoteException { // connect to a new MyClient with id x
		Client temp = new Client(x); // create new Client object with id x
		UUID s = UUID.randomUUID(); // create new UUID for client
		this.mapUser.put(s, temp); // put into map
		return s; // return the uuid to MyClient
	}
	
	public synchronized String getcipher (UUID uuid) throws RemoteException { //get cipher text for MyClient given uuid 
		Client temp = this.mapUser.get(uuid); // get Client with uuid from Map
		String id = temp.id; // get client id and key
		int key = temp.secretKey;
		String s = this.ctstub.get(id, key); // use key and id to get string from vpn server
		return s; // return to MyClient
	}

	public synchronized void passValue(UUID uuid, int k) throws RemoteException { 
		// take random generated key from MyClient to generate secret key
		Client temp = this.mapUser.get(uuid); // get client with matching uuid
		BigInteger v = BigInteger.valueOf(temp.value); // get secret value 
		v = v.modPow(BigInteger.valueOf(k), BigInteger.valueOf(prime)); // create secret key
		k = Integer.valueOf(v.toString()).intValue(); //convert to int value
		temp.secretKey = k; // store key value 
	}
	
	public static void main(String args[]) {
		try {
			MyServer s = new MyServer();
		}
		catch (RemoteException e) {
			System.out.println("Cannot start server");
		}
	}
}
class Client { //represent a client
	public String id; //username 
	public int secretKey; // share secretKey
	public int value; // server's secret value 
	public Client (String id) {
		this.id = id;
	}
}