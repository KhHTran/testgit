import java.rmi.*;
import java.rmi.registry.*;
import java.util.*;
import java.math.*;

public class MyClient {
	RMI rmiServer;
	int keyVal;
	UUID uuid;
	
	public MyClient (String host, String username) {
		try {
			Registry reg = LocateRegistry.getRegistry(host); // get registry from host address
			this.rmiServer = (RMI)(reg.lookup("rmiServer")); // get Server 
			this.uuid = rmiServer.receiveID(username); // sent username to server and store the returned uuid 
			this.getKey();  // create secret key value
			String s = this.rmiServer.getcipher(this.uuid); // request server for cypher text
			s = this.decipher(s); // decipher text
			System.out.println("string deciphered:\n" + s);
		}
		catch (RemoteException e) {
			System.out.println("remote exception");
		}
		catch (NotBoundException e) {
			System.out.println("not bound exception");
		}
	}
	
	void getKey () throws RemoteException {
		Random ran = new Random();
		int b = ran.nextInt(500); // create random value
		int prime = this.rmiServer.getPrime(); // get prime and root value
		int root = this.rmiServer.getRoot();
		int value = this.rmiServer.getValue(this.uuid); // get server random value
		BigInteger p = BigInteger.valueOf(prime);
		this.rmiServer.passValue(this.uuid, b); // pass random value to server
		BigInteger pow = BigInteger.valueOf(b); // calculate and store secret key
		BigInteger v = BigInteger.valueOf(root);
		v = v.modPow(pow, p);
		pow = BigInteger.valueOf(value);
		v = v.modPow(pow, p);
		this.keyVal = Integer.valueOf(v.toString()).intValue();
	}
	
	public String decipher (String s) { // decipher a string with key value
		int len = s.length();
		int key = 2 * this.keyVal; // decipher is done twice 
		char[] arr = s.toCharArray();
		for (int i = 0; i < len/8; i++) {
			char[] temp = new char[12];
			int shift = key % 8;
			for(int j = 0; j < 8 ; j++) {
				temp[(8 + j - shift) % 8] = arr[i * 8 + j]; // left shift the characters 
			}
			shift = key % 26;
			for (int j = 0; j < 8; j++) {
				int v = (int) temp[j];
				int A = (int) 'A';
				v = v - A;
				arr[8 * i + j] = (char) ((26 + v - shift) % 26 + A); // resubtitutes the characters 
			}
		}
		String ans = String.copyValueOf(arr);
		return ans; // return decipher string
	}
	
	public static void main(String args[]) {
		if (args.length < 2) {
			// check if compile syntax is correct
			System.err.println("Wrong Syntax");
			return;
		}
		MyClient client1 = new MyClient(args[0],args[1]);
	}
}
