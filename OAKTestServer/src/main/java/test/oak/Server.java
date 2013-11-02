package test.oak;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.Arrays;

public class Server {

	private static final byte[] MESSAGE = { (byte) 0xC6, 0x57, 0x54,
			(byte) 0x95, 0x5E, (byte) 0x9E, 0x6B, (byte) 0xC6, 0x55, 0x17,
			0x55, 0x52, (byte) 0x9E, 0x21 };

	private static final byte[] OK = { (byte) 0xC6, 0x57, 0x55, 0x7A, 0x7A,
			(byte) 0x9E, 0x21 };

	private static final byte[] ERR = { (byte) 0xC6, 0x52, (byte) 0xD7, 0x45,
			(byte) 0xD2, (byte) 0x9E, 0x21 };

	private static final byte[] EXPECTED = { (byte) 0xc6, 0x55, 0x17, 0x55,
			0x52, (byte) 0x9e, 0x6b, (byte) 0xc6, 0x55, (byte) 0xd4,
			(byte) 0x95, 0x76, (byte) 0x9e, 0x21 };

	public static void main(String[] args) throws IOException {
		ServerSocket server = new ServerSocket(9999);
		System.out.println("[SERVER] Initialized...");

		System.out.println("[SERVER] Waiting for request...");

		Socket s = server.accept();
		InputStream is = s.getInputStream();
		DataInputStream reader = new DataInputStream(is);

		DataOutputStream writer = new DataOutputStream(s.getOutputStream());
		writer.write(MESSAGE);
		writer.flush();
		System.out.println("[SERVER] Sent encoded message.");
		System.out.println("[SERVER] Waiting for reply...");

		byte[] result = new byte[14];

		reader.read(result);
		System.out.println("[SERVER] Received: " + bytesToHex(result));

		if (Arrays.equals(result, EXPECTED)) {
			System.out.println("[SERVER] Answer is correct! Congratulations!");
			writer.write(OK);
		} else {
			System.out.println("[SERVER] Wrong answer... :-(");
			writer.write(ERR);
		}
		writer.flush();

		server.close();
	}

	private static final char[] hexArray = "0123456789ABCDEF".toCharArray();

	public static String bytesToHex(byte[] bytes) {
		char[] hexChars = new char[bytes.length * 2];
		int v;
		for (int j = 0; j < bytes.length; j++) {
			v = bytes[j] & 0xFF;
			hexChars[j * 2] = hexArray[v >>> 4];
			hexChars[j * 2 + 1] = hexArray[v & 0x0F];
		}
		return new String(hexChars);
	}
	
}
