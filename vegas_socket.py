#!/usr/bin/env python 

import socket
import struct

class Generic_Socket:
    """Modified version of the demonstration class from:
    https://docs.python.org/3.6/howto/sockets.html
    """

    def __init__(self, sock=None, address=2*["UNK"]):
        """Create a IPv4 TCP socket
        """
        if sock is None:
            self.sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
            self.address = address
        else:
            self.sock = sock
            self.address = address

    def connect(self, host, port):
        """ Connects socket to given host-port
        """
        self.address = [host, port]
        self.sock.connect((host, port))

    def bind(self, host, port, max_connections = 10):
        """ Makes the socket into a server
        """
        self.sock.bind((host, port))
        self.sock.listen(max_connections)

    def wait_for_client(self):
        """ Blocking call
        when a client connects to the socket
        returns a Vegas_Socket instance associated to the client socket
        """
        (client_socket, address) = self.sock.accept()
        return self.__class__(client_socket, address)

    def receive_str(self, max_len = 2048):
        """ Receive a string and strips out padding bytes
        """ 
        data = self.sock.recv(max_len)
        if data == b'':
            raise RuntimeError("socket connection broken")
        data_str = data.decode().strip()
        return data_str

    def receive_data(self, msg_len = 32):
        """ Receive any kind of binary data until it fullfills msg_len bytes off data
        Returns binary data received
        """
        chunks = []
        bytes_received = 0
        while bytes_received < msg_len:
            print("Waiting for a connection")
            chunk = self.sock.recv(min(msg_len - bytes_received, 2048))
            if chunk == b'': 
                raise RuntimeError("socket connection broken")
            chunks.append(chunk)
            bytes_received = bytes_received + len(chunk)
        return b''.join(chunks)

    def send_data(self, msg):
        """ Sends binary data
        """
        sent = self.sock.send(msg)
#         totalsent = 0
#         while totalsent < MSGLEN:
#             sent = self.sock.send(msg[totalsent:])
#             if sent == 0:
#                 raise RuntimeError("socket connection broken")
#             totalsent = totalsent + sent

    def close(self):
        self.sock.close()

class Vegas_Socket(Generic_Socket):
    """ Extension of Generic_Socket for its use within 
    Vegas
    """

    def double_to_bytes(self, double):
        """ takes a double and returns 
        its byte representation (len = 8 bytes)
        """
        return struct.pack('d', *[double])

    def bytes_to_double(self, bytedata):
        """ takes the byte representation of a double
        and returns the double
        """
        return struct.unpack('d', bytedata)[0]

    def double_array_to_bytes(self, double_array):
        """ takes a list of doubles and returns
        its byte representation so it can be sent
        via socket
        """
        arr_len = len(double_array)
        s = struct.pack('d'*arr_len, double_array)
        return s

    def read_partial_integral(self):
        """ Read the total integral from one of the jobs
        and returns a double
        """
        data = self.receive_data(8)
        double = self.bytes_to_double(data)
        return double

    def send_total_integral(self, total):
        """ Sends the total sum to a job
        """
        data = self.double_to_bytes(total)
        self.send_data(data)

    def harmonize_integral(self, n_jobs, verbose = None):
        """ Get the partial vegas integrals from the different n_jobs
        and send the sum back to each and every job.
        Only exits with success once all jobs receive their data
        """
        job_socket = self.wait_for_client()
        if verbose:
            adr = str(job_socket.address[0])
            prt = str(job_socket.address[1])
            print("Job_socket connected: %s:%s" % (adr, prt))
        partial_value = job_socket.read_partial_integral()
        if verbose:
            print("Partial value obtained: " + str(partial_value))
        integral_value = float(n_jobs)*partial_value
        print("Total value of the integral received: " + str(integral_value))
        print("Sending it back to all clients")
        job_socket.send_total_integral(integral_value)
        return 0
        
        


def example_server(port):
    s = Vegas_Socket()
    s.bind("", port)
    return s

def example_client(port):
    s = Vegas_Socket()
    s.connect("localhost", port)
    return s

if __name__ == "__main__":
    port = 8888
    socket_server = example_server(port)
    print("Now go to a different terminal and do: ~$ telnet localhost " + str(port))
    client_conn = socket_server.wait_for_client()
    first_message = "Type anything and hit enter\n"
    client_conn.send_data(first_message.encode())
    while True:
        client_conn.send_data(b"To end this connection, write 'close'\n")
        data_str = client_conn.receive_str()
        print("Received message: " + data_str)
        if data_str == "close":
            client_conn.send_data(b"Bye bye!\n")
            client_conn.close()
            break
        response = "You said: " + data_str + "... Ok!\n"
        client_conn.send_data(response.encode())

    

