#!/usr/bin/env python 

import socket
import struct

class Generic_Socket:
    """Modified version of the demonstration class from:
    https://docs.python.org/3.6/howto/sockets.html
    """

    def __init__(self, sock=None, address=2*["UNK"]):
        self.double_size = 8
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

    def receive_data(self, msg_len = 32, verbose=None):
        """ Receive any kind of binary data until it fullfills msg_len bytes off data
        Returns binary data received
        """
        chunks = []
        bytes_received = 0
        while bytes_received < msg_len:
            if verbose:
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
        total_sent = 0
        while total_sent < len(msg):
            sent = self.sock.send(msg)
            total_sent += sent

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

    def read_partial_integral(self, size = 8, verbose = None):
        """ Read the total integral from one of the jobs
        and returns a double
        """
        data = self.receive_data(size, verbose = verbose)
        double_array = []
        for i in range(0, size-1, self.double_size):
            double_array.append(self.bytes_to_double(data[i:i+8]))
        return double_array

    def send_total_integral(self, total):
        """ Sends the total sum to a job
        """
        data = []
        for double in total:
            data.append(self.double_to_bytes(double))
        self.send_data(b''.join(data))

    def get_size(self):
        """ Gets the size of the data we are going to receive
        ie, gets an integer (size = 4 bytes)
        """
        data = self.receive_data(4)
        return int.from_bytes(data, byteorder="little")

    def harmonize_integral(self, n_jobs, verbose = None, take_control = False):
        """ Get the partial vegas integrals from the different n_jobs
        and send the sum back to each and every job.
        Only exits with success once all jobs receive their data
        """
        # Connect to the endpoint
        job_sockets = []
        array_partial = []
        while len(job_sockets) < n_jobs:
            new_endpoint = self.wait_for_client()

            if verbose:
                adr = str(new_endpoint.address[0])
                prt = str(new_endpoint.address[1])
                print("New endpoint connected: %s:%s" % (adr, prt))
        
            # Get the size of the array of doubles we are going to receive
            size = new_endpoint.get_size()
            doubles = int(size / 8)
            if verbose:
                print("Size of array: " + str(size))
                print("Meaning we will get " + str(doubles) + " doubles")

            # Get the actual array of data
            partial_value = new_endpoint.read_partial_integral(size, verbose = verbose)
            if verbose:
                print("Partial value obtained: " + str(partial_value))


            # Store the socket and the array we just received, we will use it in the future
            array_partial.append(partial_value)
            job_sockets.append(new_endpoint)

        integral_value = doubles*[0.0]
        for array_values in array_partial:
            if len(array_values) != doubles:
                raise Exception("Received arrays of different length!")
            integral_value = list(map(lambda x,y: x+y, integral_value, array_values))

        if verbose:
            print("Total value of the integral received: " + str(integral_value))
            print("Sending it back to all clients")

        if take_control:
            print("Warning, you just hijacked the connection, don't do anything evil!")
            print("Variable to be sent is integral_value, number of elements: {}".format(doubles))
            import pdb 
            pdb.set_trace()

        while job_sockets:
            job_socket = job_sockets.pop()
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

    

