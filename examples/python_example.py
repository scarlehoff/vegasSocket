#!/usr/bin/env python3

"""Example python server for the Vegas warmup cheat
Receives a double and sends 2*double
"""

import socket
import sys
from argparse import ArgumentParser

sys.path.append('..')
sys.path.append('.')
from src import vegas_socket


parser = ArgumentParser()

parser.add_argument("-H", "--hostname", help = "Hostname", default = "")
parser.add_argument("-p", "--port", help = "Port", default = "8888")
parser.add_argument("-n", "--nclients", help = "Number of clientes to wait for", default = "2")
parser.add_argument("-v", "--verbose", help = "Print debug message", action = "store_true")
parser.add_argument("-c", "--control", help = "Take control through pdb before returning", action = "store_true")

args = parser.parse_args()

HOST = args.hostname
PORT = int(args.port) 
n_clients = int(args.nclients)

# Open the server, bind it to HOST:PORT and wait for a connection
# (for instance c_example)

server = vegas_socket.Vegas_Socket()
server.bind(HOST, PORT)

if HOST == "":
    host_str = socket.gethostname()
else:
    host_str = HOST

print("Server up. Connect to " + host_str + ":" + str(PORT))
print("Waiting for " + str(n_clients) + " clients")

counter = 0
try:
    while True:
        counter += 1
        success = server.harmonize_integral(n_clients, verbose = args.verbose, take_control = args.control)
        print("Iteration {} completed".format(counter))

    if success < 0:
        print("Something went wrong")
finally:
    server.close()

