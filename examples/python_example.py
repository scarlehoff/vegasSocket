#!/usr/bin/env python

"""Example python server for the Vegas warmup cheat
Receives a double and sends 2*double
"""

import socket

import sys
sys.path.append('..')
sys.path.append('.')
from src import vegas_socket


HOST = ""
PORT = 8888
n_clients = 2

# Open the server, bind it to HOST:PORT and wait for a connection
# (for instance c_example)

server = vegas_socket.Vegas_Socket()
server.bind(HOST, PORT)

if HOST == "":
    host_str = "localhost"
else:
    host_str = HOST

print("Server up. Connect to " + host_str + ":" + str(PORT))
print("Waiting for " + str(n_clients) + " clients")

while True:
    success = server.harmonize_integral(n_clients, verbose = True)

if success < 0:
    print("Something went wrong")

server.close()

