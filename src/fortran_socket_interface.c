#include <stdio.h>
#include <stdlib.h>

#include <netdb.h>
#include <netinet/in.h>

#include <string.h>
#include <unistd.h>

void socket_creation_(int*, char*, int*, int*);
void socket_exchange_(unsigned char*, int*, char*, int*, int*);
void socket_send_(unsigned char*, int*, char*, int*, int*);
void socket_read_(unsigned char*, int*, char*, int*, int*);
void socket_send_with_id_(int*, unsigned char*, int*);
void socket_read_with_id_(int*, unsigned char*, int*);




void socket_creation_(int *socket_id, char *host, int *port, int *ifail) {
    *ifail = 1;

    printf("Connecting to: '%s'\n", host);

    *socket_id = socket(AF_INET, SOCK_STREAM, 0);

    if (socket_id < 0) {
        perror("Error opening socket");
        exit(1);
    }

    struct hostent *server;
    server = gethostbyname(host);
    if (server == NULL) {
        fprintf(stderr, "Error: no such host\n");
        exit(0);
    }

    struct sockaddr_in serv_addr;
    bzero((char *) &serv_addr, sizeof(serv_addr));
    serv_addr.sin_family = AF_INET;
    bcopy((char *) server->h_addr, (char *) &serv_addr.sin_addr.s_addr, server->h_length);
    serv_addr.sin_port = htons(*port);

    if (connect(*socket_id, (struct sockaddr *) &serv_addr, sizeof(serv_addr)) < 0 ) {
        perror("Error connecting");
        exit(1);
    }

}


/*
 * These functions receive as an input host and port and create the socket before
 * doing anything else
 *
 */
void socket_exchange_(unsigned char *data, int *len, char *host, int *port, int *ifail) {
    /*
     * Sends len bytes of data to the socket at host:port and
     * waits for a response of the same length
     */
    *ifail = 1;

    int sockfd;
    socket_creation_(&sockfd, host, port, ifail);

    int n;

    /*
     * Tell the server the length of the data it has to expect
     */
    union {
       int size;
       unsigned char bytes[4];
    } array_info;
    array_info.size = *len;
    n = write(sockfd, array_info.bytes, 4);

    /*
     * Send the information to the server
     */
    n = write(sockfd, data, *len);

    /*
     * Wait for the new batch of information
     */
    n = read(sockfd, data, *len);

    if (n < 0) {
        perror("Error reading from socket");
        exit(1);
    }

    *ifail = 0;

}

void socket_send_(unsigned char *data, int *len, char *host, int *port, int *ifail) {
    /*
     * Sends len bytes of data to the socket found at host:port
     */

    *ifail = 1;
    int sockfd;
    socket_creation_(&sockfd, host, port, ifail);

    int n = write(sockfd, data, *len);

    *ifail = 0;
}

void socket_read_(unsigned char *data, int *len, char *host, int *port, int *ifail) {
    /*
     * Reads len bytes of data from socket host:port
     */

    *ifail = 1;
    int sockfd;
  
    int n = read(sockfd, data, *len);
    if (n < 0) {
        perror("Error reading from socket");
        exit(1);
    }

    *ifail = 0;
}

/*
 * These functions receive a socket id and use it to send data
 * they are basically a wrapper for fortran for read and write
 *
 */

void socket_send_with_id_(int *socket_id, unsigned char *data, int *len) {
    /*
     * Sends len bytes of data to socket_id
     */
    int n = write(*socket_id, data, *len);
}

void socket_read_with_id_(int *socket_id, unsigned char *data, int *len) {
    /*
     * Reads len bytes of data from socket_id
     */
    int n = read(*socket_id, data, *len);
    if (n < 0) {
        perror("Error reading from socket");
    }
}
    


