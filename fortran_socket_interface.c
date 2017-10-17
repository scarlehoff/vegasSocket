#include <stdio.h>
#include <stdlib.h>

#include <netdb.h>
#include <netinet/in.h>

#include <string.h>
#include <unistd.h>

void socket_exchange_(unsigned char *data, int *len, char *host, int *port, int *ifail) {
    /*
     * Sends len bytes of data to the socket at host:port and
     * waits for a response of the same length
     */
    *ifail = 1;

    int sockfd = socket(AF_INET, SOCK_STREAM, 0);
    if (sockfd < 0) {
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

    if (connect(sockfd, (struct sockaddr *) &serv_addr, sizeof(serv_addr)) < 0 ) {
        perror("Error connecting");
        exit(1);
    }

    int n = write(sockfd, data, *len);

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

    int sockfd = socket(AF_INET, SOCK_STREAM, 0);
    if (sockfd < 0) {
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

    if (connect(sockfd, (struct sockaddr *) &serv_addr, sizeof(serv_addr)) < 0 ) {
        perror("Error connecting");
        exit(1);
    }

    int n = write(sockfd, data, *len);

    *ifail = 0;

}

void socket_read_(unsigned char *data, int *len, char *host, int *port, int *ifail) {
    /*
     * Reads len bytes of data from socket host:port
     */

    *ifail = 1;
    int sockfd = socket(AF_INET, SOCK_STREAM, 0);
    if (sockfd < 0) {
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

    if (connect(sockfd, (struct sockaddr *) &serv_addr, sizeof(serv_addr)) < 0 ) {
        perror("Error connecting");
        exit(1);
    }

    int n = read(sockfd, data, *len);

    if (n < 0) {
        perror("Error reading from socket");
        exit(1);
    }

    *ifail = 0;

}
