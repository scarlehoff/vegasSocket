/*
 * Example in C to send and receive doubles to (from) a python socket
 * most of it stems from https://www.tutorialspoint.com/unix_sockets/socket_client_example.htm
 */

#include <stdio.h>
#include <stdlib.h>

#include <netdb.h>
#include <netinet/in.h>

#include <string.h>
#include <unistd.h>

int main(int argc, char *argv[]) {
    if (argc < 3) {
        fprintf(stderr, "usage %s hostname port\n", argv[0]);
        exit(0);
    }

    int portno = atoi(argv[2]);

    /* Create a socket point */
    int sockfd = socket(AF_INET, SOCK_STREAM, 0);
    if (sockfd < 0) {
        perror("ERROR opening socket");
        exit(1);
    }

    struct hostent *server;
    server = gethostbyname(argv[1]);
    if (server == NULL) {
        fprintf(stderr, "ERROR, no such host\n");
        exit(0);
    }

    // Still not sure about the role of this part
    struct sockaddr_in serv_addr;
    bzero((char *) &serv_addr, sizeof(serv_addr));
    serv_addr.sin_family = AF_INET;
    bcopy((char *) server->h_addr, (char *) &serv_addr.sin_addr.s_addr, server->h_length);
    serv_addr.sin_port = htons(portno); // host byte to unsigned short integer


    /* Actual connection to the server */
    if (connect(sockfd, (struct sockaddr*) &serv_addr, sizeof(serv_addr)) < 0 ) {
        perror("ERROR connecting");
        exit(1);
    }

    printf("Please enter the double you want to send: ");
    char buffer[256];
    bzero(buffer, 256);
    fgets(buffer, 255, stdin);
    union {
        double d;
        unsigned char bytes[8];
    } d_input, d_output;

    d_input.d = atof(buffer);
    printf("You entered: %f\n", d_input.d);

    // Send message (buffer) to server sockfd of length strlen(buffer)
    int n = write(sockfd, d_input.bytes, strlen(d_input.bytes));

    /* Now read server response */
    n = read(sockfd, d_output.bytes, 8);
    if (n< 0) {
        perror("ERROR reading from socket");
        exit(1);
    }
    printf("And the server answered: %f\n", d_output.d);
    return 0;
        

}
