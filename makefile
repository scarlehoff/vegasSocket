FC = gcc

examples: c_example


c_example: c_socket_example.c
	@echo "Building example of C socket client"
	@$(FC) $(FFLAGS) -o $@ $^

clean:
	@echo "cleaning ..."
	@rm c_example
