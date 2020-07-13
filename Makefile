CP					=	cp
RM					=	rm -rf

EXEC				=	imageCompressor
EXEC_STACK			=	imageCompressor-exe
EXEC_PATH			=	./.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.0.1.0/build/imageCompressor-exe/$(EXEC_STACK)


all: $(EXEC)

$(EXEC):
	@stack build
	@$(CP) $(EXEC_PATH) ./
	@mv $(EXEC_STACK) $(EXEC) 

clean:
	@stack clean

fclean: clean
	@$(RM) $(EXEC)

re: fclean all

.PHONY:	all clean fclean re
