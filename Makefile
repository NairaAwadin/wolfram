##
## EPITECH PROJECT, 2024
## B-FUN-400-PAR-4-1-wolfram-naira.awadin
## File description:
## Makefile
##

NAME =	wolfram
BIN_PATH	:=	$(shell stack path --local-install-root)

all:
	stack build
	cp $(BIN_PATH)/bin/$(NAME)-exe ./$(NAME)

clean:
	stack clean

fclean:	clean
	rm -f $(NAME)

re:	fclean all

.PHONY: re clean fclean all
