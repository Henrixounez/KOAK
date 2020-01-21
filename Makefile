##
## EPITECH PROJECT, 2019
## Eval expr
## File description:
## makefile of project
##

BINPATH = stack path --local-install-root

all:
		stack build;\
		cp koak-exe koak

tests_run:	all
		stack test;
		cp ./koak-exe ./test/koak && cd test && ./tester.sh

clean:
		$(RM) koak

fclean:	clean

re: fclean all

.PHONY: all clean fclean re
