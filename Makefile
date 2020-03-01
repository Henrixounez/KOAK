##
## EPITECH PROJECT, 2019
## Eval expr
## File description:
## makefile of project
##

BINPATH = stack path --local-install-root

all:
		stack build

tests_run:	all
		stack test :unit-HUnit

clean:
		$(RM) koak

fclean:	clean

re: fclean all

.PHONY: all clean fclean re
