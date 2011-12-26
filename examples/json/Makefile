# 2011, Kotelnikov Evgeny <evgeny.kotelnikov@gmail.com>

.PHONY: all run

NAME := json

all: $(NAME).sg.scm

$(NAME).sg.scm: $(NAME).sg
	syngen -i $(NAME).sg -o $(NAME).sg.scm -m $(NAME)

run: $(NAME).sg.scm
	@bigloo -i $(NAME).scm

clean:
	rm -rf $(NAME).sg.scm