# 2011, Kotelnikov Evgeny <evgeny.kotelnikov@gmail.com>

.PHONY: all install run version clean

NAME := syngen
VERSION := $(shell cat VERSION)

SRC := ./src/
BIN := ./bin/
TARGETS := $(SRC)targets/
INSTALL_DIR := /usr/bin

SYNGEN_MAIN := $(SRC)main.scm
SYNGEN_MODULES := adt cfg common parser transform
SYNGEN_O := $(addsuffix .o, $(addprefix $(SRC), $(SYNGEN_MODULES)))

SYNGEN_TARGETS := scheme
SYNGEN_TARGETS_DIRS := $(addsuffix /, $(addprefix $(TARGETS), $(SYNGEN_TARGETS)))
SYNGEN_O += $(addsuffix .o, $(join $(SYNGEN_TARGETS_DIRS), $(SYNGEN_TARGETS)))
SYNGEN_O += $(addsuffix -generator.o, $(join $(SYNGEN_TARGETS_DIRS), $(SYNGEN_TARGETS)))

all: $(BIN)$(NAME)

$(BIN)$(NAME): $(SYNGEN_O) $(SYNGEN_MAIN)
	mkdir -p $(BIN)
	bigloo -afile $(SRC).afile $(SYNGEN_MAIN) $(SYNGEN_O) -o $(BIN)$(NAME)

%.o: %.scm
	bigloo -afile $(@D)/.afile -c $<

install:
	install $(BIN)$(NAME) $(INSTALL_DIR)

version:
	@echo $(VERSION)

clean:
	rm -rf $(BIN) $(SRC)*.o $(TARGETS)*/*.o