# Make the interpreter.
CC 		   = gcc
CCFLAGS    = -Wall -Werror -Wpedantic -Wextra -Wwrite-strings -Warray-bounds \
			 -Weffc++ --std=c++20 -O0 										 \
			 $(shell pkgconf sdl2 --cflags)
LDFLAGS    = -lstdc++ -lpthread 											 \
			 $(shell pkgconf sdl2 --libs) -lSDL2_ttf -lSDL2_image -lSDL2_mixer
DEBUG_INFO = no

ifeq ($(DEBUG_INFO), yes)
	CCFLAGS += -g
endif

BIN       = chip8
BIN_FLAGS = assets/test_opcode.ch8
SRCS      = main.cc
OBJS      = $(SRCS:.cc=.o)
DEPS 	  = $(SRCS:.cc=.d)

.PHONY: all clean test leaks

all: $(BIN)

$(BIN): $(OBJS)
	ctags -R
	$(CC) $(CCFLAGS) $(LDFLAGS) -o $@ $^

-include $(DEPS)

%.o: %.cc Makefile
	$(CC) $(CCFLAGS) -MMD -MP -c -o $@ $<

test: $(BIN)
	./$< $(BIN_FLAGS)

leaks: $(BIN)
	valgrind -s --leak-check=full --show-leak-kinds=all ./$<

clean:
	rm -f *.o *.d $(BIN) tags
