.PHONY: clean run all

CC=gcc
SRCDIR=src
OUTDIR=out
APP=stars-c

SRCS=$(wildcard $(SRCDIR)/*.c)
OBJS=$(patsubst $(SRCDIR)/%.c, $(OUTDIR)/%.o, $(SRCS))
DEPS=$(patsubst $(SRCDIR)/%.c, $(OUTDIR)/%.d, $(SRCS))

CFLAGS:=-std=c89 -MMD -O3 $(shell pkg-config --cflags sdl2)
LDFLAGS:=$(shell pkg-config --libs sdl2) -lm

all: $(OUTDIR)/$(APP)

clean:
	rm -rf $(OUTDIR)

run: $(OUTDIR)/$(APP)
	./$<

-include $(DEPS)

$(OUTDIR)/$(APP): $(OUTDIR) $(OBJS)
	$(CC) -o $@ $(OBJS) $(CFLAGS) $(LDFLAGS)

$(OUTDIR):
	mkdir -p $(OUTDIR)

$(OUTDIR)/%.o: $(SRCDIR)/%.c | $(OUTDIR)
	$(CC) -o $@ -c $< $(CFLAGS)
