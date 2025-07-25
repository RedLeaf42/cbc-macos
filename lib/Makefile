CBC = ../bin/cbc
CBFLAGS = -O -fPIC
TARGET = libcbc.a

# 检测架构
ARCH := $(shell uname -m)
ifeq ($(ARCH),arm64)
	OBJS = stdarg.o alloca_arm64.o
else
	OBJS = stdarg.o alloca.o
endif

CC = gcc
CFLAGS = -Wall
AR_CREATE = ar crs

.SUFFIXES:
.SUFFIXES: .cb .s .o

.cb.o:
	$(CBC) $(CBFLAGS) -c $< -o $@
.s.o:
	$(CBC) -c $<

$(TARGET): $(OBJS)
	$(AR_CREATE) $(TARGET) $(OBJS)

stdarg.o: stdarg.cb

alloca_arm64.o: alloca_arm64.s
	$(CC) $(CFLAGS) -c $< -o $@

sizeof_jmpbuf: sizeof_jmpbuf.c
	$(CC) $(CFLAGS) -o $@ $<

clean:
	rm -f $(TARGET) *.o stdarg.s alloca_arm64.s
