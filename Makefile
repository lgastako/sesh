all:
	@cat Makefile

build:
	stack build

env:
	stack exec env

exec:
	stack exec sesh-exe

setup:                       #  <== run this first
	stack setup

test:
	stack test

which-ghc:
	stack exec which ghc

b: build
e: exec
s: setup
t: test
w: which-ghc

.PHONY: all setup build exec test which-ghc
.PHONY: b e t w
