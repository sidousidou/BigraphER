-include Makefile.config

configure: configure.ac m4/*.m4
	aclocal -I m4
	autoconf
