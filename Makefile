#########################################################################
#                                                                       #
#                            Objective Caml                             #
#                                                                       #
#            Xavier Leroy, projet Cristal, INRIA Rocquencourt           #
#                                                                       #
#   Copyright 1999 Institut National de Recherche en Informatique et    #
#   en Automatique.  All rights reserved.  This file is distributed     #
#   under the terms of the GNU Library General Public License, with     #
#   the special exception on linking described in file ../../LICENSE.   #
#                                                                       #
#########################################################################

# $Id: Makefile 9547 2010-01-22 12:48:24Z doligez $

# Makefile for the str library


LIBNAME=str
COBJS=strstubs.$(O)
CLIBNAME=camlstr
CAMLOBJS=str.cmo

include ../Makefile

depend:

str.cmo: str.cmi
str.cmx: str.cmi

depend:
	gcc -MM $(CFLAGS) *.c > .depend
	../../boot/ocamlrun ../../tools/ocamldep *.mli *.ml >> .depend

include .depend
