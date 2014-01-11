#!/bin/sh

ocamlc -c structure.ml

ocamllex lexer.mll
ocamlyacc parser.mly    
ocamlc -c parser.mli
ocamlc -c lexer.ml
ocamlc -c parser.ml

ocamlc -c printing.ml 

ocamlc -c interfaceparsing.ml
ocamlc -o kalamiparsing lexer.cmo parser.cmo structure.cmo printing.cmo interfaceparsing.cmo