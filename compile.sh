#!/bin/sh

ocamlc -c structure.ml

ocamllex lexer.mll
ocamlyacc parser.mly    
ocamlc -c parser.mli
ocamlc -c lexer.ml
ocamlc -c parser.ml

ocamlc -c printing.ml
ocamlc -c evaluation.ml

ocamlc -c interfacewoa.ml
ocamlc -o kalamievalwoa lexer.cmo parser.cmo structure.cmo printing.cmo evaluation.cmo interfacewoa.cmo


#ocamlc -c analysisInline.ml
#ocamlc -c analysisStaticEval.ml
#ocamlc -c analysisGuessElimination.ml

#ocamlc -c interfacewa.ml
#ocamlc -o kalami lexer.cmo parser.cmo structure.cmo printing.cmo analysisCore.cmo analysisInline.cmo analysisStaticEval.cmo analysisGuessElimination.cmo evaluation.cmo interface.cmo
