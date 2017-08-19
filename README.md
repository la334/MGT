This file contains documentation and instructions for the project Multilingual
Grammatical Translator (MGT).

PROJECT AND FILE NAMES
----------------------
This project is named the MGT and contains the following files:

Makefile - allows for the following targets: make, make run, make clean

_tags - a file containing tags for ocamlbuild to ensure compilation

MGT_writeup.pdf - the technical writeup describing the MGT

mgt.ml - the access pooint of this project from outside of the various modules

uI.ml, uI.mli - compile into uI.cmo, handles the user-interface

translator.ml, translator.mli - compile into translator.cmo, handles translating
based on input to the MGT

parser.ml, parser.mli - compile into parser.cmo, handles parsing of translation
input to the MGT

database.json - the file which contains the language database of the MGT

databaseCreator.ml, databaseCreator.mli - compile into databaseCreator.cmo,
create a code version of the database from database.json

rules.ml - the file containing the hard-coded database, used only to update the
file database.json

morphology.ml, morphology.mli - compile into morphology.cmo, handles the
morphological aspect of the grammars available to the MGT

database.ml, database.mli - compile into database.cmo, handles the database of
the MGT

grammar.ml, grammar.mli - compile into grammar.cmo, handles the grammar of the
MGT


INSTALL INSTRUCTIONS
--------------------
This project requires that the following packages and libraries be installed:

PA_Comprehension

ANSITerminal

CONFIGURE INSTRUCTIONS
----------------------
This project requires the camlp4o syntax and OCamlMake.

RUN INSTRUCTIONS
----------------
To run this project, put all of the above files into a directory. Then, type
either of "make" or "make run" into the command line. You should see text
displaying instructions on how to use the project, which is a multilingual
grammatical translator. Once you have read through the instructions, follow
the last visible instruction in order to begin the translation process. Once
you have entered valid input for that instruction, another instruction will
appear. Follow that instruction and repeat this process until an instruction
asks whether you want to run the process again. The program exits if you decide
not to and restarts if you do. At each instruction prompt, you may type any of
the keywords described in the first set of instructions in order to change
program flow accordingly. Once finished using the MGT, please type "make clean"
into the command line.

AUTHORS
-------
Lavanya Aprameya (la334@cornell.edu) - primary author

John Hale (jthale@cornell.edu) - Computational Linguistics professor for primary
author in Spring 2017, provided framework for parsing and provided some helper
functions for parsing (i.e., in parser.ml and parser.mli).