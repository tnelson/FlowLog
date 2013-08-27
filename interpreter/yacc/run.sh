# Create ML code for the lexer and parser, and put them in the directory below.
# If we tried to have everything (.mll, .mly, .ml) in the same directory, we'd
# get an error message when trying to compile via make.

ocamllex -o ../Surface_Lexer.ml Surface_Lexer.mll
ocamlyacc -b../Surface_Parser Surface_Parser.mly
sed -i '1iopen Flowlog_Types;;' ../Surface_Parser.mli

