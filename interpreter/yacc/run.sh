# Create ML code for the lexer and parser, and put them in the directory below.
# If we tried to have everything (.mll, .mly, .ml) in the same directory, we'd
# get an error message when trying to compile via make.
echo "Creating lexer..."
ocamllex -o ../lexer.ml lexer.mll
echo "Creating parser..."
ocamlyacc -b../parser parser.mly
