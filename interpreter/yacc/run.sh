# Create ML code for the lexer and parser, and put them in the directory below.
# If we tried to have everything (.mll, .mly, .ml) in the same directory, we'd
# get an error message when trying to compile via make.
echo "Creating lexer..."
ocamllex -o ../lexer.ml lexer.mll
echo "Creating parser..."
ocamlyacc -b../parser parser.mly
# Because ocamlyacc doesn't add the headers to .mli, 
# add manually. DO NOT add Flowlog_Parsing.
echo "Adding header to mli file..."
sed -i '1iopen Type_Helpers.Parsing;;' ../parser.mli
sed -i '1iopen Flowlog_Types.Syntax;;' ../parser.mli
