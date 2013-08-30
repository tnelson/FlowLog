
The code for the interpreter goes here.

Test cases (Flowlog source) are in the <examples> subfolder.

You'll need to install: packet, openflow, and frenetic packages, along with
their dependencies. The best way to do this is via their repos, rather than
using opam. 


make clean
make reinstall

If that doesn't work, try:

make clean
make
sudo make install

