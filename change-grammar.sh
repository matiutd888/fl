bnfc-linux --functor -o build gramatyka.cf
cd build
make
cd ..
./copy-essential-files.sh
./test-przyklady.sh
