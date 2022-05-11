bnfc-linux -m --functor -o build gramatyka.cf
cp TestGramatyka.hs build/
cd build
make
cd ..
./copy-essential-files.sh
./test-przyklady.sh
