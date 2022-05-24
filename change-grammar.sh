rm -rf build/
bnfc-linux -m --functor -o build gramatyka.cf
cd build
make
cd ..
cp build/*.hs src/grammar/
rm -rf src/TestGramatyka.hs
./test-przyklady.sh
