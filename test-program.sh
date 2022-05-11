cd build/
./TestGramatyka <../przyklady/$1 >../src/TestProgram.hs
cd ../src/
ghc -o main Main.hs
rm -rf *.o *.hi
./main
rm -rf main

