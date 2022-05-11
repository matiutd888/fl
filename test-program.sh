cd build/
./TestGramatyka <../przyklady/$1 >../src/TestProgram.hs
cd ../src/
ghc -o main main.hs
./main

