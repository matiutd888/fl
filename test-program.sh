cd build/
if [ ! -f ../przyklady/$1 ]; then
    echo "File przyklady/$1 does not exist!"
    exit 1
fi
./TestGramatyka <../przyklady/$1 >../src/TestProgram.hs
if [ ! $? -eq 0 ]; then
    echo "Syntax error with the file"
    cat ../src/TestProgram.hs
    exit 1
fi
cd ../src/
ghc -o main Main.hs
rm -rf *.o *.hi
./main
rm -rf main

