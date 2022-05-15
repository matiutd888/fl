if [ ! -f $1 ]; then
    echo "File $1 does not exist!"
    exit 1
fi
build/./TestGramatyka <$1 >src/TestProgram.hs
if [ ! $? -eq 0 ]; then
    echo "Syntax error with the file"
    cat ../src/TestProgram.hs
    exit 1
fi
cd src/
ghc -o main Main.hs
rm -rf *.o *.hi
./main
rm -rf main

