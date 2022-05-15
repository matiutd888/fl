temp_file=$(mktemp tempfileXXXXXXX)
hindent <src/CheckType.hs >$temp_file
mv $temp_file src/CheckType.hs
hindent <src/Errors.hs >$temp_file
mv $temp_file src/Errors.hs
hindent <src/Interpreter.hs >$temp_file
mv $temp_file src/Interpreter.hs

rm -rf $temp_file
