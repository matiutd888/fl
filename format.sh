temp_file=$(mktemp tempfileXXXXXXX)
hindent <src/CheckType.hs >$temp_file
mv $temp_file src/CheckType.hs
hindent <src/Errors.hs >$temp_file
mv $temp_file src/Errors.hs
hindent <src/Interpreter.hs >$temp_file
mv $temp_file src/Interpreter.hs
hindent <src/RunInterpreter.hs >$temp_file
mv $temp_file src/RunInterpreter.hs
hindent <src/Utils.hs >$temp_file
mv $temp_file src/Utils.hs

rm -rf $temp_file
