temp_file=$(mktemp tempfileXXXXXXX)
hindent <src/TypeChecker.hs >$temp_file
mv $temp_file src/TypeChecker.hs
hindent <src/StatementChecker.hs >$temp_file
mv $temp_file src/StatementChecker.hs
hindent <src/Errors.hs >$temp_file
mv $temp_file src/Errors.hs

rm -rf $temp_file
