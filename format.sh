temp_file=$(mktemp tempfileXXXXXXX)
hindent <src/TypeChecker.hs >$temp_file
mv $temp_file src/TypeChecker.hs
rm -rf $temp_file
