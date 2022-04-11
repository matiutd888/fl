for f in $(ls good/*.fl)
do
    echo $f
    if ! cat $f | ../build/./TestGramatyka ; then
        echo "Error in file" $f
        exit 1
    fi
done

echo "All examples ok!"