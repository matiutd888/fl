for f in $(ls good/*.fl)
do
    echo $f
    if ! ./interpreter $f >/dev/null; then
        echo "Error in file" $f
        exit 1
    fi
done

echo "All good examples interpreted correctly!"
echo ""

for f in $(ls bad/*.fl)
do
    echo $f
    if ./interpreter $f >/dev/null 2>/dev/null; then
        echo "Error in file" $f
        exit 1
    fi
done

echo "All bad examples interpreted correctly!"
