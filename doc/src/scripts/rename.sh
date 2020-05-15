


for f in *.PS; do
    mv -- "$f" "${f%.PS}.ps"
done
