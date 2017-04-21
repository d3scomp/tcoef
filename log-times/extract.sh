rm out.csv
for f in *.log; do
  echo "$f" >> out.csv
  sed -n "s/^.*think took \([0-9]\)/\1/p" $f >> out.csv
done
