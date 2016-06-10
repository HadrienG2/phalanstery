# This is a simple profiler based on gcov data

cd ../obj
gcov *.gcda

cd ../tools
echo "File:Line#:Calls" > profile.csv
grep -no "^[[:space:]]*[0-9]\+" ../obj/*.gcov | grep -v -e "../obj/a-" -e "../obj/s-" -e "../obj/b__" >> profile.csv
