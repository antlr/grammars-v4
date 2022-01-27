#

full_path_script=$(realpath $0)
full_path_script=$(dirname $full_path_script)
for f in `ls $full_path_script/skip-*.txt`
do
	echo $f
	dirs="`cat $f`"
	for p in $dirs
	do
		if [[ -f $p ]]
		then
			echo In $f, $p is a file. Remove.
			continue
		fi
		if [[ ! -d $p ]]
		then
			echo In $f, $p does not exist. Remove.
			continue
		fi
	done
done
