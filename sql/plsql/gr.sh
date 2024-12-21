
# Compute graphs.
# Grouped
rm -f times.txt

targets=( Antlr4ng CSharp Cpp Dart Go Java JavaScript TypeScript Python3 )
c=${#targets[@]}
sq=`seq -s " " 1 $c | tr -d '\n'`
n=`grep SampleSize= Generated-${targets[0]}-0/parse.txt | sed 's/^[^=]*=//'`
name=`head -2 Generated-${targets[0]}-0/parse.txt | tail -1 | tr -d '\n' | tr -d '\r'`

# collect grouped data in one file. Order important.
for t in ${targets[@]}
do
    echo $t
    grep "group Total Time" Generated-$t-0/parse.txt >> times.txt
done

rm -f zzz.m
cat > zzz.m << EOF
pkg load statistics
n=$n;
c=$c;
x=[ $sq ];
disp(x)
p=[ `cat times.txt | awk '{print $4}'` ];
s=[ `echo ${targets[@]} | sed 's/\([a-zA-Z0-9/._-]\+\)/"\1"/g' | awk 'NR==1 {printf "%s", $0; next} {printf "; %s", $0} END {print ""}' ` ];
h={ `echo ${targets[@]} | sed 's/\([a-zA-Z0-9/._-]\+\)/"\1"/g' | awk 'NR==1 {printf "%s", $0; next} {printf "; %s", $0} END {print ""}' ` };
rp=reshape(p, n, c);
mq=mean(rp);
y=mq;
disp(x)
sq=std(rp);
colours = { '#88085c', '#e8600a', '#5468ff', '#ee243c', '#FCD34D', '#7ee787', '#F17FA2', '#5BC1D7', '#56C271' };
disp(n)
disp(c)
disp(mq)
disp(sq)
hold on
for i = 1 : length(mq)
   H(i) = bar( x(i), y(i), 0.4, 'facecolor', colours{i} );
endfor
xlim([0 10])
hold off;
hold on
er = errorbar(x, mq, sq);
set(er, "color", [0 0 0])
set(er, "linewidth", 1);
set(er, "linestyle", "none");
hold off
set(gca, 'XTickLabel', x, 'XTick', 1:numel(x));
set(gca, "fontsize", 6)
xlabel("Target");
ylabel("Runtime (s)");
title("Comparison of Runtimes for $name")
l=legend(h, "location", "northwest");
print("./times.svg", "-dsvg")
EOF

cat zzz.m | octave
