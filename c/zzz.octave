pkg load statistics
n=20;
c=5;
x=[ 1 2 3 4 5 ];
disp(x)
p=[ 4.369
4.209
4.125
4.002
4.009
4.063
4.05
4.017
4.083
3.996
4.119
4.062
4.046
4.033
4.073
4.05
4.031
4.039
4.031
4.063
3.1142547
3.1788963
3.2963924
3.2880938
3.4958498
3.1346876
3.9861028
3.1624491
3.5946762
3.2946872
3.1408217
3.3669313
3.9953616
3.4007475
3.7436492
3.0987159
3.1878814
3.104246
3.3762266
3.206116
2.529
2.655
2.576
2.586
2.566
2.545
2.559
2.527
2.563
2.53
2.558
2.535
2.527
2.528
2.578
2.563
2.551
2.52
2.516
2.524
2.869
2.934
2.825
2.774
2.832
2.766
2.814
2.689
2.762
2.853
2.89
2.77
2.907
2.808
2.747
2.732
2.844
2.706
2.752
2.889
4.939
5
4.974
4.9190000000000005
5.01
4.967
4.916
4.888
5.044
4.868
5.012
5.024
4.85
4.975
4.9559999999999995
4.912
5.017
4.87
4.963
4.99 ];
s=[ "Antlr4ng" "CSharp" "Dart" "Java" "TypeScript" ];
h={ "Antlr4ng" "CSharp" "Dart" "Java" "TypeScript" };
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
xlim([0 6])
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
title("Comparison of Runtimes for CParser")
l=legend(h, "location", "northeastoutside");
print("./times.svg", "-dsvg")
