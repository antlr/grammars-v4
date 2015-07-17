sampleproc(z) ; a sample routine
       write "This is a sample procedure",!
       new a,b,c
 dosets set a=10,b=20,c=30
       do subproc(b)
       if z set c=a+c+z
       quit c
 subproc(y) set a=(a+y)*2 quit
 