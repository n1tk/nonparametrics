proc import datafile="C:\Users\bbarney2\Desktop\birthwt.csv" out=birthwt dbms=csv replace;
run;

/* We'll look at some examples designed to detect differences in the distribution of birth weight (bwt) based on 
  the mother's smoking status  (smoke: 1=smoker, 0=nonsmoker)  */

ods html style=theme;

/* Wilcoxon Rank Sum Test */
proc npar1way data=birthwt wilcoxon;
  class smoke;
  var bwt;
run;

/* Wilcoxon Rank Sum Test: Don't run this one! */
proc npar1way data=birthwt wilcoxon;
  class smoke;
  exact ;   *Far too computationally intensive!;
  var bwt;
run;




/* Wilcoxon Rank Sum Test: Monte Carlo p-value */
proc npar1way data=birthwt wilcoxon;
  class smoke;
  exact /seed=189791 n=10000 ;
  var bwt;
run;

/* Van der Waerden scores */
proc npar1way data=birthwt vw;
  class smoke;
  exact /seed=189791 n=10000 ;
  var bwt;
run;

/* Savage Scores */
proc npar1way data=birthwt savage;
  class smoke;
  exact /seed=189791 n=10000 ;
  var bwt;
run;



/* Siegel-Tukey Test */
proc npar1way data=birthwt st;
  class smoke;
  exact /seed=189791 n=10000 ;
  var bwt;
run;

/* Ansari-Bradley Test */
proc npar1way data=birthwt ab;
  class smoke;
  exact /seed=189791 n=10000 ;
  var bwt;
run;
