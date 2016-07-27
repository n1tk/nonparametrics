data final4;
	input detergent stain count;
	Datalines;
	1 1 4
	1 2 2 
	1 3 2
	2 1 5
	2 2 1
	2 3 2
	3 1 6
	3 2 2
	3 3 0
	4 1 2
	4 2 1
	4 3 5
	;
	proc freq data = final4;
	weight count;
	Tables detergent*stain;
	Exact jt;
	run;