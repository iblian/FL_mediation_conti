The R code FL_mediation_conti calculate the federated statistics to test the mediation effect of M among the X--> M --> Y relation, using the summary statistics from multi-data sources (k sites). Here the X, M, Y are continuous. 
Input:
The input data should contain the (correlation (X,M), correlation (X,Y), correlation (M,Y), mean_X, mean_M, mean_Y, var_X, var_M, var_Y, sample_size_n) from each source. The example data shows the format of data to be input.
Output: 
The test of mediation comprises of the following estimates
a: effect of X on M (regressing M on X)
b: effect of M on Y given X (regressing Y on M+X)
c: effect of X on Y given X (regressing Y on X)
and corresponding standard error se(a), se(b) 
the indirect effect is ab, with Sobel’s standard error, and the direct effect is c-ab

The package output 
1. the point estimate of indirect effect ab and its standard error (Sobel's)
2. the direct effect is c-ab, and propotion of mediation effect (ab/c).


