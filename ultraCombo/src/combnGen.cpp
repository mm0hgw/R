#include <Rcpp.h>
using namespace Rcpp;

// based on code from Ben Voight
// http://stackoverflow.com/users/103167/ben-voigt
// http://stackoverflow.com/questions/9330915/number-of-combinations-n-choose-r-in-c
double nCkd( int n, int k )
{
    if (k > n) return 0;
    if (k * 2 > n) k = n-k;
    if (k == 0) return 1;
    if (k == 1) return n;

    register double result = n;
    register int i;
    for( i = 2; i <= k; ++i ) {
        result *= (n-i+1);
        result /= i;
    }
    return result;
}

// [[Rcpp::export]]
IntegerVector combnGenElemRcpp(
	NumericVector xv,
	IntegerVector nv,
	IntegerVector kv,
	NumericVector chv
){
		// set up loop variables
	register double xr=xv[0], ch, oldch=chv[0];
	int n=nv[0], k=kv[0];
	register int i=n, j=k;
		// output vector
	IntegerVector out(k);
		// iterate until k-1 elements are chosen
	while(j>1){
			// iterate until the next element should be chosen
		while(xr > (ch = (oldch * j) / i)){
			xr -= ch;
			oldch -= ch;
			i--;
		}
			// choose an element
		out[k-j] = n-i+1;
		oldch = ch;
		i--;
		j--;
	}
		// choose last element
	out[k-1] = n-i+(int)xr;
	return(out);
}

// [[Rcpp::export]]
NumericVector revCombnGenElemRcpp(
	IntegerVector xv,
	IntegerVector nv,
	NumericVector chv
){
	int n=nv[0], k=xv.size(), pl=k-1;
    register int i=n, j=k, p, q, ch, oldch=chv[0], ql, xr=1;
	NumericVector out(1);
    
	for(p=0;p<pl;p++){
		ql=xv[p+1]-xv[p]-1;
        for(q=0;q<ql;q++){
            ch = (oldch * j) / i;
            xr += ch;
			oldch -= ch;
            i--;
        }
        ch = (oldch * j) / i;
        oldch = ch;
        i--;
        j--;
    }
    out[0] = xr;
 	return out;
}
