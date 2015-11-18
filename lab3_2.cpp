#include<Rcpp.h> 

using namespace Rcpp;

double summationKahan(NumericVector x);
  
// [[Rcpp::export]]
/*Approximation of a log Function with Taylor Expansion*/
double logApproxFloat(double x, int k)
{
  x = 1.0 - x;
  
  float s;
  float term;
  
  s = 0;
  for (int i = 1; i <= k; i++){
    term = pow(x,i);
    s -= 1.0 / i * term;
  }
  return s;
}

// [[Rcpp::export]]
/*Double Version of log Approximation*/
double logApproxDouble(double x, int k)
{
  x = 1.0 - x;
  
  double s;
  double term;
  
  s = 0;
  for (int i = 1; i <= k; i++){
    term = pow(x,i);
    s -= 1.0 / i * term;
  }
  return s;
}

// [[Rcpp::export]]
/*Double Version of log Approximation with Vector Index*/
double logApproxDouble1(double x, int k){
  
  x = 1.0 - x;
  NumericVector i(k);
  i = seq_len(k);
  NumericVector term = -exp(i*log(x))/i;
  return sum(term);
}

// [[Rcpp::export]]
/*Double Version of log Approximation with Vector Index and Kahan Summation*/
double logApproxDouble2(double x, int k){
  
  x = 1.0 - x;
  NumericVector i(k);
  i = seq_len(k);
  NumericVector term = -exp(i*log(x))/i;
  return summationKahan(term);
}

// [[Rcpp::export]]
/*Kahan's summation*/
double summationKahan(NumericVector x){
  double sum = 0.0; 
  double c = 0.0;
  double y, t;
  int i;
  
  for(i = 0; i < x.length(); i++){
    y = x[i]-c;
    t = sum + y;
    c = (t - sum) - y;
    sum = t; 
  }
  return sum;
}