#include <TMB.hpp>     
#define _USE_MATH_DEFINES
#include<cmath>

template<class Type>
Type objective_function<Type>::operator() ()
{
  DATA_VECTOR(x);    
  //DATA_VECTOR(co2);
  DATA_MATRIX(M);
  DATA_SCALAR(init); 
  //DATA_VECTOR(logadjust);
//  PARAMETER(kappa);
  PARAMETER(omega);
  PARAMETER_VECTOR(alpha);
  //PARAMETER(beta);
  PARAMETER_VECTOR(beta);
  PARAMETER_VECTOR(theta);
  //PARAMETER(gamma);
  //PARAMETER(tau);
  //PARAMETER(a);

  Type mean = 0.0;
  Type sd = 1.0;
  Type a = 1.0;
  for (int j =0; j < alpha.size();j++){
          a -= alpha(j);
  }
  for (int j =0; j < beta.size();j++){
    a -= beta(j);
  }
  omega = omega*a;
  for (int j =0; j < theta.size();j++){
    theta(j) =a*theta(j);
  }
  
  
  int n = x.size();
  vector<Type> sigma(n);
  sigma(0)=sqrt(init*init);
  if(sigma(0) == 0){
    for (int j =0; j < M.row(0).size();j++){
      sigma(0) += M(0,j) * theta(j);
    }
  }
  
  Type f;
  f=0;
  //making sure of positivity
  for (int i = 0; i < n-1; i++) {
    sigma(i+1) = omega; 
    for (int j =0; j < alpha.size();j++){
      if(i-j >= 0){
        sigma(i+1) += alpha(j) * x(i-j)*x(i-j);
      }
    }
    for (int j =0; j < beta.size();j++){
      if(i-j >= 0){
        sigma(i+1) += beta(j) * sigma(i-j)*sigma(i-j);
      }
    }
    for (int j =0; j < M.row(0).size();j++){
      sigma(i+1) += M(i+1,j) * theta(j);
    }
    if(sigma(i+1)<0) return 999999;
    sigma(i+1) = sqrt(sigma(i+1));
    f -= dnorm(x(i+1),mean,sigma(i+1),true);
    }
  REPORT(sigma);
  
  SIMULATE{
    sigma(0)=sqrt(init*init);
    //Type z = rnorm(n,mean,sd)
    for(int i = 0;i<n-1;i++){
      sigma(i+1) = omega*(1-sum(alpha)-sum(beta)); 
      for (int j =0; j < alpha.size();j++){
        if(i-j >= 0){
          sigma(i+1) += alpha(j) * x(i-j)*x(i-j);
        }
      }
      for (int j =0; j < beta.size();j++){
        if(i-j >= 0){
          sigma(i+1) += beta(j) * sigma(i-j)*sigma(i-j);
        }
      }
      for (int j =0; j < M.row(0).size();j++){
        sigma(i+1) += M(i,j) * theta(j)*(1-sum(alpha)-sum(beta));
      }
      sigma(i+1) = sqrt(sigma(i+1));
      if(true) x(i+1)=sigma(i+1) * rnorm(mean, sd);
      else x(i+1)=sigma(i+1)* rt(3.0)/sqrt(3.0/1.0);
    }
    REPORT(sigma);
    REPORT(x);
  }
  
                                         // Declare the "objective function" (neg. log. likelihood)
  return f;
  }

