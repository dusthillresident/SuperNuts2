#include <stdio.h>

double n1 = 0.0;
double n2 = 0.0;

#define lim 7772345.67

double fnr( double s ){
 double a = s*0.768567965987636759608412853505842308898385650814;
 double b = s*0.411534687656783463715361117436596435037355834417;
 double v = s*0.117897765443787942569497802054514193224765904051
              * (a-(int)a) * (b-(int)b);
 return v-(int)v;
}

double supernuts2n(){
 n1 += 0.991743537859395982857;
 if( n1 > lim ){
  n1 = lim * (n2-(int)n2);
 }
 n2 += 0.73262452;
 if( n2 > lim ){
  n2 = n1-(int)n1;
 }
 double v = fnr(n2)+fnr(n1);
 return v-(int)v;
}

int main(){
 unsigned int o;
 while(1){
  o = (unsigned int)(supernuts2n() * 4294967296.0);
  fwrite(&o, 4, 1, stdout);
 }
 return 0;
}