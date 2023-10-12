#include <stdio.h>

int main(){
 double v;
 unsigned int o;
 while(1){
  fscanf(stdin,"%lf",&v);
  o = (unsigned int)(v * 4294967296.0);
  fwrite(&o, 4, 1, stdout);
 }
}
