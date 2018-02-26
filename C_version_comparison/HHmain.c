/* HH Model in pure C
author: Manjusha Nair
*/
#include "HHInCPU.h"

int main()
{
 double I;
struct timeval t_start,t_end;  
 HHPtr hh = (HHPtr) malloc(sizeof(HH)); 
 ExPtr ex = (ExPtr) malloc(sizeof(Experiment));
 setExperiment(ex);
 printf("Enter the current to be injected into the Neuron(pA): ");  
 scanf("%lf",&I);  //10 PA for Granule, 100 PA for Golgi
 gettimeofday(&t_start,0);
 setHH(hh,I);
 Stimulate(hh,ex);
gettimeofday(&t_end,0);
StoreData(ex->TimeArray,hh->VmArray,hh->mArray,hh->nArray,hh->hArray,ex->size); //keep the arrays in CSV files
 float time_d = (t_end.tv_sec-t_start.tv_sec) *1000000 + t_end.tv_usec - t_start.tv_usec;
  printf(" Time elapsed (CPU):%f ms\n",time_d* 1E-3);
}
/* Compilation instructions
#gcc HHmain.c -lm 
 To execute it,
#./a.out
*/
 
