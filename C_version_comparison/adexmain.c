/* Adex Model in pure C
author: Manjusha Nair

*/
#include "AdexInCPU.h"

int main()
{
 double I;
struct timeval t_start,t_end;  
 AdexPtr ad = (AdexPtr) malloc(sizeof(Adex)); 
 ExPtr ex = (ExPtr) malloc(sizeof(Experiment));
 setExperiment(ex);
 printf("Enter the current to be injected into the Neuron(pA): ");  
 scanf("%lf",&I);  //10 PA for Granule, 100 PA for Golgi
 
 
clock_t start = clock();
gettimeofday(&t_start,0);
setAdex(ad,I);
//setAdexAsGranule(ad,I);
//setAdexAsPurkinje(ad,I);
//setAdexAsGolgi(ad,I);
 Stimulate(ad,ex,I);
 printf("Total spikes: %d\n",ad->spikes);
gettimeofday(&t_end,0);
StoreData(ad->VmArray,ex->TimeArray,ad->wArray,ex->size); //keep the arrays in CSV files
printf("Time elapsed (using clock function): %f\n", ((double)clock() - start) / CLOCKS_PER_SEC);
 float time_d = (t_end.tv_sec-t_start.tv_sec) *1000000 + t_end.tv_usec - t_start.tv_usec;
  printf(" Time elapsed (CPU):%f ms\n",time_d* 1E-3);
}
/* Compilation instructions
gcc  adexmain.c -lm
 To execute it,
#./a.out
*/
 
