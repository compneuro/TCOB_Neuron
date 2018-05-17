/* Adex Model in C
author: Manjusha Nair @Amrita Vishwa Vidyapeetham

*/
#include "Adex.h"

int main()
{
 double I;
 struct timeval t_start,t_end;    //to calculate the execution time
 AdexPtr ad = (AdexPtr) malloc(sizeof(Adex)); 
 ExPtr ex = (ExPtr) malloc(sizeof(Experiment));
 setExperiment(ex);
 printf("Enter the current to be injected into the Neuron(pA): ");  
 scanf("%lf",&I);   
 gettimeofday(&t_start,0);
 setAdex(ad,I);
 Stimulate(ad,ex,I);
 printf("Total spikes: %d\n",ad->spikes);
 gettimeofday(&t_end,0);
 StoreData(ad->VmArray,ex->TimeArray,ad->wArray,ex->size); //keep the arrays in CSV files
 float time_d = (t_end.tv_sec-t_start.tv_sec) *1000000 + t_end.tv_usec - t_start.tv_usec;
  printf(" Time elapsed (CPU):%f ms\n",time_d* 1E-3);
}
/* Compilation instructions
gcc  Adexmain.c -lm
 To execute it,
 ./a.out
*/
 
