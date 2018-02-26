#include "NeuronAndSynapse.h"

int main()
{
 
struct timeval t_start,t_end;
float ISI,IBI;
int SPB;  

 AdexPtr ad = (AdexPtr) malloc(sizeof(Adex));
 
 ExPtr ex = (ExPtr) malloc(sizeof(Experiment));
 setExperiment(ex);
 printf("\n\t Input Parameters\n");
 printf("Enter the Spikes Per Burst, Inter Spike Interval, Inter Burst Interval:");
 scanf("%d %f %f",&SPB,&ISI,&IBI);
int no1,no2;
 printf("Enter the no of MF synapses: ");
 scanf("%d",&no1);
printf("Enter the no of Inh synapses: ");
 scanf("%d",&no2);
int length = SpikeGenerator(ex->dt,ex->startTime,ex->endTime,ISI,IBI,SPB);
StoreInput(HInputSpikeTime,length);
//setAdex(ad);
 setAdexAsGranule(ad);
 //setAdexAsGolgi(ad);
clock_t start = clock();
gettimeofday(&t_start,0);
Stimulate(ad,ex,HInputSpikeTime,no1,no2);
printf("No of Spikes %d\n",ad->spikes);
gettimeofday(&t_end,0);
printf("Time elapsed (using clock function): %f\n", ((double)clock() - start) / CLOCKS_PER_SEC);
 float time_d = (t_end.tv_sec-t_start.tv_sec) *1000000 + t_end.tv_usec - t_start.tv_usec;
  printf(" Time elapsed (CPU):%f ms\n",time_d* 1E-3);
}
/* Compilation instructions
#gcc adexsmain.c -lm
To execute it,
#./a.out
*/
