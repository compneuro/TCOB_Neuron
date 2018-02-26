/* Adex Model  with synapse input in pure C
author: Manjusha Nair

*/
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>
#include "sys/time.h"
float *HInputSpikeTime;
typedef struct
{ 
 
 double Cm;
 double Gl;        // The leak conductance
 double El;        // The leak reversal potential
 double delta;    //the slope factor.
 double Vt;        //Threshold voltage
 double a;         //Adaptation coupling parameter;
 double Tw;        //the adaptation time constant.
    
 double Vr;        //reset of the voltage
 double b;        //reset adaptation value
 double w;        //adaptation variable
 double Iin;        //Input current

 double Vm;
 int spikes;
 double Vreset;
 
 double* wArray ;//the parameter w changes with time
 double* VmArray;//The output voltage of the neuron

}Adex;

typedef struct 
{
 double dt;        //Integration time
 double startTime;
 double endTime;
 double duration;
 int size;
 
}Experiment;
typedef Adex* AdexPtr;
typedef Experiment* ExPtr;
typedef struct
{
   
       
    float gAMPA; //Total excitatory synaptic conductance.
    float gAMPAMax;//maximum excitatory synaptic conductance.
    float wAMPA; // excitatory connection wt;
    float IAMPA; //Synaptically mediated current
    float EAMPA; //Reverse potential
             
}AMPASynapse;
typedef struct
{
  
          float gNMDA; //Total excitatory synaptic conductance.
	    float gNMDAMax;//maximum excitatory synaptic conductance.
	    float wNMDA; // excitatory connection wt;
	    float INMDA; //Synaptically mediated current
	    float ENMDA; //Reverse potential
	    float mgC; //magnesium concentration
}NMDASynapse;
typedef struct
{
  
 float gGABAa,gGABAb; //peak individual inhibitory synaptic conductance.
 float gGABAaMax,gGABAbMax; //Total inhibitory synaptic conductance.
 float wGABA; //Inhibitory connection weight
 float EGABAa,EGABAb;
 float IGABAa,IGABAb,IGABA;
}GABASynapse;
void setSynapse(AMPASynapse ampa,NMDASynapse nmda,GABASynapse gaba)
{
ampa.gAMPAMax=50;nmda.gNMDAMax=50;gaba.gGABAaMax=100;gaba.gGABAbMax=4;
ampa.EAMPA = 0.0;nmda.ENMDA=0.0;gaba.EGABAa=-75;gaba.EGABAb=-100;
nmda.mgC = 1.2;
}
float calculateSynapticCurrent(float t, float Vm, AMPASynapse ampa,NMDASynapse nmda,GABASynapse gaba, int no1, int no2)
    {
        float gbyg0,kmg;
        ampa.gAMPA = ampa.gAMPAMax * exp(-t/18)* (1- exp(-t/2.2))/0.68;                
        ampa.IAMPA = ampa.gAMPA * (Vm - ampa.EAMPA) ;
        
        kmg = 0.00107 * exp(2*0.73*Vm*0.0389);
        gbyg0 = 1- (1/(1+(kmg/nmda.mgC)));
        nmda.gNMDA = nmda.gNMDAMax* gbyg0 *  exp(-(t-5)/18)  * (1-exp(-(t-5)/13.2))/0.60;
        nmda.INMDA = nmda.gNMDA* (Vm - nmda.ENMDA) ; 
        
        
        gaba.gGABAa = gaba.gGABAaMax * exp(-t/25) * (1 - exp(-t/1.0))/0.84;
        gaba.gGABAb = gaba.gGABAbMax * 0.84 * (exp(-t/283) + 0.16 * exp(-t/10226))* (pow((1-exp(-t/112)),4)/0.31);
        gaba.IGABAa = gaba.gGABAa* (Vm -gaba.EGABAa) ;
        gaba.IGABAb = gaba.gGABAb* (Vm -gaba.EGABAb) ;
        gaba.IGABA = gaba.IGABAa+gaba.IGABAb;
      double I = ampa.IAMPA+gaba.IGABA+nmda.INMDA;
     return I;   
    }
    
void setAdex(AdexPtr ad)
{
 //Scaling Parameters
       ad->Cm= 200;            //capacitance
        ad->Gl=10;
       ad->El=-70;
        ad->delta =2;
        ad->Vt = -50;

      //  Bifurcation parameters
        ad->a= 2;        
        ad->Tw = 30;

        ad->b = 0;
        ad->Vr= -58;
              
        ad->Vm=-70;       
        ad->w=0; 
        ad->Vreset = -30;        
    }   
void setAdexAsGranule(AdexPtr ad)
{
 
        ad->Iin = 0;    
      //Parameters  for MF input                As per Chaitanya's paper
			        ad->Cm= 150;
					ad->Gl=10;
					ad->El= -70;
					ad->delta =4;
					ad->Vt = -50;
			      //  Bifurcation parameters
					ad->a= 9;
					ad->Tw = 13;
					ad->b = 250;
					ad->Vr= -64;
					ad->Vm=ad->El;
					ad->w=0;
                                         ad->spikes=0;
                                          ad->Vreset = -43;    
}
void setAdexAsGolgi(AdexPtr ad)
{
 
        ad->Iin = 0;    
      //parameters for MF input:  As per Chaitanya's paper
	   		ad->Cm= 350;
	   		ad->Gl=12;
	   		ad->El= -58;
	   		ad->delta =7;
	   		ad->Vt = -60;
	   	//  Bifurcation parameters
	   		ad->a= 12;
	   		ad->Tw = 7;
	   		ad->b = 1460;
	   		ad->Vr= -50;
	   		ad->Vm=ad->El;
	   		ad->w=0;
                         ad->spikes=0;
                          ad->Vreset = 0;    
}

void setExperiment(ExPtr ex)
{
 
  
 printf("\tREADING THE EXPERIMENT PARAMETERS\n");
 printf("Enter start time of simulation(ms): ");
 scanf("%lf",&ex->startTime);
 printf("Enter duration  of simulation(ms): ");
 scanf("%lf",&ex->endTime);
 ex->endTime += ex->startTime;
 printf("Enter the Integration time step(ms):");
 scanf("%lf",&ex->dt);
 ex->size = (int) ( (ex->endTime) / ex->dt);

}
double DeltaV(double t, double v, double w, AdexPtr ad)
{
       double result;
   
    result = ((-ad->Gl*(v-ad->El)+ad->Gl*ad->delta*(exp((v-ad->Vt)/ad->delta))-w+ad->Iin)/ad->Cm);
        return result;
}
double DeltaW(double t, double v, double w, AdexPtr ad)
{
    double result;
      result=  ((ad->a*(v-ad->El)-w)/ad->Tw);
return result;
}
void RengekuttaSolver_AdExLif(double t, AdexPtr ad, ExPtr ex)
    {
        //t is not used in the equations

         double k1_v=0,k2_v=0,k3_v=0,k4_v=0,KV=0;   //rengakutta varaibles for voltage
         double k1_w=0,k2_w=0,k3_w=0,k4_w=0,KW=0;

              double v1= ad->Vm; double w1 = ad->w;       // for change in voltage
              double v2=v1; double w2 = w1;               //for change in w
              k1_v=DeltaV(t,v1,w1,ad);
              k1_w=DeltaW(t,v2,w2,ad);

              t=t+ex->dt/2;
              v1=ad->Vm+(ex->dt/2)*k1_v;
              w1=ad->w+(ex->dt/2)*k1_v;
              k2_v=DeltaV(t,v1,w1,ad);
              
              v2= ad->Vm+(ex->dt/2)*k1_w;
              w2=ad->w+(ex->dt/2)*k1_w;
              k2_w=DeltaW(t,v2,w2,ad);

              t=t+ex->dt/2;
              v1=ad->Vm+(ex->dt/2)*k2_v;
              w1=ad->w+(ex->dt/2)*k2_v;
              k3_v=DeltaV(t,v1,w1,ad);

              v2=ad->Vm+(ex->dt/2)*k2_w;
              w2=ad->w+(ex->dt/2)*k2_w;
              k3_w=DeltaW(t,v2,w2,ad);
          
              t=t+ex->dt;
              v1=ad->Vm+(ex->dt)*k3_v;
              w1=ad->w+(ex->dt)*k3_v;
              k4_v=DeltaV(t,v1,w1,ad);
          
              v2=ad->Vm+(ex->dt)*k3_w;
              w2=ad->w+(ex->dt)*k3_w;
              k4_w=DeltaW(t,v2,w2,ad);

              KV=ex->dt * ((k1_v+2*k2_v+2*k3_v+k4_v)/6);  // change in voltage
              ad->Vm +=KV;
              KW=ex->dt * ((k1_w+2*k2_w+2*k3_w+k4_w)/6);  // change in w
              ad->w +=KW;
    }
void StoreData(double* VmArray,double* TimeArray,double* wArray,double* IArray,int size)
{
  int i;
  FILE* fp;

   // fp = fopen("Data/Invivo_GranuleCellMF4Go4.csv","w");
    fp = fopen("Data/test.csv","w");
  if(fp == NULL)
   {
     printf("Error in opening the file");
   }
   else
   {
             
    fprintf(fp,"Time,voltage,w,Syn.Current\n");
    for( i = 0; i < size;i++)
            {
            fprintf(fp,"%lf",TimeArray[i]);
            fprintf(fp,",");
            fprintf(fp,"%lf",VmArray[i]);
            fprintf(fp,",");
            fprintf(fp,"%lf",wArray[i]);
            fprintf(fp,",");
            fprintf(fp,"%lf",IArray[i]);
            fprintf(fp,"\n");
            }
   }
  fclose(fp);
}
void Stimulate(AdexPtr ad,ExPtr ex,float* HInputSpikeTime, int no1, int no2)
 {
   float ISyn=0.0;       // The synaptic current
   double elapsed=0.0;
    ad->wArray = (double *) malloc((ex->size+2) * sizeof(double));
    ad->VmArray = (double *) malloc((ex->size+2) * sizeof(double));
    double *TimeArray = (double *) malloc((ex->size+2) * sizeof(double));
    double *IArray = (double *) malloc((ex->size+2) * sizeof(double));
     double *GArray = (double *) malloc((ex->size+2) * sizeof(double));
  int i =0;   // for keeping the voltage and time in the Array: Initialise the values.
        TimeArray[i] = elapsed;
         ad->VmArray[i]= ad->Vm;
        ad->wArray[i] = ad->w;
 AMPASynapse ampa;
 GABASynapse gaba;
 NMDASynapse nmda;
 setSynapse(ampa,nmda,gaba);

 int j=0;
 float t=0,t0=100000;
 
  for(i = 0; i < ex->size;i++)
  {
        t = i * ex->dt;
      if(HInputSpikeTime[j] == t)   //spike occurrence
      {
        t0= HInputSpikeTime[j];   //time when the spike occures
         j++;
         
      }
     if(t < t0)     // Before the spike occurrence time
     {
       ISyn=0;
       ad->Iin = ISyn;
       ad->VmArray[i]= ad->Vm;  
       IArray[i] = ad->Iin;
       GArray[i]=0;
     }
     else
     { 
             
    // ISyn = calculateInvitroSynapticCurrent(t,ad->Vm,ampa,gaba,no1,no2);
     ISyn = calculateSynapticCurrent(t,ad->Vm,ampa,nmda,gaba,no1,no2);  
      ad->Iin =-ISyn; 
              IArray[i] = ad->Iin;  
              GArray[i]= ampa.gAMPA;   
          RengekuttaSolver_AdExLif(elapsed,ad,ex);  //elapsed is not used actually
            if(ad->Vm >  ad->Vreset )
           // if(ad->Vm > 0)  for golgi cells
            {
                ad->VmArray[i] = 30 ;    // set the peak of spike value to some voltage
                ad->Vm=ad->Vr;    //voltage is reset
                ad->w= ad->w +ad->b; 
                 ad->spikes++;
            }
          else
             {
               ad->VmArray[i]=ad->Vm;
             }
       } 
           TimeArray[i] = elapsed;
            ad->wArray[i] = ad->w;             
                 
       
elapsed += ex->dt; 
  }
StoreData(ad->VmArray,TimeArray,ad->wArray,IArray,ex->size); //keep the arrays in CSV files
 }
int SpikeGenerator(float dt,float start, float duration,float ISI,float IBI,int SPB)  //in milli seconds
{ 

 float burstDuration = ISI*SPB;
 int noOfBursts = ceil(duration / (burstDuration + IBI));
 float spikeTime[30];
 float time=0;
 
 int spikes = 0,i=0;
float TimeOfSpike=start;
 
        for(time = 0; time < duration;time+=dt)
        {     
                           
             if(time >TimeOfSpike)            //during spike occurrences
             { 
            
               spikeTime[spikes] = TimeOfSpike;
               
                i++;
                spikes++;
                 if(i < SPB)
                  TimeOfSpike += ISI; 
                 else
                 {
                  TimeOfSpike += IBI; 
                  i = 0;
                 }
              }
            
         } 
      HInputSpikeTime = (float *) malloc(spikes * sizeof(float));
      int j = 0;
     for(j = 0; j < spikes;j++)
        HInputSpikeTime[j] = spikeTime[j];
    
    return spikes;
}
void StoreInput(float *H_InputSpikeTime,int length)
{
  char* str1 = "Data/Input.csv";
     FILE *fp;
    
      fp = fopen(str1,"w");
       if(fp == NULL)
       {
        printf("Error in opening the file");
       }
       else
       {
         fprintf(fp,"Input");
          
           fprintf(fp,"\n");
           int j = 0;
                            
		      for( j = 0; j < length;j++)
                      { 
                         fprintf(fp,"%f",H_InputSpikeTime[j]);
                         fprintf(fp,",");
                      }
            fprintf(fp,"\n");
        }
}  



 
 
