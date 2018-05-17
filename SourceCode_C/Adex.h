/* Adex Model in  C
author: Manjusha Nair @Amrita Vishwa Vidyapeetham

*/
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>
#include "sys/time.h"

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
 double* TimeArray; // To save the time
 double startTime;
 double endTime;
 int size;
}Experiment;
typedef Adex* AdexPtr;
typedef Experiment* ExPtr;
/*..................................................Functions......................................*/
void setExperiment(ExPtr ex);
void setAdex(AdexPtr ad,float I);
void setAdexAsGranule(AdexPtr ad, float I);
void setAdexAsGolgi(AdexPtr ad, float I);
void setAdexAsPurkinje(AdexPtr ad, float I);
void Stimulate(AdexPtr ad,ExPtr ex, double I);
void RungekuttaSolver_AdExLif(double t, AdexPtr ad, ExPtr ex);
double DeltaV(double t, double v, double w, AdexPtr ad);
double DeltaW(double t, double v, double w, AdexPtr ad);
void StoreData(double* VmArray,double* TimeArray,double* wArray,int size);

/*.................................................................................................*/
/*Set Adex model parameters*/
void setAdex(AdexPtr ad,float I)
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
        ad->spikes=0;
        ad->Vreset =0;
        ad->Iin =  I;  
}
void setAdexAsGranule(AdexPtr ad, float I)
{
 //Scaling Parameters
        ad->Cm= 1;            //capacitance
        ad->Gl=10;              //leak conductance
        ad->El=-70;              //resting potential
        ad->delta =2;           //slope factor
        ad->Vt = -50;          //threshold potential
 //Bifurcation parameters
        ad->a= -10;             //level of subthreshold adaptation
        ad->Tw = 0.71;          
        ad->b = 265;
        ad->Vr= -58;         //reversal potential              
        ad->Vm=-62.4;       
        ad->w=0;          //level of adaptation
        ad->Iin =  I;   // The input current   
         ad->spikes=0;  
         ad->Vreset =-43;         
}
void setAdexAsPurkinje(AdexPtr ad, float I)
{

 //Scaling Parameters
        ad->Cm= 100;            //capacitance
        ad->Gl=10;              //leak conductance
       ad->El=-65;              //resting potential
        ad->delta =2;           //slope factor
        ad->Vt = -50;          //threshold potential
      //  Bifurcation parameters
        ad->a= -13;             //level of subthreshold adaptation
        ad->Tw = 1;          
        ad->b = 260;
        ad->Vr= -50;         //reversal potential              
        ad->Vm=-62.4;       
        ad->w=0;          //level of adaptation
        ad->Iin =  I;   // The input current   
         ad->spikes=0;  
         ad->Vreset =-34;;
         }
void setAdexAsGolgi(AdexPtr ad, float I)
{
 //Scaling Parameters
       ad->Cm= 511;            //capacitance
        ad->Gl=13.1;              //leak conductance
       ad->El=-58;              //resting potential
        ad->delta =7;           //slope factor
        ad->Vt = -60;          //threshold potential
      //  Bifurcation parameters
        ad->a= -20;             //level of subthreshold adaptation
        ad->Tw = 14.65;          
        ad->b = 1033;
        ad->Vr= -50;         //reversal potential              
        ad->Vm=-62.4;       
        ad->w=0;          //level of adaptation
        ad->Iin =  I;   // The input current  
         ad->spikes=0;
         ad->Vreset =0;

}
/*Set the simulation parameters*/
void setExperiment(ExPtr ex)
{ 
 ex->startTime=0.00 ;//both in milli seconds  
 printf("\tREADING THE EXPERIMENT PARAMETERS\n");
 printf("Enter Duration of simulation(ms): ");
 scanf("%lf",&ex->endTime);
 printf("Enter the Integration time step(ms):");
 scanf("%lf",&ex->dt);
 ex->size = (int) ( (ex->endTime-ex->startTime) / ex->dt);
 ex->TimeArray = (double *) malloc((ex->size+2) * sizeof(double));
}

/*Function to perform Rungekutta Integration */
void RungekuttaSolver_AdExLif(double t, AdexPtr ad, ExPtr ex)
    {
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
/*To store the output of simulation into file*/
void StoreData(double* VmArray,double* TimeArray,double* wArray,int size)
{
  int i;
  FILE* fp;
  fp = fopen("Data/output.csv","w");
  if(fp == NULL)
   {
     printf("Error in opening the file");
   }
   else
   {             
    fprintf(fp,"Time,voltage,w\n");
    for( i = 0; i < size;i++)
            {
            fprintf(fp,"%lf",TimeArray[i]);
            fprintf(fp,",");
            fprintf(fp,"%lf",VmArray[i]);
            fprintf(fp,",");
            fprintf(fp,"%lf",wArray[i]);
            fprintf(fp,"\n");
            }
   }
  fclose(fp);
}
/*The simulate function in which integration of the differential equations take place */
void Stimulate(AdexPtr ad,ExPtr ex, double I)
 {
   ad->Iin =  I;   // The input current
   double elapsed=ex->startTime;
    ad->wArray = (double *) malloc((ex->size+2) * sizeof(double));
    ad->VmArray = (double *) malloc((ex->size+2) * sizeof(double));
  int i =0;   // for keeping the voltage and time in the Array: Initialise the values.
        ex->TimeArray[i] = elapsed;
         ad->VmArray[i]= ad->Vm;
        ad->wArray[i] = ad->w;

  while(elapsed < ex->endTime)   //until the time is elapsed
        {
                                       
            i++;
          RungekuttaSolver_AdExLif(elapsed,ad,ex);  //elapsed is not used actually
            if(ad->Vm > ad->Vreset)
            {
                ad->spikes++;
                ad->VmArray[i] = 0 ;    // set the peak of spike value to some voltage
                ad->Vm=ad->Vr;    //voltage is reset
                ad->w= ad->w +ad->b; 
            }
          else
             {
               ad->VmArray[i]=ad->Vm;
             }
           ex->TimeArray[i] = elapsed;
            ad->wArray[i] = ad->w;
                   
                 elapsed += ex->dt;

        }

 }



 
