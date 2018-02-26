/* HH Model in pure C
author: Manjusha Nair
*/
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>
#include "sys/time.h"

typedef struct
{
     double Gk;
    double Ek;        //Pottassium  current reversal potential:[mV]
    double Ik;         //Membrane current density for K;
    double n;          // State variable for potassium current activation.
    double dn;      //corresponding change;
    double alphan;  //rate constants;
    double betan;
}KChannel;
typedef struct
{
    double Gna;   //Specific conductance for sodium current: [mS/cm^2]
    double Ena;   //Sodium current reversal potential:[mV]
    double Ina;   //Membrane current densities for Na;

     double m;     // State variable for sodium current activation.
     double h;     // State variable for sodium current inactivation.
     double dm;     //corresponding deltas;
     double dh;
  //rate constants;
     double alpham;
     double alphah;
     double betam;
     double betah;
}NaChannel;
typedef struct
{
 double Gleak;   //Specific conductance for Leakage current: [mS/cm^2]
    double Eleak;    //Leakage  current reversal potential:[mV]
     double Ileak;    //Membrane current density for Leak;
}LeakChannel;
typedef struct
{ 
     double RestVm;    //Membrane voltage at a specific instance
     double Cm;
     double Iin;
     double Vm;
     NaChannel Na;
     KChannel K;
     LeakChannel L;

    //The Arrays holding the results
     double* VmArray;//The output voltage of the neuron
     double* IArray;
     double* mArray;
     double* hArray ;
     double* nArray ;
    /* double* NaIArray ;
     double* KIArray ;
     double* LIArray ;
     double* TotalIArray ;
*/
     

}HH;

typedef struct 
{
 double dt;        //Integration time
 double* TimeArray; // To save the time
 double startTime;
 double endTime;
 double Iin;
 int size;
 
}Experiment;
typedef HH* HHPtr;
typedef Experiment* ExPtr;
/***************************************************************Functions*************************************************************/
void setExperiment(ExPtr ex);
void setHH(HHPtr hh,double I);
void Stimulate(HHPtr hh,ExPtr ex);
        void RengekuttaSolver_HH(double t,HHPtr hh,ExPtr ex);
        double DeltaV(double I,double dt, double Cm);
        double CalcDelta_mhn(double dt,double alphax,double betax,double x);
        double CalcAlpham(double v);
        double CalcAlphan(double v) ;
        double CalcAlphah(double v);
        double CalcBetam(double v);
        double CalcBetan(double v);
        double CalcBetah(double v);
	double getNaCurrent(NaChannel Na,  double V);
	double getKCurrent(KChannel K, double V);
	double getLCurrent( LeakChannel L, double V);
void StoreData(double* TimeArray,double* VmArray,double* mArray,double* nArray,double* hArray,int size);
/***************************************************************************************************************************************/


void setHH(HHPtr hh,double I)
{
        hh->RestVm = -70.0;
        hh->Cm = 1.0;
        hh->Iin = I;
        hh->Vm = hh->RestVm;
        //set Sodium channel values
        hh->Na.Gna = 120;                       
        hh->Na.Ena =  115;
       hh->Na.Ina = 0;
       //hh->Na.m= 0.05;
       //hh->Na.h=0.6;
     
       //set Pottassium channel values
       hh->K.Gk = 36;
                                        
        hh->K.Ek = -12;
          hh->K.Ik = 0;
       //hh->K.n=0.35;
      
      //set Leak channel values

       hh->L.Gleak = 0.3;
       hh->L.Eleak = 10.6;
       hh->L.Ileak = 0;


   //initialize the activation and inactivation parameters
           

             // start these paramaters in steady state
           hh->Na.m = CalcAlpham(hh->Vm)/ (CalcAlpham(hh->Vm) + CalcBetam(hh->Vm));
            hh->Na.h = CalcAlphah(hh->Vm) / (CalcAlphah(hh->Vm) + CalcBetah(hh->Vm));
            hh->K.n = CalcBetan(hh->Vm) / (CalcBetan(hh->Vm) + CalcAlphan(hh->Vm));
            
         
       
       hh->Na.m = 0.068775;
       hh->Na.h = 0.515186;
      hh-> K.n = 0.35286656; 
      
}
void setExperiment(ExPtr ex)
{
 
 ex->startTime=0.00 ;//both in milli seconds  
 printf("\tREADING THE EXPERIMENT PARAMETERS\n");
 printf("Enter Duration of simulation(ms): ");
 scanf("%lf",&ex->endTime);
 printf("Enter the Integration time step(ms):");
 scanf("%lf",&ex->dt);
 
 ex->size = (int) ( (ex->endTime-ex->startTime) / ex->dt);
 
}

void Stimulate(HHPtr hh,ExPtr ex)
 {
   
   double Vm;
   //double Iionic,Itotal;

   double elapsed=ex->startTime;
    hh->IArray = (double *) malloc((ex->size+2) * sizeof(double));
    ex->TimeArray = (double *) malloc((ex->size+2) * sizeof(double));
    hh->VmArray = (double *) malloc((ex->size+2) * sizeof(double));
    hh->mArray = (double *) malloc((ex->size+2) * sizeof(double));
    hh->nArray = (double *) malloc((ex->size+2) * sizeof(double));
    hh->hArray = (double *) malloc((ex->size+2) * sizeof(double));
  int i =0;   // for keeping the voltage and time in the Array: Initialise the values.
          hh->VmArray[i]= hh->RestVm;
          Vm= hh->RestVm;
        

  while(elapsed < ex->endTime)   //until the time is elapsed
        {
         
          Vm = hh->Vm;
          hh->VmArray[i] = Vm;
          
          //The current for the new membrane voltage
          // hh->Na.Ina = getNaCurrent(hh->Na,Vm);
          // hh->K.Ik = getKCurrent(hh->K,Vm);
          // hh->L.Ileak = getLCurrent(hh->L,Vm);
           //Iionic = hh->Na.Ina+hh->K.Ik+hh->L.Ileak;
           //Itotal =Iionic-hh->Iin;  
          //  The array values to be stored
           ex->TimeArray[i] = elapsed;
           hh->mArray[i] = hh->Na.m;
           hh->hArray[i] = hh->Na.h;
           hh->nArray[i] = hh->K.n;

           RengekuttaSolver_HH(elapsed,hh,ex);  //elapsed is not used actually 
            elapsed += ex->dt;    
                   // elapsed += 1;          
            i++;

       }

   
}
void RengekuttaSolver_HH(double t,HHPtr hh,ExPtr ex)
    {
        double k1_v=0,k2_v=0,k3_v=0,k4_v=0,KV=0;   //rengakutta varaibles for voltage
         double k1_m=0,k2_m=0,k3_m=0,k4_m=0,Km=0;  //rengakutta varaibles for activation and inactivation parameters
          double k1_h=0,k2_h=0,k3_h=0,k4_h=0,Kh=0;
           double k1_n=0,k2_n=0,k3_n=0,k4_n=0,Kn=0;
           double Itotal=0;   //total current
           double V = hh->Vm;
         if(t !=ex->startTime)  //Not the first step
         {
           //solving for m, n and h
           k1_m = CalcDelta_mhn(ex->dt,CalcAlpham(V),CalcBetam(V),hh->Na.m) ;
           k1_h = CalcDelta_mhn(ex->dt,CalcAlphah(V),CalcBetah(V),hh->Na.h) ;
           k1_n = CalcDelta_mhn(ex->dt,CalcAlphan(V),CalcBetan(V),hh->K.n) ;


           k2_m = CalcDelta_mhn(ex->dt,CalcAlpham(V),CalcBetam(V),hh->Na.m+0.5*k1_m*ex->dt) ;
           k2_h = CalcDelta_mhn(ex->dt,CalcAlphah(V),CalcBetah(V),hh->Na.h+0.5*k1_h*ex->dt) ;
           k2_n = CalcDelta_mhn(ex->dt,CalcAlphan(V),CalcBetan(V),hh->K.n+0.5*k1_n*ex->dt) ;

           k3_m = CalcDelta_mhn(ex->dt,CalcAlpham(V),CalcBetam(V),hh->Na.m+0.5*k2_m*ex->dt) ;
           k3_h = CalcDelta_mhn(ex->dt,CalcAlphah(V),CalcBetah(V),hh->Na.h+0.5*k2_h*ex->dt) ;
           k3_n = CalcDelta_mhn(ex->dt,CalcAlphan(V),CalcBetan(V),hh->K.n+0.5*k2_n*ex->dt) ;

           k4_m = CalcDelta_mhn(ex->dt,CalcAlpham(V),CalcBetam(V),hh->Na.m+k3_m*ex->dt) ;
           k4_h = CalcDelta_mhn(ex->dt,CalcAlphah(V),CalcBetah(V),hh->Na.h+k3_h*ex->dt) ;
           k4_n = CalcDelta_mhn(ex->dt,CalcAlphan(V),CalcBetan(V),hh->K.n+k3_n*ex->dt) ;

           Km= ((k1_m+2*k2_m+2*k3_m+k4_m)/6);
           Kh=((k1_h+2*k2_h+2*k3_h+k4_h)/6);
           Kn= ((k1_n+2*k2_n+2*k3_n+k4_n)/6);

           hh->Na.m +=Km;
           hh->Na.h +=Kh;
           hh->K.n +=Kn;
        }
           //Calculate the ionic currents and the Total current
       //   Itotal = E.InI+(getNaCurrent(Na.m,Na.h,V)+getKCurrent(K.n,V)+getLCurrent(V));
        //  double dv = DeltaV(Itotal);
       //   C.Vm += dv;

            Itotal = (getNaCurrent(hh->Na,V)+getKCurrent(hh->K,V)+getLCurrent(hh->L,V))-hh->Iin ;
            k1_v=DeltaV(Itotal,ex->dt,hh->Cm);

            Itotal = (getNaCurrent(hh->Na,V+0.5*k1_v*ex->dt)+getKCurrent(hh->K,V+0.5*k1_v*ex->dt)+getLCurrent(hh->L,V+0.5*k1_v*ex->dt)) -hh->Iin ;
            k2_v=DeltaV(Itotal,ex->dt,hh->Cm);

            Itotal =(getNaCurrent(hh->Na,V+0.5*k2_v*ex->dt)+getKCurrent(hh->K,V+0.5*k2_v*ex->dt)+getLCurrent(hh->L,V+0.5*k2_v*ex->dt)) - hh->Iin;
            k3_v=DeltaV(Itotal,ex->dt,hh->Cm);

            Itotal = (getNaCurrent(hh->Na,V+k3_v*ex->dt)+getKCurrent(hh->K,V+k3_v*ex->dt)+getLCurrent(hh->L,V+k3_v*ex->dt)) - hh->Iin;
            k4_v=DeltaV(Itotal,ex->dt,hh->Cm);
                   
            KV= ((k1_v+2*k2_v+2*k3_v+k4_v)/6);  // change in voltage
            hh->Vm +=KV;
                

    }
double DeltaV(double I,double dt, double Cm)
    {
        
            return (- dt *I/Cm);
      
        
    }
double CalcDelta_mhn(double dt,double alphax,double betax,double x)
    {
        return dt* (alphax * (1 - x) - betax * x);
    }
    
 double CalcBetah(double v)
    {
        return (1 / (exp((-v-62 + 30) / 10) + 1));
    }
 double CalcAlphah(double v)
    {
        return (0.07 * exp((v+62) / -20));
    }
 double CalcBetam(double v)
    {
        return (4 * exp((-v-62) / 18));
    }
 double CalcAlpham(double v)
    {
       
        return  (0.1 * (-v-62 + 25)) / (exp((-v-62 + 25)/10 ) - 1);
    }
 double CalcBetan(double v)
    {
     
       return (0.125 * exp((-v-62) / 80)) ;
    }
 double CalcAlphan(double v)
    {
        return (0.01 * (-v-62+10) / (exp((-v -62+ 10) / 10) - 1)) ;
    }
double getNaCurrent(NaChannel Na,  double V)
    {
        return (Na.Gna *(Na.m * Na.m * Na.m) * Na.h * (V+62-Na.Ena));
    }
double getKCurrent(KChannel K, double V)
    {
        return (K.Gk * (K.n * K.n * K.n * K.n )* (V +62 - K.Ek));
    }
double getLCurrent( LeakChannel L, double V)
  {
        //return (L.Gleak*(L.Eleak-62-V));
        return (L.Gleak*(V+62-L.Eleak));
    }
void StoreData(double* TimeArray,double* VmArray,double* mArray,double* nArray,double* hArray,int size)
{
  int i;
  FILE* fp;
 fp = fopen("hh.csv","w");
  if(fp == NULL)
   {
     printf("Error in opening the file");
   }
   else
   {
             
    fprintf(fp,"Time,voltage,m,n,h\n");
    for( i = 0; i < size;i++)
            {
            fprintf(fp,"%lf",TimeArray[i]);
            fprintf(fp,",");
            fprintf(fp,"%lf",VmArray[i]);
            fprintf(fp,",");
            fprintf(fp,"%lf",mArray[i]);
            fprintf(fp,",");
	    fprintf(fp,"%lf",nArray[i]);
            fprintf(fp,",");
            fprintf(fp,"%lf",hArray[i]);
            fprintf(fp,"\n");
            }
   }
  fclose(fp);
}

