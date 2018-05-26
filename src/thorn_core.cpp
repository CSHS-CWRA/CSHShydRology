#include <Rcpp.h>
#include <cmath>
using namespace Rcpp;

/* Core code for the computation of the USGS type monthly thornthwaite water balance on a list of locations.  The model is adapted byMoore
et al 2012 to account for forest cover variations and glaciers.  The input is typically from ClimateWNA.  
JWT March 2015
 
July 2015 JWT modified to include precip as snow from ClimateWNA instead of the partitioning equation.  
Additionally, soil moisture storage
capacity was changed to be supplied by the input data, and have a multiplier supplied as a parameter for calibration
This should make it more future proof
 
*/

//constants
//Number of days in a month, assume non-leap year because it is only for calculating mean day length in hamon equation of pet
const NumericVector d = NumericVector::create(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31);
const double pi = 3.1415926535897;


//Input and/or calibration parameters
double T_rain = 3.0;                    //Temp (C) where precip is 100% rain
double T_snow = -5.0;                   //Temp (C) where precip is 100% snow
double melttemp = 0.0;                  //Temp (C) where snowmelt begins
//double STC = 150.0;                     //Soil Moisture Storage Capacity    (mm)
double drofrac = 0.05;                  //Direct runoff fraction
double rfactor = 0.5;                   //fraction of surplus that becomes runoff in a month (Wolock and McCabe(1999)
double meltrate = 2.0;                  //base snowmelt rate (mm/deg C) under forest coverage
double glaciermeltfactor = 2.25;        //glacier melt factor multiplier
  
//initial interception factors, for error handling
double meltfactor = 1.75;               //multiplier for snowmelt
double snowintfactor = 0.0;             //snow interception fraction
double rainintfactor = 0.0;             //rain interception fraction

//initial runoff factor, for error handling


//////////////////////////////////
//Initialization of vectors (monthly) for point scale storage, unless otherwise noted, units are in mm
NumericVector D(365);                   //Approximate day length (hours)
NumericVector m_day(12);                //Average day length per month (units of 12 hours)

//Precip inputs
NumericVector P_rain(12);               //Precipitation falling as rain
NumericVector P_snow(12);               //Precipitation falling as snow
NumericVector rainint(12);              //Intercepted rain
NumericVector snowint(12);              //Intercepted snow

//Snow and glaciers
NumericVector snostor(12);              //Snow stored in snowpack
NumericVector P_remain(12);             //Remaining precip (after direct runoff)
NumericVector glaciermelt(12);          //Glacier melt
NumericVector linearmelt(12);           //Snowmelt as a linear amount


//ET
NumericVector PETh(12);                 //Potential evapotranspiration from Hamon
NumericVector adPET(12);                //Adjusted pet (after accounting for canopy evaporation)
NumericVector deficit(12);              //Deficit between AET and PET
NumericVector AET(12);                  //Actual evapotranspiration

//Soil water
NumericVector P_total(12);              //total amount of water that proceeds to the soil
NumericVector STW(12);                  //Soil moisture storage withdrawal
NumericVector ST(12);                   //Soil storage from previous month
NumericVector surp(12);                 //Soil moisture surplus from previous month

//Runoff
NumericVector DRO(12);                  //Direct runoff fraction
NumericVector RO(12);                   //Runoff from surplus
NumericVector total_RO(12);             //Total runoff (surplus plus direct)
///////////////////////////////////////

//Functions used in model

//compute mean of an array
//// [[Rcpp::export]]
double meanC(NumericVector x) {
  int n = x.size();
  double total = 0;

  for(int i = 0; i < n; ++i) {
    total += x[i];
  }
  return total / n;
}


//compute mean day length per month
//// [[Rcpp::export]]
NumericVector daylen(double Lat){
  //NumericVector D(365);               //Approximate Day Length
  D = rep(-9999,365);                   //reset day length
  //loop through all the days of a year
  for (int j = 0; j < 365; j++) {
    double P = asin(0.39795 * cos(0.2163108 + 2.0 * atan(0.9671396 * tan(0.00860 * (j - 185)))));
    D(j) = 24 - (24 / pi) * acos((sin(0.8333 * pi/180) + 
    sin( Lat * pi / 180) * sin(P))/(cos(Lat * pi / 180) * cos(P)));
    }
  //return D;                           //don't need to return anything when its a global variable running in the model
}


//get the days that are in a certain month
//// [[Rcpp::export]]
NumericVector ss(NumericVector input, int startday, int endday){
  NumericVector ss(input.begin()+startday, input.begin()+endday+1);
  return ss;
}

//Compute mean day length per month in units of 12 hours
//// [[Rcpp::export]]
NumericVector meandaylen(){
  m_day = rep(-9999,12); //reset m_day

  m_day[0]=meanC(ss(D,0,30))/12; 
  m_day[1]=meanC(ss(D,31,58))/12; 
  m_day[2]=meanC(ss(D,59,89))/12; 
  m_day[3]=meanC(ss(D,90,119))/12; 
  m_day[4]=meanC(ss(D,120,150))/12;
  m_day[5]=meanC(ss(D,151,180))/12; 
  m_day[6]=meanC(ss(D,181,211))/12; 
  m_day[7]=meanC(ss(D,212,242))/12; 
  m_day[8]=meanC(ss(D,243,272))/12; 
  m_day[9]=meanC(ss(D,273,303))/12; 
  m_day[10]=meanC(ss(D,304,333))/12; 
  m_day[11]=meanC(ss(D,334,364))/12;  
  
  //return(m_day);  //don't need to return anything when its a global variable in the model
}

// //Prefer to have shorter code to get the mean day length per month, doesn't work now though.
// //must be some sort of type conversion error
// // [[Rcpp::export]]
// NumericVector testmdl(NumericVector DOY, NumericVector d){
//   int start;
//   int end;
//   NumericVector m_day(12);
//   //m_day = rep(-9999,12); //reset m_day
// 
//   for (int j = 0; j < 12; j++) {
//     if (j==0){
//       //do stuff for first month
//       start = 0;
//     }
//     else {
//       //do stuff for other months
//       start = sum(ss(d,0,(j-1)));
//     }
//     end = sum(ss(d,0,j))-1;
//     m_day[j] = meanC(ss(D,start,end))/12;
//   }
//   
//   return m_day; //don't need to return anything when its a global variable in the model
// }




////Calculate Hamon PET
//// [[Rcpp::export]]
double calcPETh(int month, double meanairtemp){
  //Saturated Water Vapor Density
  double Wt=(4.95*exp(0.062*meanairtemp))/100.0;
  //Potential Evapotranspiration from Hamon
  return 13.97*d[month]*pow(m_day[month],2)*Wt;
}

//partition precipitation
double part(double airtemp, double T_rain, double T_snow){
  return (T_rain-airtemp)/(T_rain-T_snow);  
}

//Update interception and melt rates based on canopy
double rates(int LC, NumericVector calvals){
  //Melt Factor increases snowmelt by X if no tree coverage (alpine or grassland).  Also an interception factor decreases snow and rain when there is forest coverage
  //Future expansion= different interception rates for different BEC zones.
        if (LC == 1 || LC == -9999){ //CHANGED //CHANGED 04-06-2014
          //Clearcut/alpine values  
          //meltfactor = 1.75
          //snowintfactor=0      
          //rainintfactor=0
          snowintfactor = calvals[3];
          rainintfactor = calvals[3];
          meltfactor=calvals[0];
        } else {
          if (LC == 2){ //CHANGED //CHANGED 04-06-2014
            //subalpine values
            //meltfactor=1.25
            //snowintfactor=0.1
            //rainintfactor=0.1
            snowintfactor = calvals[4];
            rainintfactor = calvals[4];
            meltfactor = calvals[1];
            }else{
              if (LC == 3){ //CHANGED //CHANGED 04-06-2014
                //full canopy values
                //meltfactor=1
                //snowintfactor=0.25
                //rainintfactor=0.25
                snowintfactor = calvals[5];
                rainintfactor = calvals[5];
                meltfactor = calvals[2];
                }
              }
            }  
}


//Water balance calculation
// [[Rcpp::export]]
NumericVector waterbalance(NumericVector calvals, NumericMatrix data, std::string out, std::string rsnowpart){
  int nrow = data.nrow();
  
  //Initialization of Matrices for output
  NumericMatrix flow(nrow,12);
  NumericMatrix snowpack(nrow,12);
  NumericMatrix pet(nrow,12);
  NumericMatrix st(nrow,12);
  NumericMatrix aet(nrow,12);
  NumericMatrix totalint(nrow,12);
  
  //Reset Day lengths and mean day length to 0
  D = rep(0.0,365);
  m_day = rep(0.0,12);
  
  
  //Start loop for input data
  for (int i = 0; i < nrow; i++) {
    
    double L = data(i,1);     //Latitude 
    daylen(L);                //Update Day length
    meandaylen();             //Update Mean Day Length
    double STC = data(i,43);  //Soil Storage Capacity from the data
    
    //Multiply STC by a calibration parameter.  Leave at 1 for now
    //Eventually either supply STC, or make a linear relationship with elevation
    STC = STC * calvals[6];
    
    //reset point storages to 0.
    P_rain = rep(0.0,12);
    P_snow = rep(0.0,12);
    rainint = rep(0.0,12);
    snowint = rep(0.0,12);
    snostor = rep(0.0,12);
    DRO = rep(0.0,12);
    P_remain = rep(0.0,12);
    glaciermelt = rep(0.0,12);
    linearmelt = rep(0.0,12);
    P_total = rep(0.0,12);
    PETh = rep(0.0,12);
    adPET = rep(0.0,12);
    STW = rep(0.0,12);
    ST = rep(0.0,12);
    deficit = rep(0.0,12);
    AET = rep(0.0,12);
    surp = rep(0.0,12);
    RO = rep(0.0,12);
    total_RO = rep(0.0,12);
    
    //Set initial boundary conditions
    snostor[11] = 0.0;      //mm snowpack storage
    ST[11] = STC;           //initial soil moisture storage
    
    
    //Spinup 20x
    int a = 0;
    while (a < 20){
      //loop through the months
      for (int x = 0; x < 12; x++) {
        //Waterbalance calculations
        
        //Partition precipitation between rain and snow based on temperature
    if (rsnowpart == "fromdata"){
        //get P_rain and P_snow directly from ClimateWNA
        P_rain[x] = data(i,16+x)-data(i,28+x);
        P_snow[x] = data(i,28+x);
    } else {
        T_rain = calvals[7];
        T_snow = calvals[8];
        
        double frac = part(data(i,4+x),T_rain,T_snow);
        P_snow[x] = data(i,16+x)*frac;                  //amount of precip falling as snow
        P_rain[x] = data(i,16+x)-P_snow[x];             //remaining amount falls as rain
        if (P_snow[x]<0.0) {                            //P_snow can't be negative
          P_snow[x] = 0.0;
          P_rain[x] = data(i,16+x);
            }                                         

        if (P_rain[x]<0.0) {                            //P_rain can't be negative either
          P_rain[x] = 0.0;
          P_snow[x] = data(i,16+x);
          }
    }   

        //Update melt factors and interception factors based on landcover
        rates(data(i,40), calvals);
        
        //Calculate the intercepted precip
        snowint[x] = P_snow[x] * snowintfactor;
        rainint[x] = P_rain[x] * rainintfactor;
        //Update precip to only throughfall
        P_snow[x] = P_snow[x] - snowint[x];
        P_rain[x] = P_rain[x] - rainint[x];
        
        //snowpack storage =previous snowpack storage plus precip falling as snow
        if (x>0) {
          snostor[x] = snostor[x-1] + P_snow[x];
          } else {
            snostor[x] = snostor[11] + P_snow[x];
            } 
        //Direct runoff Fraction (resulting from infiltration excess overland flow
        DRO[x] = P_rain[x] * drofrac;

        //amount of precipitation that proceeds directly to the soil
        P_remain[x] = P_rain[x] - DRO[x];

        //Snowmelt as a linear rate:   (degree day model)
        linearmelt[x] = (data(i,4+x) - melttemp) *meltrate * meltfactor * d[x];
        
        //Not storing cold content
        if (linearmelt[x] <= 0.0){
          linearmelt[x] = 0.0;
          } 
        
        //snowpack remaining after melt
        if (linearmelt[x] >= snostor[x]){
          linearmelt[x] = snostor[x];
          snostor[x] = 0.0;
          } else {
            snostor[x] = snostor[x] - linearmelt[x];
            } 

        //glacier melt
        if ((data(i,42) == 1.0) && (snostor[x] == 0.0)){
          glaciermelt[x] = (data(i,4+x) - melttemp) * meltrate * glaciermeltfactor * d[x];
          } else {
            glaciermelt[x] = 0.0;
            }

        //Don't want to store cold content in glacier either
        if (glaciermelt[x] <= 0.0){
          glaciermelt[x] = 0.0;
          }   
          
        //total amount water  that proceeds to the soil
        P_total[x] = P_remain[x] + linearmelt[x] + glaciermelt[x];
          
        //Potential Evapotranspiration from Hamon
        //If there is snow on the ground and no tree cover, PET is 0, so all precip and melt goes to the soil
        PETh[x] = calcPETh(x,data(i,4+x));
        if ((data(i,40) == 1) && (snostor[x] > 0.0)){
          PETh[x] = 0.0;
          }
        
        //adjust PET by subtracting canopy evaporation 
        adPET[x] = PETh[x] - rainint[x] - snowint[x];        
        
        //adjusted PET cant be negative
        if (adPET[x] < 0.0){                          
          adPET[x] = 0.0;
          }               
        
        //rollover soil moisture and surplus from previous month
        if (x > 0) {
          ST[x] = ST[x-1];                                           
          surp[x] = surp[x-1];
          }else{
            ST[x] = ST[11];
            surp[x] = surp[11];
            }
          
        //Is it a glacier? 
        if (data(i,42) == 1){
          
          //If snow is on the ground PET is 0, and all precip and melt goes directly to the surplus
          if(snostor[x] > 0.0){
            PETh[x] = 0.0;
            adPET[x] = 0.0;
            AET[x] = 0.0;
            ST[x] = 0.0;
            surp[x] = P_total[x] + surp[x];
            rfactor = 0.7;
            }
              //If snowpack is 0, AET and PET is 0, and all melt goes straight to the surplus.  
              //rfactor is set to 0.9 due to faster release from the glacier than from the soil
              else {
                PETh[x] = 0.0;
                adPET[x] = 0.0;
                AET[x] = 0.0;
                ST[x] = 0.0;
                surp[x] = P_total[x] + surp[x];
                rfactor = 0.9;
                }
          }
            else {
              //Not on a glacier
              //Base situation (land)
              rfactor = 0.5;
              //What happens is Precip is less than PET?
              if (P_total[x] < adPET[x]) {                                       
                STW[x] = fabs(P_total[x] - adPET[x]) * (ST[x] / STC);           //Soil Moisture Storage withdrawal if P_total < adPET
                AET[x] = P_total[x] + STW[x];                                   //AET if P_total < adPET
                ST[x] = ST[x] - STW[x];                                         //Subtract STW from soil moisture
                deficit[x] = adPET[x] - AET[x];                                 //deficit between AET and PET if AET is less than PET
                }
                else{                                                           //What happens if P_total is greater than PET
                  AET[x] = adPET[x];                                            //AET equals Pet
                  ST[x] = ST[x] + (P_total[x] - adPET[x]);                      //Soil moisture increases by difference between P_total and adPET
                }
                                          
              //If soil moisture goes over soil storage capacity, the excess is turned to surplus
              if (ST[x] > STC){
                surp[x] = ST[x] - STC + surp[x];                 
                ST[x] = STC;                                                    //Soil Moisture is set back to STC
                }                                                            
              }

        //runoff generated from surplus
        RO[x] = surp[x] * rfactor;                                                    
        
        //amount left over for next month
        surp[x] = surp[x] - RO[x];                                                    
        
        //Total Runoff (direct runoff plus surplus runoff) 
        total_RO[x] = RO[x] + DRO[x];                                                  

        // If the ground surface is water and there is no snowpack (implying that the lake isn't frozen) then the runoff is precip-pet.
        //if pet is greater than precip, the runoff will be negative, acting as a sink)
        //do we want this for wetlands?
        if ((data(i,41) == 1) && (snostor[x] == 0.0)){
          total_RO[x] = data(i,16+x) - PETh[x];   
          surp[x] = 0.0;
          RO[x] = 0.0;
          ST[x] = 0.0;
          STW[x] = 0.0;
          AET[x] = PETh[x]; 
        }

        }         //end loop through months
      a++;
    }             //end spinup

    //assignment to output matrices
    int k = 0;
    while(k < 12){
      flow(i,k) = total_RO[k];
      snowpack(i,k) = snostor[k];
      pet(i,k) = PETh[k];
      st(i,k) = ST[k];
      aet(i,k) = AET[k];
      totalint(i,k) = rainint[k] + snowint[k];
      k++;
    }
 }
  //options to return other data
  if (out == "snow"){
  return snowpack;
  } 
    else {
      return flow;
    }
}