#include <R.h>
#include <Rmath.h>

// R CMD SHLIB newratings.c   # comando usado para compilar o arquivo .c

//---------------------------------PROTOTIPOS-----------------------------------

//(Adapted-PlayerRatings)
void glicko2_c(int *np, int *nr, int *white, int *black, double *score, 
              double *crats, double *gdevs, double *gamma, double *dscore);
  
void ELO_c(int *np, int *njogos, int *white, int *black, double *score, double *delta,   
  double *Ratingsi, double *dscore);


void Gli_c(int *np, int *njogos, int *white, int *black, double *score,  
  double *Ratingsi, double *GD, double *del,double *dscore); 


void Bp_c( int *nj, int *w, int *b, double *score, 
  double *R, double *var,double *del,double *vdel, double *lam,double *vlam,double *ATR, double *ATD,double *ATd,double *ATl,double *Avd,double *AvL);

void Analise_c( int *nj, int *w, int *b,   double *yv, double *ye, double *yd,double *score, 
  double *RE,double *DE,double *RG,double *varG, double *del,double *RG2,double *varG2, double *del2, 
  double *RBP,  double *delBP , double *lamBP, double *LE2, double *DE2,double *LE3,double *DE3,
  double *LG2, double *DG2,double *LG3,double *DG3,
  double *LG22, double *DG22,double *LG23,double *DG23,
  double *LBP2, double *DBP2,double *LBP3,double *DBP3);

//------------------------------------------------------------------------------

//------------------------------------------------------------

void glicko2_c(int *np, int *nr, int *white, int *black, double *score, 
              double *crats, double *gdevs, double *gamma, double *dscore)
{
  
  double *escore;
  double *ascore;
  double *dval;
  double escorek;
  int k;
  
  escore = (double *)R_alloc(*np, sizeof(double));
  ascore = (double *)R_alloc(*np, sizeof(double));
  dval = (double *)R_alloc(*np, sizeof(double));
  
  for(k=0;k<*np;k++) {  
    escore[k] = 0;
    ascore[k] = 0;
    dval[k] = 0;
  }  
  
  for(k=0;k<*nr;k++) {
    ascore[white[k]] = ascore[white[k]] + score[k];
    escorek = 1/(1 + exp(gdevs[black[k]] * (crats[black[k]] - crats[white[k]] - gamma[k])));
    escore[white[k]] = escore[white[k]] + escorek;
    dval[white[k]] = dval[white[k]] + R_pow(gdevs[black[k]],2) * escorek * (1-escorek);
    dscore[white[k]] = dscore[white[k]] + gdevs[black[k]] * (score[k] - escorek);
    
    ascore[black[k]] = ascore[black[k]] + 1 - score[k];
    escorek = 1/(1 + exp(gdevs[white[k]] * (crats[white[k]] - crats[black[k]] + gamma[k])));
    escore[black[k]] = escore[black[k]] + escorek;
    dval[black[k]] = dval[black[k]] + R_pow(gdevs[white[k]],2) * escorek * (1-escorek);
    dscore[black[k]] = dscore[black[k]] + gdevs[white[k]] * (1 - score[k] - escorek);
  }
  for(k=0;k<*np;k++) dscore[k + *np] = dval[k];
}


//----------------------------------------------------------------------------------------------
void ELO_c(int *np, int *njogos, int *white, int *black, double *score, 
   double *delta, double *Ratingsi, double *dscore)
{
  double *escore;
  double *ascore;
  int k;

  escore = (double *)R_alloc(*np, sizeof(double));   
  ascore = (double *)R_alloc(*np, sizeof(double));   

  for(k=0;k<*np;k++) {                               
    escore[k] = 0;
    ascore[k] = 0;
  }  

  for(k=0;k<*njogos;k++) {                                                        
    ascore[white[k]] = ascore[white[k]] + score[k];                         
    escore[white[k]] = escore[white[k]] + 
      1/(1 + R_pow(10,(Ratingsi[black[k]] - Ratingsi[white[k]] -delta[k])/400)); 
   
    ascore[black[k]] = ascore[black[k]] + 1 - score[k];                     
    escore[black[k]] = escore[black[k]] + 
      1/(1 + R_pow(10,(Ratingsi[white[k]] - Ratingsi[black[k]] +delta[k])/400));  
  }
  for(k=0;k<*np;k++) dscore[k] = ascore[k] - escore[k];                   
}

//-----------------------------------------------------------------------------------------------
void Gli_c(int *np, int *njogos, int *white, int *black, double *score, 
  double *Ratingsi, double *GD,double *del, double *dscore)
{

  double *escore;
  double *ascore;
  double *dval;
  double escorek;
  double qv2 = R_pow(M_LN10/400, 2);
  int k;

  escore = (double *)R_alloc(*np, sizeof(double));
  ascore = (double *)R_alloc(*np, sizeof(double));
  dval = (double *)R_alloc(*np, sizeof(double));

  for(k=0;k<*np;k++) {  // Preenche os vetores definidos
    escore[k] = 0;  // Escore esperado por partida
    ascore[k] = 0;  // Escore observado
    dval[k] = 0;    //Desvio
  }  


  for(k=0;k<*njogos;k++) {
    ascore[white[k]] = ascore[white[k]] + score[k];                                                       
    escorek = 1/(1 + R_pow(10,(GD[black[k]] * (Ratingsi[black[k]] - Ratingsi[white[k]]-del[k]))/400));   
    escore[white[k]] = escore[white[k]] + escorek;                                                        

	dval[white[k]] = dval[white[k]] + qv2 * R_pow(GD[black[k]],2) * escorek * (1-escorek);            
    dscore[white[k]] = dscore[white[k]] + GD[black[k]] * (score[k] - escorek);                         

    ascore[black[k]] = ascore[black[k]] + 1 - score[k];                                                   
    escorek = 1/(1 + R_pow(10,(GD[white[k]] * (Ratingsi[white[k]] - Ratingsi[black[k]]+del[k]))/400));  
    escore[black[k]] = escore[black[k]] + escorek;
	  
    dval[black[k]] = dval[black[k]] + qv2 * R_pow(GD[white[k]],2) * escorek * (1-escorek);
    dscore[black[k]] = dscore[black[k]] + GD[white[k]] * (1 - score[k] - escorek);
  }
  for(k=0;k<*np;k++) dscore[k + *np] = dval[k];
}

//------------------------------------------------------------------

void Bp_c( int *nj, int *w, int *b, double *score, 
  double *R, double *var,double *del,double *vdel, double *lam,double *vlam,double *ATR, double *ATD,double *ATd,double *ATl,double *Avd,double *AvL)
{

  double pv;  // Probabilidade de vitória
  double pe;  // Probabilidade de empate
  double pd;  //Probabiliades de derrota
  int k;

  for(k=0;k<*nj;k++) {

  pv =  exp(R[w[k]]+del[k])/( exp(R[w[k]]+del[k])+exp(lam[k]+(R[w[k]]+R[b[k]]+del[k])/2)+exp(R[b[k]]));

  pe =  exp(lam[k]+(R[w[k]]+R[b[k]]+del[k])/2)/( exp(R[w[k]]+del[k])+exp(lam[k]+(R[w[k]]+R[b[k]]+del[k])/2)+exp(R[b[k]]));

  pd =  exp(R[b[k]])/( exp(R[w[k]]+del[k])+exp(lam[k]+(R[w[k]]+R[b[k]]+del[k])/2)+exp(R[b[k]]));

    if(score[k]==0)
    {
   ATR[w[k]] = ATR[w[k]] -var[w[k]]*(pv+0.5*pe);
   ATR[b[k]] = ATR[b[k]] +var[b[k]]*(1-pd-0.5*pe); 

   ATD[w[k]] = ATD[w[k]] +R_pow(var[w[k]]*(pv+0.5*pe),2);
   ATD[b[k]] = ATD[b[k]] +R_pow(var[b[k]]*(1-pd-0.5*pe),2);

   ATd[k] = -vdel[k]*(pv+0.5*pe);
   ATl[k] = -vlam[k]*pe;
   Avd[k] = R_pow(vdel[k]*(pv+0.5*pe),2);
   AvL[k] = R_pow(vlam[k]*pe,2);
    }
    else 
    { 
    if(score[k]==0.5)
    {
   ATR[w[k]] = ATR[w[k]] +var[w[k]]*(0.5-pv-0.5*pe);
   ATR[b[k]] = ATR[b[k]] +var[b[k]]*(0.5-pd-0.5*pe);

   ATD[w[k]] = ATD[w[k]] +R_pow(var[w[k]]*(0.5-pv-0.5*pe),2);
   ATD[b[k]] = ATD[b[k]] +R_pow(var[b[k]]*(0.5-pd-0.5*pe),2);

   ATd[k] = vdel[k]*(0.5-pv-0.5*pe) ;
   ATl[k] = vlam[k]*(1-pe) ;
   Avd[k] = R_pow(vdel[k]*(0.5-pd-0.5*pe),2) ;
   AvL[k] = R_pow(vlam[k]*(1-pe),2);
    }
    else
    {
   ATR[w[k]] = ATR[w[k]] +var[w[k]]*(1-pv-0.5*pe);
   ATR[b[k]] = ATR[b[k]] -var[b[k]]*(pd+0.5*pe);

   ATD[w[k]] = ATD[w[k]] +R_pow(var[w[k]]*(1-pv-0.5*pe),2);
   ATD[b[k]] = ATD[b[k]] +R_pow(var[b[k]]*(pd+0.5*pe),2);

   ATd[k] = vdel[k]*(1-pv-0.5*pe);
   ATl[k] = -vlam[k]*pe;
   Avd[k] = R_pow(vdel[k]*(1-pv-0.5*pe),2);
   AvL[k] = R_pow(vlam[k]*pe,2);
    }
    }

}


}

//------------------------------------------------------------------

//          Analise Descritiva e Preditiva

void Analise_c( int *nj, int *w, int *b,   double *yv, double *ye, double *yd,double *score, 
  double *RE,double *DE,double *RG,double *varG, double *del,double *RG2,double *varG2, double *del2, 
  double *RBP,  double *delBP , double *lamBP, double *LE2, double *DE2,double *LE3,double *DE3,
  double *LG2, double *DG2,double *LG3,double *DG3,
  double *LG22, double *DG22,double *LG23,double *DG23,
  double *LBP2, double *DBP2,double *LBP3,double *DBP3)
{

double qv = M_LN10/400;
double aux1,aux2,aux3,Den1,Aux1,Aux2,Aux3,Den2,G,GR,Aux21,Aux22,Aux23,Den22, G2,GR2;

	
  double Epv, Epe ,Epd;     // Probabilidade de vitória, empate e derrota -ELO
  
  double Gpv,Gpe,Gpd;  // Probabilidade de vitória, empate e derrota -Glickman

  double G2pv,G2pe,G2pd;  // Probabilidade de vitória, empate e derrota -Glickman
  
  double Bpv,Bpe,Bpd;  // Probabilidade de vitória, empate e derrota -- BP

  	
  int k;

  for(k=0;k<*nj;k++) {

	  
//--------------------ELO-----------------------

	  aux1 = 1/(1+ R_pow(10,(-RE[w[k]]+RE[b[k]]-DE[k])/400));
      aux2 = sqrt( aux1*(1-aux1));
	  aux3 = (1-aux1);
	  Den1 = (aux1+aux2+aux3);
	  
  Epv =  aux1/Den1;
  Epe =  aux2/Den1;
  Epd =  aux3/Den1;	

	  

  LE2[k] = score[k]*log(aux1)+(1-score[k])*log(1-aux1);
  DE2[k] =  R_pow(score[k]-aux1,2);
	  
  LE3[k] = yv[k]*log(Epv) +ye[k]*log(Epe)+yd[k]*log(Epd);		  
  DE3[k] = R_pow(yv[k]-Epv,2)+R_pow(ye[k]-Epe,2) +R_pow(yd[k]-Epd,2);

//-----------------Glickman--------------------------------------

	   G = sqrt(R_pow(varG[w[k]],2)+R_pow(varG[b[k]],2));
	  GR =  1/sqrt(1+3*R_pow(qv*G/M_PI,2));
	  
	Aux1 = 1/(1+R_pow(10,GR*(RG[b[k]]-RG[w[k]]-del[k])/400)); 
  	Aux2 = sqrt(Aux1*(1-Aux1));
	Aux3 = (1-Aux1);
	Den2 = (Aux1+Aux2+Aux3);
	  
  Gpv =  Aux1/Den2;
  Gpe =  Aux2/Den2;
  Gpd =  Aux3/Den2;		  

	  
  LG2[k] = score[k]*log(Aux1)+(1-score[k])*log(1-Aux1);
  DG2[k] =  R_pow(score[k]-Aux1,2);
	  
  LG3[k] = yv[k]*log(Gpv) +ye[k]*log(Gpe)+yd[k]*log(Gpd);		  
  DG3[k] = R_pow(yv[k]-Gpv,2)+R_pow(ye[k]-Gpe,2) +R_pow(yd[k]-Gpd,2);

//---------------------------------------------------------------------------------------

	   G2 = sqrt(R_pow(varG2[w[k]],2)+R_pow(varG2[b[k]],2));
	  GR2 =  1/sqrt(1+3*R_pow(qv*G2/M_PI,2));
	  
	Aux21 = 1/(1+R_pow(10,GR*(RG2[b[k]]-RG2[w[k]]-del2[k])/400)); 
  	Aux22 = sqrt(Aux21*(1-Aux21));
	Aux23 = (1-Aux21);
	Den22 = (Aux21+Aux22+Aux23);
	  
  G2pv =  Aux21/Den22;
  G2pe =  Aux22/Den22;
  G2pd =  Aux23/Den22;		  

	  
  LG22[k] = score[k]*log(Aux21)+(1-score[k])*log(1-Aux21);
  DG22[k] =  R_pow(score[k]-Aux21,2);
	  
  LG23[k] = yv[k]*log(G2pv) +ye[k]*log(G2pe)+yd[k]*log(G2pd);		  
  DG23[k] = R_pow(yv[k]-G2pv,2)+R_pow(ye[k]-G2pe,2) +R_pow(yd[k]-G2pd,2);
//----------------------------BP ---------------------------------------------

Bpv =  exp((RBP[w[k]]+delBP[k])*qv)/(exp((RBP[w[k]]+delBP[k])*qv)+exp(qv*lamBP[k]+qv*(RBP[w[k]]+RBP[b[k]]+delBP[k])/2)+exp(qv*RBP[b[k]]));
Bpe =  exp(qv*lamBP[k]+qv*(RBP[w[k]]+RBP[b[k]]+delBP[k])/2)/(exp((RBP[w[k]]+delBP[k])*qv)+exp(qv*lamBP[k]+qv*(RBP[w[k]]+RBP[b[k]]+delBP[k])/2)+exp(qv*RBP[b[k]]));
Bpd =  exp(qv*RBP[b[k]])/(exp((RBP[w[k]]+delBP[k])*qv)+exp(qv*lamBP[k]+qv*(RBP[w[k]]+RBP[b[k]]+delBP[k])/2)+exp(qv*RBP[b[k]]));

  LBP2[k] = score[k]*log(Bpv+Bpe/2)+(1-score[k])*log(1-Bpv-Bpe/2);
  DBP2[k] =  R_pow(score[k]-Bpv-Bpe/2,2);
	  
  LBP3[k] = yv[k]*log(Bpv) +ye[k]*log(Bpe) +yd[k]*log(Bpd);		  
  DBP3[k] = R_pow(yv[k]-Bpv,2) +R_pow(ye[k]-Bpe,2) +R_pow(yd[k]-Bpd,2);

if(score[k]==0.5){
 DE2[k] = 0.5*R_pow(1-aux1,2)+0.5*R_pow(0-aux1,2);
 DG2[k] = 0.5*R_pow(1-Aux1,2)+0.5*R_pow(0-Aux1,2); 
 DG22[k]= 0.5*R_pow(1-Aux21,2)+0.5*R_pow(0-Aux21,2);
 DBP2[k] = 0.5*R_pow(1-Bpv-Bpe/2,2)+0.5*R_pow(0-Bpv-Bpe/2,2);  
                  }
  }  



}


