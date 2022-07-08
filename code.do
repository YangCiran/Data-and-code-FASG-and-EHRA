use "C:\Users\yangc\Desktop\data.dta"


encode province , gen(id)
tab year,gen(dyear)
xtset id  year
gen ahrh=num_nurse + num_doc
gen lnahrh=ln(ahrh)


*************************************************
*Panel unit root test

xtline theil, overlay
xtunitroot llc theil,demean lags(bic 1)
xtunitroot ips theil,demean lags(bic 1)
xtunitroot fisher theil, dfuller drift lags(1) demean
xtunitroot fisher theil, pperron  lags(1) demean

*************************************************
xtline cv, overlay
xtunitroot llc cv,demean lags(bic 1)
xtunitroot ips cv,demean lags(bic 1)
xtunitroot fisher cv, dfuller drift lags(1) demean 
xtunitroot fisher cv, pperron  lags(1) demean
*************************************************

xtline gini, overlay
xtunitroot llc gini,demean lags(bic 1)
xtunitroot ips gini,demean lags(bic 1) 
xtunitroot fisher gini, dfuller drift lags(1) demean
xtunitroot fisher gini, pperron  lags(1) demean
*************************************************
xtline ahrh, overlay
xtunitroot llc ahrh,trend demean lags(bic 1)
xtunitroot ips ahrh,trend demean lags(bic 1)
xtunitroot fisher ahrh, dfuller trend lags(1) demean
xtunitroot fisher ahrh, pperron trend lags(1) demean
*************************************************
xtline lnahrh, overlay
xtunitroot llc lnahrh,trend demean lags(bic 1) 
xtunitroot ips lnahrh,trend demean lags(bic 1)  
xtunitroot fisher lnahrh, dfuller trend lags(1) demean
xtunitroot fisher lnahrh, pperron trend lags(1) demean
*************************************************
xtline x, overlay

xtunitroot llc x,trend demean lags(bic 1) 
xtunitroot ips x,trend demean lags(bic 1)  
xtunitroot fisher x, dfuller trend lags(1) demean 
xtunitroot fisher x, pperron trend lags(1) demean
*************************************************
xtline lnpgdp, overlay
xtunitroot llc lnpgdp,demean lags(bic 1)
xtunitroot ips lnpgdp,demean lags(bic 1)
xtunitroot fisher lnpgdp, dfuller drift lags(1) demean
xtunitroot fisher lnpgdp, pperron  lags(1) demean
*************************************************
xtline lnpd, overlay 
xtunitroot llc lnpd,demean lags(bic 1)
xtunitroot ips lnpd,demean lags(bic 1)  
xtunitroot fisher lnpd, dfuller drift lags(1) demean 
xtunitroot fisher lnpd, pperron  lags(1) demean
*************************************************
xtline ta, overlay
xtunitroot llc ta,trend demean lags(bic 1)
xtunitroot ips ta,trend demean lags(bic 1)
xtunitroot fisher ta, dfuller trend lags(1) demean
xtunitroot fisher ta, pperron trend lags(1) demean 
*************************************************
xtline dr, overlay
xtunitroot llc dr,trend demean lags(bic 1)   
xtunitroot ips dr,trend demean lags(bic 1)    
xtunitroot fisher dr, dfuller trend lags(1) demean  
xtunitroot fisher dr, pperron trend lags(1) demean  
*************************************************
xtline ir, overlay
xtunitroot llc ir,demean lags(bic 1)  
xtunitroot ips ir,demean lags(bic 1)  
xtunitroot fisher ir, dfuller drift lags(1) demean
xtunitroot fisher ir, pperron  lags(1) demean 
*************************************************
* Calculation results of EHRA index for each province
sum theil if id ==1
sum theil if id ==2
sum theil if id ==3
sum theil if id ==4
sum theil if id ==5
sum theil if id ==6
sum theil if id ==7
sum theil if id ==8
sum theil if id ==9
sum theil if id ==10
sum theil if id ==11
sum theil if id ==12
sum theil if id ==13
sum theil if id ==14
sum theil if id ==15
sum theil if id ==16
sum theil if id ==17
sum theil if id ==18
sum theil if id ==19
sum theil if id ==20
sum theil if id ==21
sum theil if id ==22

*************************************************
* descriptive statistics of different variables

summarize cv theil gini  x x1 lnpd lnpgdp ta dr ir
tabstat theil cv  gini  x lnpgdp lnpd   ta dr ir, s(mean sd p25 med p75 min max) c(s) f(%8.4f)

twoway (scatter theil x) (lfit  theil x)
reg theil  x lnpgdp lnpd  ta dr ir dyear2-dyear10  i.id //LSDV
avplot x  
avplots  
xtline theil 
xtline x 

reg cv  x lnpgdp lnpd  ta dr ir,r
estat vif

xtreg theil x lnpd  lnpgdp ta dr ir i.year,fe 
**model choosing**

*1.fe and ols*
xtreg theil x lnpgdp lnpd   ta dr ir i.year,fe 
xtcsd,pes
xtcsd,fri
xtcsd,fre
*fre test's alpha=0.01:0.5198，
testparm  _Iid*  //province dummy variable F test
*choosed fe model

*2.ols and re*
xtreg theil x lnpgdp lnpd   ta dr ir dyear2-dyear10,re  //re regression
xttest0  //LM test
xttest1  //LM test

*choosed re model

*3.re and fe*
*Husman test1*
xtreg theil x lnpgdp lnpd   ta dr ir dyear2-dyear10, fe   //fe regression
est store FE   
xtreg theil x lnpgdp lnpd   ta dr ir dyear2-dyear10, re   //re regression
hausman FE, sigmamore 

*Hausman test2*
quietly xtreg theil x lnpgdp lnpd  ta dr ir dyear2 dyear3 dyear4 dyear5 dyear6 dyear7 dyear8 dyear9 dyear10, re

scalar theta=e(theta)

global yandxforhausman theil x lnpgdp lnpd ta dr ir dyear2 dyear3 dyear4 dyear5 dyear6 dyear7 dyear8 dyear9 dyear10

sort id
foreach x of varlist $yandxforhausman{
     by id:egen mean`x'=mean(`x') 
     gen md`x'=`x'-mean`x' 
     gen red`x'=`x'-theta*mean`x' 
      }
quietly  reg redtheil redx redlnpgdp redlnpd   redta reddr redir reddyear2 reddyear3 reddyear4 reddyear5 reddyear6 reddyear7 reddyear8 reddyear9 reddyear10 mdx  mdlnpgdp mdlnpd   mdta mddr mdir  mddyear2 mddyear3 mddyear4 mddyear5 mddyear6 mddyear7 mddyear8 mddyear9 mddyear10, vce(cluster id)

test mdx  mdlnpgdp mdlnpd   mdta mddr mdir  mddyear2 mddyear3 mddyear4 mddyear5 mddyear6 mddyear7 mddyear8 mddyear9 mddyear10

*Hausman test3*
xtreg theil x lnpgdp lnpd   ta dr ir i.year,re  //re regression
xtcsd,pes
xtcsd,fri
xtcsd,fre

quietly xtscc redtheil redx redlnpgdp redlnpd   redta reddr redir reddyear2 reddyear3 reddyear4 reddyear5 reddyear6 reddyear7 reddyear8 reddyear9 reddyear10 mdx mdlnpgdp  mdlnpd   mdta mddr mdir  mddyear2 mddyear3 mddyear4 mddyear5 mddyear6 mddyear7 mddyear8 mddyear9 mddyear10

test mdx  mdlnpgdp mdlnpd   mdta mddr mdir  mddyear2 mddyear3 mddyear4 mddyear5 mddyear6 mddyear7 mddyear8 mddyear9 mddyear10

*choosed fe model

**regressiong 
*vif test
reg theil x lnpgdp lnpd  ta dr ir
estat vif
*1.fe regression *
*cross-section correlation test
xtreg theil x lnpgdp lnpd   ta dr ir dyear2-dyear10, fe 
xtcsd,pes abs 
xtcsd,fri abs 
xtcsd,fre abs
*heteroskedasticity test*
xtreg theil x lnpgdp lnpd   ta dr ir dyear2-dyear10 , fe 
xttest3 
*autocorrelation test *
tab id,gen(id)
xtserial theil x lnpgdp lnpd  ta dr ir  id2-id22 dyear2-dyear10
*The estimated results after dealing with three major problems
xtscc theil x lnpd  lnpgdp ta dr ir  dyear2-dyear10,fe

reg theil x lnpgdp lnpd   ta dr ir  dyear2 dyear3 dyear4 dyear5 dyear6 dyear7 dyear8 dyear9 dyear10  id2 id3 id4 id5 id6 id7 id8 id9 id10 id11 id12 id13 id14 id15 id16 id17 id18 id19 id20 id21 id22,r
est store twoOLS

xtreg  theil x lnpgdp lnpd   ta dr ir  ,fe r
est store oneFE
xtreg theil x lnpgdp lnpd   ta dr ir   dyear2-dyear10,fe r
est store towFE
xtscc theil x i.year,fe
est store FEdk
xtscc theil x lnpgdp lnpd   ta dr ir,fe
est store onedk 
xtscc theil x lnpgdp lnpd   ta dr ir  dyear2-dyear10,fe
est store twodk 
*2.fe regression*
xtreg theil x lnpgdp lnpd   ta dr ir dyear2-dyear10, re r  
est store re1
xtscc redtheil redx redlnpgdp redlnpd   redta reddr redir reddyear2 reddyear3 reddyear4 reddyear5 reddyear6 reddyear7 reddyear8 reddyear9 reddyear10 ,re
est store re2
reg theil x lnpgdp lnpd   ta dr ir dyear2-dyear10 i.id,r
est store OLS
esttab FEdk onedk twodk towFE re1 OLS using tab3.rtf ,b(%10.4f) ar2(%10.4f) se(%10.4f)  mtitle(FEdk onedk twodk towFE re1 OLS) star (* 0.1 ** 0.05 *** 0.01) replace

***Robustness test
*1.replace  dependent variable  theil、gini
xtscc  cv  x lnpgdp lnpd   ta dr ir dyear2-dyear10,fe 
est store cv

xtscc  gini  x lnpgdp lnpd   ta dr ir  dyear2-dyear10,fe 
est store gini

*2.delete samples of 2020
xtreg  theil  x lnpgdp lnpd   ta dr ir  dyear3-dyear9 if year !=2020 ,fe r
est store shijian
*3.add additional variable x2 servi x19
xtscc  theil  x lnpgdp lnpd   ta dr ir servi pubserv edu dyear2-dyear10,fe
est store kongzhi

esttab cv  gini shijian kongzhi using tab4.rtf ,b(%10.4f) ar2(%10.4f) se(%10.4f)  mtitle(cv  gini shijian kongzhi) star (* 0.1 ** 0.05 *** 0.01) replace
*************************************************
*************************************************
** Endogenous treatment**

xtivreg theil   lnpgdp lnpd ta dr ir  i.year (x = L1.x), fe  first
est store re2sls

*************************************************
**Mechanism analysis**
xtscc  ahrh  x lnpgdp lnpd ta dr ir  i.year,fe  //channel one
est store jizhi1

xtscc  lnahrh  x lnpgdp lnpd   ta dr ir  i.year,fe  //channel tow 
est store jizhi2

esttab   jizhi1 jizhi2 using tab4.rtf ,b(%10.4f) ar2(%10.4f) se(%10.4f)  mtitle(  jizhi1 jizhi2) star (* 0.1 ** 0.05 *** 0.01) replace 

**Heterogeneity analysis: two-way FE threshold regression**
xi:xthreg theil  lnpd  ta dr ir i.year, rx(x) qx(lnpgdp) thnum(2) grid(400) trim(0.01 0.01 ) bs(300 300 )
 _matplot e(LR21), columns(1 2) yline(7.35, lpattern(dash)) connect(direct) msize(small) mlabp(0) mlabs(zero) ytitle("LR Statistics") xtitle("First Threshold") recast(line) name(LR1) nodraw
 _matplot e(LR22), columns(1 2) yline(7.35, lpattern(dash)) connect(direct) msize(small)  mlabp(0) mlabs(zero) ytitle("LR Statistics") xtitle("Second Threshold") recast(line) name(LR2) nodraw

xi:xthreg theil   lnpgdp  ta dr ir i.year, rx(x) qx(lnpd) thnum(1) grid(400) trim(0.01 ) bs( 300 )
 _matplot e(LR), columns(1 2) yline(7.35, lpattern(dash)) connect(direct)  msize(small) mlabp(0) mlabs(zero) ytitle("LR Statistics") xtitle("Threshold")  recast(line) name(LR01) nodraw
 
xi:xthreg theil  lnpd  lnpgdp ta  ir i.year, rx(x) qx(dr) thnum(1) grid(400) trim(0.01 ) bs(  300 ) 
  _matplot e(LR), columns(1 2) yline(7.35, lpattern(dash)) connect(direct)  msize(small) mlabp(0) mlabs(zero) ytitle("LR Statistics") xtitle("Threshold")  recast(line) name(LR02) nodraw

 graph combine LR1 LR2 LR01 LR02, cols(2)



  
 
 
 
 