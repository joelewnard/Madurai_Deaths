library(lubridate)
library(MASS)
library(readxl)



predates = dmy('01-03-2018'):dmy('30-07-2019')
pandates = dmy('01-03-2020'):dmy('30-07-2021')

dates0W1 = as.numeric(c(dmy('01-06-2019'):dmy('30-09-2019'),
                        dmy('01-06-2018'):dmy('30-09-2018')))
dates1W1 = as.numeric(c(dmy('01-06-2020'):dmy('30-09-2020')))

dates0W2 = as.numeric(c(dmy('16-03-2019'):dmy('15-07-2019'),
                        dmy('16-03-2018'):dmy('15-07-2018')))
dates1W2 = as.numeric(c(dmy('16-03-2021'):dmy('15-07-2021')))

dates0W0 = as.numeric(c(dmy('24-03-2018'):dmy('31-05-2018'),
                        dmy('24-03-2019'):dmy('31-05-2019')))
dates1W0 = as.numeric(c(dmy('24-03-2020'):dmy('31-05-2020')))

dates0Ctl = as.numeric(c(dmy('01-03-2018'):dmy('23-03-2018'),
                        dmy('01-03-2019'):dmy('23-03-2019')))
dates1Ctl = as.numeric(c(dmy('01-03-2020'):dmy('23-03-2020')))

predatesNum = as.numeric(predates)
pandatesNum = as.numeric(pandates)

dates0Tot = predatesNum
dates1Tot = pandatesNum

length0Tot = length(predatesNum); length1Tot = length(pandatesNum)
length0W0 = length(dates0W0); length1W0 = length(dates1W0)
length0W1 = length(dates0W1); length1W1 = length(dates1W1)
length0W2 = length(dates0W2); length1W2 = length(dates1W2)
length0Ctl = length(dates0Ctl); length1Ctl = length(dates1Ctl)


sexes = c(1,0)
agelb = seq(0,80,10)
ageub = c(seq(10,80,10),1e3)
hosps = c(1,0)

cause0 = death0$cause0; cause1 = death1$cause1
causes0 = unique(cause0); causes1 = unique(cause1)
causes = unique(c(causes0,causes1))

bigcat = c('noncom','injury','injury','noncom','oth','noncom','injury','ID',
           'oth','injury','noncom','noncom','ID','noncom','ID','noncom',
           'ID','ID','noncom','injury','ID','injury','noncom','ID',
           'ID','ID','ID','injury','noncom','ID','ID','ID',
           'ID','ID','ID','ID','ID','ID','injury','ID',
           'covid','oth')
smallcat = c('cvd','unintent','intent','cvd','senility','diab','unintent','mat',
             'uncat','intent','oth noncom','cancer','lrti','oth noncom','lrti','liver',
             'std','tb','oth noncom','unintent','gastro','unintent','oth noncom','oth ID',
             'gastro','oth ID','mat','unintent','oth noncom','oth ID','gastro','oth ID',
             'oth ID','oth ID','oth ID','oth ID','lrti','oth ID','unintent','lrti',
             'covid','uncat')

bigcats = unique(bigcat); smallcats = unique(smallcat)

countTot = c(sum(death0$dodNum0%in%dates0Tot),sum(death1$dodNum1%in%dates1Tot))
countW0 = c(sum(death0$dodNum0%in%dates0W0),sum(death1$dodNum1%in%dates1W0))
countW1 = c(sum(death0$dodNum0%in%dates0W1),sum(death1$dodNum1%in%dates1W1))
countW2 = c(sum(death0$dodNum0%in%dates0W2),sum(death1$dodNum1%in%dates1W2))
countCtl = c(sum(death0$dodNum0%in%dates0Ctl),sum(death1$dodNum1%in%dates1Ctl))

denomTot = c(length0Tot,length1Tot*popChange)
denomW0 = c(length0W0,length1W0*popChange)
denomW1 = c(length0W1,length1W1*popChange)
denomW2 = c(length0W2,length1W2*popChange)
denomCtl = c(length0Ctl,length1Ctl*popChange)


countSexTot = countSexW0 = countSexW1 = countSexW2 = countSexCtl = array(NA,dim=c(length(sexes),2))
denomSexTot = denomSexW0 = denomSexW1 = denomSexW2 = denomSexCtl = array(NA,dim=dim(countSexTot))
for (j in 1:length(sexes)){
  countSexTot[j,1] = sum(death0$sex0==sexes[j]&death0$dodNum0%in%dates0Tot)
  countSexTot[j,2] = sum(death1$sex1==sexes[j]&death1$dodNum1%in%dates1Tot)
  denomSexTot[j,1] = length0Tot
  denomSexTot[j,2] = length1Tot*popChangeSex[j]
  
  countSexW0[j,1] = sum(death0$sex0==sexes[j]&death0$dodNum0%in%dates0W0)
  countSexW0[j,2] = sum(death1$sex1==sexes[j]&death1$dodNum1%in%dates1W0)
  denomSexW0[j,1] = length0W0
  denomSexW0[j,2] = length1W0*popChangeSex[j]
  
  countSexW1[j,1] = sum(death0$sex0==sexes[j]&death0$dodNum0%in%dates0W1)
  countSexW1[j,2] = sum(death1$sex1==sexes[j]&death1$dodNum1%in%dates1W1)
  denomSexW1[j,1] = length0W1
  denomSexW1[j,2] = length1W1*popChangeSex[j]
  
  countSexW2[j,1] = sum(death0$sex0==sexes[j]&death0$dodNum0%in%dates0W2)
  countSexW2[j,2] = sum(death1$sex1==sexes[j]&death1$dodNum1%in%dates1W2)
  denomSexW2[j,1] = length0W2
  denomSexW2[j,2] = length1W2*popChangeSex[j]
  
  countSexCtl[j,1] = sum(death0$sex0==sexes[j]&death0$dodNum0%in%dates0Ctl)
  countSexCtl[j,2] = sum(death1$sex1==sexes[j]&death1$dodNum1%in%dates1Ctl)
  denomSexCtl[j,1] = length0Ctl
  denomSexCtl[j,2] = length1Ctl*popChangeSex[j]
}

countAgeTot = countAgeW0 = countAgeW1 = countAgeW2 = countAgeCtl = array(NA,dim=c(length(agelb),2))
denomAgeTot = denomAgeW0 = denomAgeW1 = denomAgeW2 = denomAgeCtl = array(NA,dim=dim(countAgeTot))
for (i in 1:length(agelb)){
  countAgeTot[i,1] = sum(death0$age0>=agelb[i]&death0$age0<ageub[i]&death0$dodNum0%in%dates0Tot)
  countAgeTot[i,2] = sum(death1$age1>=agelb[i]&death1$age1<ageub[i]&death1$dodNum1%in%dates1Tot)
  denomAgeTot[i,1] = length0Tot
  denomAgeTot[i,2] = length1Tot*popChangeAge[i]
  
  countAgeW0[i,1] = sum(death0$age0>=agelb[i]&death0$age0<ageub[i]&death0$dodNum0%in%dates0W0)
  countAgeW0[i,2] = sum(death1$age1>=agelb[i]&death1$age1<ageub[i]&death1$dodNum1%in%dates1W0)
  denomAgeW0[i,1] = length0W0
  denomAgeW0[i,2] = length1W0*popChangeAge[i]

  countAgeW1[i,1] = sum(death0$age0>=agelb[i]&death0$age0<ageub[i]&death0$dodNum0%in%dates0W1)
  countAgeW1[i,2] = sum(death1$age1>=agelb[i]&death1$age1<ageub[i]&death1$dodNum1%in%dates1W1)
  denomAgeW1[i,1] = length0W1
  denomAgeW1[i,2] = length1W1*popChangeAge[i]
  
  countAgeW2[i,1] = sum(death0$age0>=agelb[i]&death0$age0<ageub[i]&death0$dodNum0%in%dates0W2)
  countAgeW2[i,2] = sum(death1$age1>=agelb[i]&death1$age1<ageub[i]&death1$dodNum1%in%dates1W2)
  denomAgeW2[i,1] = length0W2
  denomAgeW2[i,2] = length1W2*popChangeAge[i]
  
  countAgeCtl[i,1] = sum(death0$age0>=agelb[i]&death0$age0<ageub[i]&death0$dodNum0%in%dates0Ctl)
  countAgeCtl[i,2] = sum(death1$age1>=agelb[i]&death1$age1<ageub[i]&death1$dodNum1%in%dates1Ctl)
  denomAgeCtl[i,1] = length0Ctl
  denomAgeCtl[i,2] = length1Ctl*popChangeAge[i]
}


countAgeSexTot = countAgeSexW0 = countAgeSexW1 = countAgeSexW2 = countAgeSexCtl = array(NA,dim=c(length(agelb),length(sexes),2))
denomAgeSexTot = denomAgeSexW0 = denomAgeSexW1 = denomAgeSexW2 = denomAgeSexCtl = array(NA,dim=dim(countAgeSexTot))
for (i in 1:length(agelb)){
  for (j in 1:length(sexes)){
    countAgeSexTot[i,j,1] = sum(death0$age0>=agelb[i]&death0$age0<ageub[i]&death0$sex0==sexes[j]&death0$dodNum0%in%dates0Tot)
    countAgeSexTot[i,j,2] = sum(death1$age1>=agelb[i]&death1$age1<ageub[i]&death1$sex1==sexes[j]&death1$dodNum1%in%dates1Tot)
    denomAgeSexTot[i,j,1] = length0Tot
    denomAgeSexTot[i,j,2] = length1Tot*popChangeAgeSex[i,j]
    
    countAgeSexW0[i,j,1] = sum(death0$age0>=agelb[i]&death0$age0<ageub[i]&death0$sex0==sexes[j]&death0$dodNum0%in%dates0W0)
    countAgeSexW0[i,j,2] = sum(death1$age1>=agelb[i]&death1$age1<ageub[i]&death1$sex1==sexes[j]&death1$dodNum1%in%dates1W0)
    denomAgeSexW0[i,j,1] = length0W0
    denomAgeSexW0[i,j,2] = length1W0*popChangeAgeSex[i,j]
    
    countAgeSexW1[i,j,1] = sum(death0$age0>=agelb[i]&death0$age0<ageub[i]&death0$sex0==sexes[j]&death0$dodNum0%in%dates0W1)
    countAgeSexW1[i,j,2] = sum(death1$age1>=agelb[i]&death1$age1<ageub[i]&death1$sex1==sexes[j]&death1$dodNum1%in%dates1W1)
    denomAgeSexW1[i,j,1] = length0W1
    denomAgeSexW1[i,j,2] = length1W1*popChangeAgeSex[i,j]
    
    countAgeSexW2[i,j,1] = sum(death0$age0>=agelb[i]&death0$age0<ageub[i]&death0$sex0==sexes[j]&death0$dodNum0%in%dates0W2)
    countAgeSexW2[i,j,2] = sum(death1$age1>=agelb[i]&death1$age1<ageub[i]&death1$sex1==sexes[j]&death1$dodNum1%in%dates1W2)
    denomAgeSexW2[i,j,1] = length0W2
    denomAgeSexW2[i,j,2] = length1W2*popChangeAgeSex[i,j]
    
    countAgeSexCtl[i,j,1] = sum(death0$age0>=agelb[i]&death0$age0<ageub[i]&death0$sex0==sexes[j]&death0$dodNum0%in%dates0Ctl)
    countAgeSexCtl[i,j,2] = sum(death1$age1>=agelb[i]&death1$age1<ageub[i]&death1$sex1==sexes[j]&death1$dodNum1%in%dates1Ctl)
    denomAgeSexCtl[i,j,1] = length0Ctl
    denomAgeSexCtl[i,j,2] = length1Ctl*popChangeAgeSex[i,j]
  }
}


countFacTot = countFacW0 = countFacW1 = countFacW2 = countFacCtl = array(NA,dim=c(length(hosps),2))
for (k in 1:2){
  countFacTot[k,1] = sum(death0$dodNum0%in%dates0Tot&death0$inhosp0==hosps[k])  
  countFacTot[k,2] = sum(death1$dodNum1%in%dates1Tot&death1$inhosp1==hosps[k])  
  
  countFacW0[k,1] = sum(death0$dodNum0%in%dates0W0&death0$inhosp0==hosps[k])  
  countFacW0[k,2] = sum(death1$dodNum1%in%dates1W0&death1$inhosp1==hosps[k])  
  
  countFacW1[k,1] = sum(death0$dodNum0%in%dates0W1&death0$inhosp0==hosps[k])  
  countFacW1[k,2] = sum(death1$dodNum1%in%dates1W1&death1$inhosp1==hosps[k])  
  
  countFacW2[k,1] = sum(death0$dodNum0%in%dates0W2&death0$inhosp0==hosps[k])  
  countFacW2[k,2] = sum(death1$dodNum1%in%dates1W2&death1$inhosp1==hosps[k]) 
  
  countFacCtl[k,1] = sum(death0$dodNum0%in%dates0Ctl&death0$inhosp0==hosps[k])  
  countFacCtl[k,2] = sum(death1$dodNum1%in%dates1Ctl&death1$inhosp1==hosps[k]) 
}


countSexFacTot = countSexFacW0 = countSexFacW1 = countSexFacW2 = array(NA,dim=c(length(sexes),length(hosps),2))
for (j in 1:2){
  for (k in 1:2){
    countSexFacTot[j,k,1] = sum(death0$sex0==sexes[j]&death0$dodNum0%in%dates0Tot&death0$inhosp0==hosps[k])  
    countSexFacTot[j,k,2] = sum(death1$sex1==sexes[j]&death1$dodNum1%in%dates1Tot&death1$inhosp1==hosps[k])  
    
    countSexFacW0[j,k,1] = sum(death0$sex0==sexes[j]&death0$dodNum0%in%dates0W0&death0$inhosp0==hosps[k])  
    countSexFacW0[j,k,2] = sum(death1$sex1==sexes[j]&death1$dodNum1%in%dates1W0&death1$inhosp1==hosps[k])  
    
    countSexFacW1[j,k,1] = sum(death0$sex0==sexes[j]&death0$dodNum0%in%dates0W1&death0$inhosp0==hosps[k])  
    countSexFacW1[j,k,2] = sum(death1$sex1==sexes[j]&death1$dodNum1%in%dates1W1&death1$inhosp1==hosps[k])  
    
    countSexFacW2[j,k,1] = sum(death0$sex0==sexes[j]&death0$dodNum0%in%dates0W2&death0$inhosp0==hosps[k])  
    countSexFacW2[j,k,2] = sum(death1$sex1==sexes[j]&death1$dodNum1%in%dates1W2&death1$inhosp1==hosps[k])  
  }
} 

countAgeFacTot = countAgeFacW0 = countAgeFacW1 = countAgeFacW2 = array(NA,dim=c(length(agelb),length(hosps),2))
for (i in 1:length(agelb)){
  for (k in 1:2){
    countAgeFacTot[i,k,1] = sum(death0$age0>=agelb[j]&death0$age0<ageub[i]&death0$dodNum0%in%dates0Tot&death0$inhosp0==hosps[k])  
    countAgeFacTot[i,k,2] = sum(death1$age1>=agelb[j]&death1$age1<ageub[i]&death1$dodNum1%in%dates1Tot&death1$inhosp1==hosps[k])  
    
    countAgeFacW0[i,k,1] = sum(death0$age0>=agelb[j]&death0$age0<ageub[i]&death0$dodNum0%in%dates0W0&death0$inhosp0==hosps[k])  
    countAgeFacW0[i,k,2] = sum(death1$age1>=agelb[j]&death1$age1<ageub[i]&death1$dodNum1%in%dates1W0&death1$inhosp1==hosps[k]) 
    
    countAgeFacW1[i,k,1] = sum(death0$age0>=agelb[j]&death0$age0<ageub[i]&death0$dodNum0%in%dates0W1&death0$inhosp0==hosps[k])  
    countAgeFacW1[i,k,2] = sum(death1$age1>=agelb[j]&death1$age1<ageub[i]&death1$dodNum1%in%dates1W1&death1$inhosp1==hosps[k]) 
    
    countAgeFacW2[i,k,1] = sum(death0$age0>=agelb[j]&death0$age0<ageub[i]&death0$dodNum0%in%dates0W2&death0$inhosp0==hosps[k])  
    countAgeFacW2[i,k,2] = sum(death1$age1>=agelb[j]&death1$age1<ageub[i]&death1$dodNum1%in%dates1W2&death1$inhosp1==hosps[k]) 
  }
} 

countAgeSexFacTot = countAgeSexFacW0 = countAgeSexFacW1 = countAgeSexFacW2 = array(NA,dim=c(length(agelb),length(sexes),length(hosps),2))
for (i in 1:length(agelb)){
  for (j in 1:2){
    for (k in 1:2){
      countAgeSexFacTot[i,j,k,1] = sum(death0$age0>=agelb[j]&death0$age0<ageub[i]&death0$sex0==sexes[j]&death0$dodNum0%in%dates0Tot&death0$inhosp0==hosps[k])  
      countAgeSexFacTot[i,j,k,2] = sum(death1$age1>=agelb[j]&death1$age1<ageub[i]&death1$sex1==sexes[j]&death1$dodNum1%in%dates1Tot&death1$inhosp1==hosps[k])  
      
      countAgeSexFacW0[i,j,k,1] = sum(death0$age0>=agelb[j]&death0$age0<ageub[i]&death0$sex0==sexes[j]&death0$dodNum0%in%dates0W0&death0$inhosp0==hosps[k])  
      countAgeSexFacW0[i,j,k,2] = sum(death1$age1>=agelb[j]&death1$age1<ageub[i]&death1$sex1==sexes[j]&death1$dodNum1%in%dates1W0&death1$inhosp1==hosps[k]) 
      
      countAgeSexFacW1[i,j,k,1] = sum(death0$age0>=agelb[j]&death0$age0<ageub[i]&death0$sex0==sexes[j]&death0$dodNum0%in%dates0W1&death0$inhosp0==hosps[k])  
      countAgeSexFacW1[i,j,k,2] = sum(death1$age1>=agelb[j]&death1$age1<ageub[i]&death1$sex1==sexes[j]&death1$dodNum1%in%dates1W1&death1$inhosp1==hosps[k]) 
      
      countAgeSexFacW2[i,j,k,1] = sum(death0$age0>=agelb[j]&death0$age0<ageub[i]&death0$sex0==sexes[j]&death0$dodNum0%in%dates0W2&death0$inhosp0==hosps[k])  
      countAgeSexFacW2[i,j,k,2] = sum(death1$age1>=agelb[j]&death1$age1<ageub[i]&death1$sex1==sexes[j]&death1$dodNum1%in%dates1W2&death1$inhosp1==hosps[k]) 
    }
  } 
}

death0$bigcat = NA; death0$smallcat = NA; death1$bigcat = NA; death1$smallcat = NA
for (i in 1:length(causes)){
  death0$bigcat[which(death0$cause0==causes[i])] = bigcat[i]
  death1$bigcat[which(death1$cause1==causes[i])] = bigcat[i]
  
  death0$smallcat[which(death0$cause0==causes[i])] = smallcat[i]
  death1$smallcat[which(death1$cause1==causes[i])] = smallcat[i]
}



countBigTot = countBigW0 = countBigW1 = countBigW2 = countBigCtl = array(NA,dim=c(length(bigcats),2))
countSmallTot = countSmallW0 = countSmallW1 = countSmallW2 = countSmallCtl = array(NA,dim=c(length(smallcats),2))
countBigFacTot = countBigFacW0 = countBigFacW1 = countBigFacW2 = countBigFacCtl = array(NA,dim=c(length(bigcats),length(hosps),2))
countSmallFacTot = countSmallFacW0 = countSmallFacW1 = countSmallFacW2 = countSmallFacCtl = array(NA,dim=c(length(smallcats),length(hosps),2))
for (i in 1:length(bigcats)){
  countBigTot[i,1] = sum(death0$dodNum0%in%dates0Tot&death0$bigcat==bigcats[i])
  countBigTot[i,2] = sum(death1$dodNum1%in%dates1Tot&death1$bigcat==bigcats[i])
  
  countBigW0[i,1] = sum(death0$dodNum0%in%dates0W0&death0$bigcat==bigcats[i])
  countBigW0[i,2] = sum(death1$dodNum1%in%dates1W0&death1$bigcat==bigcats[i])
  
  countBigW1[i,1] = sum(death0$dodNum0%in%dates0W1&death0$bigcat==bigcats[i])
  countBigW1[i,2] = sum(death1$dodNum1%in%dates1W1&death1$bigcat==bigcats[i])
  
  countBigW2[i,1] = sum(death0$dodNum0%in%dates0W2&death0$bigcat==bigcats[i])
  countBigW2[i,2] = sum(death1$dodNum1%in%dates1W2&death1$bigcat==bigcats[i])
  
  countBigCtl[i,1] = sum(death0$dodNum0%in%dates0Ctl&death0$bigcat==bigcats[i])
  countBigCtl[i,2] = sum(death1$dodNum1%in%dates1Ctl&death1$bigcat==bigcats[i])
  
  for (j in 1:length(hosps)){
    countBigFacTot[i,j,1] = sum(death0$dodNum0%in%dates0Tot&death0$bigcat==bigcats[i]&death0$inhosp0==hosps[j])
    countBigFacTot[i,j,2] = sum(death1$dodNum1%in%dates1Tot&death1$bigcat==bigcats[i]&death1$inhosp1==hosps[j])
    
    countBigFacW0[i,j,1] = sum(death0$dodNum0%in%dates0W0&death0$bigcat==bigcats[i]&death0$inhosp0==hosps[j])
    countBigFacW0[i,j,2] = sum(death1$dodNum1%in%dates1W0&death1$bigcat==bigcats[i]&death1$inhosp1==hosps[j])
    
    countBigFacW1[i,j,1] = sum(death0$dodNum0%in%dates0W1&death0$bigcat==bigcats[i]&death0$inhosp0==hosps[j])
    countBigFacW1[i,j,2] = sum(death1$dodNum1%in%dates1W1&death1$bigcat==bigcats[i]&death1$inhosp1==hosps[j])
    
    countBigFacW2[i,j,1] = sum(death0$dodNum0%in%dates0W2&death0$bigcat==bigcats[i]&death0$inhosp0==hosps[j])
    countBigFacW2[i,j,2] = sum(death1$dodNum1%in%dates1W2&death1$bigcat==bigcats[i]&death1$inhosp1==hosps[j])
    
    countBigFacCtl[i,j,1] = sum(death0$dodNum0%in%dates0Ctl&death0$bigcat==bigcats[i]&death0$inhosp0==hosps[j])
    countBigFacCtl[i,j,2] = sum(death1$dodNum1%in%dates1Ctl&death1$bigcat==bigcats[i]&death1$inhosp1==hosps[j])
  }
}

for (i in 1:length(smallcats)){
  countSmallTot[i,1] = sum(death0$dodNum0%in%dates0Tot&death0$smallcat==smallcats[i])
  countSmallTot[i,2] = sum(death1$dodNum1%in%dates1Tot&death1$smallcat==smallcats[i])
  
  countSmallW0[i,1] = sum(death0$dodNum0%in%dates0W0&death0$smallcat==smallcats[i])
  countSmallW0[i,2] = sum(death1$dodNum1%in%dates1W0&death1$smallcat==smallcats[i])
  
  countSmallW1[i,1] = sum(death0$dodNum0%in%dates0W1&death0$smallcat==smallcats[i])
  countSmallW1[i,2] = sum(death1$dodNum1%in%dates1W1&death1$smallcat==smallcats[i])
  
  countSmallW2[i,1] = sum(death0$dodNum0%in%dates0W2&death0$smallcat==smallcats[i])
  countSmallW2[i,2] = sum(death1$dodNum1%in%dates1W2&death1$smallcat==smallcats[i])
  
  countSmallCtl[i,1] = sum(death0$dodNum0%in%dates0Ctl&death0$smallcat==smallcats[i])
  countSmallCtl[i,2] = sum(death1$dodNum1%in%dates1Ctl&death1$smallcat==smallcats[i])
  
  for (j in 1:length(hosps)){
    countSmallFacTot[i,j,1] = sum(death0$dodNum0%in%dates0Tot&death0$smallcat==smallcats[i]&death0$inhosp0==hosps[j])
    countSmallFacTot[i,j,2] = sum(death1$dodNum1%in%dates1Tot&death1$smallcat==smallcats[i]&death1$inhosp1==hosps[j])
    
    countSmallFacW0[i,j,1] = sum(death0$dodNum0%in%dates0W0&death0$smallcat==smallcats[i]&death0$inhosp0==hosps[j])
    countSmallFacW0[i,j,2] = sum(death1$dodNum1%in%dates1W0&death1$smallcat==smallcats[i]&death1$inhosp1==hosps[j])
    
    countSmallFacW1[i,j,1] = sum(death0$dodNum0%in%dates0W1&death0$smallcat==smallcats[i]&death0$inhosp0==hosps[j])
    countSmallFacW1[i,j,2] = sum(death1$dodNum1%in%dates1W1&death1$smallcat==smallcats[i]&death1$inhosp1==hosps[j])
    
    countSmallFacW2[i,j,1] = sum(death0$dodNum0%in%dates0W2&death0$smallcat==smallcats[i]&death0$inhosp0==hosps[j])
    countSmallFacW2[i,j,2] = sum(death1$dodNum1%in%dates1W2&death1$smallcat==smallcats[i]&death1$inhosp1==hosps[j])
    
    countSmallFacCtl[i,j,1] = sum(death0$dodNum0%in%dates0Ctl&death0$smallcat==smallcats[i]&death0$inhosp0==hosps[j])
    countSmallFacCtl[i,j,2] = sum(death1$dodNum1%in%dates1Ctl&death1$smallcat==smallcats[i]&death1$inhosp1==hosps[j])
  }
}




wards = unique(c(death0$ward0,death1$ward1))
wards[11:99] = wards[12:100]; wards[100] = 'WARD 100'

countWardTot = countWardW0 = countWardW1 = countWardW2 = countWardCtl = array(NA,dim=c(length(wards),2))
for (j in 1:length(wards)){
  countWardTot[j,1] = sum(death0$dodNum0%in%dates0Tot&death0$ward0==wards[j])
  countWardTot[j,2] = sum(death1$dodNum1%in%dates1Tot&death1$ward1==wards[j])
  
  countWardW0[j,1] = sum(death0$dodNum0%in%dates0W0&death0$ward0==wards[j])
  countWardW0[j,2] = sum(death1$dodNum1%in%dates1W0&death1$ward1==wards[j])
  
  countWardW1[j,1] = sum(death0$dodNum0%in%dates0W1&death0$ward0==wards[j])
  countWardW1[j,2] = sum(death1$dodNum1%in%dates1W1&death1$ward1==wards[j])
  
  countWardW2[j,1] = sum(death0$dodNum0%in%dates0W2&death0$ward0==wards[j])
  countWardW2[j,2] = sum(death1$dodNum1%in%dates1W2&death1$ward1==wards[j])
  
  countWardCtl[j,1] = sum(death0$dodNum0%in%dates0Ctl&death0$ward0==wards[j])
  countWardCtl[j,2] = sum(death1$dodNum1%in%dates1Ctl&death1$ward1==wards[j])
}

countFacWardTot = countFacWardW0 = countFacWardW1 = countFacWardW2 = countFacWardCtl = array(NA,dim=c(length(hosps),length(wards),2))
for (i in 1:2){
  for (j in 1:length(wards)){
    countFacWardTot[i,j,1] = sum(death0$dodNum0%in%dates0Tot&death0$ward0==wards[j]&death0$inhosp0==hosps[i])
    countFacWardTot[i,j,2] = sum(death1$dodNum1%in%dates1Tot&death1$ward1==wards[j]&death1$inhosp1==hosps[i])
    
    countFacWardW0[i,j,1] = sum(death0$dodNum0%in%dates0W0&death0$ward0==wards[j]&death0$inhosp0==hosps[i])
    countFacWardW0[i,j,2] = sum(death1$dodNum1%in%dates1W0&death1$ward1==wards[j]&death1$inhosp1==hosps[i])
    
    countFacWardW1[i,j,1] = sum(death0$dodNum0%in%dates0W1&death0$ward0==wards[j]&death0$inhosp0==hosps[i])
    countFacWardW1[i,j,2] = sum(death1$dodNum1%in%dates1W1&death1$ward1==wards[j]&death1$inhosp1==hosps[i])
    
    countFacWardW2[i,j,1] = sum(death0$dodNum0%in%dates0W2&death0$ward0==wards[j]&death0$inhosp0==hosps[i])
    countFacWardW2[i,j,2] = sum(death1$dodNum1%in%dates1W2&death1$ward1==wards[j]&death1$inhosp1==hosps[i])
    
    countFacWardCtl[i,j,1] = sum(death0$dodNum0%in%dates0Ctl&death0$ward0==wards[j]&death0$inhosp0==hosps[i])
    countFacWardCtl[i,j,2] = sum(death1$dodNum1%in%dates1Ctl&death1$ward1==wards[j]&death1$inhosp1==hosps[i])
  }
}


countBigWardTot = countBigWardW0 = countBigWardW1 = countBigWardW2 = countBigWardCtl = array(NA,dim=c(length(bigcats),length(wards),2))
for (i in 1:length(bigcats)){
  for (j in 1:length(wards)){
    countBigWardTot[i,j,1] = sum(death0$dodNum0%in%dates0Tot&death0$ward0==wards[j]&death0$bigcat==bigcats[i])
    countBigWardTot[i,j,2] = sum(death1$dodNum1%in%dates1Tot&death1$ward1==wards[j]&death1$bigcat==bigcats[i])
    
    countBigWardW0[i,j,1] = sum(death0$dodNum0%in%dates0W0&death0$ward0==wards[j]&death0$bigcat==bigcats[i])
    countBigWardW0[i,j,2] = sum(death1$dodNum1%in%dates1W0&death1$ward1==wards[j]&death1$bigcat==bigcats[i])
    
    countBigWardW1[i,j,1] = sum(death0$dodNum0%in%dates0W1&death0$ward0==wards[j]&death0$bigcat==bigcats[i])
    countBigWardW1[i,j,2] = sum(death1$dodNum1%in%dates1W1&death1$ward1==wards[j]&death1$bigcat==bigcats[i])
    
    countBigWardW2[i,j,1] = sum(death0$dodNum0%in%dates0W2&death0$ward0==wards[j]&death0$bigcat==bigcats[i])
    countBigWardW2[i,j,2] = sum(death1$dodNum1%in%dates1W2&death1$ward1==wards[j]&death1$bigcat==bigcats[i])
    
    countBigWardCtl[i,j,1] = sum(death0$dodNum0%in%dates0Ctl&death0$ward0==wards[j]&death0$bigcat==bigcats[i])
    countBigWardCtl[i,j,2] = sum(death1$dodNum1%in%dates1Ctl&death1$ward1==wards[j]&death1$bigcat==bigcats[i])
  }
}


countSmallWardTot = countSmallWardW0 = countSmallWardW1 = countSmallWardW2 = countSmallWardCtl = array(NA,dim=c(length(smallcats),length(wards),2))
for (i in 1:length(smallcats)){
  for (j in 1:length(wards)){
    countSmallWardTot[i,j,1] = sum(death0$dodNum0%in%dates0Tot&death0$ward0==wards[j]&death0$smallcat==smallcats[i])
    countSmallWardTot[i,j,2] = sum(death1$dodNum1%in%dates1Tot&death1$ward1==wards[j]&death1$smallcat==smallcats[i])
    
    countSmallWardW0[i,j,1] = sum(death0$dodNum0%in%dates0W0&death0$ward0==wards[j]&death0$smallcat==smallcats[i])
    countSmallWardW0[i,j,2] = sum(death1$dodNum1%in%dates1W0&death1$ward1==wards[j]&death1$smallcat==smallcats[i])
    
    countSmallWardW1[i,j,1] = sum(death0$dodNum0%in%dates0W1&death0$ward0==wards[j]&death0$smallcat==smallcats[i])
    countSmallWardW1[i,j,2] = sum(death1$dodNum1%in%dates1W1&death1$ward1==wards[j]&death1$smallcat==smallcats[i])
    
    countSmallWardW2[i,j,1] = sum(death0$dodNum0%in%dates0W2&death0$ward0==wards[j]&death0$smallcat==smallcats[i])
    countSmallWardW2[i,j,2] = sum(death1$dodNum1%in%dates1W2&death1$ward1==wards[j]&death1$smallcat==smallcats[i])
    
    countSmallWardCtl[i,j,1] = sum(death0$dodNum0%in%dates0Ctl&death0$ward0==wards[j]&death0$smallcat==smallcats[i])
    countSmallWardCtl[i,j,2] = sum(death1$dodNum1%in%dates1Ctl&death1$ward1==wards[j]&death1$smallcat==smallcats[i])
  }
}


###############################################################################
###############################################################################
####### Table 1: all cause/all gender/all ages mortality ######################
###############################################################################
###############################################################################

round00 = function(x){
  out = round(x)
  out0 = round(x,1)
  out00 = round(x,2)
  if (is.nan(out)==F&is.na(out)==F){
    if (out00==out){
      out00 = paste(out,'.00',sep='')
    } else{
      if (out00==out0){
        out00 = paste(out0,'0',sep='')
      }
    }
    return(out00)
  } else{
   return(NA) 
  }
}

round0 = function(x){
  out = round(x)
  out0 = round(x,1)
  if (is.nan(out)==F&is.na(out)==F){
    if (out0==out){
      out0 = paste(out,'.0',sep='')
    }
    return(out0)       
  } else{
    return(NA)
  }
} 



print.fn = function(stratObj,denomStratObj,mainObj,denomMainObj,vecDenom=0){
  
  predOut = obs = excess = ratio = c()
  for (i in 1:dim(stratObj)[1]){
    if (vecDenom==0){
      mod = glm(stratObj[i,]~c(0,1),offset=log(denomStratObj[i,]),family='poisson')
    } else{
      mod = glm(stratObj[i,]~c(0,1),offset=log(denomStratObj),family='poisson')
    }
    
    beta = exp(rnorm(1e5,coef(mod)[2],sqrt(vcov(mod)[2,2])))
    pred = (1/beta)*rpois(1e5,stratObj[i,2])
    
    predOut[i] = paste(round(median(pred)),' (',
                       round(quantile(pred,0.025)),', ',
                       round(quantile(pred,0.975)),')',sep='')
    obs[i] = stratObj[i,2]
    excess[i] = paste(round(median(obs[i]-pred)),' (',
                      round(quantile(obs[i]-pred,0.025)),', ',
                      round(quantile(obs[i]-pred,0.975)),')',sep='')
    ratio[i] = paste(round00(median(obs[i]/pred)),' (',
                     round00(quantile(obs[i]/pred,0.025)),', ',
                     round00(quantile(obs[i]/pred,0.975)),')',sep='')
  }
  
  mod = glm(mainObj~c(0,1),offset=log(denomMainObj),family='poisson')
  beta = exp(rnorm(1e5,coef(mod)[2],sqrt(vcov(mod)[2,2])))
  pred = (1/beta)*rpois(1e5,mainObj[2])
  
  i = i+1
  predOut[i] = paste(round(median(pred)),' (',
                     round(quantile(pred,0.025)),', ',
                     round(quantile(pred,0.975)),')',sep='')
  obs[i] = mainObj[2]
  excess[i] = paste(round(median(obs[i]-pred)),' (',
                    round(quantile(obs[i]-pred,0.025)),', ',
                    round(quantile(obs[i]-pred,0.975)),')',sep='')
  ratio[i] = paste(round00(median(obs[i]/pred)),' (',
                   round00(quantile(obs[i]/pred,0.025)),', ',
                   round00(quantile(obs[i]/pred,0.975)),')',sep='')
  
  return(cbind(predOut,obs,excess,ratio))
}

ageTot = print.fn(stratObj=countAgeTot,denomStratObj=denomAgeTot,
                  mainObj=countTot,denomMainObj=denomTot)
ageW0 = print.fn(stratObj=countAgeW0,denomStratObj=denomAgeW0,
                 mainObj=countW0,denomMainObj=denomW0)
ageW1 = print.fn(stratObj=countAgeW1,denomStratObj=denomAgeW1,
                 mainObj=countW1,denomMainObj=denomW1)
ageW2 = print.fn(stratObj=countAgeW2,denomStratObj=denomAgeW2,
                 mainObj=countW2,denomMainObj=denomW2)

tableAge = rbind(ageTot,NA,ageW0,NA,ageW1,NA,ageW2)

setwd("~/Library/CloudStorage/GoogleDrive-jlewnard@berkeley.edu/My Drive/covid/india/madurai/mortality data/new analysis/outputs")
write.csv(tableAge,file='tableAge.csv')

###################### males
maleAgeTot = print.fn(stratObj=countAgeSexTot[,1,],denomStratObj=denomAgeSexTot[,1,],
                  mainObj=countSexTot[1,],denomMainObj=denomSexTot[1,])
maleAgeW0 = print.fn(stratObj=countAgeSexW0[,1,],denomStratObj=denomAgeSexW0[,1,],
                      mainObj=countSexW0[1,],denomMainObj=denomSexW0[1,])
maleAgeW1 = print.fn(stratObj=countAgeSexW1[,1,],denomStratObj=denomAgeSexW1[,1,],
                      mainObj=countSexW1[1,],denomMainObj=denomSexW1[1,])
maleAgeW2 = print.fn(stratObj=countAgeSexW2[,1,],denomStratObj=denomAgeSexW2[,1,],
                      mainObj=countSexW2[1,],denomMainObj=denomSexW2[1,])

tableMale = rbind(maleAgeTot,NA,maleAgeW0,NA,maleAgeW1,NA,maleAgeW2)

setwd("~/Library/CloudStorage/GoogleDrive-jlewnard@berkeley.edu/My Drive/covid/india/madurai/mortality data/new analysis/outputs")
write.csv(tableMale,file='tableMale.csv')

###################### females
femaleAgeTot = print.fn(stratObj=countAgeSexTot[,2,],denomStratObj=denomAgeSexTot[,2,],
                      mainObj=countSexTot[2,],denomMainObj=denomSexTot[2,])
femaleAgeW0 = print.fn(stratObj=countAgeSexW0[,2,],denomStratObj=denomAgeSexW0[,2,],
                     mainObj=countSexW0[2,],denomMainObj=denomSexW0[2,])
femaleAgeW1 = print.fn(stratObj=countAgeSexW1[,2,],denomStratObj=denomAgeSexW1[,2,],
                     mainObj=countSexW1[2,],denomMainObj=denomSexW1[2,])
femaleAgeW2 = print.fn(stratObj=countAgeSexW2[,2,],denomStratObj=denomAgeSexW2[,2,],
                     mainObj=countSexW2[2,],denomMainObj=denomSexW2[2,])

tableFemale = rbind(femaleAgeTot,NA,femaleAgeW0,NA,femaleAgeW1,NA,femaleAgeW2)

setwd("~/Library/CloudStorage/GoogleDrive-jlewnard@berkeley.edu/My Drive/covid/india/madurai/mortality data/new analysis/outputs")
write.csv(tableFemale,file='tableFemale.csv')

##################### fac/non-fac
facTot = print.fn(stratObj=countFacTot,denomStratObj=denomTot,vecDenom=1,
                  mainObj=countTot,denomMainObj=denomTot)
facW0 = print.fn(stratObj=countFacW0,denomStratObj=denomW0,vecDenom=1,
                  mainObj=countW0,denomMainObj=denomW0)
facW1 = print.fn(stratObj=countFacW1,denomStratObj=denomW1,vecDenom=1,
                  mainObj=countW1,denomMainObj=denomW1)
facW2 = print.fn(stratObj=countFacW2,denomStratObj=denomW2,vecDenom=1,
                  mainObj=countW2,denomMainObj=denomW2)

tableFac = rbind(facTot,NA,facW0,NA,facW1,NA,facW2)

setwd("~/Library/CloudStorage/GoogleDrive-jlewnard@berkeley.edu/My Drive/covid/india/madurai/mortality data/new analysis/outputs")
write.csv(tableFac,file='tableFac.csv')

###################### control analysis (march 2020)
ageCtl = print.fn(stratObj=countAgeCtl,denomStratObj=denomAgeCtl,
                  mainObj=countCtl,denomMainObj=denomCtl)
maleCtl = print.fn(stratObj=countAgeSexCtl[,1,],denomStratObj=denomAgeSexCtl[,1,],
                   mainObj=countSexCtl[1,],denomMainObj=denomSexCtl[1,])
femaleCtl = print.fn(stratObj=countAgeSexCtl[,2,],denomStratObj=denomAgeSexCtl[,2,],
                   mainObj=countSexCtl[2,],denomMainObj=denomSexCtl[2,])
facCtl = print.fn(stratObj=countFacCtl,denomStratObj=denomCtl,vecDenom=1,
                  mainObj=countCtl,denomMainObj=denomCtl)

tableCtl = rbind(ageCtl,NA,maleCtl,NA,femaleCtl,NA,facCtl)
setwd("~/Library/CloudStorage/GoogleDrive-jlewnard@berkeley.edu/My Drive/covid/india/madurai/mortality data/new analysis/outputs")
write.csv(tableCtl,file='tableCtl.csv')

####################################################################################################
####################################################################################################
####### Reported causes of death, by infection status ##############################################
####################################################################################################
####################################################################################################

#### by SARS2+/- (main table)

smallcatlist = list(c('lrti','tb','gastro','std','oth ID','mat'),
                    c('cvd','liver','cancer','diab','oth noncom'),
                    c('unintent','intent'),
                    c('senility','uncat','covid'))
bigcatlist = list('ID','noncom','injury',c('oth','covid'))

output = c()
for (i in 1:length(bigcatlist)){
  for (j in 1:length(smallcatlist[[i]])){
    tempInf = c(as.character(sum(death1$smallcat[which(death1$covid1==1&death1$dodNum1%in%dates1Tot)]==smallcatlist[[i]][j])),
                round0(100*mean(death1$smallcat[which(death1$covid1==1&death1$dodNum1%in%dates1Tot)]==smallcatlist[[i]][j])))
    tempUninf = c(as.character(sum(death1$smallcat[which(death1$covid1==0&death1$dodNum1%in%dates1Tot)]==smallcatlist[[i]][j])),
                  round0(100*mean(death1$smallcat[which(death1$covid1==0&death1$dodNum1%in%dates1Tot)]==smallcatlist[[i]][j])))
    temp0 = c(as.character(sum(death0$smallcat[which(death0$covid0==0&death0$dodNum0%in%dates0Tot)]==smallcatlist[[i]][j])),
                  round0(100*mean(death0$smallcat[which(death0$covid0==0&death0$dodNum0%in%dates0Tot)]==smallcatlist[[i]][j])))
    output = rbind(output,c(tempInf,tempUninf,temp0))
  }
  tempInf = c(as.character(sum(death1$bigcat[which(death1$covid1==1&death1$dodNum1%in%dates1Tot)]%in%bigcatlist[[i]])),
              round0(100*mean(death1$bigcat[which(death1$covid1==1&death1$dodNum1%in%dates1Tot)]%in%bigcatlist[[i]])))
  tempUninf = c(as.character(sum(death1$bigcat[which(death1$covid1==0&death1$dodNum1%in%dates1Tot)]%in%bigcatlist[[i]])),
                round0(100*mean(death1$bigcat[which(death1$covid1==0&death1$dodNum1%in%dates1Tot)]%in%bigcatlist[[i]])))
  temp0 = c(as.character(sum(death0$bigcat[which(death0$covid0==0&death0$dodNum0%in%dates0Tot)]%in%bigcatlist[[i]])),
            round0(100*mean(death0$bigcat[which(death0$covid0==0&death0$dodNum0%in%dates0Tot)]%in%bigcatlist[[i]])))
  output = rbind(output,c(tempInf,tempUninf,temp0))
  output = rbind(output,NA)
}

output = cbind(paste(output[,1],' (',output[,2],')',sep=''),
      paste(output[,3],' (',output[,4],')',sep=''),
      paste(output[,5],' (',output[,6],')',sep=''))
causeCovid = output

setwd("~/Library/CloudStorage/GoogleDrive-jlewnard@berkeley.edu/My Drive/covid/india/madurai/mortality data/new analysis/outputs")
write.csv(causeCovid,file='causeCovid.csv')

sum(death1$dodNum1%in%dates1Tot&death1$covid1==0)
sum(death0$dodNum0%in%dates0Tot)

#### by SARS2 and era

output = c()
datelist1 = list(dates1W0,dates1W1,dates1W2)
datelist0 = list(dates0W0,dates0W1,dates0W2)
for (i in 1:length(bigcatlist)){
  for (j in 1:length(smallcatlist[[i]])){
    temp = c()
    for (k in 1:length(datelist1)){
      tempInf = c(as.character(sum(death1$smallcat[which(death1$covid1==1&death1$dodNum1%in%datelist1[[k]])]==smallcatlist[[i]][j])),
                  round0(100*mean(death1$smallcat[which(death1$covid1==1&death1$dodNum1%in%datelist1[[k]])]==smallcatlist[[i]][j])))
      tempUninf = c(as.character(sum(death1$smallcat[which(death1$covid1==0&death1$dodNum1%in%datelist1[[k]])]==smallcatlist[[i]][j])),
                    round0(100*mean(death1$smallcat[which(death1$covid1==0&death1$dodNum1%in%datelist1[[k]])]==smallcatlist[[i]][j])))
      temp0 = c(as.character(sum(death0$smallcat[which(death0$covid0==0&death0$dodNum0%in%datelist0[[k]])]==smallcatlist[[i]][j])),
                round0(100*mean(death0$smallcat[which(death0$covid0==0&death0$dodNum0%in%datelist0[[k]])]==smallcatlist[[i]][j])))    
      temp = rbind(temp,c(tempInf,tempUninf,temp0))
    }
    outputTemp = c(temp[1,],temp[2,],temp[3,])
    output = rbind(output,outputTemp)
  }
  temp = c()
  for (k in 1:length(datelist1)){
    tempInf = c(as.character(sum(death1$bigcat[which(death1$covid1==1&death1$dodNum1%in%datelist1[[k]])]%in%bigcatlist[[i]])),
                round0(100*mean(death1$bigcat[which(death1$covid1==1&death1$dodNum1%in%datelist1[[k]])]%in%bigcatlist[[i]])))
    tempUninf = c(as.character(sum(death1$bigcat[which(death1$covid1==0&death1$dodNum1%in%datelist1[[k]])]%in%bigcatlist[[i]])),
                  round0(100*mean(death1$bigcat[which(death1$covid1==0&death1$dodNum1%in%datelist1[[k]])]%in%bigcatlist[[i]])))
    temp0 = c(as.character(sum(death0$bigcat[which(death0$covid0==0&death0$dodNum0%in%datelist0[[k]])]%in%bigcatlist[[i]])),
              round0(100*mean(death0$bigcat[which(death0$covid0==0&death0$dodNum0%in%datelist0[[k]])]%in%bigcatlist[[i]])))    
    temp = rbind(temp,c(tempInf,tempUninf,temp0))
  }
  outputTemp = c(temp[1,],temp[2,],temp[3,])
  output = rbind(output,outputTemp)
  output = rbind(output,NA)
}


output = cbind(paste(output[,1],' (',output[,2],')',sep=''),
               paste(output[,3],' (',output[,4],')',sep=''),
               paste(output[,5],' (',output[,6],')',sep=''),
               paste(output[,7],' (',output[,8],')',sep=''),
               paste(output[,9],' (',output[,10],')',sep=''),
               paste(output[,11],' (',output[,12],')',sep=''),
               paste(output[,13],' (',output[,14],')',sep=''),
               paste(output[,15],' (',output[,16],')',sep=''),
               paste(output[,17],' (',output[,18],')',sep=''))
               
causeCovidEra = output
setwd("~/Library/CloudStorage/GoogleDrive-jlewnard@berkeley.edu/My Drive/covid/india/madurai/mortality data/new analysis/outputs")
write.csv(causeCovidEra,file='causeCovidEra.csv')

sum(death1$dodNum1%in%dates1W0&death1$covid1==0)
sum(death0$dodNum0%in%dates0W0)

###### ctl period (no COVID cases dx'd)

output = c()
datelist1 = list(dates1Ctl)
datelist1pre = list(as.numeric(mdy('1-1-2020'):mdy('2-29-2020')))
datelist0 = list(dates0Ctl)
for (i in 1:length(bigcatlist)){
  for (j in 1:length(smallcatlist[[i]])){
    
    for (k in 1:length(datelist1)){
      
      tempUninf = c(as.character(sum(death1$smallcat[which(death1$covid1==0&death1$dodNum1%in%datelist1[[k]])]==smallcatlist[[i]][j])),
                    round0(100*mean(death1$smallcat[which(death1$covid1==0&death1$dodNum1%in%datelist1[[k]])]==smallcatlist[[i]][j])))
      tempPre = c(as.character(sum(death1$smallcat[which(death1$covid1==0&death1$dodNum1%in%datelist1pre[[k]])]==smallcatlist[[i]][j])),
                    round0(100*mean(death1$smallcat[which(death1$covid1==0&death1$dodNum1%in%datelist1pre[[k]])]==smallcatlist[[i]][j])))
      temp0 = c(as.character(sum(death0$smallcat[which(death0$covid0==0&death0$dodNum0%in%datelist0[[k]])]==smallcatlist[[i]][j])),
                round0(100*mean(death0$smallcat[which(death0$covid0==0&death0$dodNum0%in%datelist0[[k]])]==smallcatlist[[i]][j])))    
      temp = c(tempUninf,tempPre,temp0)
    }
    output = rbind(output,temp)
  }
  temp = c()
  tempUninf = c(as.character(sum(death1$bigcat[which(death1$covid1==0&death1$dodNum1%in%datelist1[[k]])]%in%bigcatlist[[i]])),
                round0(100*mean(death1$bigcat[which(death1$covid1==0&death1$dodNum1%in%datelist1[[k]])]%in%bigcatlist[[i]])))
  tempPre = c(as.character(sum(death1$bigcat[which(death1$covid1==0&death1$dodNum1%in%datelist1pre[[k]])]%in%bigcatlist[[i]])),
                round0(100*mean(death1$bigcat[which(death1$covid1==0&death1$dodNum1%in%datelist1pre[[k]])]%in%bigcatlist[[i]])))
  temp0 = c(as.character(sum(death0$bigcat[which(death0$covid0==0&death0$dodNum0%in%datelist0[[k]])]%in%bigcatlist[[i]])),
            round0(100*mean(death0$bigcat[which(death0$covid0==0&death0$dodNum0%in%datelist0[[k]])]%in%bigcatlist[[i]])))    
  temp = c(tempUninf,tempPre,temp0)
  output = rbind(output,temp)
  output = rbind(output,NA)
}

output = cbind(paste(output[,1],' (',output[,2],')',sep=''),
               paste(output[,3],' (',output[,4],')',sep=''),
               paste(output[,5],' (',output[,6],')',sep=''))
causeCovidCtl = output
setwd("~/Library/CloudStorage/GoogleDrive-jlewnard@berkeley.edu/My Drive/covid/india/madurai/mortality data/new analysis/outputs")
write.csv(causeCovidCtl,file='causeCovidCtl.csv')

sum(death1$dodNum1%in%dates1Ctl)
sum(death0$dodNum0%in%dates0Ctl)


####################################################################################################
####################################################################################################
####### Reported causes of death, obs vs expected ##################################################
####################################################################################################
####################################################################################################

print.fn = function(stratObj,denomStratObj,mainObj,denomMainObj,vecDenom=0){
  
  predOut = obs = excess = ratio = c()
  for (i in 1:dim(stratObj)[1]){
    if (vecDenom==0){
      mod = glm(stratObj[i,]~c(0,1),offset=log(denomStratObj[i,]),family='poisson')
    } else{
      mod = glm(stratObj[i,]~c(0,1),offset=log(denomStratObj),family='poisson')
    }
    
    beta = exp(rnorm(1e5,coef(mod)[2],sqrt(vcov(mod)[2,2])))
    pred = (1/beta)*rpois(1e5,stratObj[i,2])
    
    predOut[i] = paste(round(median(pred)),' (',
                       round(quantile(pred,0.025,na.rm=T)),', ',
                       round(quantile(pred,0.975,na.rm=T)),')',sep='')
    obs[i] = stratObj[i,2]
    excess[i] = paste(round(median(obs[i]-pred,na.rm=T)),' (',
                      round(quantile(obs[i]-pred,0.025,na.rm=T)),', ',
                      round(quantile(obs[i]-pred,0.975,na.rm=T)),')',sep='')
    if (obs[i]>0&mean(pred)>1){
      ratio[i] = paste(round00(median(obs[i]/pred,na.rm=T)),' (',
                       round00(quantile(obs[i]/pred,0.025,na.rm=T)),', ',
                       round00(quantile(obs[i]/pred,0.975,na.rm=T)),')',sep='')      
    } else{
      ratio[i] = '--'
    }

  }
  
  mod = glm(mainObj~c(0,1),offset=log(denomMainObj),family='poisson')
  beta = exp(rnorm(1e5,coef(mod)[2],sqrt(vcov(mod)[2,2])))
  pred = (1/beta)*rpois(1e5,mainObj[2])
  
  i = i+1
  predOut[i] = paste(round(median(pred,na.rm=T)),' (',
                     round(quantile(pred,0.025,na.rm=T)),', ',
                     round(quantile(pred,0.975,na.rm=T)),')',sep='')
  obs[i] = mainObj[2]
  excess[i] = paste(round(median(obs[i]-pred,na.rm=T)),' (',
                    round(quantile(obs[i]-pred,0.025,na.rm=T)),', ',
                    round(quantile(obs[i]-pred,0.975,na.rm=T)),')',sep='')
  if (obs[i]>0&mean(pred)>1){
    ratio[i] = paste(round00(median(obs[i]/pred,na.rm=T)),' (',
                     round00(quantile(obs[i]/pred,0.025,na.rm=T)),', ',
                     round00(quantile(obs[i]/pred,0.975,na.rm=T)),')',sep='')    
  } else{
    ratio[i] = '--'
  }

  
  return(cbind(predOut,obs,excess,ratio))
}


#### predicted, expected --- overall

idOrd = c(); for (i in 1:length(smallcatlist[[1]])){idOrd[i] = which(smallcats==smallcatlist[[1]][i])}
noncomOrd = c(); for (i in 1:length(smallcatlist[[2]])){noncomOrd[i] = which(smallcats==smallcatlist[[2]][i])}
injurOrd = c(); for (i in 1:length(smallcatlist[[3]])){injurOrd[i] = which(smallcats==smallcatlist[[3]][i])}
othOrd = c(); for (i in 1:length(smallcatlist[[4]])){othOrd[i] = which(smallcats==smallcatlist[[4]][i])}



idTot = print.fn(stratObj=countSmallTot[idOrd,],denomStratObj=denomTot,
                        mainObj=countBigTot[4,],denomMainObj=denomTot,vecDenom=1)
noncomTot = print.fn(stratObj=countSmallTot[noncomOrd,],denomStratObj=denomTot,
                 mainObj=countBigTot[1,],denomMainObj=denomTot,vecDenom=1)
injurTot = print.fn(stratObj=countSmallTot[injurOrd,],denomStratObj=denomTot,
                 mainObj=countBigTot[2,],denomMainObj=denomTot,vecDenom=1)
othTot = print.fn(stratObj=countSmallTot[othOrd,],denomStratObj=denomTot,
                 mainObj=countBigTot[3,]+countBigTot[5,],denomMainObj=denomTot,vecDenom=1)

causeObsExpect = rbind(idTot,NA,noncomTot,NA,injurTot,NA,othTot)
setwd("~/Library/CloudStorage/GoogleDrive-jlewnard@berkeley.edu/My Drive/covid/india/madurai/mortality data/new analysis/outputs")
write.csv(causeObsExpect,file='causeObsExpect.csv')

#### predicted, expected --- w0

idW0 = print.fn(stratObj=countSmallW0[idOrd,],denomStratObj=denomW0,
                 mainObj=countBigW0[4,],denomMainObj=denomW0,vecDenom=1)
noncomW0 = print.fn(stratObj=countSmallW0[noncomOrd,],denomStratObj=denomW0,
                     mainObj=countBigW0[1,],denomMainObj=denomW0,vecDenom=1)
injurW0 = print.fn(stratObj=countSmallW0[injurOrd,],denomStratObj=denomW0,
                    mainObj=countBigW0[2,],denomMainObj=denomW0,vecDenom=1)
othW0 = print.fn(stratObj=countSmallW0[othOrd,],denomStratObj=denomW0,
                  mainObj=countBigW0[3,]+countBigW0[5,],denomMainObj=denomW0,vecDenom=1)

causeObsExpectW0 = rbind(idW0,NA,noncomW0,NA,injurW0,NA,othW0)
setwd("~/Library/CloudStorage/GoogleDrive-jlewnard@berkeley.edu/My Drive/covid/india/madurai/mortality data/new analysis/outputs")
write.csv(causeObsExpectW0,file='causeObsExpectW0.csv')

#### predicted, expected --- w1

idW1 = print.fn(stratObj=countSmallW1[idOrd,],denomStratObj=denomW1,
                mainObj=countBigW1[4,],denomMainObj=denomW1,vecDenom=1)
noncomW1 = print.fn(stratObj=countSmallW1[noncomOrd,],denomStratObj=denomW1,
                    mainObj=countBigW1[1,],denomMainObj=denomW1,vecDenom=1)
injurW1 = print.fn(stratObj=countSmallW1[injurOrd,],denomStratObj=denomW1,
                   mainObj=countBigW1[2,],denomMainObj=denomW1,vecDenom=1)
othW1 = print.fn(stratObj=countSmallW1[othOrd,],denomStratObj=denomW1,
                 mainObj=countBigW1[3,]+countBigW1[5,],denomMainObj=denomW1,vecDenom=1)

causeObsExpectW1 = rbind(idW1,NA,noncomW1,NA,injurW1,NA,othW1); causeObsExpectW1
setwd("~/Library/CloudStorage/GoogleDrive-jlewnard@berkeley.edu/My Drive/covid/india/madurai/mortality data/new analysis/outputs")
write.csv(causeObsExpectW1,file='causeObsExpectW1.csv')

#### predicted, expected --- w2

idW2 = print.fn(stratObj=countSmallW2[idOrd,],denomStratObj=denomW2,
                mainObj=countBigW2[4,],denomMainObj=denomW2,vecDenom=1)
noncomW2 = print.fn(stratObj=countSmallW2[noncomOrd,],denomStratObj=denomW2,
                    mainObj=countBigW2[1,],denomMainObj=denomW2,vecDenom=1)
injurW2 = print.fn(stratObj=countSmallW2[injurOrd,],denomStratObj=denomW2,
                   mainObj=countBigW2[2,],denomMainObj=denomW2,vecDenom=1)
othW2 = print.fn(stratObj=countSmallW2[othOrd,],denomStratObj=denomW2,
                 mainObj=countBigW2[3,]+countBigW2[5,],denomMainObj=denomW2,vecDenom=1)

causeObsExpectW2 = rbind(idW2,NA,noncomW2,NA,injurW2,NA,othW2); causeObsExpectW2
setwd("~/Library/CloudStorage/GoogleDrive-jlewnard@berkeley.edu/My Drive/covid/india/madurai/mortality data/new analysis/outputs")
write.csv(causeObsExpectW2,file='causeObsExpectW2.csv')

#### predicted, expected --- Ctl

idCtl = print.fn(stratObj=countSmallCtl[idOrd,],denomStratObj=denomCtl,
                mainObj=countBigCtl[4,],denomMainObj=denomCtl,vecDenom=1)
noncomCtl = print.fn(stratObj=countSmallCtl[noncomOrd,],denomStratObj=denomCtl,
                    mainObj=countBigCtl[1,],denomMainObj=denomCtl,vecDenom=1)
injurCtl = print.fn(stratObj=countSmallCtl[injurOrd,],denomStratObj=denomCtl,
                   mainObj=countBigCtl[2,],denomMainObj=denomCtl,vecDenom=1)
othCtl = print.fn(stratObj=countSmallCtl[othOrd,],denomStratObj=denomCtl,
                 mainObj=countBigCtl[3,]+countBigCtl[5,],denomMainObj=denomCtl,vecDenom=1)

causeObsExpectCtl = rbind(idCtl,NA,noncomCtl,NA,injurCtl,NA,othCtl); causeObsExpectCtl
setwd("~/Library/CloudStorage/GoogleDrive-jlewnard@berkeley.edu/My Drive/covid/india/madurai/mortality data/new analysis/outputs")
write.csv(causeObsExpectCtl,file='causeObsExpectCtl.csv')

#### predicted, expected --- Fac

idFac = print.fn(stratObj=countSmallFacTot[idOrd,1,],denomStratObj=denomTot,
                 mainObj=countBigFacTot[4,1,],denomMainObj=denomTot,vecDenom=1)
noncomFac = print.fn(stratObj=countSmallFacTot[noncomOrd,1,],denomStratObj=denomTot,
                     mainObj=countBigFacTot[1,1,],denomMainObj=denomTot,vecDenom=1)
injurFac = print.fn(stratObj=countSmallFacTot[injurOrd,1,],denomStratObj=denomTot,
                    mainObj=countBigFacTot[2,1,],denomMainObj=denomTot,vecDenom=1)
othFac = print.fn(stratObj=countSmallFacTot[othOrd,1,],denomStratObj=denomTot,
                  mainObj=countBigFacTot[3,1,]+countBigFacTot[5,1,],denomMainObj=denomTot,vecDenom=1)

causeObsExpectFac = rbind(idFac,NA,noncomFac,NA,injurFac,NA,othFac); causeObsExpectFac
setwd("~/Library/CloudStorage/GoogleDrive-jlewnard@berkeley.edu/My Drive/covid/india/madurai/mortality data/new analysis/outputs")
write.csv(causeObsExpectFac,file='causeObsExpectFac.csv')


idNonFac = print.fn(stratObj=countSmallFacTot[idOrd,2,],denomStratObj=denomTot,
                 mainObj=countBigFacTot[4,2,],denomMainObj=denomTot,vecDenom=1)
noncomNonFac = print.fn(stratObj=countSmallFacTot[noncomOrd,2,],denomStratObj=denomTot,
                     mainObj=countBigFacTot[1,2,],denomMainObj=denomTot,vecDenom=1)
injurNonFac = print.fn(stratObj=countSmallFacTot[injurOrd,2,],denomStratObj=denomTot,
                    mainObj=countBigFacTot[2,2,],denomMainObj=denomTot,vecDenom=1)
othNonFac = print.fn(stratObj=countSmallFacTot[othOrd,2,],denomStratObj=denomTot,
                  mainObj=countBigFacTot[3,2,]+countBigFacTot[5,2,],denomMainObj=denomTot,vecDenom=1)

causeObsExpectNonFac = rbind(idNonFac,NA,noncomNonFac,NA,injurNonFac,NA,othNonFac); causeObsExpectNonFac
setwd("~/Library/CloudStorage/GoogleDrive-jlewnard@berkeley.edu/My Drive/covid/india/madurai/mortality data/new analysis/outputs")
write.csv(causeObsExpectNonFac,file='causeObsExpectNonFac.csv')

