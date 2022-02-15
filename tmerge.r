## Time varying covariates in R: Use tdc in tmerge() ##
## by Chia Liu ## 

library(survival)
library(tidyverse)

## fake data ## 

## dur(ation) to experiencing intimate partner violence "ipv" (1 or 0) in months after union formation for id 101-106

## suppose number of children and cohab/married is time-varying, whether they formed union under age 16 (uu16) does not 
## change with time (time-invariant) ##  

xid<-c(101,102,103,104,105,106)
dur<-c(36,42,78,60,55,50)
ipv<-c(1,0,0,0,1,0)
child1<-c(40,12,99999,36,25,14)
child2<-c(52,99999,99999,72,40,28)
child3<-c(99999,99999,99999,90,52,99999)
married<-c(0,0,14,0,12,0)
uu16<-c(1,0,0,0,0,1)

fdata<-data.frame(xid,dur,ipv,child1,child2,child3,married,uu16)

## now let's have a look at the data

head(fdata)

## person 101 experienced ipv 36 months after her union (ipv==1); 
## she then had a first and second child at 40 and 52 months after her union, which we do not care about because the event happened already;
## she is also married at the time of union (married at 0) so she will not have additional rows after tmerge

## person 105 experienced ipv 55 months after union;
## she was not married at the time of union formtion (our baseline time is union, and she got married 12 months after;
## she then had child1 at month 25, child2, 40, child3, 52 months so we need to split up her risk time 
## she will have additional rows after tmerge

## be sure to recode NA or events that never happened to 99999; function will not working with NA 

data1<-tmerge(fdata,fdata, ## merge data to itself 
              id=xid, ## here declare the id variable
              event=event(dur,ipv), ## here declare the duration and event variables
              c1=tdc(child1), ## here state your time-dependent variable "tdc" 
              c2=tdc(child2), ## here state your time-dependent variable "tdc" 
              c3=tdc(child3), ## here state your time-dependent variable "tdc" 
              marry=tdc(married)) ## here state your time-dependent variable "tdc"  

View(data1)

## now we've transformed duration to start and stop time (tstart/tstop) by tdc of interest
## uu16 should not change since it is time-invariant
## your old duration is now obsolete (and incorrect); need to create a new duration which is tstop-tstart
## tidy up the number of child variable by creating a new var "numchild" which is c1+c2+c3 
## you don't need your c1, c2, c3, child1, child2, child3, married variables anymore

finaldata<-data1 %>% 
  mutate(dur=tstop-tstart,
         numchild=c1+c2+c3) %>% 
  select(-c('child1','child2','child3','married','c1','c2','c3'))

## check the duration of your original data (fdata) with the new data (finaldata)
## if the durations don't match up then something went wrong
## we're only SLICING up time by what's happening in people's lives (birth of child, marriage), we're not GROWING time ##

sum(fdata$dur) ## original data
sum(finaldata$dur) ## new data 

## now you can coxph to your heart's content ##
