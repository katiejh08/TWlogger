##### This file calculates metrics of accelerometry, plots histograms of these metrics
###### and makes behavioural assignments based on these histograms. 

# to reset R removing assigned function
rm(list=ls())
library("zoo",)
options(digits.secs=2)
getwd()


#Input data should consist of one csv file of time, X,Y,Z, where X is assumed to be acceleration
# in the surge channel, Y is acceleration in sway, and Z is acceleration in heave.

Data<-read.csv(file.choose(),stringsAsFactors=FALSE)
head(Data)
tail(Data)
str(Data)



freq<-25 #The Frequency of accelerometry data
secs<-1 # the number of seconds over which to calculate the desired metrics.The manuscript says to use 1 second intervals,
#but to capture gliding flight as well I've found that a longer period is needed. I will update this on the ms! 

numrows<-freq*secs # the number of rows required to calculate metrics over the chosen period. 

##Calculate rolling means over a set period.

Data$meanX=rollapply(Data$X,numrows,mean,fill=NA)#25 = number of cells to average across, change to desired time considering sampling frequency.
Data$meancent=rollapply(Data$X,numrows,mean,fill=NA)#25 = number of cells to average across, change to desired time considering sampling frequency.


Data$meanY=rollapply(Data$Y,numrows,mean,fill=NA)#25 = number of cells to average across, change to desired time considering sampling frequency. 
Data$meanZ=rollapply(Data$Z,numrows,mean,fill=NA)#25 = number of cells to average across, change to desired time considering sampling frequency. 

#Calculate rollingstandard deviation over a set period.

Data$SDX=rollapply(Data$X,numrows,sd,fill=NA)#25 = number of cells to average across, change to desired time considering sampling frequency.
Data$SDY=rollapply(Data$Y,numrows,sd,fill=NA)#25 = number of cells to average across, change to desired time considering sampling frequency. 
Data$SDZ=rollapply(Data$Z,numrows,sd,fill=NA)#25 = number of cells to average across, change to desired time considering sampling frequency. 



##Calculate pitch

Data$pitch<-atan((Data$X/(sqrt((Data$Y*Data$Y)+(Data$Z*Data$Z)))))*(180/pi);


#Calculate roll

Data$roll<-atan((Data$Y/(sqrt((Data$X*Data$X)+(Data$Z*Data$Z)))))*(180/pi);



#####Calculate ODBA and VeDBA####

Data$RunningAx=rollapply(Data$X,numrows,mean,fill=NA) 
Data$RunningAy=rollapply(Data$Y,numrows,mean,fill=NA)
Data$RunningAz=rollapply(Data$Z,numrows,mean,fill=NA)


####Calculates DBA for each axis. 



Data$StaticX<-Data$X-Data$RunningAx
Data$StaticY<-Data$Y-Data$RunningAy
Data$StaticZ<-Data$Z-Data$RunningAz


Data$ODBA<-abs(Data$StaticX)+abs(Data$StaticY)+abs(Data$StaticZ)
Data$Vedba<-sqrt((Data$StaticX^2)+(Data$StaticY^2)+(Data$StaticZ^2))



Data<-subset(Data, select=c(NewTime,X,Y,Z,meanX,meanY,meanZ,SDX,SDY,SDZ,pitch,roll,ODBA,Vedba))


head(Data)
####Subset to 1-second intervals
Data$NewTime2<-as.POSIXct(Data$NewTime,format ="%Y-%m-%d %H:%M:%S")

library("plyr")


Data2<-ddply(Data, .(NewTime2), function(x) x[13,])


Data2<-Data2[-1,] # removes first row which is likely to be NA, depending on interval of metric calculation.This will need to be run a few times until there are no NAs, depending on the time used to average/ calculate SD over.
head(Data2)
Data2<-Data2[-nrow(Data2),]
tail(Data2)
#####Plot histograms of the calculated metrics. 
##Change bin sizes and range as shown with the SDZ metric to fully explore the data. 

maxSDZ<-max(Data2$SDZ)+0.1 # have to add a small amount for the breaks to work. 
minSDZ<-min(Data2$SDZ)-0.1
interval<-0.02 # change depending on bin size required. 
hist(Data2$SDZ,breaks=seq(minSDZ,maxSDZ, by=interval),)
str(Data2$SDZ)


print(sum(is.na(Data2$pitch)))
print(sum(is.na(Data2$SDZ)))


Data2$pitch<- na.locf(Data2$pitch,fromLast=FALSE)
Data2$SDZ<- na.locf(Data2$SDZ,fromLast=FALSE)

head(Data2)


hist(Data2$meanX)
hist(Data2$meanY)
hist(Data2$meanZ)
hist(Data2$SDX)
hist(Data2$SDY)
hist(Data2$SDZ)
hist(Data2$pitch)
hist(Data2$roll)
hist(Data2$ODBA)
hist(Data2$Vedba)

###### Identify peaks in the data and calculate the inter-peak frequency minimum by taking the metric value
###corresponding to the minimum frequency count between peaks. 

SDZhist<-hist(Data2$SDZ,breaks=seq(minSDZ,maxSDZ, by=interval),)
SDZhist

mids<-SDZhist$mids
counts<-SDZhist$counts

SDZhistdata<-data.frame(mids,counts) # make a dataframe from the histogram data
SDZhistdata

#Identify the Inter-peak frequency minimum value(s) In this example the IPFM is calculated between a value above 0.1 and below 0.5
#change this depending on where the peaks fall in your dataset. 
#If this returns numerous values, take the average.
firstpeak<-0.1 # set a value corresponding to the  value with the highest frequency at the first peak. This does not have to be accurate. 
secondpeak<-0.6 # set a value corresponding to the  value with the highest frequency at the second peak. This does not have to be accurate. 

SDZhistdata<-subset(SDZhistdata,mids>firstpeak&mids<secondpeak)

IPFM<-SDZhistdata[which(SDZhistdata$counts==min(SDZhistdata$counts)),1]
IPFM

##Assign behaviours based on the IPFM. Use numbers to indicate discrete behaviours. In this example, 1 indicates flight,
##2 indicates being on water, and 3 indicates being on land. 



for(i in 1:length(Data2))
{
  Data2$behaviour[Data2$SDZ>IPFM]<-1
}

#Assign a number to data not falling within the identified threshold. 
Data2[is.na(Data2)] <- 4

#The next round of behavioural assignment can then take place. e.g.after plotting histograms with 
#data belonging to the first behavioural assignment having been removed and the IPFM for the second
#metric having been identified


# The example below incorporates the above round of assignment as well as another set of arguments which depends on
#the IPFM found from the metric 'pitch'

head(Data2)

######For pitch
minpitch<-min(Data2$pitch)-1
minpitch
maxpitch<-max(Data2$pitch)+1
maxpitch
interval<-1
hist(Data2$pitch[Data$SDZ>IPFM],breaks=seq(minpitch,maxpitch, by=interval))
pitchhist<-hist(Data2$pitch,breaks=seq(minpitch,maxpitch, by=interval))  
pitchhist

mids<-pitchhist$mids
counts<-pitchhist$counts

pitchhistdata<-data.frame(mids,counts) # make a dataframe from the histogram data
pitchhistdata

#If this returns numerous values, take the average.
firstpeak<-10 # set a value corresponding to the  value with the highest frequency at the first peak. This does not have to be accurate. 
secondpeak<-50 # set a value corresponding to the  value with the highest frequency at the second peak. This does not have to be accurate. 


pitchhistdata<-subset(pitchhistdata,mids>firstpeak&mids<secondpeak)

IPFMpitch<-pitchhistdata[which(pitchhistdata$counts==min(pitchhistdata$counts)),1]
IPFMpitch



for(i in 1:length(Data2))
{
  Data2$behaviour[Data2$SDZ>IPFM]<-1 # 1 =flight
  Data2$behaviour[Data2$SDZ<IPFM & Data2$pitch<=IPFMpitch]<-2 # 2 = on water
  Data2$behaviour[Data2$SDZ<IPFM & Data2$pitch>IPFMpitch]<-3 # 3 = on land
}


#Assign a number to data not falling within the identified threshold. 
Data2[is.na(Data2)] <- 4




####The following is the script used for averaging pitch between flight periods in the manuscript. 
head(Data2)
Data2[1,16]<-1

rval<-as.numeric(row.names(Data2))
Data2$b0row<-rval

for(i in 1:length(Data2)){
  
  Data2$b0row[Data2$behaviour==2]<-NA
  Data2$b0row[Data2$behaviour==3]<-NA
  
}

Data2$b0na<- na.locf(Data2$b0row,fromLast=FALSE)

for(i in 1:length(Data2)){
  
  Data2$b0na[Data2$behaviour==1]<-NA
  Data2$b0na[Data2$behaviour==4]<-NA
  
}

library(plyr) 

avg<-ddply(Data2, .(b0na), transform, pitchavg=mean(pitch)) 
avg<-avg[order(avg$NewTime),]
Data2$pitchavg<-avg$pitchavg

tail(Data2)
Data2 <- subset(Data2, select = -c(NewTime2,b0row,b0na)) 

#####Behaviours can then be reassigned using the same process as above, but with assignments now dependent on the average pitch values. 
for(i in 1:length(Data2))
{
  Data2$behaviour[Data2$SDZ>IPFM]<-1
  Data2$behaviour[Data2$SDZ<IPFM & Data2$pitchavg<=IPFMpitch]<-2
  Data2$behaviour[Data2$SDZ<IPFM & Data2$pitchavg>IPFMpitch]<-3 
}


head(Data2)

Data<-subset(Data2, select=c(NewTime, behaviour,pitch,SDZ))
head(Data)
tail(Data)
write.csv(Data,"Accelerometerwithbehaviours.csv",row.names=FALSE)


