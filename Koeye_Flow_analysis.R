##########################
###    Koeye River level mixed-effects models

setwd("C:/data/koeye")

require(ggplot2)
require(ggfortify)
require(AICcmodavg)
require(arm)
require(MuMIn)
require(dplyr)
require(nlme)
require(mgcv)

library(car)

koeye_flow<-read.table("koeye_elevation_DAILY.txt",header=TRUE)

flow<-subset(koeye_flow,koeye_flow$overlap>0)

flow.new<-flow[-which(is.na(flow$mean_elev_OLD)),] ## removing NAs


########################################################
#### Plotting the relationship between the two gauges

# flow stage

### looks pretty linear without log transforming
cols<-c("seagreen2","royalblue2","darkviolet","turquoise3","orchid3","chartreuse3")

plot(mean_elev_OLD ~ mean_elev_HAKAI,ylim=c(0,2.5),xlim=c(0,2.5),
     data=subset(flow.new, flow.new$overlap_period=='A'), col=cols[1])
par(new=TRUE)
plot(mean_elev_OLD ~ mean_elev_HAKAI,ylim=c(0,2.5),xlim=c(0,2.5), 
     data=subset(flow.new, flow.new$overlap_period=='B'),
     xlab="",ylab="",axes=FALSE,col=cols[2])
par(new=TRUE)
plot(mean_elev_OLD ~ mean_elev_HAKAI,ylim=c(0,2.5),xlim=c(0,2.5), 
     data=subset(flow.new, flow.new$overlap_period=='C'), 
     xlab="",ylab="",axes=FALSE,col=cols[3])
par(new=TRUE)
plot(mean_elev_OLD ~ mean_elev_HAKAI,ylim=c(0,2.5),xlim=c(0,2.5), 
     data=subset(flow.new, flow.new$overlap_period=='D'), 
     xlab="",ylab="",axes=FALSE,col=cols[4])
par(new=TRUE)
plot(mean_elev_OLD ~ mean_elev_HAKAI,ylim=c(0,2.5),xlim=c(0,2.5), 
     data=subset(flow.new, flow.new$overlap_period=='E'), 
     xlab="",ylab="",axes=FALSE,col=cols[5])
par(new=TRUE)
plot(mean_elev_OLD ~ mean_elev_HAKAI,ylim=c(0,2.5),xlim=c(0,2.5), 
     data=subset(flow.new, flow.new$overlap_period=='F'), 
     xlab="",ylab="",axes=FALSE,col='red')
par(new=TRUE)
plot(mean_elev_OLD ~ mean_elev_HAKAI,ylim=c(0,2.5),xlim=c(0,2.5), 
     data=subset(flow.new, flow.new$overlap_period=='G'), 
     xlab="",ylab="",axes=FALSE,col=cols[6])
abline(a=mod_flow$coefficients[[1]][1],b=mod_flow$coefficients[[1]][2],
       col='black',lwd=2)

# temperature - definitely linear. probably don't even need a mixed-effects model

plot(mean_temp_OLD ~ mean_temp_HAKAI,ylim=c(0,25),xlim=c(0,25),
     data=flow.new, col=cols[1])

### the model - temperature
mod_temp<-lm(mean_temp_OLD ~ mean_temp_HAKAI, data=flow.new)
summary(mod_temp)
plot(mod_temp)

#### the model - flow
mod_flow<-lme(mean_elev_OLD ~ mean_elev_HAKAI, 
                random = list(~1|overlap_period), data=flow.new)
summary(mod_flow) # model summary
mod_flow$coefficients # coefficient estimates

plot(mod_flow) #residual plot

### predicting flow for the ice-over period in 2019
flow_pG<-flow[which(flow$overlap_period=='G'),]
pred_flow_pG<-flow_pG[which(is.na(flow_pG$mean_elev_OLD)),]

flow_predsG<-predict(mod_flow, newdata=pred_flow_pG, type ="response",se.fit=TRUE, level=1)

pred_flow_pG[,6]<-flow_predsG

### predicting flow for the period after the old gauge died
flow_recent<-koeye_flow[which(koeye_flow$year>2018),]
which(flow_recent$doy==148)
dim(flow_recent)
flow_rec<-flow_recent[148:1023,]
flow_rec<-flow_rec[-which(is.na(flow_rec$mean_elev_HAKAI)),]

flow_preds_recent<-predict(mod_flow, newdata=flow_rec, type ="response",se.fit=TRUE, level=0)
flow_rec[,6]<-flow_preds_recent$fit
write.csv(flow_rec,file="koeye_gauge_prediction20-21.csv")

### predicting temperature for the period after 5/28/2019
temp_preds<-predict(mod_temp, newdata=flow_rec,type="response",se.fit=TRUE)

length(temp_preds$fit)

flow_rec[,7]<-temp_preds$fit
write.csv(flow_rec,file="koeye_gauge_prediction20-21_temp.csv")


###### GAM model with code written by Ian Giesbrecht
#GAM
gam.fit<-gam(mean_elev_OLD ~s(mean_elev_HAKAI), data= flow.new, family=gaussian, gamma=1)
summary(gam.fit)
plot(gam.fit)

#Attach predictions and residuals to the training dataset for initial evaluations
flow_recent$elev_GAMpred<-predict(gam.fit, newdata=flow_recent)

flow.new$elev_GAMpred<-predict(gam.fit, newdata = flow.new)
flow.new$elev_GAMresid<-residuals(gam.fit)

write.csv(flow_recent,file="koeye_gauge_GAMprediction20-21.csv")



# Predicted vs observed with a 1:1 line
ggplot(flow.new, aes(x= elev_GAMpred, y= mean_elev_OLD)) + geom_abline(intercept = 0, slope = 1, colour = 'blue', size = 2) + geom_point() + theme_linedraw() + ylab(expression(paste("Observed stage (m)"))) + xlab(expression(paste("GAM predicted stage (m)")))

#Are residuals normally distributed?
ggplot(flow.new, aes(elev_GAMresid)) + geom_histogram(color = "black") + theme_linedraw()

#Residuals vs fitted
ggplot(flow.new, aes(x= elev_GAMpred, y= elev_GAMresid)) + geom_point() + theme_linedraw()

#Look at residuals over time
ggplot(flow.new, aes(x= date, y=elev_GAMresid)) + geom_point() + theme_linedraw()

### this gam that we will use to calibrate the two flow datasets








