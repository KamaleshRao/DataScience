#############################LIBRARIES##########################################

library(lasso2) 
library(lars) 
library(ggplot2)
library(reshape2)
library(MASS)
library(glmnet)
library(lubridate)
#############################GRAPHING PARAMETERS###################################

theme1<-theme_bw()+theme(panel.grid.minor = element_blank())
theme2<-theme1+theme(axis.title.y = element_text(size = rel(2)),axis.title.x = element_text(size = rel(2)),
                     axis.text.y = element_text(size = rel(2)),axis.text.x = element_text(size = rel(2)))

#############################GLOBAL VARIABLES##########################################

todaysdate<-Sys.Date()

setwd('H:/Files and Documents/Work/Calendar Data & Analysis/')

datadir<-'H:/Files and Documents/Work/Calendar Data & Analysis/Data/'
analysisdir<-'H:/Files and Documents/Work/Calendar Data & Analysis/Analysis/'
codedir<-'H:/Files and Documents/Work/Calendar Data & Analysis/Code/'
backupdir<-'C:/Users/e022147/Desktop/Temp/'

minYearMo<-200303
maxYearMo<-201612  
maxxcol<-800
testdataYearMo<-c(201012,200912,maxYearMo)

#excludevec<-c("Date","tdate.format","YearMo","tdate","WeekdayTxt","Day","NewYearDate","MLKDate","ValDayDate","PresDayDate","StPatsDate","EasterDate","MomsDayDate","MemDayDate",
#"DadsDayDate","July4thDate","LabDayDate","ColumDayDate","HallDate","VetDayDate","ThanksDate","XmasDate","NYEveDate","weekday.factor","month.factor","week.factor","xmaswk.factor",
#"year.factor","XmasSeasonNovDays", "Year", "XmasSeasonDays","MonthSumm","MonthCount","DailyPropWgt","LogDailyPropWgt",
#"year.factor1","year.factor2","XmasWk.Weekday.factor.2.1","XmasWk.Weekday.factor.2.2","XmasWk.Weekday.factor.2.3","XmasWk.Weekday.factor.8.3","XmasWk.Weekday.factor.2.4","XmasWk.Weekday.factor.8.4",
#"XmasWk.Weekday.factor.2.5","XmasWk.Weekday.factor.8.5","XmasWk.Weekday.factor.8.6","XmasWk.Weekday.factor.8.7","Week.Weekday.factor.54.3", "Week.Weekday.factor.54.4","Week.Weekday.factor.54.5",
#"Week.Weekday.factor.54.6","Week.Weekday.factor.54.7")                                              

excludevec<-c("Year.calc","Weekday","Month","tdate.format","YearMo","tdate","WeekdayTxt","Day","NewYearDate","MLKDate","ValDayDate","PresDayDate","StPatsDate","EasterDate",
"MomsDayDate","MemDayDate",
"DadsDayDate","July4thDate","LabDayDate","ColumDayDate","HallDate","VetDayDate","ThanksDate","XmasDate","NYEveDate",
"XmasSeasonNovDays", "Year", "XmasSeasonDays",
"year.factor1","year.factor2","XmasWk.Weekday.factor.2.1","XmasWk.Weekday.factor.2.2","XmasWk.Weekday.factor.2.3","XmasWk.Weekday.factor.2.4",
"XmasWk.Weekday.factor.2.5", "Week.Weekday.factor.1.1","Week.Weekday.factor.53.7","Week.Weekday.factor.1.1","Week.Weekday.factor.1.2","Week.Weekday.factor.1.3","Week.Weekday.factor.1.4","Week.Weekday.factor.53.7","xmaswk.factor8",
"XmasWk.Weekday.factor.7.7","Week.Weekday.factor.1.5","Week.Weekday.factor.53.6","Week.Weekday.factor.53.5","Week.Weekday.factor.1.6",
"XmasWk.Weekday.factor.1.1", "XmasWk.Weekday.factor.1.2", "XmasWk.Weekday.factor.1.3", "XmasWk.Weekday.factor.7.3", "XmasWk.Weekday.factor.1.4", "XmasWk.Weekday.factor.7.4",
 "XmasWk.Weekday.factor.1.5" ,"XmasWk.Weekday.factor.7.5","XmasWk.Weekday.factor.7.6","XmasWk.Weekday.factor.7.7")                                              


DailyHolidayTable<-paste(datadir,"USDailyHolidayTable.csv", sep="")
SectorDataTable<-paste(datadir,"USDailyData.csv", sep="")

#############################GENERIC FUNCTIONS##################################

nicemedian<-function(x){return(median(x,na.rm=TRUE))}
nicemean<-function(x){return(mean(x,na.rm=TRUE))}
nicemin<-function(x){return(min(x,na.rm=TRUE))}
nicemax<-function(x){return(max(x,na.rm=TRUE))}



std.data<-function(df){
x<-dim(df)[2]
y<-dim(df)[1]
std.df<-df
for (i in 1:x)
{
std.df[,i]<-(2*scale(df[,i]))
if (is.na(mean(std.df[,i])))
   {std.df[,i]<-NA}
}
std.df
}

############################PROGRAM FUNCTIONS###################################


addfactor<-function(factorv,factorname){
levelv<-as.factor(factorv)
tempvec<-NULL
for (i in 1:length(levels(levelv))){
tempvec<-cbind(tempvec,ifelse(factorv==levels(levelv)[i],1,0))
}
colnames(tempvec)<-paste(factorname,1:i,sep="")
return(tempvec)
} 

addinteractionfactor1<-function(target,mvector,coltext){
tempvec<-NULL
for (i in 1:dim(target)[2]){
tempvec<-cbind(tempvec,target[,i]*mvector)
}
colnames(tempvec)<-paste(coltext,colnames(target),sep=".")
return(tempvec)
} 

addinteractionfactor2<-function(target1,target2,coltext){
tempvec<-NULL
for (j in 1:dim(target2)[2]){
for (i in 1:dim(target1)[2]){
tempvec<-cbind(tempvec,target1[,i]*target2[,j])
colnames(tempvec)[dim(tempvec)[2]]<-paste(coltext,i,j,sep=".")
}}
return(tempvec)
} 

addinteractionfactor3<-function(target1,target2,target3,coltext){
tempvec<-NULL
for (k in 1:dim(target3)[2]){
for (j in 1:dim(target2)[2]){
for (i in 1:dim(target1)[2]){
tempvec<-cbind(tempvec,target1[,i]*target2[,j]*target3[,k])
colnames(tempvec)[dim(tempvec)[2]]<-paste(coltext,i,j,k,sep=".")
}}}
return(tempvec)
} 

addinteractionfactor3b<-function(target1,target2,target3,coltext){
tempvec<-NULL
for (j in 1:dim(target2)[2]){
for (i in 1:dim(target1)[2]){
tempvec<-cbind(tempvec,target1[,i]*target2[,j]*target3)
colnames(tempvec)[dim(tempvec)[2]]<-paste(coltext,i,j,sep=".")
}}
return(tempvec)
} 

###################################DATA PULL AND FORMAT PROGRAMS###########################################


getcalendardata<-function(){
calendardata<-na.omit(read.csv(file=DailyHolidayTable, header=TRUE))
calendardata$tdate.format<-as.Date(calendardata$tdate,"%m/%d/%Y")
calendardata$YearMo<-calendardata$Month+calendardata$Year*100
weekday.factor<-as.factor(calendardata$Weekday)
calendardata<-cbind(calendardata,addfactor(calendardata$Weekday,"weekday.factor"))
month.factor<-as.factor(calendardata$Month)
calendardata<-cbind(calendardata,addfactor(calendardata$Month,"month.factor"))
Week<-as.numeric(format(calendardata$tdate.format,"%U"))+1
Week[calendardata$Year==2006]<-Week[calendardata$Year==2006]-1
Week[calendardata$Year==2012]<-Week[calendardata$Year==2012]-1
Week[calendardata$Year==2017]<-Week[calendardata$Year==2017]-1
week.factor<-as.factor(Week)
calendardata<-cbind(calendardata,addfactor(Week,"week.factor"))
year.factor<-as.factor(calendardata$Year)
calendardata<-cbind(calendardata,addfactor(calendardata$Year,"year.factor"))
#######UPDATE!!!#########################################
#calendardata$year.factor15<-calendardata$year.factor14
#calendardata$year.factor16<-calendardata$year.factor15
calendardata$year.factor17<-calendardata$year.factor16
calendardata$year.factor18<-calendardata$year.factor17
xmaswk.factor<-as.factor(calendardata$XmasWk)
calendardata<-cbind(calendardata,addfactor(calendardata$XmasWk,"xmaswk.factor")[,2:8])
XmasSeasDaysNov<-calendardata$XmasSeasonDays*c(ifelse(calendardata$Month==11,1,0))
XmasSeasDaysNov2<-calendardata$XmasSeasonNovDays*ifelse(calendardata$Month==11,1,0)
XmasSeasDaysDec<-calendardata$XmasSeasonDays*ifelse(calendardata$Month==12,1,0)
calendardata<-cbind(calendardata,XmasSeasDaysNov,XmasSeasDaysNov2,XmasSeasDaysDec)
calendardata<-calendardata[calendardata$YearMo>=minYearMo & !is.na(calendardata$tdate.format),]
minYear<-min(calendardata$Year)
Year.calc<-calendardata$Year-minYear
calendardata<-cbind(calendardata,Year.calc)
monthfactorv<-paste("month.factor",1:12,sep="")
weekdayfactorv<-paste("weekday.factor",1:7,sep="")
xmaswkfactorv<-paste("xmaswk.factor",2:8,sep="")
weekfactorv<-paste("week.factor",1:53,sep="")
  calendardata<-cbind(calendardata,addinteractionfactor1(calendardata[,monthfactorv],calendardata[,"MonthWeek"],"MonthWeek"))
calendardata<-cbind(calendardata,addinteractionfactor2(calendardata[,c(monthfactorv)],calendardata[,c(weekdayfactorv)],"Month.Weekday.factor"))
calendardata<-cbind(calendardata,addinteractionfactor3b(calendardata[,c(monthfactorv)],calendardata[,c(weekdayfactorv)],calendardata[,"MonthWeek"],"MonthWeek.Month.Weekday.factor"))
calendardata<-cbind(calendardata,addinteractionfactor1(calendardata[,weekdayfactorv],calendardata[,"WBXmas"],"WBXmas"))
calendardata<-cbind(calendardata,addinteractionfactor2(calendardata[,c(xmaswkfactorv)],calendardata[,c(weekdayfactorv)],"XmasWk.Weekday.factor"))
calendardata<-cbind(calendardata,addinteractionfactor2(calendardata[,c(weekfactorv)],calendardata[,c(weekdayfactorv)],"Week.Weekday.factor"))
#OTHER FACTOR COMBINATIONS????
calendardata$NewTG<-ifelse(Year.calc>=10, 1,0)*calendardata$Thanks
calendardata$NewBF<-ifelse(Year.calc>=10, 1,0)*calendardata$DAThanks
calendardata$NewBSat<-ifelse(Year.calc>=10, 1,0)*calendardata$SatAThanks
calendardata$NewBSun<-ifelse(Year.calc>=11, 1,0)*calendardata$SunAThanks
calendardata$NewCMon<-ifelse(Year.calc>=10, 1,0)*calendardata$MonAThanks
calendardata$CMonDec<-calendardata$MonAThanks*ifelse(calendardata$Month==11,0,1)
return(calendardata)
}

getsectordata<-function(){
sector.dailydata<-read.csv(file=SectorDataTable, header=TRUE, na.strings = ".")
sector.dailydata$tdate.format<-as.Date(sector.dailydata$Date,"%m/%d/%Y")
newmonth<-as.numeric(format(sector.dailydata$tdate.format,"%m"))
newyear<-as.numeric(format(sector.dailydata$tdate.format,"%Y"))
sector.dailydata<-cbind(newmonth+newyear*100, sector.dailydata)
colnames(sector.dailydata)[1]<-"YearMo"
end_k<-dim(sector.dailydata)[2]-1
for (k in 3:end_k){
sector.min<-.1*min(sector.dailydata[,k], na.rm=T)
sector.dailydata[is.na(sector.dailydata[,k]),k]<-sector.min
sector.monthsumm<-aggregate(sector.dailydata[,k], by=list(YearMo=sector.dailydata$YearMo), FUN="sum")
sector.monthcount<-aggregate(sector.dailydata[,k], by=list(YearMo=sector.dailydata$YearMo), FUN="length")
sector.dailydata<-merge(sector.dailydata,sector.monthsumm, by.x="YearMo", by.y="YearMo", all.x=TRUE)
colnames(sector.dailydata)[length(colnames(sector.dailydata))]<-paste(colnames(sector.dailydata)[k],"MonthSumm",sep="")
sector.dailydata<-merge(sector.dailydata,sector.monthcount, by.x="YearMo", by.y="YearMo", all.x=TRUE)
colnames(sector.dailydata)[length(colnames(sector.dailydata))]<-paste(colnames(sector.dailydata)[k],"MonthCount",sep="")
DailyPropWgt<-(sector.dailydata[,k]/sector.dailydata[,length(colnames(sector.dailydata))-1])*sector.dailydata[,length(colnames(sector.dailydata))]
LogDailyPropWgt<-log(DailyPropWgt)
sector.dailydata<-cbind(sector.dailydata,DailyPropWgt, LogDailyPropWgt)
colnames(sector.dailydata)[length(colnames(sector.dailydata))]<-paste(colnames(sector.dailydata)[k],"LogDailyPropWgt",sep="")
colnames(sector.dailydata)[length(colnames(sector.dailydata))-1]<-paste(colnames(sector.dailydata)[k],"DailyPropWgt",sep="")
}
sector.dailydata<-sector.dailydata[((sector.dailydata$YearMo<=maxYearMo) & (sector.dailydata$YearMo>=minYearMo))
                                   &!is.na(sector.dailydata$YearMo),]

return(sector.dailydata)}

###################################QA###########################################
summarydf<-function(df1)
{
dfwdth<-dim(df1)[2]
minvec<-as.numeric(apply(df1,2,min))
maxvec<-as.numeric(apply(df1,2,max))
meanvec<-mean(df1)

minvec1<-as.numeric(apply(df1,2,nicemin))
maxvec1<-as.numeric(apply(df1,2,nicemax))
meanvec1<-mean(df1, na.rm=TRUE)

summdf<-cbind(minvec, maxvec,meanvec, minvec1, maxvec1, meanvec1)
return(summdf)
}
###################################ANALYSIS PROGRAMS###########################################

calcdailymodels<-function(j)
{
    
    #sector.calendardata<-merge(calendardata, sector.dailydata[,c(-1,-3,-7)], by.x="tdate.format", by.y="tdate.format", all.x=TRUE)
   
    sectorname<-sectornamevec[j]
    sectorlogpropwgt<-paste(sectorname,"LogDailyPropWgt",sep="")
    sectorpropwgt<-paste(sectorname,"DailyPropWgt",sep="")
    print (paste("Entering sector loop",sectorname,Sys.time(), sep="::"))

    #excludevec[is.na(match(excludevec,colnames(sector.calendardata)))]

    
    y<-sector.calendardata[sector.calendardata$YearMo>=minYearMo&sector.calendardata$YearMo<=maxYearMo,sectorlogpropwgt]
    
    y.std<-scale(y)
    sd.y<-sd(y)
    mean.y<-mean(y)

    #y.std<-exp(y)
    #temptry<-try(lasso.m1<-lars(as.matrix(sector.calendardata.xvec[trainvec,]),y.std[trainvec]), silent=F)
    
    
    #trainvec<-trainvec[1:length(y)] 
    
    lasso.res.all.matrix<-NULL
    #for(j in 1:length(YearMo.vec)){
    #print(paste("Begin lasso run",YearMo.vec[j],Sys.time(), sep=":"))
    #trainvec<-sector.calendardata$YearMo<=YearMo.vec[j]
    #trainvec<-trainvec[1:length(y)]
    print (paste("Beginning Lasso Run",sectorname,Sys.time(), sep="::"))
    #lasso.m1<-lars(as.matrix(sector.calendardata.xvec.std[trainvec,]),y[trainvec])
    #lasso.m1<-lars(as.matrix(sector.calendardata.xvec.std[trainvec,]),y.std[trainvec],intercept=FALSE)
    
    #lasso.m1<-lars(as.matrix(sector.calendardata.xvec.std[trainvec,]),y.std[trainvec],intercept=FALSE)
    print(ht(y))
    temptry<-try(lasso.m1<-lars(as.matrix(sector.calendardata.xvec.std[trainvec,]),y.std[trainvec],intercept=FALSE), silent=F)
    png(file=paste("Lasso_", sectorname, Sys.Date(), ".png",sep=""))   
    plot(lasso.m1, plottype=c("Cp"))
    title(sectorname, line=.5)
    abline(v=.3,lty=2, col="red")
    dev.off()
    lasso.res.vec<-NULL
    #print (paste("End Lasso Run/Begin Residual calc loop",sectorname,Sys.time(), sep="::"))
    #for (i in 0:100){
    #print(paste("Begin residual calc",Sys.time(), sep=":"))
    ##lasso.m1.abs.res.train<-abs(y[trainvec]-predict.lars(lasso.m1, newx=sector.calendardata.xvec.std[trainvec,],type=c("fit"),s=i/100, mode=c("fraction"))$fit)
    #lasso.m1.abs.res.exp.train<-abs(exp(y)-exp(predict.lars(lasso.m1, newx=sector.calendardata.xvec.std,type=c("fit"),s=i/100, mode=c("fraction"))$fit))
    ##lasso.m1.abs.res.test<-abs(y[!trainvec]-predict.lars(lasso.m1, newx=sector.calendardata.xvec.std[!trainvec,],type=c("fit"),s=i/100, mode=c("fraction"))$fit)
    ##lasso.m1.abs.res.exp.test<-abs(exp(y[!trainvec])-exp(predict.lars(lasso.m1, newx=sector.calendardata.xvec.std[!trainvec,],type=c("fit"),s=i/100, mode=c("fraction"))$fit))
    ##lasso.res.vec<-rbind(lasso.res.vec,c(i,median(lasso.m1.abs.res.exp.train,na.rm=TRUE),median(lasso.m1.abs.res.exp.test,na.rm=TRUE)))
    #lasso.res.vec<-rbind(lasso.res.vec,c(i,median(lasso.m1.abs.res.exp.train,na.rm=TRUE)))
   
    #print(paste("End residual calc",Sys.time(), sep=":"))
    #}
    #write.csv(lasso.res.vec,file="resvec.csv")  
    #print (paste("End Residual calc loop",sectorname,Sys.time(), sep="::"))
    #print(paste("End lasso run",YearMo.vec[j],Sys.time(), sep=":"))
    #lasso.res.all.matrix<-cbind(lasso.res.all.matrix,lasso.res.vec)
    ##plot(lasso.res.vec[,2], type="o", lty=3,main=sectorname)
    ##points(lasso.res.vec[,3], type="o", pch=2)
    #print(paste("dim sector.calendardata.xvec.std",dim(sector.calendardata.xvec.std),sep=":"))
    if (is.character(temptry)){
    lasso.lm1.pred<-rep(NA,dim(sector.calendardata.xvec.std)[1])
    }
    else{
    
      lasso.lm1.pred<-predict(lasso.m1, newx=sector.calendardata.xvec.std,s=100/100, mode=c("fraction"))$fit
    }
    #lasso.lm1.coef<-coef(lasso.m1, s=30/100, mode=c("fraction"))
    #lasso.lm1.pred2<-as.matrix(sector.calendardata.xvec.std) %*% lasso.lm1.coef
    #regtest<-cbind(rbind(sector.calendardata.xvec.std,lasso.lm1.coef),c(lasso.lm1.pred,NA),c(lasso.lm1.pred2,NA),c(y.std,rep(NA,length(lasso.lm1.pred)-length(y.std)),NA))
    #write.csv(regtest,file=paste(sectorname,"regtest.csv",sep=""))    
    
    preddata<-cbind(lasso.lm1.pred*sd.y+mean.y, calendardata$YearMo)

    colnames(preddata)<-c("lasso.lm1.pred","YearMo")
    pred.monthsumm<-aggregate(exp(preddata[,1]), by=list(YearMo=preddata[,2]), FUN="sum")
    
    predwgt<-cbind(pred.monthsumm[,1],daycount/pred.monthsumm[,2])    
    colnames(predwgt)<-c("YearMo","predwgt")
     adjpredcalc<-merge(preddata,predwgt, by.x="YearMo", by.y="YearMo", all.x=TRUE)
    #predsumm<-t(lasso.lm1.coef) %*% sector.calendardata.xvec
    adjpred<-exp(preddata[,1])*adjpredcalc$predwgt
    predsumm<-cbind(adjpred,c(exp(y), rep(NA,length(adjpred)-length(y))))

    #colnames(predsumm)[c(dim(predsumm)[2]-1,dim(predsumm)[2])]<-c(sectorname,paste(sectorname,"actual", sep=":"))
    colnames(predsumm)<-c(paste(sectorname,"forecast", sep=":"),paste(sectorname,"actual", sep=":"))
    print (paste("Exiting sector loop",sectorname,Sys.time(), sep="::"))

    return(predsumm)
}

calcdailymodels_test<-function(j)
{
  
  sectorname<-sectornamevec[j]
  sectorlogpropwgt<-paste(sectorname,"LogDailyPropWgt",sep="")
  sectorpropwgt<-paste(sectorname,"DailyPropWgt",sep="")
  print (paste("Entering sector loop",sectorname,Sys.time(), sep="::"))
  
  y<-sector.calendardata[sector.calendardata$YearMo>=minYearMo&sector.calendardata$YearMo<=maxYearMo,sectorlogpropwgt]
  y.std<-scale(y)
  #y.std<-ifelse(y.std>-2.5, y.std, -2.5+((2.5+y)/10))
  sd.y<-sd(y)
  mean.y<-mean(y)
  
  lasso.res.all.matrix<-NULL
  print (paste("Beginning Lasso Run",sectorname,Sys.time(), sep="::"))
  
  print (paste("Beginning Lasso Modeling",sectorname,Sys.time(), sep="::"))
  #print(ht(y.std))
  temptry<-try(lasso.m1<-glmnet(as.matrix(sector.calendardata.xvec[trainvec,]),y.std[trainvec],family="gaussian", 
                                standardize=FALSE, weights=weight_vec[trainvec]), silent=F)

  if (is.character(temptry)){
    print (paste("Ended Lasso Modeling-Model Error",sectorname,Sys.time(), sep="::"))
    lasso.lm1.pred<-rep(NA,dim(sector.calendardata.xvec.std)[1])
  }
  else{
    print (paste("Ended Lasso Modeling-Success",sectorname,Sys.time(), sep="::"))
    error_mat<-NULL
    print (paste("Entering Error Plotting",sectorname,Sys.time(), sep="::"))
    ###VECTORIZE!
    for (i in (1:length(lasso.m1$lambda))){
      lasso.lm1.pred<-predict(lasso.m1, newx=as.matrix(sector.calendardata.xvec),s=lasso.m1$lambda[i])
      y.std.for<-c(y.std, rep(NA, length(lasso.lm1.pred)-length(y.std)))
      medae<-nicemedian(abs(y.std.for-lasso.lm1.pred[,1]))
      meanae<-nicemean(abs(y.std.for-lasso.lm1.pred[,1]))
      error_mat<-rbind(error_mat, c(i, medae, meanae))
    }
  
    colnames(error_mat)<-c("i", "MEDAE", "MEANAE")
  
    error_reshape <- melt(error_mat[,c(2,3)])
    colnames(error_reshape)<-c("x", "measure", "value") 
    
    g_temp<-ggplot(error_reshape,
               aes(x=x, y=value,colour=measure))+
          geom_line(alpha=.75)+
          #geom_line(alpha=alphavec[error_reshape$calendardata.YearMo>=minYearMo])+
          ylab("")+
          xlab("")+
          ggtitle(sectorname)+
          scale_color_hue(l=30)+
          theme2
    print(g_temp)
    png(file=paste("ModelErrors_", sectornamevec[j], Sys.Date(), ".png",sep=""), height=480, width=900)   
    print(g_temp)
    dev.off()
    print (paste("Ending Error Plotting",sectorname,Sys.time(), sep="::"))
    lasso.lm1.pred<-predict(lasso.m1, newx=as.matrix(sector.calendardata.xvec))
    lasso.lm1.pred<-lasso.lm1.pred[,dim(lasso.lm1.pred)[2]]
  #lasso.res.vec<-NULL
  }
   preddata<-cbind(lasso.lm1.pred*sd.y+mean.y, calendardata$YearMo)
#   
   colnames(preddata)<-c("lasso.lm1.pred","YearMo")
   pred.monthsumm<-aggregate(exp(preddata[,1]), by=list(YearMo=preddata[,2]), FUN="sum")
#   
   predwgt<-cbind(pred.monthsumm[,1],daycount/pred.monthsumm[,2])    
   colnames(predwgt)<-c("YearMo","predwgt")
   adjpredcalc<-merge(preddata,predwgt, by.x="YearMo", by.y="YearMo", all.x=TRUE)
   adjpred<-exp(preddata[,1])*adjpredcalc$predwgt
   predsumm<-cbind(adjpred,c(exp(y), rep(NA,length(adjpred)-length(y))))
   
   colnames(predsumm)<-c(paste(sectorname,"forecast", sep=":"),paste(sectorname,"actual", sep=":"))
   print (paste("Exiting sector loop",sectorname,Sys.time(), sep="::"))
#   
   return(predsumm)

}


calcdailymodels_stepwise<-function(j)
{
  
  #sector.calendardata<-merge(calendardata, sector.dailydata[,c(-1,-3,-7)], by.x="tdate.format", by.y="tdate.format", all.x=TRUE)
  
  sectorname<-sectornamevec[j]
  sectorlogpropwgt<-paste(sectorname,"LogDailyPropWgt",sep="")
  sectorpropwgt<-paste(sectorname,"DailyPropWgt",sep="")
  print (paste("Entering sector loop",sectorname,Sys.time(), sep="::"))
  
  #excludevec[is.na(match(excludevec,colnames(sector.calendardata)))]
  
  
  y<-sector.calendardata[sector.calendardata$YearMo>=minYearMo&sector.calendardata$YearMo<=maxYearMo,sectorlogpropwgt]
  y.std<-scale(y)
  sd.y<-sd(y)
  mean.y<-mean(y)
  
  #trainvec<-trainvec[1:length(y)] 
  
  #lasso.res.all.matrix<-NULL
  #for(j in 1:length(YearMo.vec)){
  #print(paste("Begin lasso run",YearMo.vec[j],Sys.time(), sep=":"))
  #trainvec<-sector.calendardata$YearMo<=YearMo.vec[j]
  #trainvec<-trainvec[1:length(y)]
  print (paste("Beginning Stepwise Run",sectorname,Sys.time(), sep="::"))
  #lasso.m1<-lars(as.matrix(sector.calendardata.xvec.std[trainvec,]),y[trainvec])
  #lasso.m1<-lars(as.matrix(sector.calendardata.xvec.std[trainvec,]),y.std[trainvec],intercept=FALSE)
  
  #lasso.m1<-lars(as.matrix(sector.calendardata.xvec.std[trainvec,]),y.std[trainvec],intercept=FALSE)
  #temptry<-try(lasso.m1<-lars(as.matrix(sector.calendardata.xvec.std[trainvec,]),y.std[trainvec],intercept=FALSE), silent=F)
  lm1<-lm(y.std[trainvec]~., data=data.frame(sector.calendardata.xvec.std[trainvec,]))
  stepwise.m1<-stepAIC(lm1, direction="forward", trace=0)
  #lasso.res.vec<-NULL
  #step$anova
  #print (paste("End Lasso Run/Begin Residual calc loop",sectorname,Sys.time(), sep="::"))
  #for (i in 0:10){
  #print(paste("Begin residual calc",Sys.time(), sep=":"))
  ##lasso.m1.abs.res.train<-abs(y[trainvec]-predict.lars(lasso.m1, newx=sector.calendardata.xvec.std[trainvec,],type=c("fit"),s=i/100, mode=c("fraction"))$fit)
  #lasso.m1.abs.res.exp.train<-abs(exp(y)-exp(predict.lars(lasso.m1, newx=sector.calendardata.xvec.std,type=c("fit"),s=i/10, mode=c("fraction"))$fit))
  ##lasso.m1.abs.res.test<-abs(y[!trainvec]-predict.lars(lasso.m1, newx=sector.calendardata.xvec.std[!trainvec,],type=c("fit"),s=i/100, mode=c("fraction"))$fit)
  ##lasso.m1.abs.res.exp.test<-abs(exp(y[!trainvec])-exp(predict.lars(lasso.m1, newx=sector.calendardata.xvec.std[!trainvec,],type=c("fit"),s=i/100, mode=c("fraction"))$fit))
  ##lasso.res.vec<-rbind(lasso.res.vec,c(i,median(lasso.m1.abs.res.exp.train,na.rm=TRUE),median(lasso.m1.abs.res.exp.test,na.rm=TRUE)))
  #lasso.res.vec<-rbind(lasso.res.vec,c(i,median(lasso.m1.abs.res.exp.train,na.rm=TRUE)))
  
  #print(paste("End residual calc",Sys.time(), sep=":"))
  #}
  #write.csv(lasso.res.vec,file="resvec.csv")  
  #print (paste("End Residual calc loop",sectorname,Sys.time(), sep="::"))
  #print(paste("End lasso run",YearMo.vec[j],Sys.time(), sep=":"))
  #lasso.res.all.matrix<-cbind(lasso.res.all.matrix,lasso.res.vec)
  #plot(lasso.res.vec[,2], type="o", lty=3,main=sectorname)
  ##points(lasso.res.vec[,3], type="o", pch=2)
  #print(paste("dim sector.calendardata.xvec.std",dim(sector.calendardata.xvec.std),sep=":"))
  #if (is.character(temptry)){
  #  lasso.lm1.pred<-rep(NA,dim(sector.calendardata.xvec.std)[1])
  #}
  #else{
  step.lm1.pred<-predict(stepwise.m1, newdata=sector.calendardata.xvec.std)
  #}
  #lasso.lm1.coef<-coef(lasso.m1, s=30/100, mode=c("fraction"))
  #lasso.lm1.pred2<-as.matrix(sector.calendardata.xvec.std) %*% lasso.lm1.coef
  #regtest<-cbind(rbind(sector.calendardata.xvec.std,lasso.lm1.coef),c(lasso.lm1.pred,NA),c(lasso.lm1.pred2,NA),c(y.std,rep(NA,length(lasso.lm1.pred)-length(y.std)),NA))
  #write.csv(regtest,file=paste(sectorname,"regtest.csv",sep=""))    
  
  preddata<-cbind(step.lm1.pred*sd.y+mean.y, calendardata$YearMo)
  
  colnames(preddata)<-c("step.lm1.pred","YearMo")
  pred.monthsumm<-aggregate(exp(preddata[,1]), by=list(YearMo=preddata[,2]), FUN="sum")
  
  predwgt<-cbind(pred.monthsumm[,1],daycount/pred.monthsumm[,2])    
  colnames(predwgt)<-c("YearMo","predwgt")
  adjpredcalc<-merge(preddata,predwgt, by.x="YearMo", by.y="YearMo", all.x=TRUE)
  #predsumm<-t(lasso.lm1.coef) %*% sector.calendardata.xvec
  adjpred<-exp(preddata[,1])*adjpredcalc$predwgt
  predsumm<-cbind(adjpred,c(exp(y), rep(NA,length(adjpred)-length(y))))
  
  #colnames(predsumm)[c(dim(predsumm)[2]-1,dim(predsumm)[2])]<-c(sectorname,paste(sectorname,"actual", sep=":"))
  colnames(predsumm)<-c(paste(sectorname,"forecast", sep=":"),paste(sectorname,"actual", sep=":"))
  print (paste("Exiting sector loop",sectorname,Sys.time(), sep="::"))
  
  return(predsumm)
}

calcprederror<-function(predtemp){
  prederror<-abs(predtemp[,1]-predtemp[,2])
  print (paste("Median Error, Mean Error", nicemedian(prederror),nicemean(prederror)))
  
}

plotforecast<-function(pred.table,j){
  minplotYearMo<-201201
  error_reshape <- melt(pred.table, id=c("calendardata.tdate.format","calendardata.YearMo"))
  alphavec<-ifelse(error_reshape$variable==colnames(pred.table)[4], .25,.75)
     
  g_temp<-ggplot(error_reshape[error_reshape$calendardata.YearMo>=minplotYearMo,],
         aes(x=calendardata.tdate.format, y=value,colour=variable))+
    geom_line(alpha=.75)+
    #geom_line(alpha=alphavec[error_reshape$calendardata.YearMo>=minYearMo])+
    ylab("")+
    xlab("")+
    scale_color_hue(l=30)+
    theme2
  print (g_temp)
  png(file=paste("Forecast_", sectornamevec[j], Sys.Date(), ".png",sep=""),height=480, width=900)
  print(g_temp)
  dev.off()
}

###################################RUN PROGRAMS###########################################

calendardata<-getcalendardata()
daycount<-aggregate(calendardata$YearMo, by=list(YearMo=calendardata$YearMo), FUN="length")[,2]
sector.dailydata<-getsectordata()

sector.calendardata<-merge(calendardata, sector.dailydata[,-c(1)], by.x="tdate.format", by.y="tdate.format", all.x=TRUE)
#write.csv(sector.calendardata, file="sectorcalendardata.csv")
summ.sector.calendardata<-summarydf(sector.calendardata)
rownames(summ.sector.calendardata)<-colnames(sector.calendardata)
write.csv(summ.sector.calendardata, file="sectorcalendarsumm.csv")
sectornamevec<-colnames(read.csv(file=SectorDataTable, header=TRUE))
sector.calendardata.xvec<-sector.calendardata[,-match(c(excludevec, colnames(sector.dailydata)),colnames(sector.calendardata))]
#sector.calendardata.xvec.std<-data.frame(lapply(sector.calendardata.xvec, scale))

weight_vec<-rep(1,dim(sector.calendardata)[1])
weight_vec[sector.calendardata$Year==2013]<-2
weight_vec[sector.calendardata$Year>2013]<-4

sector.calendardata.xvec.std<-data.frame(lapply(sector.calendardata.xvec, scale))
#[,1:700]
#sector.calendardata.std<-std.data(data.frame(sector.calendardata.xvec))[sector.calendardata$YearMo>=minYearMo,]
#sector.calendardata.xvec.std<-std.data(data.frame(sector.calendardata.xvec))[sector.calendardata$YearMo>=minYearMo&sector.calendardata$YearMo<=maxYearMo,]
YearMo.vec<-as.integer(levels(as.factor(sector.calendardata$YearMo)))
trainvec<-sector.calendardata$YearMo>=minYearMo&sector.calendardata$YearMo<=testdataYearMo[3]
#trainvec<-trainvec[sector.calendardata$YearMo<=maxYearMo]
###########error checking
max(tvec<-(data.frame(lapply(sector.calendardata.xvec.std[trainvec,], sum))))
max(tvec2<-(data.frame(lapply(sector.calendardata.xvec.std[trainvec,], mean))))    
min(tvec3<-(data.frame(lapply(sector.calendardata.xvec.std[trainvec,], min))))    
max(tvec4<-(data.frame(lapply(sector.calendardata.xvec.std[trainvec,], max))))    


totalpredsumm<-NULL
# #for ( j in 2:length(sectornamevec))
# for ( j in 2:2)
# for ( j in 8:length(sectornamevec))
# #for ( j in 8:12)
# 
# {
# print (j)
# predtemp<-calcdailymodels(j)
# calcprederror(predtemp)
# pred.table<-data.frame(calendardata$YearMo,calendardata$tdate.format, predtemp)
# plotforecast(pred.table,j)
# #predtemp<-calcdailymodels_fulllambda(j)
# #calcprederror(predtemp)
# #pred.table<-data.frame(calendardata$YearMo,calendardata$tdate.format, predtemp)
# #plotforecast(pred.table,j)
# totalpredsumm<-cbind(totalpredsumm,predtemp)
# }

for (j in 2:(length(sectornamevec)-1)){
#for (j in c(3,5,11,23,36,37,21,39,68)){
print (sectornamevec[j])
predtemp<-calcdailymodels_test(j)
calcprederror(predtemp)
pred.table<-data.frame(calendardata$YearMo,calendardata$tdate.format, predtemp)
print(tail(pred.table[calendardata$Thanks==1,]))
print(tail(pred.table[calendardata$DAThanks==1,]))
print(tail(pred.table[calendardata$SatAThanks==1,]))
print(tail(pred.table[calendardata$SunAThanks==1,]))
print(tail(pred.table[calendardata$MonAThanks==1,]))
plotforecast(pred.table,j)
totalpredsumm<-cbind(totalpredsumm,predtemp)
}

write.csv(totalpredsumm, file=paste(backupdir, "test_totalpredsumm8.csv", sep=""))
write.csv(totalpredsumm, file=paste("dailypropmodels_", Sys.Date(), ".csv",sep=""))

totalpredsumm<-NULL
#for ( j in 2:length(sectornamevec))
#for ( j in 2:2)
for (j in c(15,24))
  
{
  predtemp<-calcdailymodels_stepwise(j)
  calcprederror(predtemp)
  pred.table<-data.frame(calendardata$YearMo,calendardata$tdate.format, predtemp)
  plotforecast(pred.table,j)
  totalpredsumm<-cbind(totalpredsumm,predtemp)
}
write.csv(totalpredsumm, file=paste("dailypropmodels_redo_", Sys.Date(), ".csv",sep=""))



prederrorsummary<-NULL
#for (j in 2:10){
for ( j in 10:length(sectornamevec)){
prederrorvec<-NULL
tempsectorname<-sectornamevec[j]
for (i in 720:730){
sector.calendardata.xvec.std<-data.frame(lapply(sector.calendardata.xvec, scale))[,1:i]
predtemp<-calcdailymodels(j)
prederror<-median(abs(predtemp[1:3259,1]-predtemp[1:3259,2]))
prederrorvec<-rbind(prederrorvec, c(i, prederror, mean(predtemp[,1])))
}
#print(paste("Exit loop1",Sys.time(), sep=":"))

colnames(prederrorvec)<-c(i, paste(tempsectorname,c("MedError","AvgForecast"), sep=":"))
#temp.rawabc<-temp.rawabc[,-match(c("IDCODE1.x","IDCODE2.x","IDCODE3.x","IDCODE1.y","IDCODE2.y","IDCODE3.y"),colnames(temp.rawabc))]
#print(paste("Test1",Sys.time(), sep=":"))
if(j>2){
prederrorvec<-prederrorvec[,2:3]
}
prederrorsummary<-cbind(prederrorsummary, prederrorvec)
#print(paste("Test2",Sys.time(), sep=":"))
}
write.csv(prederrorsummary, file="prederrorsummary(02-16-2012).csv")


jvec<-c(724,724,724,720,730,724,723,723,724,725,723,725,725,725,725,726,726,728,724,726,726,726)
tempsectvec<-c(2:8,10:length(sectornamevec))
totalpredsumm<-NULL
#for (j in 1:length(tempsectvec)){
for (j in 7:10){
sector.calendardata.xvec.std<-data.frame(lapply(sector.calendardata.xvec, scale))[,1:jvec[j]]
predtemp<-calcdailymodels(tempsectvec[j])
totalpredsumm<-cbind(totalpredsumm,predtemp)
}
write.csv(totalpredsumm, file="dailypropmodels(02-16-2012).csv")


sector.calendardata.xvec.std<-data.frame(lapply(sector.calendardata.xvec, scale))[,1:jvec[7]]
predtemp.jewel<-calcdailymodels(tempsectvec[7])

sector.calendardata.xvec.std<-data.frame(lapply(sector.calendardata.xvec, scale))[,1:jvec[8]]
predtemp.auto<-calcdailymodels(tempsectvec[8])


prederrorsummary<-NULL
#for (j in 2:4){
#for ( j in tempsectvec){
for (j in 18){
prederrorvec<-NULL
tempsectorname<-sectornamevec[j]
for (i in 720:730){
sector.calendardata.xvec.std<-data.frame(lapply(sector.calendardata.xvec, scale))[,1:i]
predtemp<-calcdailymodels(j)
prederror<-median(abs(predtemp[1:3259,1]-predtemp[1:3259,2]))
prederrorvec<-rbind(prederrorvec, c(i, prederror, mean(predtemp[,1])))
write.csv(predtemp, file=paste("predtemp(02-16-2012)_",sectornamevec[j],"_",i,"_",".csv",sep=""))
}
#print(paste("Exit loop1",Sys.time(), sep=":"))

colnames(prederrorvec)<-c(i, paste(tempsectorname,c("MedError","AvgForecast"), sep=":"))
#temp.rawabc<-temp.rawabc[,-match(c("IDCODE1.x","IDCODE2.x","IDCODE3.x","IDCODE1.y","IDCODE2.y","IDCODE3.y"),colnames(temp.rawabc))]
#print(paste("Test1",Sys.time(), sep=":"))
prederrorsummary<-cbind(prederrorsummary, prederrorvec)
#print(paste("Test2",Sys.time(), sep=":"))
}
write.csv(prederrorsummary, file="tempprederrorsummary(02-16-2012).csv")



sector.calendardata.xvec.std<-data.frame(lapply(sector.calendardata.xvec, scale))[,1:i]
predtemp<-calcdailymodels(j)






#testdataYearMo<-c(200712,200812)
#YearMo.vec<-as.integer(levels(as.factor(sector.calendardata$YearMo)))[25:86]

allresvec2<-NULL


#y<-sector.calendardata[,"LogDailyPropWgt"]
#sector.xvec.full<-sector.calendardata[,-match(excludevec,colnames(sector.calendardata))]
#sector.xvec.full.pc<-predict(princomp(sector.xvec.full))

#for(j in 30:length(YearMo.vec){

#sector.xvec.train<-sector.calendardata[trainvec,-match(excludevec,colnames(sector.calendardata))]

#sector.xvec.test<-sector.calendardata[!trainvec,-match(excludevec,colnames(sector.calendardata))]

#sector.xvec.train.pc<-predict(princomp(sector.xvec.train))

res1vec<-NULL


for (i in 1:maxxcol){
lm1<-lm(y[trainvec]~.,data=as.data.frame(sector.xvec.full.pc[trainvec,1:i]))
lm1.abs.res.train<-abs(lm1$residuals)
lm1.abs.res.exp.train<-abs(exp(y[trainvec])-exp(predict(lm1)))

lm1.predict.train.full<-as.matrix(cbind(rep(1,dim(sector.xvec.full.pc)[1]),sector.xvec.full.pc))[,1:(i+1)]%*%coef(lm1)
lm1.res.test<-(y-lm1.predict.train.full)[!trainvec]
lm1.abs.res.test<-abs(lm1.res.test)
lm1.abs.res.exp.test<-abs(exp(y[!trainvec])-exp(lm1.predict.train.full[!trainvec]))

res1vec<-rbind(res1vec,c(j,median(lm1.abs.res.exp.train,na.rm=TRUE),median(lm1.abs.res.exp.test,na.rm=TRUE)))
print(paste("End model calc",i, Sys.time(), sep=":"))
}
print(paste("End train run calc",j,Sys.time(), sep=":"))
colnames(res1vec)<-paste(YearMo.vec[j],c("i","median.train.exp","median.test.exp"),sep=".")
allresvec2<-cbind(allresvec2,res1vec)
}


#write.csv(res1vec, file="res1vec.csv")
#write.csv(allresvec, file="allresvec.csv")
write.csv(allresvec2, file="pc2_v2.csv")

pc.res.all<-read.csv(file="pcrestotal.csv", header=TRUE)
plot(pc.res.all$X,pc.res.all$X201002.median.train.exp, type="l", lwd=5, ylim=c(0,max(pc.res.all$X201002.median.train.exp))+.001, xlab="pc", ylab="mad", main="PC Analysis:train")
abline(a=.01437, b=0,col=4)
abline(a=.01676, b=0,col=4, lty=2)

for (i in 1:62)
{
points(pc.res.all[,(i*3)], type="l", lwd=.5, col=i)
}
points(pc.res.all$X,pc.res.all$X201002.median.train.exp, type="l", lwd=5)

plot(pc.res.all$X,pc.res.all$X201002.median.train.exp, type="l", lwd=5, ylim=c(0,max(pc.res.all$X201002.median.train.exp))+.001, xlab="pc", ylab="mad", main="PC Analysis:test")
abline(a=.01437, b=0,col=4)
abline(a=.01676, b=0,col=4, lty=2)

for (i in 1:61)
{
points(pc.res.all[,(i*3)+1], type="l", lwd=.5, col=i)
}
points(pc.res.all$X,pc.res.all$X201002.median.train.exp, type="l", lwd=5)



pc.res.all.summ<-NULL
for (i in 1:length(YearMo.vec)){
tempvec<-c(YearMo.vec[i],median(pc.res.all[,(i*3)+1], na.rm=TRUE),mean(pc.res.all[,(i*3)+1], na.rm=TRUE),min(pc.res.all[,(i*3)+1],na.rm=TRUE),pc.res.all[51,(i*3)+1],pc.res.all[151,(i*3)+1],pc.res.all[251,(i*3)+1],
median(pc.res.all[,(i*3)],na.rm=TRUE),mean(pc.res.all[,(i*3)],na.rm=TRUE),min(pc.res.all[,(i*3)],na.rm=TRUE),pc.res.all[51,(i*3)],pc.res.all[151,(i*3)],pc.res.all[251,(i*3)])
pc.res.all.summ<-rbind(pc.res.all.summ,tempvec)
}

pc.res.all.summ<-data.frame(pc.res.all.summ)
colnames(pc.res.all.summ)<-c("Date", "median.res.test","mean.res.test","min.res.test","pc51.res.test","pc151.res.test","pc251.res.test",
"median.res.train","mean.res.train","min.res.train","pc51.res.train","pc151.res.train","pc251.res.train")
plot(pc.res.all.summ$pc51.res.train, pch=16,cex=2, type="o", lty=3, xaxt="n", ylim=c(ymin=0, ymax=.1))
axis(1,at=c(1:6)*10,lab=F)
text(x=c(1:6)*10,y=-.006,labels=c(YearMo.vec[c(1:6)*10]), xpd=T)
points(pc.res.all.summ$pc151.res.train, type="o",pch=17, cex=1)
points(pc.res.all.summ$pc251.res.train, type="o",pch=18, cex=1)
points(pc.res.all.summ$min.res.train, type="o",pch=3,cex=1)
points(pc.res.all.summ$pc51.res.test, type="o",pch=16, cex=1,col=4)
points(pc.res.all.summ$pc151.res.test, type="o",pch=17, cex=1,col=4)
points(pc.res.all.summ$pc251.res.test, type="o",pch=18, cex=1,col=4)
points(pc.res.all.summ$min.res.test, type="o",pch=3, cex=1,col=4)




std.data<-function(df){
x<-dim(df)[2]
y<-dim(df)[1]
std.df<-df
for (i in 1:x)
{
std.df[,i]<-(scale(df[,i]))
if (is.na(mean(std.df[,i])))
   {std.df[,i]<-NA}
}
std.df
}


sector.xvec.full.std<-data.frame(scale(sector.xvec.full))

j<-50
lasso.res.all.matrix<-NULL
for(j in 1:length(YearMo.vec)){
print(paste("Begin lasso run",YearMo.vec[j],Sys.time(), sep=":"))
trainvec<-sector.calendardata$YearMo<=YearMo.vec[j]
lasso.m1<-lars(as.matrix(sector.xvec.full.std[trainvec,]),y[trainvec])
lasso.res.vec<-NULL
for (i in 0:100){
#print(paste("Begin residual calc",Sys.time(), sep=":"))
lasso.m1.abs.res.train<-abs(y[trainvec]-predict.lars(lasso.m1, newx=sector.xvec.full.std[trainvec,],type=c("fit"),s=i/100, mode=c("fraction"))$fit)
lasso.m1.abs.res.exp.train<-abs(exp(y[trainvec])-exp(predict.lars(lasso.m1, newx=sector.xvec.full.std[trainvec,],type=c("fit"),s=i/100, mode=c("fraction"))$fit))
lasso.m1.abs.res.test<-abs(y[!trainvec]-predict.lars(lasso.m1, newx=sector.xvec.full.std[!trainvec,],type=c("fit"),s=i/100, mode=c("fraction"))$fit)
lasso.m1.abs.res.exp.test<-abs(exp(y[!trainvec])-exp(predict.lars(lasso.m1, newx=sector.xvec.full.std[!trainvec,],type=c("fit"),s=i/100, mode=c("fraction"))$fit))
lasso.res.vec<-rbind(lasso.res.vec,c(paste(YearMo.vec[j],i,sep="."),median(lasso.m1.abs.res.exp.train,na.rm=TRUE),median(lasso.m1.abs.res.exp.test,na.rm=TRUE)))
print(paste("End residual calc",Sys.time(), sep=":"))
}
print(paste("End lasso run",YearMo.vec[j],Sys.time(), sep=":"))
lasso.res.all.matrix<-cbind(lasso.res.all.matrix,lasso.res.vec)
}

write.csv(lasso.res.all.matrix, file="lasso.res.all.matrix.csv")

lasso.res.all<-read.csv(file="lasso.res.all.matrix.csv", header=TRUE)
plot(lasso.res.all$X,lasso.res.all$V185, type="l", lwd=5, ylim=c(0,max(lasso.res.all$V185))+.001)
abline(a=.01437, b=0,col=4)
abline(a=.01676, b=0,col=4, lty=2)

for (i in 1:61)
{
points(lasso.res.all[,(i*3)+1], type="l", lwd=.5, col=i)
}
points(lasso.res.all$V185, type="l", lwd=5)

plot(lasso.res.all$X,lasso.res.all$V185, type="l", lwd=5, ylim=c(0,max(lasso.res.all$V185))+.001)
abline(a=.01437, b=0,col=4)
abline(a=.01676, b=0,col=4, lty=2)

for (i in 1:61)
{
points(lasso.res.all[,(i*3)], type="l", lwd=.5, col=i)
}
points(lasso.res.all$V185, type="l", lwd=5)


lasso.res.all.summ<-NULL
for (i in 1:length(YearMo.vec)){
tempvec<-c(YearMo.vec[i],median(lasso.res.all[,(i*3)+1]),mean(lasso.res.all[,(i*3)+1]),min(lasso.res.all[,(i*3)+1]),lasso.res.all[51,(i*3)+1],
median(lasso.res.all[,(i*3)]),mean(lasso.res.all[,(i*3)]),min(lasso.res.all[,(i*3)]),lasso.res.all[51,(i*3)])
lasso.res.all.summ<-rbind(lasso.res.all.summ,tempvec)
}
lasso.res.all.summ<-data.frame(lasso.res.all.summ)
colnames(lasso.res.all.summ)<-c("Date", "median.res.test","mean.res.test","min.res.test","midpoint.res.test",
"median.res.train","mean.res.train","min.res.train","midpoint.res.train")


plot(lasso.res.all.summ$midpoint.res.train, pch=16,cex=2, type="o", lty=3, xaxt="n", ylim=c(ymin=0, ymax=.1))
axis(1,at=c(1:6)*10,lab=F)
text(x=c(1:6)*10,y=-.006,labels=c(YearMo.vec[c(1:6)*10]), xpd=T)
points(lasso.res.all.summ$min.res.train, pch=3,cex=1)
points(lasso.res.all.summ$midpoint.res.test, pch=16, cex=1,col=4)
points(lasso.res.all.summ$min.res.test, pch=3, cex=1,col=4)


plot(df[12:xlen,i], main=temptitle,ylab=graphtitle, xlab="Date", type="o",lty=3,pch=4,col=4, yaxt="n", xaxt="n", ylim=c(ymin-.1,ymax+.1))
axis(2, at=c(-10:10)/20,lab=F)
text(x=-6.5,y=yrange/10, .32,labels=paste(formatC(yrange*10, 0, format="d"), "%", sep=""), xpd=T)
axis(1,at=c(1:as.integer((xlen-12)/3)*3),lab=F)
text(x=c(1:as.integer((xlen-12)/6)*6), y=ymin-.1, adj=c(1.5,.5),srt=90,labels=df[c(1:as.integer((xlen-12)/6)*6)+11,1], xpd=T)



library(lasso2) 

bounds<-c(1:50)/50
trainvec<-sector.calendardata$YearMo<=YearMo.vec[j]
for (j in 1:nbounds)
{
lmtest<-l1ce(y[trainvec]~.,data=sector.xvec.full.std[trainvec,1:50],bound=bounds[j], na.action=na.omit)
}

xval.mod<-function(y,xdata,index,testbounds){
xval.stats<-NULL    
xval.stats.summ<-NULL
bounds <- testbounds
nbounds <- length(bounds)
trainlength<-length(y)-index
  for (j in 1:nbounds)
  {
  lmtest<-l1ce(y[1:trainlength]~.,data=xdata[1:trainlength,],bound=bounds[j], na.action=na.omit)
  pred.lmtest<-as.matrix(cbind(rep(1,length(y)[1]),xdata))%*%coef(lmtest)
  temp.rmse<-sqrt(mean((y[(trainlength+1):length(y)]-pred.lmtest[(trainlength+1):length(y),])^2))
  xval.stats<-rbind(xval.stats,c(j,lmtest$bound, lmtest$relative.bound, lmtest$Lagrangian, temp.rmse) )
  }
colnames(xval.stats)<-c("j","bound", "relbound", "lagr", "rmse") 
xval.stats
}




weekday.factor<-as.factor(totretail.analdata$Weekday)
month.factor<-as.factor(totretail.analdata$Month)
Week<-as.numeric(format(totretail.analdata$tdate.format,"%U"))+1
Week[totretail.analdata$Year==2006]<-Week[totretail.analdata$Year==2006]-1
week.factor<-as.factor(Week)
year.factor<-as.factor(totretail.analdata$Year)
xmaswk.factor<-as.factor(totretail.analdata$XmasWk)
XmasSeasDaysNov=totretail.analdata$XmasSeasonDays*c(ifelse(totretail.analdata$Month==1,1,0))
XmasSeasDaysNov2=totretail.analdata$XmasSeasonNovDays*ifelse(totretail.analdata$Month==1,1,0)
XmasSeasDaysDec=totretail.analdata$XmasSeasonDays*ifelse(totretail.analdata$Month==1,1,0)
totretail.analdata<-cbind(totretail.analdata,DailyPropWgt,LogDailyPropWgt,weekday.factor,month.factor,Week,week.factor,xmaswk.factor,year.factor,XmasSeasDaysNov,XmasSeasDaysNov2,XmasSeasDaysDec)
totretail.dailydata.clean<-totretail.analdata[totretail.analdata$YearMo>=minYearMo,]
totretail.dailydata.clean<-totretail.dailydata.clean[!is.na(totretail.dailydata.clean$tdate.format),]
minYear<-min(totretail.dailydata.clean$Year)
Year.calc<-totretail.dailydata.clean$Year-minYear
totretail.dailydata.clean<-cbind(totretail.dailydata.clean,Year.calc)




j<-2

sectorname<-sectornamevec[j]
sectorlogpropwgt<-paste(sectorname,"LogDailyPropWgt",sep="")
sectorpropwgt<-paste(sectorname,"DailyPropWgt",sep="")
y<-sector.calendardata[sector.calendardata$YearMo>=minYearMo&sector.calendardata$YearMo<=maxYearMo,sectorlogpropwgt]
scale.y<-scale(y)
testx<-sector.calendardata.xvec.std[1:length(scale.y),1:25]
testy<-scale.y

testlm1<-lm(testy~. - 1,data=testx)
testfit1<-testlm1$fit
testcoef1<-testlm1$coef

testfit2<-as.matrix(testx[,1:2]) %*% testcoef1

testlasso.m1<-lars(as.matrix(testx[,1:2]),testy)

testlasso.lm1.pred<-predict(testlasso.m1, newx=testx[,1:2],s=1, mode=c("fraction"))$fit

testlasso.lm1.coef<-coef(testlasso.m1, s=1, mode=c("fraction"))
testlasso.lm1.pred2<-as.matrix(testx[,1:2]) %*% testlasso.lm1.coef


####################################################################OLD CODE

##for (i in 0:100){
    #print(paste("Begin residual calc",Sys.time(), sep=":"))
    ##lasso.m1.abs.res.train<-abs(y[trainvec]-predict.lars(lasso.m1, newx=sector.calendardata.xvec.std[trainvec,],type=c("fit"),s=i/100, mode=c("fraction"))$fit)
    ##lasso.m1.abs.res.exp.train<-abs(exp(y[trainvec])-exp(predict.lars(lasso.m1, newx=sector.calendardata.xvec.std[trainvec,],type=c("fit"),s=i/100, mode=c("fraction"))$fit))
    ##lasso.m1.abs.res.test<-abs(y[!trainvec]-predict.lars(lasso.m1, newx=sector.calendardata.xvec.std[!trainvec,],type=c("fit"),s=i/100, mode=c("fraction"))$fit)
    ##lasso.m1.abs.res.exp.test<-abs(exp(y[!trainvec])-exp(predict.lars(lasso.m1, newx=sector.calendardata.xvec.std[!trainvec,],type=c("fit"),s=i/100, mode=c("fraction"))$fit))
    ##lasso.res.vec<-rbind(lasso.res.vec,c(i,median(lasso.m1.abs.res.exp.train,na.rm=TRUE),median(lasso.m1.abs.res.exp.test,na.rm=TRUE)))
    #print(paste("End residual calc",Sys.time(), sep=":"))
    ##}
    ##print (paste("End Residual calc loop",sectorname,Sys.time(), sep="::"))
    #print(paste("End lasso run",YearMo.vec[j],Sys.time(), sep=":"))
    #lasso.res.all.matrix<-cbind(lasso.res.all.matrix,lasso.res.vec)
    ##plot(lasso.res.vec[,2], type="o", lty=3,main=sectorname)
    ##points(lasso.res.vec[,3], type="o", pch=2)

lasso.res.vec<-NULL
for (i in 0:10){
    #print(paste("Begin residual calc",Sys.time(), sep=":"))
    lasso.m1.abs.res.train<-abs(y[trainvec]-predict.lars(lasso.m1, newx=sector.calendardata.xvec.std[trainvec,],type=c("fit"),s=i/10, mode=c("fraction"))$fit)
    lasso.m1.abs.res.exp.train<-abs(exp(y[trainvec])-exp(predict.lars(lasso.m1, newx=sector.calendardata.xvec.std[trainvec,],type=c("fit"),s=i/10, mode=c("fraction"))$fit))
    lasso.m1.abs.res.test<-abs(y[!trainvec]-predict.lars(lasso.m1, newx=sector.calendardata.xvec.std[!trainvec,],type=c("fit"),s=i/10, mode=c("fraction"))$fit)
    lasso.m1.abs.res.exp.test<-abs(exp(y[!trainvec])-exp(predict.lars(lasso.m1, newx=sector.calendardata.xvec.std[!trainvec,],type=c("fit"),s=i/10, mode=c("fraction"))$fit))
    lasso.res.vec<-rbind(lasso.res.vec,c(i,median(lasso.m1.abs.res.exp.train,na.rm=TRUE),median(lasso.m1.abs.res.exp.test,na.rm=TRUE)))
    #print(paste("End residual calc",Sys.time(), sep=":"))
    }
    #print (paste("End Residual calc loop",sectorname,Sys.time(), sep="::"))
    #print(paste("End lasso run",YearMo.vec[j],Sys.time(), sep=":"))
    #lasso.res.all.matrix<-cbind(lasso.res.all.matrix,lasso.res.vec)
    plot(lasso.res.vec[,2], type="o", lty=3,main=sectorname)
    points(lasso.res.vec[,3], type="o", pch=2)


    lasso.m1<-lars(as.matrix(sector.calendardata.xvec.std[trainvec,]),y.std[trainvec],intercept=FALSE)
    lasso.lm1.pred<-predict(lasso.m1, newx=sector.calendardata.xvec.std, ,s=20/100, mode=c("fraction"))$fit

for (i in 1:100){
  print (i)
  lasso.lm1.coef<-predict(lasso.m1, newx=sector.calendardata.xvec.std,s=i, mode=c("step"), type=c("coefficients"))$coefficients
  lasso.lm1.fit<-predict(lasso.m1, newx=sector.calendardata.xvec.std,s=i, mode=c("step"), type=c("fit"))$fit
  #print (lasso.lm1.coef[c(lasso.lm1.coef!=0)])
  print (median(abs(y.std-lasso.lm1.fit)))
}
  
}
