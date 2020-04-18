#####DECLARE LIBRARIES########

library(reshape2)
library(plyr)
library (rgdal)
library (ggplot2)
library(data.table)
library(quantmod)
library(forecast)
library(lubridate)
source("C:/Users/e022147/Desktop/Temp/New Platform Dashboard(Temp)/spModels.R")
source("C:/Users/e022147/Desktop/Temp/New Platform Dashboard(Temp)/helper.R")
#####DECLARE WORKING DIRECTORY#######

workdir<-"H:/Files and Documents/Work/Total Retail Sales/Forecasts/"
setwd(workdir)

#####SETUP GRAPHING THEMES#######
theme1<-theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank())+
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank())+theme_bw()
theme2<-theme1+theme(axis.title=element_blank())


####HELPER FUNCTIONS####

diff_df<-function(temp_df, degree){
  #temp_df<-reg_df
  new_df<-NULL
  for (i in 1:dim(temp_df)[2]){
    diff_temp<-c(diff(temp_df[,i]), rep(NA,degree))
    new_df<-cbind(new_df, diff_temp)
  }
  colnames(new_df)<-colnames(temp_df)
return(new_df)
}

yy_diff_df<-function(temp_df, degree){
  #temp_df<-reg_df
  new_df<-NULL
  for (i in 1:dim(temp_df)[2]){
    diff_temp<-temp_df[,i]-Lag(temp_df[,i], degree)
    new_df<-cbind(new_df, diff_temp)
  }
  colnames(new_df)<-paste(colnames(temp_df), "yydiff", sep="_")
  return(new_df)
}

ratio_calc<-function(y_temp, ratio_name="Ratio"){
  y_temp$Ratio<-y_temp[,3]/y_temp[,2]
  y_temp<-y_temp[,c("Date", "Ratio")]
  colnames(y_temp)<-c("Date", ratio_name)
  return (y_temp)
}


#####PULL IN TARGET VARIABLES#######

DOCdata<-read.csv("H:/Files and Documents/Work/Data/Upload/DOCProductionData.csv")
DOCdata$Date<-as.Date(DOCdata$Date, origin="1899-12-30")

macro_data<-read.csv(paste(workdir, "Consensus_Forecasts_US.csv", sep=""))
colnames(macro_data)[1]<-"Date"
macro_data$Date<-as.Date(macro_data$Date, format="%m/%d/%Y")
macro_data$Date<-as.Date(ISOdate(year(macro_data$Date), 
                                 month(macro_data$Date), 
                                 1))

#newMacroData<-read.csv(paste(workdir, "MacroData_20170411.csv", sep=""))
#newMacroData$Date<-as.Date(newMacroData$Date, format="%m/%d/%Y")
newMacroData<-macro_data


SP_data<-read.csv("H:/Files and Documents/Work/Data/Upload/SPMonthlyHistory.csv", colClasses="numeric", na.strings=c(".", "#VALUE!"))
SP_data$Date<-as.Date(SP_data$Date, origin="1899-12-30")


SP_daily_data<-read.csv("H:/Files and Documents/Work/Data/Upload/SPDailyHistory.csv", colClasses="numeric", na.strings=c(".", "#VALUE!"))
SP_daily_data$Date<-as.Date(SP_daily_data$Date, origin="1899-12-30")
SP_daily_data$yyyymm<-as.Date(ISOdate(year(SP_daily_data$Date), month(SP_daily_data$Date), 1))

calendarCofactorsDT<-GenerateMonthlyCofactorsDT(startDate=min(newMacroData$Date),
                            endDate=max(newMacroData$Date))

calendarCofactorsDF<-dcast(data.table(calendarCofactorsDT), Date~Series)
#####MODELLING HELPER FUNCTIONS#######
ht(calendarCofactorsDT)

newMacroData<-merge(newMacroData, calendarCofactorsDF, by="Date", all.x=T)

calculate_target_df<-function(sector_name, y_table){
  #i=1
  #y_table=DOCdata
  #sector_name=y_names_vec[i]
  y_temp<-y_table[,c("Date", sector_name)]
  colnames(y_temp)<-c("Date", "Sector")
  y_temp_start_month<-min(y_temp$Date)
  
  
  y_temp_ts<-ts(y_temp$Sector, frequency=12, start=c(year(y_temp_start_month),month(y_temp_start_month)))
  y_temp_ts_decomp<-decompose(y_temp_ts, type="multiplicative")
  y_temp_df<-data.frame(y_temp$Date, y_temp_ts_decomp$x,log(y_temp_ts_decomp$x)
                        ,y_temp_ts_decomp$seasonal,
                        y_temp_ts_decomp$trend,y_temp_ts_decomp$random, log(y_temp_ts_decomp$trend))
  colnames(y_temp_df)<-c("Date","y", "log_y","seasonal_decomp", "trend_decomp", "random_decomp", "log_trend_decomp")
  
  y_temp_df_yy<-data.frame(y_temp$Date)
  temp_colnames<-c("y", "log_y", "trend_decomp", "log_trend_decomp")
  for (colname in temp_colnames){
    y_temp_df_yy<-cbind(y_temp_df_yy, c(rep(NA,12), 
              Lag(y_temp_df[,colname], k=12)/y_temp_df[,colname]))
  }
  
  colnames(y_temp_df_yy)<-c("Date",paste(temp_colnames, "_yy", sep=""))
  y_temp_df<-merge(y_temp_df, y_temp_df_yy)  
  
  return(y_temp_df)}


graph_forecast<-function(graph_df, title="Forecast"){
  #date_vec<-reg_df$Date;temp_model<-model1;temp_model_forecast<-forecast(model3c, 
  #                                                  xreg=reg_df[,c("GDP", "PCE", "Urate", "CPI", "T10", "WTI")])
  # forecast_df<-data.frame(date_vec,temp_model$x, temp_model_forecast$mean,temp_model_forecast$lower[,1],
  #                         temp_model_forecast$lower[,2],temp_model_forecast$upper[,1],temp_model_forecast$upper[,2])
  # colnames(forecast_df)<-c("Date","y","forecast", "lower_80","lower_95","upper_80","upper_95")
  graph_df_melt<-melt(graph_df[,c("Date","y","forecast")], id="Date")
  g_temp<-ggplot()+
    geom_line(data=graph_df_melt, aes(x=Date, y=value, color=variable), size=1.5)+
    scale_colour_manual(values = c("black","blue"))+
    geom_ribbon(data=graph_df,aes(x=Date,ymin=lower_80,ymax=upper_80),fill="grey",alpha=0.5)+
    xlab("Date")+
    ylab("")+
    theme1+
    ggtitle(title)
    #+
    #ggtitle(paste("Trend ",sector_name, sep=""))
  print(g_temp)
  
}

graph_correl<-function(reg_df){
  x_names<-colnames(reg_df)[!(colnames(reg_df) %in% c("Date", "y"))]
  for (x in x_names){
  data_temp<-reg_df[,c("Date", "y", x)]  
  data_temp[,"y"]<-scale(data_temp[,"y"])
  data_temp[,3]<-scale(data_temp[,3])  
  data_temp_melt<-melt(data_temp, id="Date")
  g_temp<-ggplot(data=data_temp_melt, aes(x=Date, y=value, color=variable))+geom_line()+theme1+
    xlab("Date")+
    ylab("")+
    ggtitle(x)
  print(g_temp)
  acf(reg_df[,x], na.action=na.pass, 
      main=paste("ACF YY Trend:", x, sep=""))
  }
  
}

graph_y<-function(y_temp_df){
  #sector_name=y_names_vec[i]
  g_temp<-ggplot(data=y_temp_df, aes(x=Date, y=y))+geom_line()+theme1+
    xlab("Date")+
    ylab("")+
    ggtitle("Y")
  print(g_temp)
}

display_results<-function(model){
  summary(model)
  acf(model$residuals, na.action=na.pass)
  pacf(model$residuals, na.action=na.pass)
  print (model$coef)
  #print (sqrt(diag(model$var.coef)))
  print (model$coef/(sqrt(diag(model$var.coef))))
  sd(model$residuals, na.rm=T)
}

forecast_series<-function(target_series_name,target_df,
                          x_df, regress_vec,y_name="log_trend_decomp_yy"
                          ,temp_order=c(0,0,0)){
  #target_series_name<-"DOC_RFSxMVG"
  #target_df<-DOCdata
  #temp_order<-c(0,0,3)
  #y_name<-"log_trend_decomp_yy"
  #regress_vec=regress_vec
  x_df<-x_df[,c("Date", regress_vec)]
  x_names<-colnames(x_df)[2:dim(x_df)[2]]
  y_temp_df<-calculate_target_df(target_series_name, target_df)
  target_y<-y_temp_df[,c("Date", y_name)]
  colnames(target_y)<-c("Date", "y")
  temp_reg_df<-merge(x_df,target_y, all.x=T)
  temp_reg_df<-temp_reg_df[complete.cases(temp_reg_df),]
  temp_model<-Arima(temp_reg_df$y, xreg=temp_reg_df[,x_names], order=temp_order)
  display_results(temp_model)
  temp_model_forecast<-forecast(temp_model, xreg=x_df[,x_names])
  temp_title<-paste(target_series_name, paste(temp_order,collapse=" "), sep=":")
  graph_df<-data.frame(Date=temp_reg_df$Date,y=temp_model$x)
  graph_df<-merge(graph_df, data.frame(Date=x_df$Date, 
                    temp_model_forecast$mean,temp_model_forecast$lower[,1],
                    temp_model_forecast$lower[,2],temp_model_forecast$upper[,1],
                    temp_model_forecast$upper[,2]), by="Date", all.y=T) 
  colnames(graph_df)<-c("Date","y","forecast", "lower_80","lower_95","upper_80","upper_95")
  graph_forecast(graph_df,temp_title)
  temp_forecast<-graph_df[,c("Date", "y", "forecast")]
  colnames(temp_forecast)<-c("Date", paste(target_series_name, c("", ":forecast"), sep=""))
  forecast_df<-merge(forecast_df,temp_forecast, by="Date", all.x=T)
  return(forecast_df)
}

test_model_series<-function(target_series_name,target_df, x_df, regress_vec,y_name="log_trend_decomp_yy",temp_order=c(0,0,0)){
  x_df<-x_df[,c("Date", regress_vec)]
  x_names<-colnames(x_df)[2:dim(x_df)[2]]
  y_temp_df<-calculate_target_df(target_series_name, target_df)
  target_y<-y_temp_df[,c("Date", y_name)]
  colnames(target_y)<-c("Date", "y")
  temp_reg_df<-merge(x_df,target_y, all.x=T)
  temp_model<-Arima(temp_reg_df$y, xreg=temp_reg_df[,x_names], order=temp_order)
  display_results(temp_model)
  temp_model_forecast<-forecast(temp_model, xreg=temp_reg_df[,x_names])
  temp_title<-paste(target_series_name, paste(temp_order,collapse=" "), sep=":")
  graph_forecast(temp_reg_df$Date,temp_model,temp_model_forecast,temp_title)
  temp_forecast<-data.frame(temp_reg_df$Date, temp_model_forecast$x, temp_model_forecast$mean)
  colnames(temp_forecast)<-c("Date", paste(target_series_name, c("", ":forecast"), sep=""))
  #forecast_df<-merge(forecast_df,temp_forecast)
  #return(forecast_df)
}


forecast_ratio<-function(target_series_name,target_df, y_numerator, y_denominator, x_df, 
                         regress_vec,y_name="trend_decomp_yy",temp_order=c(0,0,0)){
  #print(head(target_df))
  #print(y_denominator)
  #print(y_numerator)
  y_temp_df<-ratio_calc(target_df[,c("Date", y_denominator, y_numerator)], target_series_name)
  forecast_df<-forecast_series(target_series_name, y_temp_df, x_df, regress_vec,
                  y_name,temp_order)
  return(forecast_df)
}

#### MODELS###


forecast_df<-data.frame(macro_data$Date)
colnames(forecast_df)<-c("Date")

forecast_df<-data.frame(newMacroData$Date)
colnames(forecast_df)<-c("Date")

x_df<-cbind(newMacroData,yy_diff_df(newMacroData[,c("Urate", 
                                                    "T10", "FF", "WTI", "CPI",
                                                    "CoreCPI", "GDP","PCE")],12))
regress_vec<-c("GDP_yydiff", "PCE_yydiff", "Urate_yydiff", 
               "CPI_yydiff", "CoreCPI_yydiff", "T10_yydiff", "WTI_yydiff",
               "Easter", "LagEaster",
               "Leap", "LagLeap", "MonCount", "TueCount",
               "WedCount","ThuCount","FriCount","SatCount",
               "SunCount")



forecast_df<-forecast_series("DOC_RFSxMVG", DOCdata, 
                             x_df,y_name = "y_yy",
                             c(regress_vec),
                             temp_order=c(0,0,5))

# test<-forecast_series("DOC_RFSxMVG", DOCdata, 
#                       x_df,y_name = "y_yy",
#                       c(regress_vec, "LagLeap"),
#                       temp_order=c(0,0,4))

forecast_df<-forecast_series("DOC_RFSxMV", DOCdata, 
                             x_df, y_name = "y_yy",
                             regress_vec, temp_order=c(0,0,3))

#temp_y_df<-macro_data
#temp_y_df$PCE<-temp_y_df$PCE/100+1
newMacroData$PCE[1:2]<-c(2.8,2.8)
forecast_df<-forecast_series("PCE", newMacroData, x_df, 
              regress_vec,y_name = "y",
                  temp_order=c(0,0,3))
forecast_df<-forecast_ratio("Apparel", DOCdata, "DOC_448","DOC_RFSxMVG",x_df, 
               regress_vec,
               temp_order=c(0,0,2))
forecast_df<-forecast_ratio("Groceries", DOCdata, "DOC_445","DOC_RFSxMVG",x_df, 
               regress_vec,
               temp_order=c(0,0,2))
forecast_df<-forecast_ratio("Electronics", DOCdata, "DOC_443","DOC_RFSxMVG",x_df, 
               regress_vec,
               temp_order=c(0,0,1))
forecast_df<-forecast_ratio("WomensApparel", DOCdata, "DOC_44812","DOC_RFSxMVG",x_df, 
               regress_vec,
               temp_order=c(0,0,2))
forecast_df<-forecast_ratio("DepartmentStoresxDisc", DOCdata, "DOC_452111","DOC_RFSxMVG",x_df, 
               regress_vec,
               temp_order=c(0,0,2))
forecast_df<-forecast_ratio("Jewelry", DOCdata, "DOC_44831","DOC_RFSxMVG",x_df, 
               regress_vec,
               temp_order=c(0,0,1))
forecast_df<-forecast_ratio("Furniture", DOCdata, "DOC_442","DOC_RFSxMVG",x_df, 
               regress_vec,
               temp_order=c(0,0,1))
forecast_df<-forecast_ratio("Gas", DOCdata, "DOC_447","DOC_RFSxMVG",x_df, 
               regress_vec,
               temp_order=c(0,0,1))
forecast_df<-forecast_ratio("Restaurants", DOCdata, "DOC_722","DOC_RFSxMVG",x_df, 
               regress_vec,
               temp_order=c(0,0,1))
forecast_df<-forecast_ratio("Luxury", SP_data, "LUXURYXJ","USRETAIL_NSA",x_df, 
               regress_vec,
               temp_order=c(0,0,1))
forecast_df<-forecast_ratio("Airlines", SP_data, "AIRLINES","USRETAIL_NSA",x_df, 
               regress_vec,
               temp_order=c(0,0,1))
forecast_df<-forecast_ratio("Hotels", SP_data, "HOTELS","USRETAIL_NSA",x_df, 
               regress_vec,
               temp_order=c(0,0,1))
forecast_df<-forecast_ratio("AutoParts", SP_data, "AUTOPANDT","USRETAIL_NSA",x_df, 
               regress_vec,
               temp_order=c(0,0,2))

forecast_df<-forecast_ratio("Hardware", SP_data, "HARDWARE","USRETAIL_NSA",x_df, 
                            regress_vec,
                            temp_order=c(0,0,1))
y_temp<-SP_daily_data[,c("yyyymm", "USRETAIL", "EUSRETAIL")]
y_temp<-ddply(SP_daily_data, c("yyyymm"), summarize, USRETAIL=sum(USRETAIL), EUSRETAIL=sum(EUSRETAIL))
y_temp<-na.omit(y_temp[y_temp$yyyymm<=as.Date(ISOdate(2015,6,1)),])
colnames(y_temp)[1]<-"Date"
forecast_df<-forecast_ratio("ECommerce", y_temp, 
                "EUSRETAIL","USRETAIL",x_df, 
               regress_vec,
               temp_order=c(0,0,1))

write.csv(forecast_df, "forecast_df_20170719.csv")
#write.csv(forecast_df, "forecast_df.csv")

test_model_series("PCE", temp_y_df, x_df, y_name="trend_decomp", regress_vec=c("GDP", "Urate_yydiff", "CPI", "CoreCPI", "T10_yydiff", "FF_yydiff"),
                                    temp_order=c(0,0,2))

######SCRATCH MODELS#####################

#no co-factors
sd(target_y$y, na.rm=T)
acf(target_y$y, na.action=na.pass)
model1<-auto.arima(target_y$y)
display_results(model1)
temp_model_forecast<-forecast(model1,h=length(reg_df$Date-length(target_y$y)+15))
graph_forecast(reg_df$Date,model1,temp_model_forecast[1:length(reg_df$Date),],"Model 1")  

test2a<-Arima(target_y$y, order=c(0,0,0), include.constant=T)
test2b<-Arima(target_y$y, order=c(0,1,0), include.constant=T)
test2c<-Arima(target_y$y, order=c(0,2,0), include.constant=T)
#test2c<-Arima(target_y$y, order=c(0,3,0))
test2d<-Arima(target_y$y, order=c(3,2,12), 
             fixed=c(0,0,NA,0,0,0,0,0,0,0,0,0,0,0,NA))

display_results(test1)
display_results(test2d)

reg_df<-merge(macro_data,target_y, all.x=T)



# test<-Arima(target_y$y, order=c(1,1,3))
regress_vec<-c("GDP", "PCE", "Urate_yydiff", "CPI", "CoreCPI", "T10_yydiff", "FF_yydiff")
model1<-auto.arima(reg_df$y, xreg=reg_df_temp[,regress_vec], d=0)
display_results(model1)
temp_model_forecast<-forecast(model1, xreg=reg_df_temp[,regress_vec])
graph_forecast(reg_df$Date,model1,temp_model_forecast,"Model 1-Auto.Arima")  

# model2<-Arima(reg_df$y, order=c(4,1,12), 
#               fixed=c(NA,NA,NA,NA,0,0,NA,NA,0,0,0,0,0,0,0,
#                       NA, NA, NA, NA, NA, NA, NA, NA, NA),
#               xreg=reg_df[,2:9])
#display_results(model2)
model3<-Arima(reg_df$y, xreg=reg_df_temp[,regress_vec], order=c(0,0,0))
display_results(model3)
temp_model_forecast<-forecast(model3, xreg=reg_df_temp[,regress_vec])
graph_forecast(reg_df$Date,model3,temp_model_forecast,"Model 3: regressors" )

reg_df_diff_temp<-diff_df(reg_df_temp[,regress_vec],1)

# model3a<-Arima(reg_df$y, xreg=reg_df_diff_temp, order=c(0,1,0))
# display_results(model3a)
# temp_model_forecast<-forecast(model3a, xreg=reg_df_diff_temp)
# graph_forecast(reg_df$Date,model3a,temp_model_forecast,"Model 3a: d=1")

model3a<-Arima(reg_df$y, xreg=reg_df_temp[,regress_vec], order=c(0,1,0))
display_results(model3a)
temp_model_forecast<-forecast(model3a, xreg=reg_df_temp[,regress_vec])
graph_forecast(reg_df$Date,model3a,temp_model_forecast,"Model 3: regressors, d=1")

model3b<-Arima(reg_df$y, xreg=reg_df_temp[,regress_vec], order=c(1,0,0))
display_results(model3b)
temp_model_forecast<-forecast(model3b, xreg=reg_df_temp[,regress_vec])
graph_forecast(reg_df$Date,model3b,temp_model_forecast,"Model 3b: regressors, ar=1")

model3c<-Arima(reg_df$y, xreg=reg_df_temp[,regress_vec], order=c(0,0,1))
display_results(model3c)
temp_model_forecast<-forecast(model3c, xreg=reg_df_temp[,regress_vec])
graph_forecast(reg_df$Date,model3c,temp_model_forecast,"Model 3c: regressors, ma=1")

model3d<-Arima(reg_df$y, xreg=reg_df_temp[,regress_vec], order=c(0,1,1))
display_results(model3d)
temp_model_forecast<-forecast(model3d, xreg=reg_df_temp[,regress_vec])
graph_forecast(reg_df$Date,model3d,temp_model_forecast,"Model 3d: regressors, d=1,ma=1")

model3e<-Arima(reg_df$y, xreg=reg_df_temp[,regress_vec], order=c(1,1,0))
display_results(model3e)
temp_model_forecast<-forecast(model3e, xreg=reg_df_temp[,regress_vec])
graph_forecast(reg_df$Date,model3e,temp_model_forecast,"Model 3e: regressors, d=1,ma=1")

model3f<-Arima(reg_df$y, xreg=reg_df_temp[,regress_vec], order=c(1,0,1))
display_results(model3f)
temp_model_forecast<-forecast(model3f, xreg=reg_df_temp[,regress_vec])
graph_forecast(reg_df$Date,model3f,temp_model_forecast,"Model 3f: regressors, d=1,ma=1")


  

####SCRATCH##################

plot(forecast(model3, xreg=reg_df[275:300,2:9]))

forecast(model3, xreg=reg_df[275:300,2:9])
# test<-Arima(target_y$y)
# test<-Arima(target_y$y, order=c(0,2,0))
# test<-Arima(target_y$y, order=c(1,1,0))
#est2<-Arima(target_y$y, order=c(4,1,12), fixed=c(NA,NA,NA,NA,0,0,NA,NA,0,0,0,0,0,0,0,NA))
# test<-Arima(target_y$y, order=c(1,1,3))


#fit<-auto.arima(reg_df$y, xreg=reg_df[,2:9], d=0)


tsdisplay(residuals(test))


sector_name=y_names_vec[i]
y_temp<-y_table[,c("Date", sector_name)]
colnames(y_temp)<-c("Date", "Sector")
fit <- Arima(y_temp_ts, xreg=forecastdata[(1:dim(y_temp)[1]),2:4],
             order=c(0,0,0), seasonal=c(0,1,0))
tsdisplay(arima.errors(fit))
Box.test(residuals(fit),fitdf=5,lag=10,type="Ljung")
fcast <- forecast(fit,xreg=forecastdata[,2:4])
plot(fcast)

fit <- Arima(h02, order=c(3,0,1), seasonal=c(0,1,2), lambda=0)


arima_model<-function(i, y_table){
  
  
}

###SELECT TARGETS

#y_names_vec<-c("DOC_RFSxMV","DOC_RFSxMVG", "PCE", "Ratio")
#y_diff<-list(c(1,12))

#   y_temp_diff<-data.frame(y_temp[c(13:(dim(y_temp)[1])),1],diff(y_temp[, 2], lag=12))
#   y_temp_log_diff<-data.frame(y_temp[c(13:(dim(y_temp)[1])),1],diff(log(y_temp[, 2]), lag=12))
#   y_temp_trend_diff<-data.frame(y_temp[c(13:(dim(y_temp)[1])),1],diff(y_temp_ts_decomp$trend, lag=12))
#   
#   colnames(y_temp_diff)<-colnames(y_temp) 
#   colnames(y_temp_log_diff)<-colnames(y_temp)
#   colnames(y_temp_trend_diff)<-colnames(y_temp)
#   


# graph_target<-function(y_temp_df, i){
#   sector_name=y_names_vec[i]
#   g_temp<-ggplot(data=y_temp_df, aes(x=Date, y=y))+geom_line()+theme1+
#     xlab("Date")+
#     ylab("")+
#     ggtitle(sector_name)
#   print(g_temp)
#   
#   g_temp<-ggplot(data=y_temp_df, aes(x=Date, y=log_y))+geom_line()+theme1+
#     xlab("Date")+
#     ylab("")+
#     ggtitle(paste("Log: ",sector_name, sep=""))
#     print(g_temp)
# 
#   g_temp<-ggplot(data=y_temp_df, aes(x=Date, y=trend_decomp))+geom_line()+theme1+
#     xlab("Date")+
#     ylab("")+
#     ggtitle(paste("Trend ",sector_name, sep=""))
#   print(g_temp)
#   
#   g_temp<-ggplot(data=y_temp_df, aes(x=Date, y=log_trend_decomp))+geom_line()+theme1+
#     xlab("Date")+
#     ylab("")+
#     ggtitle(paste("Log Trend ",sector_name, sep=""))
#   print(g_temp)
#   
#   
#   g_temp<-ggplot(data=y_temp_df, aes(x=Date, y=y_yy))+geom_line()+theme1+
#     xlab("Date")+
#     ylab("")+
#     ggtitle(paste("YY:", sector_name, sep=""))
#   print(g_temp)
#   
#   g_temp<-ggplot(data=y_temp_df, aes(x=Date, y=log_y_yy))+geom_line()+theme1+
#     xlab("Date")+
#     ylab("")+
#     ggtitle(paste("YY Log: ",sector_name, sep=""))
#   print(g_temp)
#   
#   g_temp<-ggplot(data=y_temp_df, aes(x=Date, y=trend_decomp_yy))+geom_line()+theme1+
#     xlab("Date")+
#     ylab("")+
#     ggtitle(paste("YY Trend ",sector_name, sep=""))
#   print(g_temp)
#   
#   g_temp<-ggplot(data=y_temp_df, aes(x=Date, y=log_trend_decomp_yy))+geom_line()+theme1+
#     xlab("Date")+
#     ylab("")+
#     ggtitle(paste("YY Log Trend ",sector_name, sep=""))
#   print(g_temp)
# 
#   #par(mfrow=c(2,1))
#   
#   acf(y_temp_df$y_yy, na.action=na.pass,main=paste("ACF:", sector_name, sep=""))
#   pacf(y_temp_df$y_yy, na.action=na.pass,main=paste("PACF:", sector_name, sep=""))
#   acf(y_temp_df$log_y_yy, na.action=na.pass,main=paste("ACF Log:", sector_name, sep=""))
#   pacf(y_temp_df$log_y_yy, na.action=na.pass,main=paste("PACF Log:", sector_name, sep=""))
#   
#   acf(y_temp_df$trend_decomp_yy, na.action=na.pass, main=paste("ACF Trend:", sector_name, sep=""))
#   pacf(y_temp_df$trend_decomp_yy, na.action=na.pass,main=paste("PACF Trend:", sector_name, sep=""))
#   acf(y_temp_df$log_trend_decomp_yy, na.action=na.pass, main=paste("ACF Log Trend:", sector_name, sep=""))
#   pacf(y_temp_df$log_trend_decomp_yy, na.action=na.pass,main=paste("PACF Trend:", sector_name, sep=""))
#   
#   acf(y_temp_df$y_yy, na.action=na.pass,main=paste("ACF YY:", sector_name, sep=""))
#   pacf(y_temp_df$y_yy, na.action=na.pass,main=paste("PACF YY:", sector_name, sep=""))
#   acf(y_temp_df$log_y_yy, na.action=na.pass,main=paste("ACF Log YY:", sector_name, sep=""))
#   pacf(y_temp_df$log_y_yy, na.action=na.pass,main=paste("PACF Log YY:", sector_name, sep=""))
#   
#   acf(y_temp_df$trend_decomp_yy, na.action=na.pass, main=paste("ACF YY Trend:", sector_name, sep=""))
#   pacf(y_temp_df$trend_decomp_yy, na.action=na.pass,main=paste("PACF YY Trend:", sector_name, sep=""))
#   acf(y_temp_df$log_trend_decomp_yy, na.action=na.pass, main=paste("ACF YY Log Trend:", sector_name, sep=""))
#   pacf(y_temp_df$log_trend_decomp_yy, na.action=na.pass,main=paste("PACF YY Log Trend:", sector_name, sep=""))
#   
# }
# y_temp_df<-calculate_target_df("DOC_RFSxMVG", DOCdata)
# target_y<-y_temp_df[,c("Date", "log_trend_decomp_yy")]
# colnames(target_y)<-c("Date", "y")
# reg_df<-merge(macro_data,target_y, all.x=T)
# reg_df_temp<-cbind(reg_df, yy_diff_df(reg_df[,c("Urate", "T10", "FF", "WTI")],12))
# 
# retailxg_model<-Arima(reg_df$y, xreg=reg_df_temp[,regress_vec], order=c(0,0,3))
# display_results(retailxg_model)
# temp_model_forecast<-forecast(retailxg_model, xreg=reg_df_temp[,regress_vec])
# graph_forecast(reg_df$Date,retailxg_model,temp_model_forecast,"Retail x G Model: regressors, ma=1")
# temp_forecast<-data.frame(reg_df$Date, temp_model_forecast$x, temp_model_forecast$mean)
# colnames(temp_forecast)<-c("Date", paste(series_name, c("", ":forecast"), sep=""))
# forecast_df<-temp_forecast

# 
# model1<-auto.arima(reg_df$y, xreg=reg_df_temp[,regress_vec])
# display_results(model1)
# temp_model_forecast<-forecast(model1, xreg=reg_df_temp[,regress_vec])
# graph_forecast(reg_df$Date,model1,temp_model_forecast,"Retail x G Model:Model Auto.Arima")  


###PCE MODEL

regress_vec<-c("GDP", "PCE", "Urate_yydiff", "CPI", "CoreCPI", "T10_yydiff", "FF_yydiff")
y_temp_df<-calculate_target_df(3, reg_df)
target_y<-y_temp_df[,c("Date", "trend_decomp")]
colnames(target_y)<-c("Date", "y")
target_y$y<-target_y$y/100+1
#graph_y(target_y)
reg_df<-merge(macro_data,target_y, all.x=T)
reg_df_temp<-cbind(reg_df, yy_diff_df(reg_df[,c("Urate", "T10", "FF", "WTI")],12))
exclude_vec<-!(colnames(reg_df_temp) %in% c("PCE"))
reg_df_temp<-reg_df_temp[,exclude_vec]
regress_vec<-c("GDP", "Urate_yydiff", "CoreCPI", "T10_yydiff", "FF_yydiff")
#reg_df_diff_temp<-diff_df(reg_df_temp[,regress_vec],1)
PCE_model<-Arima(reg_df$y, xreg=reg_df_temp[,regress_vec], order=c(0,0,1))
display_results(PCE_model)
temp_model_forecast<-forecast(PCE_model, xreg=reg_df_temp[,regress_vec])
graph_forecast(reg_df$Date,PCE_model,temp_model_forecast,"PCE Model:")

series_name<-"PCE"
temp_forecast<-data.frame(reg_df$Date, temp_model_forecast$x, temp_model_forecast$mean)
colnames(temp_forecast)<-c("Date", paste(series_name, c("", ":forecast"), sep=""))
forecast_df<-merge(forecast_df, temp_forecast)

###RETAIL MODEL

regress_vec<-c("GDP", "PCE", "Urate_yydiff", "CPI", "CoreCPI", "T10_yydiff", "FF_yydiff")
y_temp_df<-calculate_target_df(1, DOCdata)
#y_temp_df<-
target_y<-y_temp_df[,c("Date", "log_trend_decomp_yy")]
colnames(target_y)<-c("Date", "y")
reg_df<-merge(macro_data,target_y, all.x=T)
reg_df_temp<-cbind(reg_df, yy_diff_df(reg_df[,c("Urate", "T10", "FF", "WTI")],12))
reg_df_diff_temp<-diff_df(reg_df_temp[,regress_vec],1)

retail_model<-Arima(reg_df$y, xreg=reg_df_temp[,regress_vec],order=c(0,0,2))
display_results(retail_model)
temp_model_forecast<-forecast(retail_model, xreg=reg_df_temp[,regress_vec])
graph_forecast(reg_df$Date,retail_model,temp_model_forecast,"Retail Model: regressors")


series_name<-"Retail"
temp_forecast<-data.frame(reg_df$Date, temp_model_forecast$x, temp_model_forecast$mean)
colnames(temp_forecast)<-c("Date", paste(series_name, c("", ":forecast"), sep=""))
forecast_df<-merge(forecast_df, temp_forecast)



###APPAREL
y_temp<-DOCdata[,c("Date", "DOC_RFSxMVG", "DOC_448")]
y_temp_df<-ratio_calc(y_temp)
#target_y<-y_temp_df[,c("Date", "trend_decomp_yy")]
target_y<-y_temp_df[,c("Date", "trend_decomp_yy")]
colnames(target_y)<-c("Date", "y")

reg_df<-merge(macro_data,target_y, all.x=T)
reg_df_temp<-cbind(reg_df, yy_diff_df(reg_df[,c("Urate", "T10", "FF", "WTI")],12))
#reg_df_diff_temp<-diff_df(reg_df_temp[,regress_vec],1)

regress_vec<-c("GDP", "PCE", "Urate_yydiff", "CPI", "T10_yydiff", "FF_yydiff")
#sector_model<-auto.arima(reg_df$y, xreg=reg_df_temp[,regress_vec])
sector_model<-Arima(reg_df$y, xreg=reg_df_temp[,regress_vec],order=c(0,0,2))
display_results(sector_model)
temp_model_forecast<-forecast(sector_model, xreg=reg_df_temp[,regress_vec])
graph_forecast(reg_df$Date,sector_model,temp_model_forecast,"Sector Model: regressors")

series_name<-"Apparel"
temp_forecast<-data.frame(reg_df$Date, temp_model_forecast$x, temp_model_forecast$mean)
colnames(temp_forecast)<-c("Date", paste(series_name, c("", ":forecast"), sep=""))
forecast_df<-merge(forecast_df, temp_forecast)


###GROCERIES

y_temp<-DOCdata[,c("Date", "DOC_RFSxMVG", "DOC_445")]
y_temp_df<-ratio_calc(y_temp)
#target_y<-y_temp_df[,c("Date", "trend_decomp_yy")]
target_y<-y_temp_df[,c("Date", "trend_decomp_yy")]
colnames(target_y)<-c("Date", "y")

reg_df<-merge(macro_data,target_y, all.x=T)
reg_df_temp<-cbind(reg_df, yy_diff_df(reg_df[,c("Urate", "T10", "FF", "WTI")],12))
#reg_df_diff_temp<-diff_df(reg_df_temp[,regress_vec],1)

regress_vec<-c("GDP", "PCE", "Urate_yydiff", "CPI", "T10_yydiff", "FF_yydiff")
#sector_model<-auto.arima(reg_df$y, xreg=reg_df_temp[,regress_vec])
sector_model<-Arima(reg_df$y, xreg=reg_df_temp[,regress_vec],order=c(0,0,2))
display_results(sector_model)
temp_model_forecast<-forecast(sector_model, xreg=reg_df_temp[,regress_vec])
graph_forecast(reg_df$Date,sector_model,temp_model_forecast,"Sector Model: regressors")

series_name<-"Groceries"
temp_forecast<-data.frame(reg_df$Date, temp_model_forecast$x, temp_model_forecast$mean)
colnames(temp_forecast)<-c("Date", paste(series_name, c("", ":forecast"), sep=""))
forecast_df<-merge(forecast_df, temp_forecast)
###ELECTRONICS

y_temp<-DOCdata[,c("Date", "DOC_RFSxMVG", "DOC_443")]
y_temp_df<-ratio_calc(y_temp)
#target_y<-y_temp_df[,c("Date", "trend_decomp_yy")]
target_y<-y_temp_df[,c("Date", "trend_decomp_yy")]
colnames(target_y)<-c("Date", "y")

reg_df<-merge(macro_data,target_y, all.x=T)
reg_df_temp<-cbind(reg_df, yy_diff_df(reg_df[,c("Urate", "T10", "FF", "WTI")],12))
#reg_df_diff_temp<-diff_df(reg_df_temp[,regress_vec],1)

regress_vec<-c("GDP", "PCE", "Urate_yydiff", "CPI", "T10_yydiff", "FF_yydiff")
#sector_model<-auto.arima(reg_df$y, xreg=reg_df_temp[,regress_vec])
sector_model<-Arima(reg_df$y, xreg=reg_df_temp[,regress_vec],order=c(0,0,1))
display_results(sector_model)
temp_model_forecast<-forecast(sector_model, xreg=reg_df_temp[,regress_vec])
graph_forecast(reg_df$Date,sector_model,temp_model_forecast,"Sector Model: regressors")

series_name<-"Electronics"
temp_forecast<-data.frame(reg_df$Date, temp_model_forecast$x, temp_model_forecast$mean)
colnames(temp_forecast)<-c("Date", paste(series_name, c("", ":forecast"), sep=""))
forecast_df<-merge(forecast_df, temp_forecast)

###WoMENS APPAREL

y_temp<-DOCdata[,c("Date", "DOC_RFSxMVG", "DOC_44812")]
y_temp_df<-ratio_calc(y_temp)
#target_y<-y_temp_df[,c("Date", "trend_decomp_yy")]
target_y<-y_temp_df[,c("Date", "trend_decomp_yy")]
colnames(target_y)<-c("Date", "y")
graph_y(target_y)

reg_df<-merge(macro_data,target_y, all.x=T)
reg_df_temp<-cbind(reg_df, yy_diff_df(reg_df[,c("Urate", "T10", "FF", "WTI")],12))
#reg_df_diff_temp<-diff_df(reg_df_temp[,regress_vec],1)

regress_vec<-c("GDP", "PCE", "Urate_yydiff", "CPI", "T10_yydiff", "FF_yydiff")
#sector_model<-auto.arima(reg_df$y, xreg=reg_df_temp[,regress_vec])
sector_model<-Arima(reg_df$y, xreg=reg_df_temp[,regress_vec],order=c(0,0,2))
display_results(sector_model)
temp_model_forecast<-forecast(sector_model, xreg=reg_df_temp[,regress_vec])
graph_forecast(reg_df$Date,sector_model,temp_model_forecast,"Sector Model: regressors")

series_name<-"WomApparel"
temp_forecast<-data.frame(reg_df$Date, temp_model_forecast$x, temp_model_forecast$mean)
colnames(temp_forecast)<-c("Date", paste(series_name, c("", ":forecast"), sep=""))
forecast_df<-merge(forecast_df, temp_forecast)

###DEPARTMENT STORESxDISC

y_temp<-DOCdata[,c("Date", "DOC_RFSxMVG", "DOC_452111")]
y_temp_df<-ratio_calc(y_temp)
#target_y<-y_temp_df[,c("Date", "trend_decomp_yy")]
target_y<-y_temp_df[,c("Date", "trend_decomp_yy")]
colnames(target_y)<-c("Date", "y")
graph_y(target_y)

reg_df<-merge(macro_data,target_y, all.x=T)
reg_df_temp<-cbind(reg_df, yy_diff_df(reg_df[,c("Urate", "T10", "FF", "WTI")],12))
#reg_df_diff_temp<-diff_df(reg_df_temp[,regress_vec],1)

regress_vec<-c("GDP", "PCE", "Urate_yydiff", "CPI", "T10_yydiff", "FF_yydiff")
#sector_model<-auto.arima(reg_df$y, xreg=reg_df_temp[,regress_vec])
sector_model<-Arima(reg_df$y, xreg=reg_df_temp[,regress_vec],order=c(0,0,2))
display_results(sector_model)
temp_model_forecast<-forecast(sector_model, xreg=reg_df_temp[,regress_vec])
graph_forecast(reg_df$Date,sector_model,temp_model_forecast,"Sector Model: regressors")

series_name<-"DeptStores"
temp_forecast<-data.frame(reg_df$Date, temp_model_forecast$x, temp_model_forecast$mean)
colnames(temp_forecast)<-c("Date", paste(series_name, c("", ":forecast"), sep=""))
forecast_df<-merge(forecast_df, temp_forecast)

###JEWELRY STORES

y_temp<-DOCdata[,c("Date", "DOC_RFSxMVG", "DOC_44831")]
y_temp_df<-ratio_calc(y_temp)
#target_y<-y_temp_df[,c("Date", "trend_decomp_yy")]
target_y<-y_temp_df[,c("Date", "trend_decomp_yy")]
colnames(target_y)<-c("Date", "y")
graph_y(target_y)

reg_df<-merge(macro_data,target_y, all.x=T)
reg_df_temp<-cbind(reg_df, yy_diff_df(reg_df[,c("Urate", "T10", "FF", "WTI")],12))
#reg_df_diff_temp<-diff_df(reg_df_temp[,regress_vec],1)

regress_vec<-c("GDP", "PCE", "Urate_yydiff", "CPI", "T10_yydiff", "FF_yydiff")
#sector_model<-auto.arima(reg_df$y, xreg=reg_df_temp[,regress_vec])
sector_model<-Arima(reg_df$y, xreg=reg_df_temp[,regress_vec],order=c(0,0,2))
display_results(sector_model)
temp_model_forecast<-forecast(sector_model, xreg=reg_df_temp[,regress_vec])
graph_forecast(reg_df$Date,sector_model,temp_model_forecast,"Sector Model: regressors")

series_name<-"Jewelry"
temp_forecast<-data.frame(reg_df$Date, temp_model_forecast$x, temp_model_forecast$mean)
colnames(temp_forecast)<-c("Date", paste(series_name, c("", ":forecast"), sep=""))
forecast_df<-merge(forecast_df, temp_forecast)

###HOMEFF STORES

y_temp<-DOCdata[,c("Date", "DOC_RFSxMVG", "DOC_442")]
y_temp_df<-ratio_calc(y_temp)
#target_y<-y_temp_df[,c("Date", "trend_decomp_yy")]
target_y<-y_temp_df[,c("Date", "trend_decomp_yy")]
colnames(target_y)<-c("Date", "y")
graph_y(target_y)

reg_df<-merge(macro_data,target_y, all.x=T)
reg_df_temp<-cbind(reg_df, yy_diff_df(reg_df[,c("Urate", "T10", "FF", "WTI")],12))
#reg_df_diff_temp<-diff_df(reg_df_temp[,regress_vec],1)

regress_vec<-c("GDP", "PCE", "Urate_yydiff", "CPI", "T10_yydiff", "FF_yydiff")
#sector_model<-auto.arima(reg_df$y, xreg=reg_df_temp[,regress_vec])
sector_model<-Arima(reg_df$y, xreg=reg_df_temp[,regress_vec],order=c(0,0,2))
display_results(sector_model)
temp_model_forecast<-forecast(sector_model, xreg=reg_df_temp[,regress_vec])
graph_forecast(reg_df$Date,sector_model,temp_model_forecast,"Sector Model: regressors")

series_name<-"Furniture"
temp_forecast<-data.frame(reg_df$Date, temp_model_forecast$x, temp_model_forecast$mean)
colnames(temp_forecast)<-c("Date", paste(series_name, c("", ":forecast"), sep=""))
forecast_df<-merge(forecast_df, temp_forecast)

###GAS STATIONS

y_temp<-DOCdata[,c("Date", "DOC_RFSxMVG", "DOC_447")]
y_temp_df<-ratio_calc(y_temp)
#target_y<-y_temp_df[,c("Date", "trend_decomp_yy")]
target_y<-y_temp_df[,c("Date", "trend_decomp_yy")]
colnames(target_y)<-c("Date", "y")
graph_y(target_y)

reg_df<-merge(macro_data,target_y, all.x=T)
reg_df_temp<-cbind(reg_df, yy_diff_df(reg_df[,c("Urate", "T10", "FF", "WTI")],12))
#reg_df_diff_temp<-diff_df(reg_df_temp[,regress_vec],1)

regress_vec<-c("GDP", "PCE", "Urate_yydiff", "CPI", "T10_yydiff", "FF_yydiff")
#sector_model<-auto.arima(reg_df$y, xreg=reg_df_temp[,regress_vec])
sector_model<-Arima(reg_df$y, xreg=reg_df_temp[,regress_vec],order=c(0,0,1))
display_results(sector_model)
temp_model_forecast<-forecast(sector_model, xreg=reg_df_temp[,regress_vec])
graph_forecast(reg_df$Date,sector_model,temp_model_forecast,"Sector Model: regressors")

series_name<-"Gas"
temp_forecast<-data.frame(reg_df$Date, temp_model_forecast$x, temp_model_forecast$mean)
colnames(temp_forecast)<-c("Date", paste(series_name, c("", ":forecast"), sep=""))
forecast_df<-merge(forecast_df, temp_forecast)

###RESTAURANTS 

y_temp<-DOCdata[,c("Date", "DOC_RFSxMVG", "DOC_722")]
y_temp_df<-ratio_calc(y_temp)
#target_y<-y_temp_df[,c("Date", "trend_decomp_yy")]
target_y<-y_temp_df[,c("Date", "trend_decomp_yy")]
colnames(target_y)<-c("Date", "y")
graph_y(target_y)

reg_df<-merge(macro_data,target_y, all.x=T)
reg_df_temp<-cbind(reg_df, yy_diff_df(reg_df[,c("Urate", "T10", "FF", "WTI")],12))
#reg_df_diff_temp<-diff_df(reg_df_temp[,regress_vec],1)

regress_vec<-c("GDP", "PCE", "Urate_yydiff", "CPI", "T10_yydiff", "FF_yydiff")
#sector_model<-auto.arima(reg_df$y, xreg=reg_df_temp[,regress_vec])
sector_model<-Arima(reg_df$y, xreg=reg_df_temp[,regress_vec],order=c(0,0,1))
display_results(sector_model)
temp_model_forecast<-forecast(sector_model, xreg=reg_df_temp[,regress_vec])
graph_forecast(reg_df$Date,sector_model,temp_model_forecast,"Sector Model: regressors")

series_name<-"Restaurants"
temp_forecast<-data.frame(reg_df$Date, temp_model_forecast$x, temp_model_forecast$mean)
colnames(temp_forecast)<-c("Date", paste(series_name, c("", ":forecast"), sep=""))
forecast_df<-merge(forecast_df, temp_forecast)

#LUXURY, AUTOPARTS, AIRLINES, HOTELS, CHECK ON DEF FOR HARDWARE, DEPARTMENT STORES

###LUXURY 
y_temp<-SP_data[,c("Date", "USRETAIL_NSA", "LUXURYXJ")]
y_temp_df<-ratio_calc(y_temp)
target_y<-y_temp_df[,c("Date", "trend_decomp_yy")]
colnames(target_y)<-c("Date", "y")
graph_y(target_y)

reg_df<-merge(macro_data,target_y, all.x=T)
reg_df_temp<-cbind(reg_df, yy_diff_df(reg_df[,c("Urate", "T10", "FF", "WTI")],12))
#reg_df_diff_temp<-diff_df(reg_df_temp[,regress_vec],1)

regress_vec<-c("GDP", "PCE", "Urate_yydiff", "CPI", "T10_yydiff", "FF_yydiff")
#sector_model<-auto.arima(reg_df$y, xreg=reg_df_temp[,regress_vec])
sector_model<-Arima(reg_df$y, xreg=reg_df_temp[,regress_vec],order=c(0,0,2))
display_results(sector_model)
temp_model_forecast<-forecast(sector_model, xreg=reg_df_temp[,regress_vec])
graph_forecast(reg_df$Date,sector_model,temp_model_forecast,"Sector Model: regressors")

series_name<-"Luxury"
temp_forecast<-data.frame(reg_df$Date, temp_model_forecast$x, temp_model_forecast$mean)
colnames(temp_forecast)<-c("Date", paste(series_name, c("", ":forecast"), sep=""))
forecast_df<-merge(forecast_df, temp_forecast)

###AIRLINES 
y_temp<-SP_data[,c("Date", "USRETAIL_NSA", "AIRLINES")]
y_temp_df<-ratio_calc(y_temp)
target_y<-y_temp_df[,c("Date", "trend_decomp_yy")]
colnames(target_y)<-c("Date", "y")
graph_y(target_y)

reg_df<-merge(macro_data,target_y, all.x=T)
reg_df_temp<-cbind(reg_df, yy_diff_df(reg_df[,c("Urate", "T10", "FF", "WTI")],12))
#reg_df_diff_temp<-diff_df(reg_df_temp[,regress_vec],1)

regress_vec<-c("GDP", "PCE", "Urate_yydiff", "CPI", "T10_yydiff", "FF_yydiff")
#sector_model<-auto.arima(reg_df$y, xreg=reg_df_temp[,regress_vec])
sector_model<-Arima(reg_df$y, xreg=reg_df_temp[,regress_vec],order=c(0,0,2))
display_results(sector_model)
temp_model_forecast<-forecast(sector_model, xreg=reg_df_temp[,regress_vec])
graph_forecast(reg_df$Date,sector_model,temp_model_forecast,"Sector Model: regressors")

series_name<-"Airlines"
temp_forecast<-data.frame(reg_df$Date, temp_model_forecast$x, temp_model_forecast$mean)
colnames(temp_forecast)<-c("Date", paste(series_name, c("", ":forecast"), sep=""))
forecast_df<-merge(forecast_df, temp_forecast)

###HOTELS 
y_temp<-SP_data[,c("Date", "USRETAIL_NSA", "HOTELS")]
y_temp_df<-ratio_calc(y_temp)
target_y<-y_temp_df[,c("Date", "trend_decomp_yy")]
colnames(target_y)<-c("Date", "y")
graph_y(target_y)

reg_df<-merge(macro_data,target_y, all.x=T)
reg_df_temp<-cbind(reg_df, yy_diff_df(reg_df[,c("Urate", "T10", "FF", "WTI")],12))
#reg_df_diff_temp<-diff_df(reg_df_temp[,regress_vec],1)

regress_vec<-c("GDP", "PCE", "Urate_yydiff", "CPI", "T10_yydiff", "FF_yydiff")
#sector_model<-auto.arima(reg_df$y, xreg=reg_df_temp[,regress_vec])
sector_model<-Arima(reg_df$y, xreg=reg_df_temp[,regress_vec],order=c(0,0,2))
display_results(sector_model)
temp_model_forecast<-forecast(sector_model, xreg=reg_df_temp[,regress_vec])
graph_forecast(reg_df$Date,sector_model,temp_model_forecast,"Sector Model: regressors")

series_name<-"Hotels"
temp_forecast<-data.frame(reg_df$Date, temp_model_forecast$x, temp_model_forecast$mean)
colnames(temp_forecast)<-c("Date", paste(series_name, c("", ":forecast"), sep=""))
forecast_df<-merge(forecast_df, temp_forecast)

###AUTOPANDT 
y_temp<-SP_data[,c("Date", "USRETAIL_NSA", "AUTOPANDT")]
y_temp_df<-ratio_calc(y_temp)
target_y<-y_temp_df[,c("Date", "trend_decomp_yy")]
colnames(target_y)<-c("Date", "y")
graph_y(target_y)

reg_df<-merge(macro_data,target_y, all.x=T)
reg_df_temp<-cbind(reg_df, yy_diff_df(reg_df[,c("Urate", "T10", "FF", "WTI")],12))
#reg_df_diff_temp<-diff_df(reg_df_temp[,regress_vec],1)

regress_vec<-c("GDP", "PCE", "Urate_yydiff", "CPI", "T10_yydiff", "FF_yydiff")
#sector_model<-auto.arima(reg_df$y, xreg=reg_df_temp[,regress_vec])
sector_model<-Arima(reg_df$y, xreg=reg_df_temp[,regress_vec],order=c(0,0,2))
display_results(sector_model)
temp_model_forecast<-forecast(sector_model, xreg=reg_df_temp[,regress_vec])
graph_forecast(reg_df$Date,sector_model,temp_model_forecast,"Sector Model: regressors")

series_name<-"AutoPandT"
temp_forecast<-data.frame(reg_df$Date, temp_model_forecast$x, temp_model_forecast$mean)
colnames(temp_forecast)<-c("Date", paste(series_name, c("", ":forecast"), sep=""))
forecast_df<-merge(forecast_df, temp_forecast)

###ECOMM

y_temp<-SP_daily_data[,c("yyyymm", "USRETAIL", "EUSRETAIL")]
y_temp<-ddply(SP_daily_data, c("yyyymm"), summarize, USRETAIL=sum(USRETAIL), EUSRETAIL=sum(EUSRETAIL))
y_temp<-na.omit(y_temp[y_temp$yyyymm<=as.Date(ISOdate(2015,6,1)),])
colnames(y_temp)[1]<-"Date"
y_temp_df<-ratio_calc(y_temp)
target_y<-y_temp_df[,c("Date", "trend_decomp_yy")]
colnames(target_y)<-c("Date", "y")
graph_y(target_y)

reg_df<-merge(macro_data,target_y, all.x=T)
reg_df_temp<-cbind(reg_df, yy_diff_df(reg_df[,c("Urate", "T10", "FF", "WTI")],12))
#reg_df_diff_temp<-diff_df(reg_df_temp[,regress_vec],1)

regress_vec<-c("GDP", "PCE", "Urate_yydiff", "CPI", "T10_yydiff", "FF_yydiff")
#sector_model<-auto.arima(reg_df$y, xreg=reg_df_temp[,regress_vec])
sector_model<-Arima(reg_df$y, xreg=reg_df_temp[,regress_vec],order=c(0,0,2))
display_results(sector_model)
temp_model_forecast<-forecast(sector_model, xreg=reg_df_temp[,regress_vec])
graph_forecast(reg_df$Date,sector_model,temp_model_forecast,"Sector Model: regressors")

series_name<-"EComm"
temp_forecast<-data.frame(reg_df$Date, temp_model_forecast$x, temp_model_forecast$mean)
colnames(temp_forecast)<-c("Date", paste(series_name, c("", ":forecast"), sep=""))
forecast_df<-merge(forecast_df, temp_forecast)
