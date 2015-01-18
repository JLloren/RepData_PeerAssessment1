unzip("repdata%2Fdata%2Factivity.zip")
### mean number
data<-read.table("file"activity.csv"",header=TRUE,sep=",",stringsAsFactors=FALSE, dec=".")
library(date)
library(ggplot2)
library(stringr)
library(knitr)
library(rmarkdown)
#data$date<-as.date(data$date,order="ymd")
data$date<-as.factor(data$date)
suma<-tapply(data$steps,data$date,sum,na.rm=TRUE)
psuma<-qplot(suma,xlab="Total number of steps per day",ylab="Number of days",binwidth=1000)
psuma+scale_y_continuous(breaks=c(0,2,4,6,8,10))
mean(suma)
median(suma)
# create a data.frame, with interval in first and avg of steps in second column
avg<-aggregate(x=list(steps_avg=data$steps),by=list(interval=data$interval), FUN=mean, na.rm=TRUE)
# Make a plot 
ggplot(avg,aes(x=interval,y=steps_avg))+geom_line()+ylab("average number of steps")+xlab("interval")
# Search the maximum 
str_pad(avg[which.max(avg$steps_avg),1], width = 4, side = "left", pad = 0)

## number of na
# only in the column steps
table(is.na(data$steps))
# fill in with mean values of interval
data_all<-data
for (i in 1:nrow(data_all)){
  if (is.na(data_all[i,1])){
    data_all[i,1]<-avg$steps_avg[which(avg$interval==data_all[i,3])]
  }
}
## i have the data.frame filled in 
data_all$date<-as.factor(data_all$date)
sum2<-tapply(data_all$steps,data_all$date,sum,na.rm=TRUE)
psum2<-qplot(sum2,xlab="Total number of steps per day",ylab="Number of days",binwidth=1000)
psum2+scale_y_continuous(breaks=c(0,2,4,6,8,10,12,14,16,18))
mean(sum2)
median(sum2)

## activities weekend and weekdays
data_all$date<-as.Date(as.character(data_all$date),format="%Y-%m-%d")
data_all$week<-weekdays(data_all$date)
for (i in 1:nrow(data_all)){
  if (data_all[i,4]%in%c("sábado","domingo"))
    data_all[i,4]<-"Weeekend"
  else data_all[i,4]<-"Weekday"
}
data_all$week<-as.factor(data_all$week)
weeks<-aggregate(x=list(steps_week=data_all$steps),by=list(week=data_all$week,interval=data_all$interval), FUN=mean, na.rm=TRUE)
ggplot(weeks,aes(x=interval,y=steps_week))+facet_grid(week~.)+geom_line()+ylab("average number of steps")+xlab("interval")

knit2html("PA1_Template.Rmd")
render("PA1_Template.md")
browseURL("PA1_Template.html")
