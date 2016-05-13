library(dplyr)
library(manipulate)
library(R.utils)

setwd("~/UvA - MsC Economics/Thesis/HTMLs")
source("read_data.R")

group<-group_by(data,trip,When,Downloaded)
summary <- data.frame(summarise(group,coaches=n()))
summary$Weekday <-weekdays(as.Date(summary$When))
summary$DaysToTrip <- as.numeric(summary$Downloaded - summary$When)
avgCoaches<-with(summary,tapply(coaches,list(trip,Weekday),mean))[,c("lunes", "martes", "miércoles", "jueves", "viernes", "sábado", "domingo")]
sdCoaches<-with(summary,tapply(coaches,list(trip,Weekday),sd))[,c("lunes", "martes", "miércoles", "jueves", "viernes", "sábado", "domingo")]
options(digits = 2)
avgCoaches
sdCoaches

prices <- data[data$Price!="Completo",]
prices$Price=as.numeric(prices$Price)
priceTab<-with(prices,tapply(Price,list(trip,weekday),median,na.rm=TRUE))[,c("lunes", "martes", "miércoles", "jueves", "viernes", "sábado", "domingo")]
priceTab

Full<-data[data$isFull,]
group_full <- group_by(Full,trip,Anticipation,Downloaded)
summary_full <- data.frame(summarise(group_full,full=n()))
summary_full$DaysToTrip <- - as.numeric(summary_full$Anticipation)
fullTab <- with(summary_full,tapply(full,trip,mean,na.rm=TRUE))
fullTab

temp<-prices[prices$When == "2016-05-14",]
group<-group_by(temp,Anticipation,trip)
tmp<-data.frame(summarize(group,avgPrice=mean(Price),na.rm=TRUE))
tmp$DaysToTrip<- - as.numeric(tmp$Anticipation)
ggplot(tmp,aes(x=DaysToTrip,y=avgPrice))+facet_wrap(~trip,scale="free_y",ncol=4)+geom_line()

prices <- prices[complete.cases(prices),]
prices$DaysToTrip <- -as.numeric(prices$Anticipation)
library(ggplot2)
chart <- ggplot(prices,aes(x=DaysToTrip,y=Price))+facet_wrap(~trip,scale="free_y",ncol=2)+geom_smooth()+ theme(legend.position="none")+theme_bw()
chart

linchar <-function(date, funName = "mean",scale="fixed",rescaled = FALSE){
        temp<-prices[prices$When == date,]
        group<-group_by(temp,Anticipation,trip)
        fun <- eval(parse(text=funName))
        tmp<-data.frame(summarize(group,avgPrice=fun(Price)))
        tmp$DaysToTrip<- - as.numeric(tmp$Anticipation)
        tmp<-tmp[order(tmp$trip,-tmp$DaysToTrip),]
        scale100 <- function(x){(x / x[NROW(x)])*100}
        if(rescaled){
                tmp$avgPrice = unlist(lapply(split(tmp$avgPrice,tmp$trip),scale100))
        }
        tit <-paste(c(capitalize(funName),"price for trip on",as.character.Date(date)),collapse= " ")
        ggplot(tmp,aes(x=DaysToTrip,y=avgPrice))+facet_wrap(~trip,scales=scale,ncol = 2)+geom_line()+ggtitle(tit)
}

dates <- as.list(row.names(table(data$When)))
funs <- list("mean","median","min","max")
scale_y = list("fixed","free_y")
manipulate(linchar(as.Date(When),fun,scale,rescaled),When = picker(dates),
           fun = picker(funs),scale = picker(scale_y),rescaled =checkbox(FALSE))


temp<-prices[prices$When == "2016-05-14",]
group<-group_by(temp,Anticipation,trip)
tmp<-data.frame(summarize(group,avgPrice=mean(Price)))
tmp$DaysToTrip<- - as.numeric(tmp$Anticipation)

ggplot(tmp,aes(x=DaysToTrip,y=avgPrice))+facet_wrap(~trip,scale="free_y")+geom_line()
ggplot(tmp,aes(x=DaysToTrip,y=avgPrice))+facet_wrap(~trip,scale="free_y",columns=4)+geom_



chart2 <- ggplot(summary,aes(x=DaysToTrip,y=coaches))+facet_wrap(~trip,scale="free_y",ncol=2)+geom_smooth()+ theme(legend.position="none")+theme_bw()
chart2

chart3 <- ggplot(summary_full,aes(x=DaysToTrip,y=full))+facet_wrap(~trip,scale="free_y",ncol=2)+ geom_point()+ theme(legend.position="none")+theme_bw()
chart3

tmp <-group_by(prices,trip,Downloaded,When)
summ_range <- data.frame(summarise(tmp,Range=max(Price)-min(Price)))
rangeTab <-with(summ_range,tapply(Range,trip,mean,na.rm=TRUE))


data$ArrivalTime<-(strptime(substr(data$Arrival,1,15),"%d %b | %R",tz="Europe/Paris"))
data$ArrivalTime[data$To == "london"]<-as.POSIXct(strptime(substr(data$Arrival,1,15),"%d %b | %R",tz="Europe/London")[data$To == "london"],tz="Europe/Paris",usetz=TRUE)
data$DepartureTime<-(strptime(substr(data$Departure,1,15),"%d %b | %R",tz="Europe/Amsterdam"))
data$TripLength <- data$ArrivalTime - data$DepartureTime 
data$TripLength[data$To == "london"] <-data$TripLength[data$To == "london"] + 1

