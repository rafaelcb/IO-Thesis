library(dplyr)
library(manipulate)

data <- read.csv("~/UvA - MSc Economics/Thesis/HTMLs/Data.csv",stringsAsFactors=FALSE)
data <-data[-grep("DeutscheBahn|bokolina|Thalys|oyages-sncf.com|SNCBEurope",data$Companies),]
data$trip<-paste0(paste0(data$From ,"-",""),data$To,"")
data$When <- as.Date(data$When,"%d/%m/%Y")
data$weekday <-weekdays(as.Date(data$When))
data$Companies <- as.factor(data$Companies)

table(data$trip,data$weekday)
data$Downloaded <- substr(data$Downloaded,1,8)
data$Downloaded <- as.Date(data$Downloaded,"%d-%m-%y")
data$Anticipation<-data$When - data$Downloaded
data$isFull <- data$Price=="Completo"
duplicated<-duplicated(data.frame(data$Departure,data$Companies,data$trip,data$Downloaded))
data<-data[!duplicated,]
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
tmp<-data.frame(summarize(group,avgPrice=mean(Price)))
tmp$DaysToTrip<- - as.numeric(tmp$Anticipation)
ggplot(tmp,aes(x=DaysToTrip,y=avgPrice))+facet_wrap(~trip,scale="free_y",ncol=4)+geom_line()

prices$DaysToTrip <- -as.numeric(prices$Anticipation)
library(ggplot2)
chart <- ggplot(prices,aes(x=DaysToTrip,y=Price))+facet_wrap(~trip,scale="free_y",ncol=2)+geom_smooth()+ theme(legend.position="none")+theme_bw()
chart

linchar <-function(date){
        temp<-prices[prices$When == date,]
        group<-group_by(temp,Anticipation,trip)
        tmp<-data.frame(summarize(group,avgPrice=mean(Price)))
        tmp$DaysToTrip<- - as.numeric(tmp$Anticipation)
        ggplot(tmp,aes(x=DaysToTrip,y=avgPrice))+facet_wrap(~trip,scale="free_y")+geom_line()
}

dates <- as.Date(row.names(table(data$When)))
manipulate(linchar(dates[i]),i = slider(1,NROW(dates)))


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

