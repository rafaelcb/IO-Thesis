#version 3.2.1

rm(list=ls())

packages <- c("ggplot2", "dplyr", "plm","reshape2", "AER","stargazer","xtable",
              "reporttools")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
        install.packages(setdiff(packages, rownames(installed.packages())))  
}

library(ggplot2)
library(dplyr)
library(plm)
library(reshape2)
library(AER)
library(stargazer)
library(xtable)
library(reporttools)
setwd("~/UvA - MsC Economics/Thesis/HTMLs")
source("read_data.R")

group <- split(data,data$id)
fullBuses <- sapply(group,function(x){sum(x$isFull)})
daysToTrip<-sapply(group,function(x){x$DaysToTrip[1]})
price_mean <- sapply(group,function(x){z <- x[!x$isFull,];mean(as.numeric(z$Price)
                                                          ,na.rm=TRUE)})
competitors <- sapply(group, function(x){z <- x[(!x$isFull)&(!x$multiple),];
                NROW(unique(z$Companies))})
tripLength <- sapply(group,function(x){mean(x$TripLength)})
range <- tapply(price$Price,price$id,function(x){max(x)-min(x)})
dispersion <- tapply(price$Price,price$id,function(x){median(x)-min(x)})
weekdays <-sapply(group,function(x){x$weekday[1]})
tripDay <- names(fullBuses)

oil <-read.csv("oil.csv",colClasses = c("Date","numeric","numeric"),
               col.names = c("date","oil","EURUSD") )
trips <- c("Amsterdam-Brussels","Amsterdam-London","Amsterdam-Berlin","Amsterdam-Paris",
           "Paris-Amsterdam","Paris-Brussels","Paris-London","Paris-Berlin")

panel <-colsplit(string=tripDay, pattern="[.]", 
                 names=c("trip", "when","Downloaded"))

panel$when <- as.Date(as.character(panel$when))
time <- daysToTrip + 46
panel <-cbind(panel,price_mean, competitors, tripLength, weekdays, time,
              fullBuses,range, dispersion)
panel$Downloaded=as.Date(panel$Downloaded)
panel$fullID <- paste(panel$trip,panel$when,panel$Downloaded,sep=".")



complete <- colsplit(string=tripDay, pattern="[.]", 
                     names=c("trip", "when","Downloaded"))
complete <- complete[complete$trip=="Amsterdam-Brussels",]
complete<-data.frame(cbind(trip=rep(trips,each=NROW(complete)),when=rep(complete$when,8),
                Downloaded=rep(complete$Downloaded,8)))
complete$when <- as.Date(complete$when)
complete$Downloaded <- as.Date(complete$Downloaded)
complete$fullID <- paste(complete$trip,complete$when,complete$Downloaded,sep=".") 

panel <- merge(complete,panel,by.x = "fullID", by.y = "fullID",all.x = TRUE)
names(panel)[names(panel)=="when.x"] <- "when"
names(panel)[names(panel)=="Downloaded.x"] <- "Downloaded"
names(panel)[names(panel)=="trip.x"] <- "trip"

panel <-panel[,!names(panel) %in% c("when.y","Downloaded.y","trip.y")]
panel$id <- paste(panel$trip,panel$when,sep=".")
panel <- panel[order(panel$id,panel$Downloaded,panel$when),]
replacements <- c("price_mean","competitors","fullBuses","tripLength","range","dispersion")
panel[which(is.na(panel$price_mean)), 
      replacements] <- 0.5*(panel[which(is.na(panel$price_mean))-1, replacements]+
                panel[which(is.na(panel$price_mean))+1, replacements])
panel[which(is.na(panel$time)),"time"] <- panel[which(is.na(panel$time))-1, "time"]+1

curr_locale <- Sys.getlocale("LC_TIME")
Sys.setlocale("LC_TIME", "English")
panel$weekdays <-weekdays(as.Date(panel$when))
panel$weekdays<-factor(panel$weekdays, 
                     levels = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))
Sys.setlocale("LC_TIME",curr_locale)


panel <-merge(panel, oil, by.x = "Downloaded", by.y = "date", all.x = TRUE)
panel <- panel[order(panel$id,panel$Downloaded,panel$when),]



stargazer(panel,out ="C:/Users/Castrillo/Documents/UvA - MSc Economics/Thesis/Thesis - Document/img/summary_tab_panel.tex"
          ,title="Summary statistics - panel")



results <- matrix(ncol=8,nrow=4)
rownames(results) <- c("Beta competitors","p-value (beta)",
                    "Weak instruments test (p-value)", "Wu-Hausmann (p-value)")
colnames(results) <- trips
regressions <- list()
for(i in 1:8){
        print(trips[i])
        dat <- panel[panel$trip == trips[i],]
        model <- ivreg(price_mean ~ competitors + time 
                       + I(weekdays) + oil  | time + I(weekdays)+ oil  +fullBuses,
                       data = dat) 
        print(summary(model, vcov = sandwich, diagnostics = TRUE))
        results[1,i ] <- model$coefficients[2]
        results[2,i ] <-  summary(model,diagnostics=TRUE, 
                                  vcov = sandwich)$coefficients[2,4]
        results[3:4, i] <- summary(model,diagnostics=TRUE, 
                                   vcov = sandwich)$diagnostics[1:2,4]
        regressions[[i]]<-model

}
results <- round(results,3)
results
xtable(results)

x <- colsplit(string = trips,pattern = "-", names = c("from","to")
              
results_tab <-stargazer(regressions,column.labels=paste(x$from,x$to,sep =" {\\newline} "),
        add.lines=list(c("Weak instruments test (p-value)",format(round(results[3,],3),nsmall=3)),
        c( "Wu-Hausmann (p-value)",format(round(results[4,],3)),nsmall=4)),
        out ="C:/Users/Castrillo/Documents/UvA - MSc Economics/Thesis/Thesis - Document/img/ols_tab.tex",font.size = "tiny",title="2SLS regression results for each trip")

extremes <- lapply(group, function(x){
        z <- x[!x$isFull,]
        z <- z[order(z$Price),]
        cbind(as,character(z$trip),as.character(z$Companies[1]),
              as.character(z$Companies[NROW(z)]))})


extremes <- data.frame(do.call(rbind, extremes)))
colnames(extremes)<- c("trip", "most_expensive", "cheapest")

cheap_tab <- table(extremes$cheapest,extremes$trip)
cheap_prop_tab <- round(prop.table(cheap_tab,2)*100,2)
cheap_prop_tab <- cheap_prop_tab[rowSums(cheap_prop_tab)>5,]
cheap_prop_tab <- rbind(cheap_prop_tab,Other = 100 - colSums(cheap_prop_tab))
cheap_xtable <- xtable(cheap_prop_tab,caption ="Percentage of time being cheapest")
print(cheap_xtable,type="latex",size="tiny",
      file ="C:/Users/Castrillo/Documents/UvA - MSc Economics/Thesis/Thesis - Document/img/cheap.tex")

expensive_tab <- table(extremes$most_expensive,extremes$trip)
expensive_prop_tab <- round(prop.table(expensive_tab,2)*100,2)
expensive_prop_tab <- expensive_prop_tab[rowSums(expensive_prop_tab)>5,]
expensive_prop_tab <- rbind(expensive_prop_tab,Other = 100 - colSums(expensive_prop_tab))
expensive_xtable <- xtable(expensive_prop_tab,caption ="Percentage of time being more expensive")
print(expensive_xtable,type="latex",size="tiny",
      file ="C:/Users/Castrillo/Documents/UvA - MSc Economics/Thesis/Thesis - Document/img/expensive.tex")


lifecycle_plot <- ggplot(price,aes(x=DaysToTrip,y=Price))+stat_smooth()+
        facet_wrap(~trip,ncol = 4)

png(filename="C:/Users/Castrillo/Documents/UvA - MSc Economics/Thesis/Thesis - Document/img/lifecycle.png")
        lifecycle_plot
dev.off()


summary_weekday <- melt(tapply(price$Price,list(price$weekday,price$trip),mean))
colnames(summary_weekday)<-c("weekday","trip","price")
summary_weekday <- summary_weekday[order(summary_weekday$trip,
                                         summary_weekday$weekday),]
summary_weekday$norm <- as.vector(sapply(split(summary_weekday,
                summary_weekday$trip),function(x){x$price/x$price[1]*100}))
weekday_plot <-ggplot(summary_weekday,aes(x=weekday,y=norm,group=1))+geom_point()+
        geom_line()+ facet_wrap(~trip,ncol=4)+
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
png(filename="C:/Users/Castrillo/Documents/UvA - MSc Economics/Thesis/Thesis - Document/img/weekday.png")
        weekday_plot
dev.off()
