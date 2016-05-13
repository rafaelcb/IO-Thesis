#version 3.2.1

rm(list=ls())

packages <- c("ggplot2", "dplyr", "plm","reshape2", "AER","stargazer","xtable")
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

setwd("~/UvA - MsC Economics/Thesis/HTMLs")
source("read_data.R")

group <- split(data,data$id)
fullBuses <- sapply(group,function(x){sum(x$isFull)})
daysToTrip<-sapply(group,function(x){x$DaysToTrip[1]})
price <- sapply(group,function(x){z <- x[!x$isFull,];median(as.numeric(z$Price)
                                                          ,na.rm=TRUE)})
competitors <- sapply(group, function(x){z <- x[(!x$isFull)&(!x$multiple),];
                NROW(unique(z$Companies))})
tripLength <- sapply(group,function(x){mean(x$TripLength)})
weekdays <-sapply(group,function(x){x$weekday[1]})
tripDay <- names(fullBuses)
panel <-colsplit(string=tripDay, pattern="[.]", 
                 names=c("trip", "when","Downloaded"))
panel$id <- paste(panel$trip,panel$when,".")
panel$when <- as.Date(as.character(panel$when))
time <- daysToTrip + 46
panel <-cbind(panel,price, competitors, tripLength, weekdays, time,
              fullBuses)
#panel <-pdata.frame(panel, index = c("id", "time"), drop.index = TRUE, 
#                    row.names = TRUE)

trips <- as.character(unique(panel$trip))

results <- matrix(ncol=8,nrow=4)
rownames(results) <- c("Beta competitors","p-value (beta)",
                    "Weak instruments test (p-value)", "Wu-Hausmann (p-value)")
colnames(results) <- trips
for(i in 1:8){
        print(trips[i])
        dat <- panel[panel$trip == trips[i],]
        model <- ivreg(price ~ competitors + time 
                       + I(weekdays)  +when | time + I(weekdays) + when +fullBuses,
                       data = dat) 
        print(summary(model, vcov = sandwich, diagnostics = TRUE))
        results[1,i ] <- model$coefficients[2]
        results[2,i ] <-  summary(model,diagnostics=TRUE, 
                                  vcov = sandwich)$coefficients[2,4]
        results[3:4, i] <- summary(model,diagnostics=TRUE, 
                                   vcov = sandwich)$diagnostics[1:2,4]
}
results <- round(results,3)
results
xtable(results)


extremes <- lapply(group, function(x){z <- x[!x$isFull,]; z <- z[order(z$Price),];cbind(as.character(z$Companies[1]),as.character(z$Companies[NROW(z)]))})
extremes <- data.frame(cbind(panel$trip, do.call(rbind, extremes)))
colnames(extremes)<- c("trip", "most_expensive", "cheapest")

cheap_tab <- table(extremes$cheapest,extremes$trip)
cheap_prop_tab <- round(prop.table(cheap_tab,2)*100,2)
cheap_prop_tab <- cheap_prop_tab[rowSums(cheap_prop_tab)>5,]
cheap_prop_tab <- rbind(cheap_prop_tab,Other = 100 - colSums(cheap_prop_tab))

expensive_tab <- table(extremes$most_expensive,extremes$trip)
expensive_prop_tab <- round(prop.table(expensive_tab,2)*100,2)
expensive_prop_tab <- expensive_prop_tab[rowSums(expensive_prop_tab)>5,]
expensive_prop_tab <- rbind(expensive_prop_tab,Other = 100 - colSums(expensive_prop_tab))

