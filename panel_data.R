#version 3.2.1

all.data <- all.data[(all.data$when >="2016-04-29")&(all.data$when <=Sys.Date()+15),]

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

load("coach_prices.RData")

stargazer(panel,out ="C:/Users/Castrillo/Documents/UvA - MSc Economics/Thesis/Thesis - Document/img/summary_tab_panel.tex"
          , title="Summary statistics - panel")


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

x <- colsplit(string = trips,pattern = "-", names = c("from","to"))
              
results_tab <-stargazer(regressions,column.labels=paste(x$from,x$to,sep =" {\\newline} "),
        add.lines=list(c("Weak instruments test (p-value)",format(round(results[3,],3),nsmall=3)),
        c( "Wu-Hausmann (p-value)",format(round(results[4,],3)),nsmall=4)),
        out ="C:/Users/Castrillo/Documents/UvA - MSc Economics/Thesis/Thesis - Document/img/ols_tab.tex",font.size = "tiny",title="2SLS regression results for each trip")

for(i in 1:8){
        print(trips[i])
        dat <- panel[panel$trip == trips[i],]
        model <- lm(price_mean ~ competitors + time 
                       + I(weekdays) + oil, data = dat) 
        print(summary(model, vcov = sandwich, diagnostics = TRUE))
        results[1,i ] <- model$coefficients[2]
        results[2,i ] <-  summary(model,diagnostics=TRUE, 
                                  vcov = sandwich)$coefficients[2,4]
        regressions[[i]]<-model
        
}
results <- round(results,3)
results

results_tab <-stargazer(regressions,column.labels=paste(x$from,x$to,sep =" {\\newline} "),
                                      out ="C:/Users/Castrillo/Documents/UvA - MSc Economics/Thesis/Thesis - Document/img/ols_noiv_tab.tex",font.size = "tiny",title="OLS regression results for each trip")
              