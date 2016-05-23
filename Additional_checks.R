#version 3.2.1

packages <- c("ggplot2", "reporttools", "stargazer")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
        install.packages(setdiff(packages, rownames(installed.packages()))) 
}

library(ggplot2)
library(reporttools)
library(stargazer)

setwd("~/UvA - MsC Economics/Thesis/HTMLs")
source("read_data.R")

stargazer(all.data,out ="C:/Users/Castrillo/Documents/UvA - MSc Economics/Thesis/Thesis - Document/img/summary_tab_all.tex"
          ,title="Summary statistics - all all.data")
        
full <- all.data[all.data$is.full == TRUE,c("trip","when","downloaded","companies","departure.time")]
full$full.id <-paste(full$trip,full$when,full$downloaded-1,full$companies,full$departure.time,sep=".")

full <- merge(full,price,by="full.id")


rank_chart<-ggplot(full,aes(x=ranking))+ geom_histogram(bins = max(full$ranking),
        colour = "black", fill = "skyblue", binwidth = 1,aes(y = ..density..))+
        facet_wrap(~trip.x,ncol=4)+scale_x_continuous(name="Ranking",
        limits=c(0, max(full$ranking)))+
        ggtitle("Ranking of prices (1 equal lowest) on the day before a coach gets full")

png(filename="C:/Users/Castrillo/Documents/UvA - MSc Economics/Thesis/Thesis - Document/img/rank.png")
rank_chart
dev.off()

price_chart<-ggplot(price,aes(x=trip,y=Price))+ geom_boxplot()+
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
png(filename="C:/Users/Castrillo/Documents/UvA - MSc Economics/Thesis/Thesis - Document/img/dispersion.png")
price_chart
dev.off()


extremes <- lapply(group, function(x){
        z <- x[!x$isFull,]
        z <- z[order(z$Price),]
        cbind(as.character(z$trip),as.character(z$Companies[1]),
              as.character(z$Companies[NROW(z)]))})


extremes <- data.frame(do.call(rbind, extremes))
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
