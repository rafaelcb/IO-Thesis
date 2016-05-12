#version 3.2.1

packages <- c("ggplot2")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
        install.packages(setdiff(packages, rownames(installed.packages()))) 

full <- data[data$isFull == TRUE,c("trip","When","Downloaded","Companies","DepartureTime")]
full$full_id <-paste(full$trip,full$When,full$Downloaded-1,full$Companies,full$DepartureTime,sep=".")


full <- merge(full,price,by="full_id")

rank_chart<-ggplot(full,aes(x=ranking))+ geom_histogram(bins = max(full$ranking),
        colour = "black", fill = "skyblue", binwidth = 1,aes(y = ..density..))+
        facet_wrap(~trip.x,ncol=4)+scale_x_continuous(name="Ranking",
        limits=c(0, max(full$ranking)))+
        ggtitle("Ranking of prices (1 equal lowest) on the day before a coach gets full")
rank_chart


