curr_locale <- Sys.getlocale("LC_TIME")

data <- read.csv("~/UvA - MSc Economics/Thesis/HTMLs/Data.csv",stringsAsFactors=FALSE)
data <-data[-grep("DeutscheBahn|bokolina|Thalys|oyages-sncf.com|AirBerlin|SNCBEurope",data$Companies),]

capwords <- function(s, strict = FALSE) {
        cap <- function(s) paste(toupper(substring(s, 1, 1)),
                                 {s <- substring(s, 2); if(strict) tolower(s) else s},
                                 sep = "", collapse = " " )
        sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}

data$From <- capwords(data$From)
data$To <- capwords(data$To)
data$trip<-paste0(paste0(data$From ,"-",""),data$To,"")

Sys.setlocale("LC_TIME", "English")
data$weekday <-weekdays(as.Date(data$When))
data$weekday<-factor(data$weekday, 
       levels = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))
Sys.setlocale("LC_TIME",curr_locale)

data$Companies <- as.factor(data$Companies)
data <- data[(data$When >="2016-04-29")&(data$When <=Sys.Date()+15),]
data$When <-as.Date(data$When)

table(data$trip,data$weekday)
data$Downloaded <- substr(data$Downloaded,1,8)
data$Downloaded <- as.Date(data$Downloaded,"%d-%m-%y")
data$Anticipation<-data$When - data$Downloaded
data$DaysToTrip <- -as.numeric(data$Anticipation)
data$isFull <- grepl("*Completo",data$Price)== TRUE
data$multiple <- grepl("[&]",data$Companies)
duplicated<-duplicated(data.frame(data$Departure,data$Companies,data$trip,data$Downloaded))
data<-data[!duplicated,]
rm(duplicated)


data$ArrivalTime<-(strptime(substr(data$Arrival,-2,15),"%d %b | %R",tz="Europe/Paris"))
data$ArrivalTime[data$To == "london"]<-as.POSIXct(strptime(substr(data$Arrival,1,15),"%d %b | %R",tz="Europe/London")[data$To == "london"],tz="Europe/Paris",usetz=TRUE)
data$DepartureTime<-(strptime(substr(data$Departure,1,15),"%d %b | %R",tz="Europe/Amsterdam"))
data$TripLength <- data$ArrivalTime - data$DepartureTime 
data$TripLength[data$To == "london"] <-data$TripLength[data$To == "london"] + 1
data$id <- paste(data$trip,data$When,data$Downloaded,sep=".")

data$full_id <- paste(data$trip,data$When,data$Downloaded,data$Companies,data$DepartureTime,sep=".")
#data$Companies[grepl("*urolines",data$Companies)== TRUE] <- "Eurolines"

data <- data[complete.cases(data$TripLength),]

data$Price <- as.numeric(data$Price)
price <-data[!data$isFull,]

Rank <- function(x){as.numeric(factor(x))}
price$ranking <- unlist(tapply(price$Price,price$id,Rank))

data$Price <- as.numeric(data$Price)
stargazer(data,out ="C:/Users/Castrillo/Documents/UvA - MSc Economics/Thesis/Thesis - Document/img/summary_tab_all.tex"
          ,title="Summary statistics - all data")