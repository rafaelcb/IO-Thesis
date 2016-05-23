#version 3.2.1
#Program to read data from csv and generate data frames for analysis

rm(list=ls())

packages <- c("reshape2")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
        install.packages(setdiff(packages, rownames(installed.packages())))  
}

library(reshape2)

setwd("~/UvA - MsC Economics/Thesis/HTMLs")


kLocale <- Sys.getlocale()

all.data <- read.csv("~/UvA - MSc Economics/Thesis/HTMLs/Data.csv",
                     stringsAsFactors=FALSE)

all.data <- all.data[-grep("DeutscheBahn|bokolina|Thalys|oyages-sncf.com|AirBerlin|SNCBEurope",
                           all.data$companies), ]

Capwords <- function(s, strict = FALSE) {
        cap <- function(s) paste(toupper(substring(s, 1, 1)),
               {s <- substring(s, 2); if(strict) tolower(s) else s},
               sep = "", collapse = " " )
        sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}

all.data$from <- Capwords(all.data$from)
all.data$to <- Capwords(all.data$to)
all.data$trip<-paste0(paste0(all.data$from ,"-",""), all.data$to,"")



all.data$companies <- as.factor(all.data$companies)
all.data$when <-as.Date(all.data$when)
all.data$downloaded <- substr(all.data$downloaded, 1, 8)
all.data$downloaded <- as.Date(all.data$downloaded, "%d-%m-%y")
all.data$days.to.trip <- -as.numeric(all.data$when - all.data$downloaded)
all.data$is.full <- grepl("*Completo", all.data$price)== TRUE
all.data$multiple <- grepl("[&]", all.data$companies)
duplicated <- duplicated(data.frame(all.data$departure, all.data$companies,
                                      all.data$trip, all.data$downloaded))
all.data<-all.data[!duplicated, ]
rm(duplicated)

#Language settings modified temporarily to ensure datetime data (which is in 
#Spanish is translate to English)

Sys.setlocale("LC_TIME", "English")
all.data$weekday <-weekdays(as.Date(all.data$when))
all.data$weekday<-factor(all.data$weekday, levels = c("Monday","Tuesday",
        "Wednesday","Thursday","Friday","Saturday","Sunday"))

Sys.setlocale("LC_TIME", "Spanish")
all.data$arrival.time <- strptime(substr(all.data$arrival, 1, 15),"%d %b. | %R",
        tz="Europe/Paris")
all.data$arrival.time[all.data$To == "london" ]<-
        as.POSIXct(strptime(substr(all.data$arrival, 1, 15),"%d %b | %R",
        tz="Europe/London")[all.data$To == "london"], tz="Europe/Paris",
        usetz=TRUE)
all.data$departure.time <- strptime(substr(all.data$departure, 1, 15),"%d %b. | %R",
                                  tz="Europe/Amsterdam")
Sys.setlocale("LC_TIME", kLocale)

all.data$trip.length <- all.data$arrival.time - all.data$departure.time 
all.data$trip.length[all.data$To == "london"] <- 
        all.data$trip.length[all.data$To == "london"] + 1
all.data$id <- paste(all.data$trip, all.data$when, all.data$downloaded,sep=".")

all.data$full.id <- with(all.data,paste(trip ,when, downloaded, companies,
        departure.time,sep="."))

all.data <- all.data[complete.cases(all.data$trip.length), ]

all.data$price <- as.numeric(all.data$price)
price <-all.data[!all.data$is.full, ]

Rank <- function(x){as.numeric(factor(x))}

price$ranking <- unlist(tapply(price$price,price$id,Rank))

all.data$price <- as.numeric(all.data$price)


#Generate panel

group <- split(all.data,all.data$id)

full.buses <- sapply(group, function(x){sum(x$is.full)})
days.to.trip <- sapply(group, function(x){x$days.to.trip[1]})
time <- days.to.trip + 46
price.mean <- sapply(group, function(x){mean(x$price, na.rm=TRUE)})
price.median <- sapply(group, function(x){median(x$price, na.rm=TRUE)})
price.min <- sapply(group, function(x){min(x$price, na.rm=TRUE)})
price.max <- sapply(group, function(x){max(x$price, na.rm=TRUE)})
price.sd <- sapply(group, function(x){sd(x$price, na.rm=TRUE)})
price.range <- price.max - price.min
price.dispersion <- price.median - price.min
competitors <- sapply(group, function(x){z <- x[(!x$is.full)&
        (!x$multiple), ]; NROW(unique(z$companies))})
num.buses <- sapply(group, function(x){z <- x[(!x$is.full), ];
        NROW(z)})
weekday <-sapply(group,function(x){x$weekday[1]})
id <- colsplit(string=names(full.buses), pattern="[.]", names=c("trip", 
        "when","downloaded"))
id$when <- as.Date(as.character(id$when))
id$downloaded <- as.Date(as.character(id$downloaded))
trip.date = paste(id$trip, id$when, sep =".")

panel <- data.frame(trip.date, time, price.mean, price.median, price.min,
        price.max, price.range, price.dispersion, competitors, num.buses,
        full.buses, weekday, id, check.rows = TRUE)

oil <-read.csv("oil.csv",colClasses = c("Date","numeric","numeric"),
               col.names = c("date","oil","eurusd") )
panel <- merge(panel, oil, by.x = "downloaded", by.y = "date", all.x = TRUE)
trips <- c("Amsterdam-Brussels","Amsterdam-London","Amsterdam-Berlin","Amsterdam-Paris",
           "Paris-Amsterdam","Paris-Brussels","Paris-London","Paris-Berlin")
save(all.data, panel, price, trips, file = "~/UvA - MSc Economics/Thesis/HTMLs/coach_prices.RData")

rm(list=ls())