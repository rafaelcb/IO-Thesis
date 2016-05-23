library(XML)
library(stringr)

setwd("~/UvA - MsC Economics/Thesis/HTMLs")
source("parseData.R")

setwd("~/UvA - MsC Economics/Thesis/HTMLs/Non-parsed files")
folders <-list.files()
paths<-paste0("~/UvA - MsC Economics/Thesis/HTMLs/Non-parsed files/",folders,"")
for(j in 1:NROW(paths)){
        setwd(paths[j])
        allfiles <-list.files()
        for(i in 1:NROW(allfiles)){
                print(paste(paths[j],allfiles[i],sep="/"))
                data<-parseData(allfiles[i])
                write.table(data, file = "~/UvA - MsC Economics/Thesis/HTMLs/Data.csv", append = TRUE, quote = TRUE, sep = ",",
                            eol = "\n", na = "NA", dec = ".", row.names = FALSE,
                            col.names = FALSE, qmethod = c("escape", "double"),
                            fileEncoding = "")
        }        
}
