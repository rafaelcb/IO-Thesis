parseData<-function(file){
        rawHTML <- paste(readLines(file), collapse="\n")
        xml <- htmlTreeParse(rawHTML,useInternalNodes = TRUE)
        xmltop = xmlRoot(xml)
        prices_raw<-xpathApply(xmltop, "//div[@class='col-sm-18 col-md-16 col-price']", xmlValue)
        prices <- gsub("[[:space:]]", "", prices_raw)
        prices <- sapply(strsplit(prices, split="[\\\\]|[^[:print:]]",fixed = FALSE), function(x) (x[1]))
        prices <-gsub("desde","",prices)
        prices <-gsub(",",".",prices)
        
        xmltop = xmlRoot(xml)
        companies_raw<-unlist(xpathApply(xmltop, "//div[@class=' col-sm-18 col-md-20 col-companies']", xmlValue))
        companies <- gsub("[[:space:]]", "", companies_raw)
        companies <- sapply(strsplit(companies, split='Carac', fixed=TRUE), function(x) (x[1]))
        
        departure_raw<-unlist(xpathApply(xmltop, "//div[@class='col-sm-14 col-xs-18 col-departure']", xmlValue))
        departure<-gsub("   ","",gsub("\n","",str_trim(departure_raw)))
        
        arrival_raw<-unlist(xpathApply(xmltop, "//div[@class='col-sm-14 col-xs-18 col-arrival']", xmlValue))
        arrival<-gsub("   ","",gsub("\n","",str_trim(arrival_raw)))
        
        additional<-unlist(strsplit(file,"_"))
        from<-rep(additional[1],NROW(prices))
        to<-rep(additional[2],NROW(prices))
        when<-rep(additional[3],NROW(prices))
        downloaded<-rep(additional[4],NROW(prices))
        data<-data.frame(from,to,when,arrival,departure,companies,prices,downloaded)
        return(data)
}




