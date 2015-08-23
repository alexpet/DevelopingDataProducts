
#Create directories
if(!file.exists("data")) {
        dir.create("data")
}
if(!file.exists("data/raw")) {
        dir.create("data/raw")
}

#Construct a geocode url
construct.geocode.url <- function(address, return.call = "json", sensor = "false") {
        root <- "http://maps.google.com/maps/api/geocode/"
        u <- paste(root, return.call, "?address=", address, "&sensor=", sensor, sep = "")
        return(URLencode(u))
}

#Construct a geocode function
gGeoCode <- function(address,verbose=FALSE) {
        if(verbose) cat(address,"\n")
        u <- construct.geocode.url(address)
        doc <- getURL(u)
        x <- fromJSON(doc,simplify = FALSE)
        if(x$status=="OK") {
                lat <- x$results[[1]]$geometry$location$lat
                lng <- x$results[[1]]$geometry$location$lng
                return(c(lat, lng))
        } else {
                return(c(NA,NA))
        }
}

#Set the file name
file <- "ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-history.csv"
mFilePath <- "data/isd-history.csv"
ftpLoc <- "ftp://ftp.ncdc.noaa.gov/pub/data/noaa/"

#Attempt to load the file
#Todo: Change this to check if file is available then download.
if(!file.exists(mFilePath)) {
        try(download.file(file, mFilePath,
                          quiet = TRUE))
        if (file.info("data/isd-history.csv")$size >
            0) {
                break
        }
}

st <- read.csv("data/isd-history.csv")
names(st)[c(3, 9)] <- c("NAME", "ELEV")
st$BEGIN <- as.numeric(substr(st$BEGIN, 1, 4))
st$END <- as.numeric(substr(st$END, 1, 4))

getStationInfo <- function(name) {
        station <- st[st$NAME == name, ]
        return(station)
}

weatherDownload <- function(filename, year, ftp = ftpLoc) {
        destFile <- paste("data/raw/", filename, sep = "")
        urlFile <- paste(ftpLoc, year, "/", filename, sep = "")
        if(!file.exists(destFile)) {
                tryCatch({
                        download.file(url = urlFile, destfile = destFile)
                        cat("<download>",urlFile,destFile,"</download>",sep=",")
                        return("<download successful>")
                },
                warning = function(e) {return(e)},
                error = function(e) {unlink(destFile)
                        return(e)})
        } else {
                return("<file already downloaded>")
        }
}

getWeather <- function(stations, sDate, eDate) {
        begin <- year(sDate)
        end <- year(eDate)
        years <- abs(end-begin)
        outputs <- as.data.frame(matrix(NA, dim(stations)[1]*years,3))
        names(outputs) <- c("FILE","YEAR","STATUS")
        i <- 1
        for (y in begin:end) {
                y.stations <- stations[stations$BEGIN <= y & stations$END >=
                                               y, ]
                cat("Stations",dim(outputs)[1],sep=":")
                for (s in 1:dim(y.stations)[1]) {
                        outputs[i, 1] <- paste(sprintf("%06d", y.stations[s,1]), 
                                               "-",
                                               sprintf("%05d", y.stations[s,2]),
                                               "-",
                                               y,
                                               ".gz",
                                               sep = "")
                        outputs[i, 2] <- y
                        #Download files
                        outputs[i, 3] <- weatherDownload(outputs[i,1], y)
                        cat("<record>",s,outputs[s,1],outputs[s,2],outputs[s,3],
                            "</record",sep=",")
                        i = i + 1
                }
        }
        outputs
}

loadWeather <- function(stationResults) {
        files <- subset(stationResults, 
                                grepl("download", STATUS),select = FILE)
        files <- files[,1]
        column.widths <- c(4, 6, 5, 4, 2, 2, 2, 2, 1, 6, 7, 5, 5, 5, 4, 3, 1, 1,
                           4, 1, 5, 1, 1, 1, 6, 1, 1, 1, 5, 1, 5, 1, 5, 1)
        stations <- as.data.frame(matrix(NA, length(files), 6))
        names(stations) <- c("USAFID", "WBAN", "YR", "LAT", "LONG", "ELEV")
        weatherData <- NULL
        for (i in 1:length(files)) {
                data <- read.fwf(paste("data/raw/", files[i], sep = ""), 
                                 column.widths)
                data <- data[, c(2:8, 10:11, 13, 16, 19, 29, 31, 33)]
                names(data) <- c("USAFID", "WBAN", "YR", "M", "D", "HR", "MIN", 
                                 "LAT", "LONG", "ELEV", "WIND.DIR", "WIND.SPD", 
                                 "TEMP", "DEW.POINT", "ATM.PRES")
                data$LAT <- data$LAT/1000
                data$LONG <- data$LONG/1000
                data$WIND.SPD <- data$WIND.SPD/10
                data$TEMP <- data$TEMP/10
                data$DEW.POINT <- data$DEW.POINT/10
                data$ATM.PRES <- data$ATM.PRES/10
                stations[i, 1:3] <- data[1, 1:3]
                stations[i, 4:6] <- data[1, 8:10]
                weatherData <- rbind(weatherData, data)
                cat('<processWeather>',files[i],'</processWeather',sep=",")
        }
        return(weatherData)
}

processWeather <- function(rw) {
        cat('Processing weather...\n')
        rw$TEMP[rw$TEMP == 999.9] <- NA
        rw$WIND.DIR[rw$WIND.DIR == 999] <- NA
        rw$WIND.SPD[rw$WIND.SPD == 999.9] <- NA
        rw$DEW.POINT[rw$DEW.POINT == 999.9] <- NA
        rw$ATM.PRES[rw$ATM.PRES == 9999.9] <- NA
        m <- min(rw$MIN)
        rw <- rw[rw$MIN == m, ]
        rw <- rw[order(rw$M, rw$D, rw$HR), ]
        rw$DATE <- as.Date(paste(rw$YR, rw$M, rw$D, sep = "-"),
                           format = "%Y-%m-%d")
        return(rw)
}

dailyWeather <- function(pw) {
        cat('Processing Daily weather...\n')
        return(aggregate(TEMP ~ DATE, data = pw, mean))
}

findClosestStation <- function(geoCode, stations, dStart, dEnd) {
        r <- dim(stations)[1]
        st <- stations[!(is.na(stations$LAT) | is.na(stations$LON) |
                                 stations$LAT == 0 | stations$LON == 0) &
                               stations$BEGIN <= year(dEnd) &
                               stations$END >= year(dStart), ]
        #Add a few colums
        st$LOC.LAT <- geoCode[1]
        st$LOC.LON <- geoCode[2]
        st$DIS <- sqrt( (st$LAT - st$LOC.LAT)^2 +(st$LON - st$LOC.LON)^2 )
        return(st[st$DIS == min(st$DIS), ])
        # return the results
        return(st[st$USAF == CLO.USAF & st$WBAN == CLO.WBAN, ])
}

readUsageHistory <- function(uFilePath, uHeader, uSep, uQuote) {
        usageHistory <- read.csv(file = uFilePath,
                                 header = uHeader,
                                 sep = uSep,
                                 quote = uQuote)
        names(usageHistory) <- c("start", "end", "usage")
        usageHistory$usage <- as.numeric(usageHistory$usage)
        usageHistory$start <- as.Date(dmy(usageHistory$start))
        usageHistory$end <- as.Date(dmy(usageHistory$end))
        usageHistory$usageDays <- as.integer(difftime(usageHistory$end, 
                                                   usageHistory$start,
                                                   units = "days")) # days
        usageHistory$avgDailyUsage <- usageHistory$usage / usageHistory$usageDays
        usageHistory$chartDate <- usageHistory$start + 
                days(as.integer(usageHistory$usageDays/2))
        return(usageHistory)
}

prepareModelData <- function(usages, dailyWeather) {
        join_string <- "select dw.*, 
                                usg.* 
                        from dw 
                        inner join usg 
                                on dw.DATE between usg.start and usg.end
        "
        mrg <- sqldf(join_string, stringsAsFactors = FALSE)
        agg_string <- "select chartDate, 
                                avg(TEMP) as avgTemp, 
                                avg(avgDailyUsage) as avgUsage
                        from mrg 
                        group by chartdate
        "
        dm <- sqldf(agg_string)
        dm$FH <- dm$avgTemp*9/5+32
        dm$CDD <- dm$FH-65
        dm$CDD[dm$CDD<0] <- 0
        dm$HDD <- 65-dm$FH
        dm$HDD[dm$HDD<0] <- 0
        dm$dayCount <- as.integer(format(dm$chartDate, "%j")) 
        dm$season <- 'Winter'
        dm$season[dm$dayCount >= 80 & dm$dayCount < 173] <- 'Spring'
        dm$season[dm$dayCount >= 173 & dm$dayCount < 267] <- 'Summer'
        dm$season[dm$dayCount >= 267 & dm$dayCount < 357] <- 'Fall'
}