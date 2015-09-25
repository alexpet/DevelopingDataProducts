
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
        cat('Get station info...\n')
        station <- st[st$NAME == name, ]
        return(station)
}

weatherDownload <- function(filename, year, ftp = ftpLoc) {
        cat('Download weather file...\n')
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
        cat('Get weather data...\n')
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
        cat('Loading weather...\n')
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
                cat('<processWeather>',files[i],'</processWeather>\n',sep=",")
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

normalWeather <- function(pw, d) {
        cat('Processing Normal weather...\n')
        weather <- pw
        weather$DATE <- ymd(paste(format(d, "%Y"), 
                                  format(weather$DATE,"%m"),
                                  '01',
                                  sep = ""))
        return(aggregate(TEMP ~ DATE, data = weather, mean))
}

findClosestStation <- function(geoCode, stations, dStart, dEnd) {
        cat('Finding closest station...\n')
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

readUsageHistory <- function(uFilePath, uHeader = FALSE, uSep = ",", uQuote = "") {
        cat('Reading usage history...\n')
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

prepareModelData <- function(usg, dw) {
        cat('Prepare data model...\n')
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
        dm$logAvgUsage <- log(dm$avgUsage + 1)
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
        dm$season <- as.factor(dm$season)
        return(dm)
}

prepareNormalModelData <- function(dw) {
        dm <- dw
        names(dm) = c("chartDate", "avgTemp")
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
        dm$season <- as.factor(dm$season)
        return(dm)
}

findBestRegression <- function(md) {
        cat('Find best regression...\n')
        #The following code will run a series of predetermined models to 
        # find best regression from the set using best Adjusted R-squared
        bestfit <- NULL
        bestAdjustRsquared <- 0
        
        cat('Model 1: avgUsage ~ HDD...\n')
        fit1 <- lm(avgUsage ~ HDD, data = md)
        adjR1 <- summary(fit1)$adj.r.squared
        if(adjR1 > bestAdjustRsquared) {
                cat('Model 1 selected as best...\n')
                bestfit <- fit1
                bestAdjustRsquared <- adjR1
        }
        
        cat('Model 2: avgUsage ~ CDD...\n')
        fit2 <- lm(avgUsage ~ CDD, data = md)
        adjR2 <- summary(fit2)$adj.r.squared
        if(adjR2 > bestAdjustRsquared) {
                cat('Model 2 selected as best...\n')
                bestfit <- fit2
                bestAdjustRsquared <- adjR2
        }
        
        cat('Model 3: avgUsage ~ avgTemp...\n')
        fit3 <- lm(avgUsage ~ avgTemp, data = md)
        adjR3 <- summary(fit3)$adj.r.squared
        if(adjR3 > bestAdjustRsquared) {
                cat('Model 3 selected as best...\n')
                bestfit <- fit3
                bestAdjustRsquared <- adjR3
        }
        
        if(length(md$logAvgUsage[is.infinite(md$logAvgUsage) | 
                                      is.nan(md$logAvgUsage)]) == 0) {
                cat('Model 1(l): logAvgUsage ~ HDD...\n')
                fit1l <- lm(logAvgUsage ~ HDD, data = md)
                adjR1l <- summary(fit1l)$adj.r.squared
                if(adjR1l > bestAdjustRsquared) {
                        cat('Model 1 selected as best...\n')
                        bestfit <- fit1l
                        bestAdjustRsquared <- adjR1l
                }
                
                cat('Model 2(l): logAvgUsage ~ CDD...\n')
                fit2l <- lm(logAvgUsage ~ CDD, data = md)
                adjR2l <- summary(fit2l)$adj.r.squared
                if(adjR2l > bestAdjustRsquared) {
                        cat('Model 2 selected as best...\n')
                        bestfit <- fit2l
                        bestAdjustRsquared <- adjR2l
                }
                
                cat('Model 3(l): logAvgUsage ~ avgTemp...\n')
                fit3l <- lm(logAvgUsage ~ avgTemp, data = md)
                adjR3l <- summary(fit3l)$adj.r.squared
                if(adjR3l > bestAdjustRsquared) {
                        cat('Model 3 selected as best...\n')
                        bestfit <- fit3l
                        bestAdjustRsquared <- adjR3l
                }                
        }
        
        cat('Model 4: avgUsage ~ avgTemp:season...\n')
        fit4 <- lm(avgUsage ~ avgTemp:season, data = md)
        adjR4 <- summary(fit4)$adj.r.squared
        if(adjR4 > bestAdjustRsquared) {
                cat('Model 4 selected as best...\n')
                bestfit <- fit4
                bestAdjustRsquared <- adjR4
        }
        
        if(length(md$logAvgUsage[is.infinite(md$logAvgUsage) | 
                                 is.nan(md$logAvgUsage)]) == 0) {
                cat('Model 4(l): logAvgUsage ~ avgTemp:season...\n')
                fit4l <- lm(logAvgUsage ~ avgTemp:season, data = md)
                adjR4l <- summary(fit4l)$adj.r.squared
                if(adjR4l > bestAdjustRsquared) {
                        cat('Model 4l selected as best...\n')
                        bestfit <- fit4l
                        bestAdjustRsquared <- adjR4l
                }
                
        }
        
        cat('Model 5: avgUsage ~ avgTemp + season...\n')
        fit5 <- lm(avgUsage ~ avgTemp + season, data = md)
        adjR5 <- summary(fit5)$adj.r.squared
        if(adjR5 > bestAdjustRsquared) {
                cat('Model 5 selected as best...\n')
                bestfit <- fit5
                bestAdjustRsquared <- adjR5
        }
        
        if(length(md$logAvgUsage[is.infinite(md$logAvgUsage) | 
                                 is.nan(md$logAvgUsage)]) == 0) {
                cat('Model 5(l): logAvgUsage ~ avgTemp + season...\n')
                fit5l <- lm(logAvgUsage ~ avgTemp + season, data = md)
                adjR5l <- summary(fit5l)$adj.r.squared
                if(adjR5l > bestAdjustRsquared) {
                        cat('Model 5l selected as best...\n')
                        bestfit <- fit5l
                        bestAdjustRsquared <- adjR5l
                }
                
        }
        
        cat('Best Adjusted R squared: ',bestAdjustRsquared,'\n',sep='')
        return(bestfit)
}

isLogModel <- function(bestFit) {
        grepl("log",as.character(bestFit$call)[2])
}

buildPredictionModel <- function(md, bestFit) {
        cat('Build a prediction model...\n')
        p <- predict(bestFit, interval = "prediction")
        pm <- cbind(md, p)
        if(isLogModel(bestFit)) {
                pm <- subset(pm, select = c(chartDate, avgUsage, fit, lwr, upr))
                pm$fit <- exp(pm$fit)-1
                pm$lwr <- exp(pm$lwr)-1
                pm$upr <- exp(pm$upr)-1
        } else {
                pm <- subset(pm, select = c(chartDate, avgUsage, fit, lwr, upr))
        }
        return(pm)
}

buildNormalModel <- function(normal, bestFit) {
        cat('Build a prediction model...\n')
        p <- predict(bestFit, newdata = normal, interval = "prediction")
        pm <- cbind(normal, p)
        if(isLogModel(bestFit)) {
                pm <- subset(pm, select = c(chartDate, fit, lwr, upr))
                pm$fit <- exp(pm$fit)-1
                pm$lwr <- exp(pm$lwr)-1
                pm$upr <- exp(pm$upr)-1
        } else {
                pm <- subset(pm, select = c(chartDate, fit, lwr, upr))
        }
        return(pm)
}

plotPredictionModel <- function(predModel) {
        cat('Plot the prediction model...\n')
        g <-  ggplot(predModel, aes(x=chartDate,y=fit)) + 
                geom_point() + 
                geom_point(aes(x = predModel$chartDate,
                               y = predModel$avgUsage), color = "red") +
                geom_errorbar(aes(ymax = upr, ymin = lwr)) +
                scale_colour_manual("", breaks = c("Prediction", "Actual"),
                                    values = c("black", "red")) +
                labs(title = "Prediction Model")
        g
}