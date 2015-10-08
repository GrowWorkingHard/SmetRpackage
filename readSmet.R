#************************************************************************************#
#*          - GrowWorkingHard'script - www.growworkinghard.wordpress.com -          *#
#************************************************************************************#
#                                                                                    #
# This file is a script written to complete our blog's articles (all the external    #
# reference can be found there). It's needed to explain better concepts and give     #
# to the users an example or a reference.                                            #
# We don't give you the warranty that is the best solution, but we do all the        #
# possible to hit it.                                                                #
#                                                                                    #
# You're invited to try our solution and communicate if something does't work or if  #
# there is a better way to do it!                                                    #
#                                                                                    #
# Follow our blog: <http://www.growworkinghard.wordpress.com>                        #
#                                                                                    #
# Author: Francesco Serafin & Daniele Dalla Torre                                    #
# Date: 2015-04-24                                                                   #
#                                                                                    #
# License: GPL v3                                                                    #
#                                                                                    #
#************************************************************************************#

library(stringr)

smet <- function(x, ...) UseMethod("smet")

smet.default <- function
(x
### The relative path with the name of the file to read
 ) {

    tot <- .readSmet(x)

    class(tot) <- "SMET"
    tot
### The SMET object with all metadata
}

.cleanLine <- function(line)
{

    tmp <- unlist(strsplit(line, '[[:blank:]]'))
    ind <- which(tmp == "")
    if (length(ind) > 0){
        tmp <- tmp[-ind]
    }

    return(tmp)

}

.readSmet <- function(filename)
{

    id = NA
    name = NA
    lat = NA
    lon = NA
    alt = NA
    east = NA
    north = NA
    epsg = NA
    novalue = NA
    timezone = NA
    field = NA

    header <- list(
        station_id=id,
        station_name=name,
        latitude=lat,
        longitude=lon,
        altitude=alt,
        easting=east,
        northing=north,
        epsg=epsg,
        nodata=novalue,
        tz=timezone,
        fields=field
        )
    
    connection = file(filename, open="r")

    linn = readLines(connection)

    i <- 1
    while (i <= length(linn)) {

        if (substr(linn[i], 1, 8) == "[HEADER]") {

            i <- i+1 # go to next line
            i <- .readHeader(linn, i, header)

        }

        if (substr(linn[i], 1, 6) == "[DATA]") {

            i <- i + 1 # go to next line
            val <- .readData(linn, i, header$fields, header$tz, header$nodata)
            break
        }

        i <- i + 1

    }

    close(connection)

    data <- list(
        station_id=header$station_id,
        station_name=header$station_name,
        latitude=header$latitude,
        longitude=header$longitude,
        altitude=header$altitude,
        easting=header$easting,
        northing=header$northing,
        epsg=header$epsg,
        nodata=header$nodata,
        tz=header$tz,
        fields=header$fields,
        mat=val
        )

    return(data)

}

.readHeader <- function(linn, i, header) {

    id = NA
    name = NA
    lat = NA
    lon = NA
    alt = NA
    east = NA
    north = NA
    epsg = NA
    novalue = NA
    timezone = NA
    fields = NA

    while(substr(linn[i], 1, 6) != "[DATA]"){

        tmp <- .cleanLine(linn[i])
        if (tmp[1] == "station_id") id <- as.character(tmp[length(tmp)])
        else if (tmp[1] == "station_name") name <- as.character(tmp[length(tmp)])
        else if (tmp[1] == "latitude") lat <- as.numeric(tmp[length(tmp)])
        else if (tmp[1] == "longitude") lon <- as.numeric(tmp[length(tmp)])
        else if (tmp[1] == "altitude") alt <- as.numeric(tmp[length(tmp)])
        else if (tmp[1] == "easting") east <- as.numeric(tmp[length(tmp)])
        else if (tmp[1] == "northing") north <- as.numeric(tmp[length(tmp)])
        else if (tmp[1] == "epsg") epsg <- as.numeric(tmp[length(tmp)])
        else if (tmp[1] == "nodata") novalue <- as.numeric(tmp[length(tmp)])
        else if (tmp[1] == "tz") timezone <- as.numeric(tmp[length(tmp)])
        else if (tmp[1] == "fields") {

            field=c(3:length(tmp))
            index = 1
            for (cont in 3:length(tmp)) {

                field[index] = tmp[cont]
                index <- index + 1

            }

        }
        i <- i + 1
    }

    tmp <- list(
        station_id=id,
        station_name=name,
        latitude=lat,
        longitude=lon,
        altitude=alt,
        easting=east,
        northing=north,
        epsg=epsg,
        nodata=novalue,
        tz=timezone,
        fields=field
        )

    eval.parent(substitute(header <- tmp))
    
    return(i)

}

.readData <- function(linn, start, field, timezone, nodata) {

    mat <- matrix(NA, nrow=(length(linn)+1-start), ncol=length(field))
    data.tot <- data.frame(mat)
    colnames(data.tot, do.NULL = FALSE)
    colnames(data.tot) <-  field
    for (i in start:length(linn)){

        tmp <- .cleanLine(linn[i])

        for (j in 1:length(field)) {

            if (j == 1) {data.tot[i-start+1,1] <- as.character(tmp[j])}
            else if (tmp[j] == nodata) data.tot[i-start+1,j] <- NA
            else data.tot[i-start+1,j] <- as.numeric(tmp[j])

        }
        
    }

    eval.parent(substitute(start <- i))
    data.tot[,1] <- as.POSIXct(data.tot[,1], "%Y-%m-%dT%H:%M", origin="1970-01-01", tz='GMT')
    
    return(data.tot)

}

print.smet <- function
(x
### The filled object of class SMET
 )
{

    cat("Class: ",class(x),"\n")
    cat("[HEADER]\n")
    cat("Station Name:\t",x$station_name,"\n")
    cat("Station ID:\t",x$station_id,"\n")
    cat("Altitude:\t",x$altitude,"\n")
    cat("Longitude: ",x$longitude,"\tLatitude: ",x$latitude,"\n")
    cat("Easting:   ",x$easting,"\tNorthing: ",x$northing,"\n")
    cat("\n")
    cat("\n")
    cat("[GAUGES]\n")
    for (i in 1:length(x$fields)) {

        if (x$fields[i] == "timestamp"){}
        else cat(x$fields[i],"\t")

    }
    cat("\n")

}
