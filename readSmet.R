library(stringr)

smet <- function(x, ...) UseMethod("smet")

smet.default <- function(x, ...)
{

    tot <- readSmet(x)

    class(tot) <- "smet"
    tot

}

cleanLine <- function(line)
{

    tmp <- unlist(strsplit(line, '[[:blank:]]'))
    ind <- which(tmp == "")
    if (length(ind) > 0){
        tmp <- tmp[-ind]
    }

    return(tmp)

}

readSmet <- function(filename, ...)
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

            i <- i+1
            i <- readHeader(linn, i, header)

        }

        if (substr(linn[i], 1, 6) == "[DATA]") {

            i <- i + 1
            val <- readData(linn, i, header$fields, header$tz)
            i <- length(linn)
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
        mat=val
        )

    return(data)

}

readHeader <- function(linn, i, header) {

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

        tmp <- cleanLine(linn[i])
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

readData <- function(linn, start, field, timezone) {

    mat <- matrix(NA, nrow=(length(linn)+1-start), ncol=length(field))
    data.tot <- data.frame(mat)
    colnames(data.tot, do.NULL = FALSE)
    colnames(data.tot) <-  field
    for (i in start:length(linn)){

        tmp <- cleanLine(linn[i])

        for (j in 1:length(field)) {

            if (j == 1) {data.tot[i-start+1,1] <- as.character(tmp[j])}
            else {data.tot[i-start+1,j] <- as.numeric(tmp[j])}

        }
        
    }

    data.tot[,1] <- as.POSIXct(data.tot[,1], "%Y-%m-%dT%H:%M", origin="1970-01-01", tz='GMT')
    
    return(data.tot)

}

print.smet <- function(x, ...)
{

    cat("Smet file\n")
    cat("Station ID:\t",x$station_id,"\n")

}
