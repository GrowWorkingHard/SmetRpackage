library(stringr)

smet <- function(x, ...) UseMethod("smet")

smet.default <- function(x, ...)
{

    tot <- readSmet(x)

    class(tot) <- "smet"
    tot

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
    
    connection = file(filename, open="r")

    linn = readLines(connection)

    for (i in 1:length(linn)) {

        if (substr(linn[i], 1, 8) == "[HEADER]") {## i <- smet.readHeader(linn, i)}

                while(substr(linn[i], 1, 6) != "[DATA]"){

                    tmp <- unlist(strsplit(linn[i], '[[:blank:]]'))
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
                                        #else if (substr(tmp))
                    
                    i <- i + 1
                }
            }
        
        if (substr(linn[i], 1, 6) == "[DATA]") {val <- smet.readData(linn, i+1);i <- length(linn)}

    }

    data <- list(
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
        mat=val
        )

    close(connection)
    return(data)

}

readHeader <- function(linn, i) {
    
    while(substr(linn[i], 1, 6) != "[DATA]"){

        tmp <- unlist(strsplit(linn[i], '[[:blank:]]'))
        if (tmp[1] == "station_id") id <<- as.character(tmp[length(tmp)])
        else if (tmp[1] == "station_name") station_name <- as.character(tmp[length(tmp)])
        #else if (substr(tmp))

        i <- i + 1
    }

    return(i)

}

readData <- function(linn, start) {

    cont <- 0
    tmp <- unlist(strsplit(linn[start], '[[:blank:]]'))
    for (j in 1:length(tmp)) {

        if (tmp[j] != "") cont <- cont + 1

    }


    mat <- matrix(NA, nrow=(length(linn)+1-start), ncol=cont)
    for (i in start:length(linn)){

        tmp <- unlist(strsplit(linn[i], '[[:blank:]]'))
        cont <- 1
        for (j in 1:length(tmp)) {

            if (j == 1) {mat[i-start+1,cont] <- as.character(tmp[j]); cont <- cont +1}
            else if (tmp[j] != "") {mat[i-start+1,cont] <- as.character(tmp[j]); cont <- cont +1}

        }
        
    }

    df <- data.frame(as.POSIXct(mat[,1], "%Y-%-m%dT%H:%M", origin="1970-01-01", tz='GMT'), as.numeric(mat[,2]))
    
    return(df)

}

## print.smet <- function(x, ...)
## {

##     print(x$station_id)

## }
