library(reshape2)

labelSubset <- function(df,label) {
  readings <- as.numeric(rownames(df[df[,1] %in% c("Cycle Nr.","&lt;&gt;","Well"),]))
  endings <- as.numeric(rownames(df[df[,1] == "End Time:",]))
  labelRow <- max(as.numeric(rownames(df[grep(paste0(label,"$"),df[,1]),])))
  if(adjecantLabels(df)) {
    endings <- as.numeric(rownames(df[df[,2] == "",]))
  }
  fromRow <- readings[readings > labelRow][1]
  endRow <- endings[endings>fromRow][1]
  return(df[(fromRow):(endRow-1),])
}

multiFileMeasurementTime <- function(df,label) {
  labelRow <- max(as.numeric(rownames(df[grep(paste0(label,"$"),df[,1]),])))
  times <- as.numeric(rownames(df[df[,1] == "Start Time:",]))
  timeRow = times[times>labelRow][1]
  return(strftime(strptime(df[timeRow,2],format="%d/%m/%Y %H:%M:%S")))
}

multiFile <- function(df) {
  return(length(grep("Cycle Nr.",df[,1])) == 0)
}

multiread <- function(df) {
  return(length(grep("Mean",df[,2])) == 1)
}

getMultiread <- function(df) {
  wells <- df[-1,1]
  means <- df[-1,2]
  names(means) <- wells
  return(means)
}

getMatrixRead <- function(df) {
  rownames(df) <- df[,1]
  colnames(df) <- df[1,]
  df <- df[-1,-1]
  res <- c()
  for(c in colnames(df))
    for(r in rownames(df))
      res[paste0(r,c)] = df[r,c]
  return(res)
}

getSingleFileLabelMeasurements <- function(df,label) {
  labelData <- labelSubset(df,label)
  if(multiFile(df)) {
    return(NULL)
  }
  
  else if(dataInRows(labelData)) {
    print("data in rows")
    firstcol <- labelData[,1] # The first column contains the descriptors of the rows
    labelData <- as.data.frame(t(labelData)) # Get the data to be in columns form
    colnames(labelData) <- firstcol # Set the proper column names
  } else {
    print("data in columns")
    colnames(labelData) <- labelData[1,]
  }
  irrelevantCols <- colnames(labelData)[grep("^Cycle|^Temp|^O2|^CO2",colnames(labelData))]
  labelData <- labelData[,!(names(labelData) %in% irrelevantCols)]
  labelData <- labelData[-1,] # Omit headings row
  labelData <- na.omit(labelData)
  colnames(labelData)[1] <- "Time" #Rename the time column so that it has a "nicer" name
  labelData[] <- lapply(labelData,function(x) {as.numeric(as.character(x))}) # Convert values to numeric
  labelData$Time = labelData$Time/3600 # convert time to hours
  return(labelData)
}

dataInRows <- function(df) {
  return(df[2,1] != "1")
}

adjecantLabels <- function(df) {
  return(length(df[grep("^Label: ",df[,1]),1]) == 0)
}

getReaderData <- function(df) {
  df[df=="OVER"] <- "71000"
  labels <- df[grep("^Label: ",df[,1]),1]
  labels <- substring(labels,8)
  if(adjecantLabels(df)) {
    labels <- as.character(df[as.numeric(rownames(df[df[,1]=="Cycle Nr.",]))-1,1]) #in this version labels are two lines above measurement tables
  }
  print(labels)
  measurements <- vector(mode = "list", length = length(labels))
  names(measurements) <- labels
  for (label in labels) {
    measurements[[label]] <- getSingleFileLabelMeasurements(df,label)
  }
  return(measurements)  
}

getLables <- function(filepath) {
  df <- read.xls(filepath,stringsAsFactors=FALSE,header=FALSE)
  labels <- df[grep("^Label: ",df[,1]),1]
  labels <- substring(labels,8)
  return(labels)
}

readMultiFiles <- function(filepath,platesnum) {
  retval <- c()
  for(i in 1:platesnum) {
    print("getting labels from:")
    print(filepath[i])
    labels <- getLables(filepath[i])
    measurements <- vector(mode = "list", length = length(labels))
    times <- vector(mode = "list", length = length(labels))
    names(measurements) <- labels
    names(times) <- labels
    for (label in labels) {
      measurements[[label]] <- data.frame()
      times[[label]] <- c()
    }
    for(file in filepath[i+platesnum*(1:(length(filepath)/platesnum))-platesnum]) {
      print("reading")
      print(file)
      df <- read.xls(file,stringsAsFactors=FALSE,header=FALSE)
      fileSource <- df[1,1]
      if(fileSource != 'Application: Tecan i-control')
        return(NULL)
      if(!multiFile(df)) {
        return(NULL)
      }
      for(label in labels) {
        labelData <- labelSubset(df,label)
        time <- multiFileMeasurementTime(df,label)
        times[[label]] <- append(times[[label]],time)
        if(multiread(labelData)){
          data <- getMultiread(labelData)
        }
        else {
          data <- getMatrixRead(labelData)
        }
        if(length(measurements[[label]]) == 0) {
          measurements[[label]] <- data.frame(data)
          colnames(measurements[[label]]) <- time
        } else {
          measurements[[label]][[time]] <- data
          wells <- names(data)
        }
      }
    }
    for(label in labels) {
      times <- colnames(measurements[[label]])
      measurements[[label]] <- t(measurements[[label]])
      rownames(measurements[[label]]) <- times
      colnames(measurements[[label]]) <- wells
      measurements[[label]] <- as.data.frame(measurements[[label]])
      measurements[[label]]$Time <- times
    }
    retval[[i]] <- measurements
  }
  return(retval)
}

readSingleFile <- function(filepath) {
  df <- read.xls(filepath,stringsAsFactors=FALSE,header=FALSE)
  fileSource <- df[1,1]
  print(fileSource)
  if(fileSource != 'Application: Tecan i-control')
    return(NULL)
  return(getReaderData(df))
}
