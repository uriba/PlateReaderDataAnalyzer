labelSubset <- function(df,label) {
  readings <- as.numeric(rownames(df[df[,1] %in% c("Cycle Nr.","&lt;&gt;","Well"),]))
  endings <- as.numeric(rownames(df[df[,1] == "End Time:",]))
  labelRow <- max(as.numeric(rownames(df[grep(paste0(label,"$"),df[,1]),])))
  if(adjecantLabels(df)) {
    endings <- as.numeric(rownames(df[df[,2] == "",]))
  }
  fromRow <- readings[readings > labelRow][1]
  endRow <- endings[endings>fromRow][1]
  print(label)
  print(fromRow)
  print(endRow)
  return(df[(fromRow):(endRow-1),])
}

multiFileMeasurementTime <- function(df,label) {
  labelRow <- max(as.numeric(rownames(df[grep(paste0(label,"$"),df[,1]),])))
  times <- as.numeric(rownames(df[df[,1] == "Start Time:",]))
  timeRow = times[times>labelRow][1]
  return(strptime(df[timeRow,2],format="%d/%m/%Y %H:%M:%S"))
}

multiFile <- function(df) {
  return(length(grep("Cycle Nr.",df[,1])) == 0)
}

getLabelMeasurements <- function(df,label) { #should return a "line" for multi-file, and df for single file
  labelData <- labelSubset(df,label)
  if(multiFile(df)) {
     time <- multiFileMeasurementTime(df,label)
     #take care of multi-reads and of standard reads.
  }
  if(dataInRows(labelData)) {
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
    measurements[[label]] <- getLabelMeasurements(df,label)
  }
  return(measurements)  
}
processReaderFile <- function(filepath) {
  df <- read.xls(filepath,stringsAsFactors=FALSE,header=FALSE)
  fileSource <- df[1,1]
  print(fileSource)
  if(fileSource != 'Application: Tecan i-control')
    return(NULL)
  return(getReaderData(df))
}
