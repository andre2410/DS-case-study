# Build bedcount column...
#


# IN: data - table of patient arrivals
# OP: Calculate a table that has ordered date and
# number of beds in use over time.
# OUT: 

bedcount <- function(data,LOScolumn=which(colnames(data)=="LOS"))
{
  # Order data from time zero onwards...
  
  data$EVENDATE <- as.Date(data$EVENDATE)
  data$EVSTDATE <- as.Date(data$EVSTDATE)
  
  data <- data[order(data$EVSTDATE),]
  uniquedays <- sort(unique(data$EVSTDATE))
  numdays <- length(uniquedays)
  
  totaldays <- as.numeric(max(data$EVENDATE)) - 
               as.numeric(min(data$EVSTDATE))
  
  res <- as.data.frame(matrix(nrow=totaldays,ncol=2,data=0))
  colnames(res) <- c("Day","Beds")
  res$Day <- min(data$EVSTDATE) + 0:(totaldays-1)
  
  # for each day - just add the people to the bed count
  # and the future bed count.  
  # Note that data seems to be so that last date has only a LOS 
  # of 1.  
  
  for (index in 1:numdays)
  {
    theday <- data[which(data$EVSTDATE==uniquedays[index]),LOScolumn]
    tt <- table(theday) # Count of num people per length of stay
    losvals <- as.numeric(labels(tt)$theday)
    numpeople <- as.numeric(tt)
    
    for (lov.index in 1:length(losvals))
    {
      res$Beds[index:(index+losvals[lov.index]-1)] <- 
        res$Beds[index:(index+losvals[lov.index]-1)] + 
        numpeople[lov.index]
    }
    
  }
  res
}

plot.bc <- function(bc,r=1:nrow(bc),pts=TRUE,...)
{
  plot(bc$Day[r],bc$Beds[r],type='l',
       ylab="Occupied Beds",xlab="Date",...)
  
  ######################################################
  # See help("POSIXlt") for details of date object.
  # $wday has values 0-6 
  #  as day of the week, starting on Sunday.
  #
  # Below we colour the points by adding 1 (since wday starts at 0)
  # So Sunday == black, Monday == red, ...Saturday == orange
  # 
  #####################################################
  if (pts==TRUE) points(bc$Day[r],bc$Beds[r],pch=16,
         col=as.POSIXlt(bc$Day[r])$wday+1)
  
}