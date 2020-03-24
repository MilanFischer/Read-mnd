
 ############################################
 #                                          #
 #             mnd files reader             #
 #        ------------------------          #
 #  Created on 2019-09-11 by Milan Fischer  #
 #  email: fischer.milan@gmail.com          #
 #                                          #
 ############################################

rm(list = ls())

library(openair)

# Create a csv file for each mnd file (TRUE or FALSE)
write_csv = TRUE

   dir.create('../outputs', showWarnings = FALSE)

if(write_csv == TRUE){
   dir.create('../outputs/csv_files', showWarnings = FALSE)
}

mnd_Files<-list.files('../mnd_files', pattern="\\.mnd$")

for(i in 1:length(mnd_Files))
{

  Metadata<-readLines(paste('../mnd_files/', mnd_Files[i], sep=''), 500)

  start=which(Metadata == 'Main Data')
  end=which(Metadata == '')[2]

  # Extract the names of variables
  Names <- character()
  for(k in (start+1):(end-1)){
     Names <- c(Names, strsplit(Metadata[k], split = '#')[[1]][2])
  }

  # Remove the empty space from the names
  Names <- gsub(' ', '', Names)

  # Extract the variable units
  Units <- character()
  for(k in (start+1):(end-1)){
     Units <- c(Units, strsplit(Metadata[k], split = '#')[[1]][3])
  }

  # Remove the empty space from the names
  Names <- gsub(' ', '', Names)

  suppressWarnings(Test <- count.fields(paste('../mnd_files/', mnd_Files[i], sep = ""), blank.lines.skip = FALSE))

  L_skip <- which(!Test %in% length(Names)); L_skip <- L_skip[which(L_skip > end)]

  # This if else statement is necessary in case of corrupted mnd files
  if(length(L_skip) > 0){
    suppressWarnings(Input <- readLines(paste('../mnd_files/', mnd_Files[i], sep = "")))
    Input <- Input[-L_skip]
    Data <- read.table(textConnection(Input), header = FALSE, skip = end, na.strings = 'N/A')
  }else{
    Data <- read.table(paste('../mnd_files/', mnd_Files[i], sep=''), header = FALSE, skip = end, na.strings = 'N/A')
  }

  # Name the variables
  names(Data) <- Names

  Date <- substr(mnd_Files[i], 1, nchar(mnd_Files[i])-4)

  if(write_csv == TRUE){
    write.table(Data, paste('../outputs/csv_files/', Date,'.csv', sep=''), col.names = TRUE, row.names = FALSE, sep = ',')
  }

  if(i>1){
    Data_merged <- rbind(Data_merged, Data)
  }else{
    Data_merged <- Data
  }

  print(Date); flush.console()
}

# Write final output
Data_merged$time <- gsub(pattern = '.*/', replacement = '', x = Data_merged$time)
Data_merged$time <- gsub(pattern = 'T', replacement = ' ', x = Data_merged$time)
Data_merged$time <- gsub(pattern = '+', replacement = 'R', x = Data_merged$time, fixed = TRUE)
Data_merged$time <- gsub(pattern = 'R.*', replacement = '', x = Data_merged$time)

write.table(Data_merged, '../outputs/Data_mnd_converted.csv', col.names = TRUE, row.names = FALSE, sep = ',')

# The aggregation assumes that the raw data are in 1 minute time step
Data_merged$time <- as.POSIXct(Data_merged$time, origin = '1970-01-01 00:00:00.1', tz = 'GMT', format = '%Y-%m-%d %H:%M')
colnames(Data_merged)[which(colnames(Data_merged) == 'time')] <- 'date'
Min10 <- timeAverage(Data_merged, avg.time = '10 min', data.thresh = 0, statistic = 'mean',
type = 'default', percentile = NA, start.date = NA, end.date = NA, interval = '1 min', vector.ws = FALSE, fill = FALSE)

# Necessary to allign the time vector in the way that the timestamp indicates the end of the averaging interval
Min10$date <- Min10$date+9*60

write.table(Min10, '../outputs/Data_mnd_converted_10min.csv', col.names = TRUE, row.names = FALSE, sep = ',')
