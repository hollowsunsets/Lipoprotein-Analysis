library(chron) 
library(xlsx)

# amp_scans
test <- read.xlsx("data\ampdDataset2.xlsx",sheetIndex = 1,as.data.frame = T,header = F,stringsAsFactors=FALSE)[c(1,3)]
amplog_ts <- as.POSIXct(strptime(test$X1, "%Y-%m-%d %H:%M:%S"))
amplog_ts2 <- times(format(amplog_ts, "%H:%M:%S")) 

sparklink_ts <- read.csv("data\\SparkRunlistDataset2.csv", stringsAsFactors=FALSE)[ ,c('Executed')]
sparklink_ts<-as.POSIXct(strptime(sparklink_ts, "%d-%m-%Y %H:%M:%S"))
sparklink_ts_t<-times(format(sparklink_ts, "%H:%M:%S"))


index_amp <- 0 # starting index? this seems like an unnecessary for-loop, the index_amp is only for finding the last index that is smaller than 00:12:01
for (i in 1:length(amplog_ts)){
  if (as.character(sparklink_ts_t[1]-amplog_ts2[i])<'00:12:01'){ # 00:12:01 seems like an arbitrarily defined starting point
    index_amp=i
    break
  }
}

# splits data into data frames which have nanometer data, except that they are separated by 12 minute intervals. Also seems like an arbitrary measure. s
test2 <- split(test[index_amp:nrow(test),], cut(as.POSIXlt(test$X1[index_amp:nrow(test)], format="%Y-%m-%d %H:%M:%S"),breaks="12 mins"))

# avg_scans
sparklink <- read.csv("data\\SparkRunlistDataset2.csv")[ ,c('Sample.Name', 'Sample.Vial')]
sparklink2 <- sparklink
sparklink2$Sample.Name = (as.character(sparklink$Sample.Name))
sparklink2$Sample.Vial = (as.character(sparklink$Sample.Vial))
merge<-t(sparklink2[,2]) # makes table (with each sample vial being made a column) from sample vial names

## working with scans dataset
svp <- read.csv("data\\AIMDataset2.csv", stringsAsFactors = FALSE) 
sample_start_index<<-which(svp[,1]=='Raw Data - Time(s)') # why not use dplyr to get this, omg.
dia_start_index<<-which(svp[,1]=='Raw Data - Time(s)') # indices are for selecting the appropriate data. This is so unnecessary and gross looking. 
datetime<<-which(svp[,1]=='Date')

svp_diameters <- read.csv("data\\AIMDataset2.csv", skip = dia_start_index)[,2] 
svp_ts <- read.csv("data\\AIMDataset2.csv",skip=(datetime),header=F,stringsAsFactors=FALSE)[,c(F,T)][1:2,]
svp_ts<-as.POSIXct(paste(svp_ts[1,], svp_ts[2,], sep=" "), format="%m/%d/%Y %H:%M:%S")


# compute_graphs

result <- vector("list", (ncol(svp)-1)/4) # svp?
