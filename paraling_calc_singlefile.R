#####
# Paralinguistic Calculater 1.01
# by Pavel Kuzkin
# Mod Lab, UC Davis
# 01/25/2017
#  updates:
# drift takes only first 2 columns and drops NA
# gentle works for a file edited file with gaps.

############################################
## FUNCTION TO READ GENTLE FILE AND TWEAK IT
## reads raw gentle file. adds headers, removes empty slots, calculates pause value.
############################################
read_gentle_single <- function (file_name){
  cls <- c(lat="numeric", lon="numeric")
  RAW_GENTLE_FILE = read.csv(file_name, header = FALSE, colClasses = cls, stringsAsFactors=FALSE, na.strings=c("", "NA"))
  if(ncol(RAW_GENTLE_FILE)>4){RAW_GENTLE_FILE = RAW_GENTLE_FILE <- RAW_GENTLE_FILE[c(1,2,3,4)]}
  if(!is.numeric(RAW_GENTLE_FILE[1,3])){ RAW_GENTLE_FILE <- RAW_GENTLE_FILE[-c(1),]}
  names(RAW_GENTLE_FILE) = c("Word1", "Word2", "Start", "End") #create header
  RAW_GENTLE_FILE = RAW_GENTLE_FILE[complete.cases(RAW_GENTLE_FILE),] #remove empty values
  RAW_GENTLE_FILE$Start = as.numeric(RAW_GENTLE_FILE$Start)
  RAW_GENTLE_FILE$End = as.numeric(RAW_GENTLE_FILE$End)
  RAW_GENTLE_FILE$Pause = 0 #create the pause column
  for (j in 1: (length(RAW_GENTLE_FILE$Word1)-1)){
    RAW_GENTLE_FILE$Pause[j+1] =  RAW_GENTLE_FILE$Start[j+1] - RAW_GENTLE_FILE$End[j]}
  return(RAW_GENTLE_FILE)
}
###############################################
# Following script takes two csv files produced by 
# gentle & drift and generates statstical data
# files should have standard (unmodified) headers
# empty values ok
# designed to work with raw files but removing adding ROWS is fine.

###############################################
# SET WORKING DIRECTORY WHERE THE INPUT FILES ARE LOCATED
setwd("~/Documents/VocalProfiles/paraling_calculator")

# file id is the name or id of the recording used.
FILE_ID = "tester3"
GENTLE_FILE = paste(FILE_ID, "_gentle.csv", sep = "") # gentle file should be called id_gentle.csv
DRIFT_FILE = paste(FILE_ID, "_drift.csv", sep = "")   # drift file should be called id_drift.csv
OUT_FILE = paste(FILE_ID, "_stats.csv", sep = "")

#gentle generates a csv with no headers
PAUSE_DATA = read_gentle_single(GENTLE_FILE)
PAUSE_DATA = subset(PAUSE_DATA, End <= 60 + min(PAUSE_DATA$Start))  # trim to 60 seconds
PAUSE_DATA[which(PAUSE_DATA$Pause <0),]$Pause = 0 # remove the  negative pauses that come from gentle errors

#drift generates csv with headers 'seconds', 'hertz'
FREQ_DATA = read.csv(DRIFT_FILE, header = TRUE, stringsAsFactors=FALSE, na.strings = c("", "NA"))
FREQ_DATA = FREQ_DATA[c(1,2)]
FREQ_DATA = FREQ_DATA[complete.cases(FREQ_DATA),]
FREQ_DATA = subset(FREQ_DATA, seconds <= 60 + min(FREQ_DATA$seconds))
FREQ_DATA = subset(FREQ_DATA, hertz != 60 || hertz != 120) # 60 and 120 are errors
FREQ_DATA$logf0 = log(FREQ_DATA$hertz, base = 10)

#create the final data output.
NUMS = data.frame(row.names = 1)
NUMS$Name = FILE_ID
NUMS$WPM = length(PAUSE_DATA$Word1)*60/(max(PAUSE_DATA$End)-min(PAUSE_DATA$Start)) 
NUMS$PauseCount01 = length(subset(PAUSE_DATA, Pause >= 0.1)$Pause)  # count
NUMS$PauseAvVal01 = mean(subset(PAUSE_DATA, Pause >= 0.1)$Pause)    # avrg
NUMS$PauseSD = sd(subset(PAUSE_DATA, Pause >= 0.1)$Pause)    # sd
NUMS$PauseCount025 = length(subset(PAUSE_DATA, Pause >= 0.25)$Pause) 
NUMS$PauseAvVal025 = mean(subset(PAUSE_DATA, Pause >= 0.25)$Pause)
NUMS$PauseCount05 = length(subset(PAUSE_DATA, Pause >= 0.5)$Pause) 
NUMS$PauseCount1 = length(subset(PAUSE_DATA, Pause >= 1)$Pause) 
NUMS$PauseCount2 = length(subset(PAUSE_DATA, Pause >= 2)$Pause) 
NUMS$TotalSilence = sum(PAUSE_DATA$Pause)
NUMS$MeanLinear = mean(FREQ_DATA$hertz)
NUMS$MeanLog = mean(FREQ_DATA$logf0)
NUMS$SDLinear = sd(FREQ_DATA$hertz)
NUMS$SDLog = sd(FREQ_DATA$logf0)
NUMS$RangeLog = (quantile(FREQ_DATA$logf0, c(.75), type=1)-(quantile(FREQ_DATA$logf0, c(.25), type=1)))   

write.table(NUMS, file = OUT_FILE, row.names = FALSE, sep = ",")



 

