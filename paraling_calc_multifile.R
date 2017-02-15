#####
# Paralinguistic Calculater 1.1
# by Pavel Kuzkin
# Mod Lab, UC Davis
# 01/12/2017

############################################
## FUNCTION TO READ GENTLE FILE AND TWEAK IT
## reads raw gentle file. adds headers, removes empty slots, calculates pause value.
############################################
read_gentle_single <- function (file_name){
  RAW_GENTLE_FILE = read.csv(file_name, header = FALSE, stringsAsFactors=FALSE)
  RAW_GENTLE_FILE <- RAW_GENTLE_FILE[c(1,2,3,4)]
  if(!is.numeric(RAW_GENTLE_FILE[1,3])){ RAW_GENTLE_FILE <- RAW_GENTLE_FILE[-c(1),]}
  names(RAW_GENTLE_FILE) = c("Word1", "Word2", "Start", "End") #create header
  RAW_GENTLE_FILE = RAW_GENTLE_FILE[complete.cases(RAW_GENTLE_FILE),] #remove empty values
  RAW_GENTLE_FILE$Start = as.double(RAW_GENTLE_FILE$Start)
  RAW_GENTLE_FILE$End = as.double(RAW_GENTLE_FILE$End)
  RAW_GENTLE_FILE$Pause = 0 #create the pause column
  RAW_GENTLE_FILE$RecLength = 0
  RAW_GENTLE_FILE$RecLength[1]= max(RAW_GENTLE_FILE$End)-min(RAW_GENTLE_FILE$Start)
  for (j in 1: (length(RAW_GENTLE_FILE$Word1)-1)){
    RAW_GENTLE_FILE$Pause[j+1] =  RAW_GENTLE_FILE$Start[j+1] - RAW_GENTLE_FILE$End[j]}
  return(RAW_GENTLE_FILE)
}

MYWD = "~/Documents/VocalPROFILEs/OCEAN/outputs"
PROFILE = "AGR15"
setwd( paste(MYWD, PROFILE, sep = "/"))
ID_LIST = list.files()

# CREATE EMPTY FILE TO SAVE ALL ITERATIONS
MASTER_FILE_NAME = paste(PROFILE, "_output.csv", sep = "")
MASTER_DF = data.frame(Participant=NA, OceanType=NA,RecordLength=NA, WPM=NA, PauseCount01=NA, PauseAvVal01=NA, PauseSD=NA, PauseCount025=NA, PauseAvVal025=NA, 
                          PauseCount05=NA, PauseCount1=NA, PauseCount2=NA, TotalSilence=NA, MeanLinear=NA, MeanLog=NA, SDLinear=NA, SDLog=NA, RangeLog=NA)


########################################################################
## LOOP THROUGH EVERY PARTICIPANT FOLDER IN DIRECTORY
########################################################################
for (i in 1:length(ID_LIST)){
  FID = ID_LIST[i]                                                   
  OUT_FILE = paste(FID, "_output.csv", sep = "")
  setwd( paste(MYWD, PROFILE, FID, sep = "/"))

## GENTLE FILES
  G_FILES = list.files(pattern="*gentle") # Get the files names
  PAUSE_DATA = do.call(rbind, lapply(G_FILES, function(x) read_gentle_single(x))) # First apply read.csv, then rbind
  PAUSE_DATA$SpeakTime = PAUSE_DATA$End - PAUSE_DATA$Start
  PAUSE_DATA[which(PAUSE_DATA$Pause <=0),]$Pause = 0
  
## DRIFT FILES
  D_FILES = list.files(pattern="*drift") # Get the files names
  FREQ_DATA = do.call(rbind, lapply(D_FILES, function(x) read.csv(x, stringsAsFactors = FALSE)))# First apply read.csv, then rbind
  FREQ_DATA = subset(FREQ_DATA, seconds <= 60 + min(FREQ_DATA$seconds))
  FREQ_DATA = subset(FREQ_DATA, hertz != 60 || hertz != 120) # 60 and 120 are errors
  FREQ_DATA$logf0 = log(FREQ_DATA$hertz, base = 10)


## CREATE A TABLE AND FILL IT WITH VALUES
  NUMS = data.frame(row.names = 1)
  NUMS$Participant = FID
  NUMS$OceanType = PROFILE
  NUMS$RecordLength = sum(PAUSE_DATA$RecLength)
  NUMS$WPM = length(PAUSE_DATA$Word1)*60/sum(PAUSE_DATA$RecLength)
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
  
  MASTER_DF = rbind(MASTER_DF, NUMS)
  #write.table(NUMS, file = OUT_FILE, row.names = FALSE, sep = ",")
} 
# end of i-for loop
########################################################################

setwd("~/Documents/VocalPROFILEs/OCEAN/stats_output")
MASTER_DF <- MASTER_DF[-c(1),]
write.table(MASTER_DF, file = MASTER_FILE_NAME, row.names = FALSE, sep = ",")

