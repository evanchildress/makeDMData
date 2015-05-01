makeDMData<-function(species='bkt', 
                     cohorts=c(1996,2014),
                     studyYears = c(2000,2014),
                     modelType = 'js',
                     dbCredentials="~/wb_credentials.rds", 
                     processedDir="~/process-data/data_store/processed_data" 
){

# species:    specify a species (one of 'ats','bkt','bnt')
# cohorts:    specify the min and max cohort to include e.g., c(1996,2014)
# studyYears: just cuts off samples not in the period of interest
# modelType: currently set up for either 'js' or 'cjs'
#  cjs cuts off anything prior to first observations whereas
#  js includes everything back to studyYears[1]
# dbCredentials: to construct link to the database
# processedDir: directory to write to and that holds sampleNames for season definition 

##########################################################################
# Things to be careful of
# 1. Make sure all the samples that need to included in dMdata are in adjDates 

# 2. fill in yearSeasonListAnt for non-salmon samples
# 3. if have any samples that were not attempted (propSampledATSWest ==0), update the df 'temp' before it gets rbind to sampleDateList
# 4. Update new rivers (West and Shorey done) for propSampled when any sample was incomplete. This is just for ATS. need to ignorre or change for bkt
# 4. When writing code, make sure to order on tagNumber and sampleNumber after a merge if the order matters (e.g. evalRows)
#############################################################################

#############################################################################
# Major steps:
#   pheno: raw data from access
#     access query subsets on area = "inside" Or "trib" Or "above" Or "below", excludes antennas
#     basic data prep, cleans up inconsistencies, etc.
#     incoroprates adjDates from spreadsheet to line up sample nums among rivers
#
#   pheno2: [prepare to create long format]
#     1st step, subset pheno based on species and river
#     
#   pheno2Long: [long format (augmented), one observation per row and one row
#               per sample (gets subsetted later)
#
#   dMData: long format, in addition to other subsetting, dMData only contains
#           available == 1 rows (truncated for fish that are too old - determined
#           by subsetDMdataAgeInSamples [ or fish that have emigrated permanently - no more like this ].
#           All environmental data get merged into dMData as well. Data for P are
#           on the occasion and data for phi and growth are over the interval.
#           Missing observations have either data from the average day during the
#           occ for P or data averaged (etc) over the median date interval.  
#
#############################################################################

#############################################################################
#   Metadata for objects saved in the .RData output file

# "evalJSRows"          rows in dMData to evaluate when estimating phi in JS context. from ageInSamples==1 to subsetDMdataAgeInSamples     
# "evalRows"            rows in dMData to evaluate when estimating phi in CJS context. from first to subsetDMdataAgeInSamples    
# "firstObsRows"        rows in dMData that represent the first observation for each fish
# "lastObsRows"         rows in dMData that represent the last observation for each fish

# "nEvalJSRows"         number of rows in evalJSRows
# "nEvalRows"           number of rows in evalRows
# "nFirstObsRows"       number of rows in firstObsRows
# "nLastObsRows"        number of rows in lastObsRows

# "nonSummerObsRows"    rows in dMData that are not summer occasions (used for estimating p(maturation)
# "nNonSummerObsRows"   number of rows in nonSummerObsRows
# "nonSummerAIS"        ageInSample values that are not summer occasions
# "nSummerObsRows"      number of rows in nonSummerAIS
# "summerAIS"           ageInSample values that are summer occasions
# "summerObsRows"       number of rows in summerAIS

# "dMData"              This is the core data input file. it is vectorized so that each row represents a potential fish observation
#                       Around the actual observation occasoins for each fish, it is augmented backwards to ageInSamples == 1
#                       and forward to subsetDMdataAgeInSamples

# variables in dMData

# $ tagNumber             : Factor: PIT tag number
# $ ageInSamples          : num: increments by one for each combination of season and age. 1 = season 3 (Fall), age 0  
# $ year                  : num:  year of sample occasion
# $ season                : Factor: season of sample occasion. 1=spring, 2=summer, 3=fall, 4=winter 
# $ sampleNum             : num:  sample number occasion. Scaled to start with 1 for first occasion in subset of data in dMData. 
#                                 May not match original sample numbers. yearSeasonList2 for original 
#                                (for WB, adjusted for other rivers-see adjSampNums.csv) sample # (sampleNumAdj) and new sample # sampleNumAdjConsec
# $ drainage              : Factor: in pheno, "CATAMARAN" "SAWMILL"   "SHOREY"    "WEST"
# $ river                 : Factor: in pheno, "CATAMARAN BROOK" "SAWMILL RIVER" "SHOREY BROOK" "WB JIMMY" "WB MITCHELL" "WB OBEAR"  "WEST BROOK" 
# $ date2                 : POSIXct: date of capture if captured
# $ length                : int: fork lenght in mm  
# $ mature01              : num: numeric indicator of male parr maturation (0=not observed mature, 1=observed mature)
# $ section               : int: stream section number (usually 20-m lengths)  
# $ enc                   : num: numeric indicator for capture (0=not captured, 1=captured)  
# $ species               : chr: in pheno,  "ATS" "BKT" "BNT"
# $ age                   : Factor: age in years at capture - be careful, this is a factor so use as.numeric(as.character(pheno$age)) to get meaningful #s
# $ gtFirstOcc            : num: numeric indicator for an occasion greater than first capture (0 = first, 1 > first)
# $ first                 : int: sample number of first capture
# $ last                  : int:  sample number of last capture
# $ cohort                : num: year that a fish is estimated to be age-0  
# $ lagDate2              : POSIXct: date of next capture occasion
# $ medianDate2           : POSIXct: median date of the sample occasion
# $ everEmWest            : num: numeric indicator for permanent emigration from the West Brook system (includes Jimmy and Mitchell).
#                                (1= yes, 0=no). Does not indcate occasion of last observation, just that a fish ever emigrated 
# $ fromTo                : int: not very useful here. indicator of movement from one stream segement in the previous occasion to the current occ 
# $ everMat01             : chr: indicator for whether a fish was ever observed as a mature male parr (1=yes, 0=no)
# $ area                  : Factor: location indicator, "ABOVE"  "BELOW"  "INSIDE" "TRIB", inisde is sections 1-47 in the WB 
# $ riverM                : num: river meter from a fixed downstream location.  
# $ tempDuringOccSampDays : num: average water temperature during the sampling occasion  
# $ dischDuringOccSampDays: num: average water dishcarge during the sampling occasion
# $ temperature           : num: average water temperature on the day of capture
# $ discharge             : num: average water discharge on the day of capture
# $ temperatureForP       : num: 'temperature' if captured, 'tempDuringOccSampDays' if not captured 
# $ dischargeForP         : num  'discharge' if captured, 'dischDuringOccSampDays' if not captured
# $ medianDateSampleNum   : POSIXct: median date of the sampling occasion
# $ propSampled           : num: proportion of the 47 sections that were sampled during occasions. Accounts for incomplete samples. Samples were all complete in the other rivers except for West Brook except for 2002 winter.  
# $ propDaysOnAntEff      : num: proportion of days over the following interval we estimated (visually from a graph) that there were no reads on Duda's antennas - just for WEST now  
# $ dateForEnv            : POSIXct: median sample date for non-capture occsasions, capture date (date2) for capture occasions. Used to fill in environmental data using addEnvironmentalData2
# $ lagDateForEnv         : POSIXct: next occasion
# $ matNA                 : num: numeric indicator for observed male parr maturity (1=yes, 0=no)  
#                            1= matured in that maturity year (suummer-spring)
#                            0 = observed immature in fall
#                            NA = not observed mature and not captured in fall
# $ intervalLength        :Class 'difftime': interval length in days
# $ emPerm                : num: numeric indicator for occasion of permanent emigration (1=yes, 0=no)  
# $ emPermNA              : num: numeric indicator for occasion of permanent emigration (1=yes, 0=no), with NAs after last occasion that a fish was observed
# $ maxT                  : num: maximun temperature during the interval following the current occasion  
# $ meanT                 : num:  mean temperature during the interval following the current occasion 
# $ medianT               : num:  median temperature during the interval following the current occasion 
# $ minT                  : num:  minimum temperature during the interval following the current occasion 
# $ sdT                   : num:  sd of temperature during the interval following the current occasion 
# $ skewT                 : num:  skew of temperature during the interval following the current occasion 
# $ maxD                  : num  maximun discharge during the interval following the current occasion 
# $ meanD                 : num  mean discharge during the interval following the current occasion
# $ medianD               : num  median discharge during the interval following the current occasion
# $ minD                  : num  minimum discharge during the interval following the current occasion
# $ sdD                   : num  sd of discharge during the interval following the current occasion
# $ skewD                 : num  skew of discharge during the interval following the current occasion
#


#############################################################################


#library(ecoPiffle) #is this necessary?
require(RPostgreSQL)
require(lubridate)
require(integrator)
require(parallel)
require(reshape2)
require(lattice)
require(latticeExtra)
require(ggplot2)
require(moments)
#library(multicore) #linux only
require(date) #to get month/day from julian date using date.mdy
require(data.table)

link=db_connector(dbCredentials)

#store the function's execution environment to 
#  enable calls to objects within a data.table
#  when the name of the object is also a column 
#  name using get('objectName',env=execEnv)
execEnv<-environment()

pheno<-data.table(dbGetQuery(link$conn,
                  "SELECT * FROM data_corrected_tag_history;"))
setnames(pheno,c("detection_date","observed_length"),
               c("date",          "measured_length"))
pheno[,cohort:=cohortEstimated]
pheno[,cohortEstimated:=NULL]
#pheno[,sample_name:=as.numeric(sample_name)]

sampleNames<-readRDS(file.path(processedDir,"sampleNames.rds"))
sampleNames[,sample_name:=as.character(sample_name)]
sampleNames<-sampleNames[drainage=='west',list(sample_name,season)]
sampleNames[sample_name=="89",season:="PostSmolt"]

setkey(pheno,sample_name)
setkey(sampleNames,sample_name)

pheno<-sampleNames[pheno]

#############################################################################

# subset variables for dMData
# if include fish from cohorts < 1997, they will have ageInSamples that
#don't go back to 1. for now we are leaving out cohort < 1997
#when we want to include them, we'll need to augment back to ageInsamples 1
#bay adding in negative smample numbers
subsetDMdataCohortMin <- cohorts[1] # >=
subsetDMdataCohortMax <- cohorts[2] # <=

subsetDMdataAgeInSamples <- 15 # <  

# exclude fish that were captured for the first time after the
# following ageInSamples
# set equal to subsetDMdataAgeInSamples - 1 to have no effect 
maxAgeInSamplesFirstCapt <- subsetDMdataAgeInSamples - 1 #4  0

# this could be specified in the function call, but for now it's just 
#  set up for west brook
if (species == 'ats'  ) {
  riverSubset <- tolower(c('WEST BROOK'))#,'WB JIMMY') 
  #,'WB MITCHELL',"WB OBEAR") 
  #riverSubset <- c("SHOREY BROOK") 
  # "SAWMILL RIVER","WB JIMMY","WB OBEAR", "WB MITCHELL", "CATAMARAN BROOK", 
  #riverSubset <- c("SAWMILL RIVER")
  areaSubset <- tolower(c('INSIDE', 'ABOVE', 'BELOW'))#, 'TRIB' ) 

}

if (species %in% c('bkt','bnt')) {
  riverSubset <- tolower(c('WEST BROOK','WB JIMMY','WB MITCHELL',"WB OBEAR")) 
  areaSubset <- tolower(c('INSIDE', 'ABOVE', 'BELOW', 'TRIB','ABOVE ABOVE',
                          'aboveabove','BELOW BELOW' )) 
}

# columns to include in  pheno2LongList
pheno2LongList <- c('tag','sample_name','sampleNumAdjConsec',
                    'river','cohort','species',
                    'season','age','measured_length',
                    'section',
                    #'sex','mature01','everMat01','measured_weight',
                    'date','medianDate','area', 'riverN'
)
# columns to include in  pheno2LongList - need to update 'names' on next line
dMDataList <-  c('tag','sample_name','sampleNumAdjConsec','measured_length',
                 'section','enc', 'river', 'speciesConsec',
                 'seasonConsec','ageConsec','ageInSamplesConsec',
                 'yearConsec',
                 'gtFirstOcc','firstConsec','lastConsec','cohortConsec',
                 'date','medianDate',
                 'area', 'riverN','emPerm'
                 #,'mature01','everMat01Consec''measured_weight','sex'
)

dMDataNames <- c('tag','sampleName','sampleNum','length',
                 'section','enc', 'river',
                 'species','season','age','ageInSamples',
                 'year',
                 'gtFirstOcc','first','last','cohort',
                 'date','medianDate',
                 'area', 'riverN','emPerm'
                 #,'everMat01','mature01','sex','weight'
)                  

#############################################################################
emigration<-pheno[sample_name %in% c("antenna_detection","trap")]

#get rid of the two summer obs and emigration detections
pheno <- pheno[season!="Summer" & !is.na(season)]
pheno[,season:=as.numeric(factor(pheno$season,
               levels=c("PreSmolt","PostSmolt","Fall","PreWinter"),
               ordered=T))]
pheno[,sample_name:=as.numeric(sample_name)]

pheno[,medianDate:=median(date),by=sample_name]

pheno[,year:=year(date)]
pheno[,julian:=as.numeric(format(date,"%j"))]
pheno[,age:=year-cohort]
pheno[,daysOld:=julian + age * 365]

# pheno[,mature01 := ifelse(maturity %in% c('p','m','f'),1,0)]
# pheno[,everMat01:=max(mature01),by=tag]

pheno$riverN<-as.numeric(factor(pheno$river))

#rename wB sample 41.8 to 42. 42 was a sampleComple==NO, 
# but there are equivalent samples in the other rivers
pheno[sample_name == 41.8,sample_name:=42 ]

# get rid of duplicate tags on the same occasion
setkey(pheno,tag,sample_name)
pheno <- pheno[ !duplicated(pheno[,list(tag,sample_name)]), ]

##########################
# End of initial data prep 
##########################

#############################################################################
# subset species,river, and area                                                            
pheno2 <- pheno[species %in% get('species',env=execEnv) &
                river   %in% riverSubset &
                area    %in% areaSubset]

#############################################################################
# Generate data to set up long data format
#############################################################################
firstYear   <- min(pheno2$year)
firstSample <- min(pheno2$sample_name)

lastYear   <- max(pheno2$year)
lastSample <- max(pheno2$sample_name)

# set up template for all possible samples
yearSeasonList <- as.data.frame(matrix(NA,(lastYear-firstYear+1)*4,2))
i=0
for (y in 1:(lastYear-firstYear+1)){
  for (s in 1:4){
    i=i+1
    yearSeasonList[i,1] <- y + firstYear -1
    yearSeasonList[i,2] <- s
  }  
}    
names(yearSeasonList) <- c('year','season')

# get list of actual samples from data
sampleYearSeasonList <- unique(pheno2[,list(sample_name,season,year)])
setkey(sampleYearSeasonList,sample_name)
#sampleYearSeasonList$season <- as.numeric(sampleYearSeasonList$season)
# sample 47 has some fish in 2003 and 2004 (caught in Jan)
#need to make sure sample 51 is year 2004
sampleYearSeasonList[sample_name == 51,year:=2004]
sampleYearSeasonList[sample_name == 47,year:=2003]

#get rid of repeats for sample 47 and 51
sampleYearSeasonList <- unique(sampleYearSeasonList)


# add sampleNumAdj to yearSeasonList (template)
yearSeasonList2 <- data.table(merge(
  x = yearSeasonList, y = sampleYearSeasonList,
  by = c('year','season'), all.x = TRUE
))

# fill in NAs for sampleNumAdj. Happens when species=ATS for winter samples
#just hard-coding this for now. sample #s will always line up w year/season
yearSeasonList2[year==2005 & season==4,sample_name:=55]
yearSeasonList2[year==2002 & season==4,sample_name:=42]
yearSeasonList2[year==2007 & season==4,sample_name:=63]



#get rid of ANY unsampled samples - if left in, 
# trailing NA sampleNumAdj screw up evalRows [1/12/2012]
yearSeasonList2 <- yearSeasonList2[ !is.na(sample_name), ] 

yearSeasonList2$sampleNumAdjConsec <- 1:nrow(yearSeasonList2)

##################################################
# add preceding sampleNums so old fish caught early can be augmented 
#  back before the study started
##################################################
firstYear   <- (yearSeasonList2$year)[1]
firstSeason <- (yearSeasonList2$season)[1]
numOccAug   <- subsetDMdataAgeInSamples - 1 

augBack <- as.data.frame(matrix(NA,numOccAug,ncol(yearSeasonList2)))
names(augBack) <- names(yearSeasonList2)

augBack$sampleNumAdjConsec <- seq(-(numOccAug-1),0) 
# -1 to account for sampleNumConsec==0

augBack$season[nrow(augBack)] <- firstSeason - 1
if (augBack$season[nrow(augBack)] == 0) augBack$season[nrow(augBack)] <- 4 

augBack$year[nrow(augBack)] <- firstYear - 0
if (augBack$season[nrow(augBack)] == 4){
  augBack$year[nrow(augBack)] <- firstYear - 1 
}

for ( i in (numOccAug - 1) : 1 ){
  augBack$season[i] <- augBack$season[i+1] - 1
  if (augBack$season[i] == 0) augBack$season[i] <- 4
  
  augBack$year[i] <- augBack$year[i+1] - 0
  if (augBack$season[i] == 4) augBack$year[i] <- augBack$year[i+1] - 1 
  
}     

yearSeasonList3 <- rbind(augBack,yearSeasonList2)
##################################################
##################################################

# add sampleNumAdjConsec to pheno2
pheno2 <- merge(
  x = pheno2, y = yearSeasonList2[,list(sample_name,sampleNumAdjConsec)],
  by = 'sample_name', all.x = TRUE
)

nOccTemplate <- nrow(yearSeasonList3)
nInd <- length(unique(pheno2$tag))

##############################################################################
#make sampleNumConsec template for long format data
##############################################################################
sampNumTemplate <- rep(yearSeasonList3$sampleNumAdjConsec,nInd)
tagNumTemplate <- sort(rep( unique(pheno2$tag), nOccTemplate ))

template <- data.table( tag=tagNumTemplate, sampleNumAdjConsec=sampNumTemplate) 

##############################################################################
# merge pheno2 into template of tags and sampleNumAdjConsec
##############################################################################
#pheno2$sampleNumConsec) <- 'sampleNumAdjConsec'

pheno2<-pheno2[ , pheno2LongList,with=F ]

setkey(pheno2,   tag, sampleNumAdjConsec)
setkey(template, tag, sampleNumAdjConsec)

pheno2Long <- pheno2[template]

pheno2Long[,enc:=ifelse(is.na(sample_name),0,1)]


##############################################################################
# season index for each sample
##############################################################################
sampleToSeason <- unique(yearSeasonList3[,list(sampleNumAdjConsec,season)])
#sort by sampleNumAdj - otherwise yearsMatrix is out of order

##############################################################################
# year index for each sample
##############################################################################
sampleToYear <- unique(yearSeasonList3[,list(sampleNumAdjConsec,year)])
#sort by sampleNumAdj - otherwise yearsMatrix is out of order
#sampleToYear <- sampleToYear[ order(sampleToYear$sampleNumAdjConsec), ]

sampleTo <- data.table(sampleNumAdjConsec = sampleToSeason$sampleNumAdjConsec,
                       seasonConsec       = sampleToSeason$season,
                       yearConsec         = sampleToYear$year)

pheno2Long <- merge(
  x = pheno2Long, y = sampleTo,
  by = c('sampleNumAdjConsec'), all.x = TRUE, sort=FALSE
)

##############################################################################
#get individual-specifc lists and prepare to merge into pheno2Long
##############################################################################
##############################################################################
# variables that don't change within fish
##############################################################################
boundaryDetections<-data.table(dbGetQuery(link$conn,
                    "SELECT * FROM data_boundary_detections;"))
boundaryDetections<-boundaryDetections[,list(
                    lastBoundaryDetection=max(detection_date)),
                    by=tag]

setkey(boundaryDetections,tag)

byTag<- unique(pheno2[,list(cohortConsec     = cohort,
                            #everMat01Consec = everMat01,
                            speciesConsec    = species,
                            firstConsec      = min(sampleNumAdjConsec,na.rm=T),
                            lastConsec       = max(sampleNumAdjConsec,na.rm=T),
                            firstDate        = min(date,na.rm=T),
                            lastDate         = max(date,na.rm=T)),
                      by=tag])

if(sum(duplicated(byTag$tag))){
  stop('duplicate tag numbers created when making byTag, 
       which includes variables that do not vary by tag')
}

setkey(byTag,tag)
byTag<-boundaryDetections[byTag]


pheno2Long <- merge(
  x = pheno2Long, y = byTag,
  by = 'tag', all.x = TRUE
)

#create ageConsec column for each fish
pheno2Long[,ageConsec:=yearConsec -cohortConsec] 
# + 2 so season 2 is ageInSamples
pheno2Long[,ageInSamplesConsec:=4*(ageConsec-1) + 2 + seasonConsec]

# delete fish that were caught for the first time too old
byTag[,notTooOld:=
        pheno2Long[sampleNumAdjConsec == firstConsec ,
                   ageInSamplesConsec <= maxAgeInSamplesFirstCapt,
                   by=tag]$V1]
pheno2Long <- pheno2Long[tag %in% byTag[notTooOld==TRUE,tag]]


setkey(pheno2Long,tag,sampleNumAdjConsec)

#pheno2Long[pheno2Long$tag=='1BF20EA597',]

# #########################################################################
# ### Create indicator of permanent emigration 
# ### (defined as last observation at boundary antenna #
# {should this include outside?})
# ### using the antenna data
pheno2Long[,medianDateSampleNum := median(date,na.rm=T),
            by=sampleNumAdjConsec]

pheno2Long[is.na(lastBoundaryDetection) | 
             lastDate            > lastBoundaryDetection |
             medianDateSampleNum > lastBoundaryDetection,
           emPerm:=0]
pheno2Long[is.na(emPerm),
           emPerm:=ifelse(sampleNumAdjConsec == max(sampleNumAdjConsec),1,0),
           by=tag]
pheno2Long[,emigrated:=ifelse(any(emPerm==1),
                              as.numeric(date>date[which(emPerm==1)]),
                              0),
           by=tag]

##############################################################################
# prepare to create availability column
##############################################################################
nOcc <- max(yearSeasonList3$sampleNumAdjConsec)

# fish are not available when they are too old                        
##or if they have emigrated
# fish are not available before first capture
if(modelType=='cjs'){
pheno2Long[,available:=(ageInSamplesConsec <  subsetDMdataAgeInSamples &
                        sampleNumAdjConsec >= firstConsec)-0]
}

if(modelType=='js'){
  pheno2Long[,available:=(ageInSamplesConsec < subsetDMdataAgeInSamples &
                          yearConsec > studyYears[1]) -0]
  
}

# delete fish that were seen for the first time after subsetDMdataAgeInSamples
tooOld <- unique(pheno2Long[ seasonConsec       == firstConsec &
                             ageInSamplesConsec >  subsetDMdataAgeInSamples,
                            tag ])

pheno2Long <- pheno2Long[ !(tag %in% tooOld) ]

pheno2Long[sampleNumAdjConsec<1,available:=0]

pheno2Long[,gtFirstOcc:=ifelse(sampleNumAdjConsec>firstConsec,1,0)]

pheno2Long[emigrated==1,available:=0]

##############################################################################
# subset for availability ==1. Final input dataset for analysis
##############################################################################

dMData <- pheno2Long[available==1,dMDataList,with=F]

# dMDataList names defined up top
setnames(dMData, dMDataNames)
dMData<-dMData[cohort       >= subsetDMdataCohortMin &
               cohort       <= subsetDMdataCohortMax &
               ageInSamples <  subsetDMdataAgeInSamples]

##############################################################################
#add in proportion of sections sampled 

dMData$propSampled<-1
# fix incomplete samples to the proportion of sections sampled

# WEST - 2002, no river was sampled in winter 2002
dMData[year == 2002 & season == 4, propSampled:=0 ]

# West Brook samples were not complete in the following years in winter
dMData[river == 'west brook' & year == 2003 & season == 4, propSampled:=30/47 ] 

dMData[ river == 'west brook' & year == 2004 & season == 4, propSampled:=3/47 ]

dMData[ river == 'west brook' & year == 2005 & season == 4, propSampled:=0]

dMData[ river == 'west brook' & year == 2007 & season == 4, propSampled:=0 ]                                 

##############################################################################

##############################################################################
# add environmental data to each row of dMData
##############################################################################
#envData <- as.data.frame(read.csv(file="./envData.txt", header=TRUE))                                         
# file created using access file query  fourRiversAllSpp:envData query
# in C:\PITTAGMAIN\CMR Analyses\Hierach_Bugs\allSpp
#export query as text file without formatting
# then copy to felek
envData<-data.table(dbGetQuery(link$conn,"SELECT * FROM data_environmental;"))
setnames(envData,"day_of_year","julian")
envData[,date:=as.Date(date_ct)]

#need to add medianDate and lagMedianDate to row for which fish were not caught
# fill in median dates for samples with no data (e.g winters)
medianDateListForMissing<-dMData[enc==1,
                                 list(min(    date , na.rm=T),
                                      max(    date , na.rm=T),
                                      median( date , na.rm=T)),
                                 by=sampleNum]
setnames(medianDateListForMissing,
         c("sampleNum","minDate","maxDate","medianDateSampleNum"))
medianDateListForMissing[,c("minJulian","maxJulian","medianJulian","year"):=
                           list(as.numeric(format(minDate,"%j")),
                                as.numeric(format(maxDate,"%j")),
                                as.numeric(format(medianDateSampleNum,"%j")),
                                as.numeric(format(medianDateSampleNum,"%Y")))]


# saving for antennaDuda.r, so we can make graphs of antenna data
#save(medianDateListForMissing, file= './medianDateListForMissing.RData')



#calulate T and discharge means between dateMin and dataMax for each sample
# I'm sure these's a better way to do this, but this works...
meansDuringSample <- as.data.frame(matrix(NA,nrow(medianDateListForMissing),6))
names(meansDuringSample) <- c('drainage','sampleNum',
                              'tempDuringOcc','dischDuringOcc',
                              'tempDuringOccSampDays','dischDuringOccSampDays')                                 

sampleDateList <- unique(dMData[!is.na(date),list(sampleNum,date)])

getMeanDuringSample<-function(sample,type,river){
  envData[date %in% sampleDateList[sampleNum==sample,date] & 
            river == river,
          mean(get(type),na.rm=T)]
}

medianDateListForMissing[,c("tempDuringOccSampDays","dischDuringOccSampDays"):=
                           list(mapply(getMeanDuringSample,
                                       sample   = sampleNum,
                                       type     = "temperature",
                                       MoreArgs = list(river='west brook')),
                                mapply(getMeanDuringSample,
                                       sample   = sampleNum,
                                       type     = "discharge",
                                       MoreArgs = list(river='west brook')))]

meansDuringSample<-medianDateListForMissing[,list(sampleNum,
                                                  tempDuringOccSampDays,
                                                  dischDuringOccSampDays)]

setkey(sampleToSeason,sampleNumAdjConsec)
setkey(meansDuringSample,sampleNum)
meansDuringSample<-meansDuringSample[sampleToSeason]

# use tempDuringOccSampDays for analyses - is the mean of actual days samples, 
# not over range like tempDuringOcc                            

#need to fill in NaN entires in meansDuringSample. 1st get means, 
#then merge in when entry is NaN

# add rows for samples not Attempted

meansBySeason<-meansDuringSample[,list(mean(tempDuringOccSampDays,na.rm=T),
                                       mean(dischDuringOccSampDays,na.rm=T)),
                                 by=season]
setnames(meansBySeason,
         c("season",'tempDuringOccSampDays','dischDuringOccSampDays'))
# replace NaNs in meansDuringSample. these come from incomplete data in envData
#should thoroughly check indexing and column names here
fillWithMeans<-function(x,type){value<-
                                 meansBySeason[season==x,
                                               ifelse(type=='temperature',
                                                      tempDuringOccSampDays,
                                                      dischDuringOccSampDays)]
                                return(value)
}

meansDuringSample[is.na(tempDuringOccSampDays),
                  c("tempDuringOccSampDays","dischDuringOccSampDays"):=
                    list(mapply(fillWithMeans,
                                x = season,
                                MoreArgs = list(type='temperature')),
                         mapply(fillWithMeans,
                                x = season,
                                MoreArgs = list(type='discharge')))]
meansDuringSample[,season:=NULL]
# for ATS, 33 is NaN becasue minDate=maxDate
#add T and flow to each row of data as covariates for p(capt)
# merging on drainage for now because data are incomplete for river
setkey(dMData,sampleNum)
dMData <-meansDuringSample[dMData]

#merge in actual temp and disch for each fish
dMData <- merge(
  x = dMData, y = envData[,list(river,date,
                                temperature, discharge)],
  by = c('river','date'), all.x = TRUE
)



#keep actual env data if capture, otherwise keep mean during sample.
# with movement model, could estimate location and then estimate day of
# capture to get est env data

# this adds in data when it's missing for the date of capture (SHOREY)
dMData[,temperatureForP:=ifelse(is.na(date) | is.na(temperature),  
                                tempDuringOccSampDays,
                                temperature )]
# this adds in data when it's missing for the date of capture (SHOREY)
dMData[,dischargeForP:=ifelse(is.na(date) | is.na(discharge),  
                              dischDuringOccSampDays,
                              discharge )]




#ggplot(envData,aes(date,temperature)) + geom_point() + 
#  facet_wrap(~drainage)
#ggplot(subset(envData,drainage=='WEST'),aes(date2,temperature)) + geom_point() + 
#  facet_wrap(~river)
#ggplot(subset(envData,river=='WEST BROOK'),aes(julian,(discharge+0.0)))  + 
#  geom_line() + ylim(c(0,15)) + theme_bw() +
#  facet_wrap(~year) + 
#  geom_vline(aes(xintercept=julianMin),medianDateListForMissing, colour='blue') +
#  geom_vline(aes(xintercept=julianMedian),medianDateListForMissing, linetype=2) +
#  geom_vline(aes(xintercept=julianMax),medianDateListForMissing,colour='red')
#
dMData <- merge(
  x = dMData, y = medianDateListForMissing[,list(sampleNum,medianDateSampleNum)],
  by = c('sampleNum'), all.x = TRUE
)


# set up template for all possible samples for antenna efficiency
firstYearAnt <- 1997; lastYearAnt <- 2015
yearSeasonListAnt <- as.data.frame(matrix(NA,(lastYearAnt-firstYearAnt+1)*4,2))
i=0
for (y in 1:(lastYearAnt-firstYearAnt+1)){
  for (s in 1:4){
    i=i+1
    yearSeasonListAnt[i,1] <- y + firstYearAnt -1
    yearSeasonListAnt[i,2] <- s
  }  
}    
names(yearSeasonListAnt) <- c('year','season')

# set default to 0.1 so fish below 
# have a chance of detection when antennas were not working
yearSeasonListAnt$propDaysOn <- 0.1

#numbers based on graph in antennaDuda.r. 
# Fill in rest for other year/season combos (e.g. when run trout)
yearSeasonListAnt$propDaysOn[19:43] <- (c(10,80,100,95,100,65,100,80,90,100,100,
                                          80,100,100,100,90,100,50,100,100,100,
                                          100,95,85,90) ) / 100
yearSeasonListAnt$drainage <- 'WEST'
#yearSeasonListAnt$antEfficiency <- 1 #0.9 #make interval-specific
#yearSeasonListAnt$propDaysOnAntEff <- yearSeasonListAnt$propDaysOn * 
#                                  yearSeasonListAnt$antEfficiency

dMData <- merge(
  x = dMData, y = yearSeasonListAnt[,c('year','season','drainage','propDaysOn')],
  by = c('year','season'), all.x = TRUE
)


#hard code missing winter samples if they are missing
# dMData[ is.na( medianDate ) &  
#         year == 2002 & 
#         season == 4 ] <- '2002-12-04' 
# dMData$medianDateSampleNum[ is.na( dMData$medianDateSampleNum ) & 
#                               dMData$drainage == 'WEST' & 
#                               dMData$year == 2005 & 
#                               dMData$season == 4 ] <- '2005-12-04' 
# dMData$medianDateSampleNum[ is.na( dMData$medianDateSampleNum ) & 
#                               dMData$drainage == 'WEST' & 
#                               dMData$year == 2007 & 
#                               dMData$season == 4 ] <- '2007-12-04' 

dMData[,dateForEnv:=date]
dMData[is.na(date),dateForEnv:=medianDateSampleNum,]

addLagged<-function(data,individual,time,lag,k=1){
  #data is the data as a data.table
  #individual is the name of the column containing the 
   #identifier for individuals in quotes
  #time is the column that determines the order of observations
  #lag is the column that will be lagged
  #k is the number of lags, positive values lag forward and 
   #negative values lag backwards
  
  if(!is.data.table(data)){stop('data must be a data.table')}
  keycols<-c(individual,time)
  setkeyv(data,keycols)
  
  for(i in lag){
    data[,
         #make the name for the new column in camelCase
         c(paste0("lag",toupper(substr(i,1,1)),substr(i,2,100))):=list( 
           #second part of the following is just to make the output of 
           #ifelse the same shape as the hole it fills
      ifelse(k>=0 & 1:length(get(i)) > 0, 
             #if k>=0, pad with NAs at the end
             c(get(i)[(k+1):length(get(i))],rep(as.numeric(NA),k)),
             #pad with NAs at the beginning if k<0
             c(rep(as.numeric(NA),abs(k)),get(i)[1:(length(get(i))-k)]))),
      by=get(individual)]
  }
  return(data)  
}

dMData <- addLagged(data = dMData, individual = "tag", time = "dateForEnv", 
                    lag ="dateForEnv")
dMData[,lagDateForEnv:=as.Date(lagDateForEnv,origin="1970-01-01")]


############################################################
# separates first occ from others for likelihood
############################################################

# use below if there is only one row of gtFirstOcc per fish
# not true when augmenting back to ageInSamples==1
#firstObsRows <- subset(1:nrow(dMData),dMData$gtFirstOcc==0) 

dMData[,lastAIS:=max(ageInSamples,na.rm=T),by=tag]

#if limit years so that early samples for an older cohort are cut off, 
# can get fish with 0
#observations. This messes up the evalRows etc
#minSAmpleNum <- min(dMData$sampleNum)
#dMData2  <- dMData[ dMData$last > minSAmpleNum, ]


setkey(dMData,tag,sampleNum)  # MUST order before doing eval rows.

#########################################################
# 1= matured in that maturity year (suummer to next spring)
# 0 = observed immature in fall
# NA = not observed mature and not captured in fall

#create an age in maturity years to group by
dMData[,ageGroup:=floor(ageInSamples/4)]

# #assign matNA based on above criteria
# dMData[ageGroup>0,bla:=ifelse(any(mature01==1,na.rm=T),
#                               1,
#                               ifelse(length(which(season==3))==0,
#                                      as.numeric(NA),
#                                      mature01[which(season==3)])),
#        by=list(tag,ageGroup)]
# dMData[,ageGroup:=NULL]

#Remove any maturity indicators that are not summer
# dMData[season!=2,bla:=NA]
##############################################################

dMData[,intervalLength:=difftime(lagDateForEnv,dateForEnv, unit="days")]





# #########################################################################
# #########################################################################
# 
# #########################################################################
# ### AntennaTrib
# #########################################################################
# # 
# # 
# 
# antennaTribs <- as.data.frame(read.csv(file="./antennaTribs.txt", header=TRUE))                                         
# # file created using access file query  fourRiversAts:antennaDuda query
# # in C:\PITTAGMAIN\CMR Analyses\Hierach_Bugs\allSpp
# #export query as text file without formatting
# # then copy to felek
# 
# antennaTribs$date2 <- as.POSIXct(strptime(antennaTribs$date, 
#                                  format = "%m/%d/%Y"))
# antennaTribs$tagNumberCH <- as.character(antennaTribs$tagNumber)
# 
# #countAntennaTribs <- ddply( antennaTribs[,c('tagNumber','river','date2')], 
                               #.(tagNumber, river),  
# #  						summarise,  ct=length(date2)
# #		   				  ) 
# #countAntennaTribs <- countAntennaTribs[ order(countAntennaTribs$tagNumber,
#                                          countAntennaTribs$river),]
# 
# 
# # count the number of time a fiSh waS oberved on trib antenna
# # for each interval between dateForEnv and lagDataForEnv
# # takeS a LONG time to run - need to improve on loopS
# # AlSo need to makeSure dateForEnv iS acting ok.
# #if (runAntennaCount) {
# 
# #dMData$antennaCountJimmy <- 0
# #dMData$antennaCountMitchell <- 0
# 
# #for ( i in 1 : nrow( dMData ) ) {
# #	hold <- antennaTribs[ antennaTribs$tagNumberCH == dMData$tagNumberCH[i], ]
# 
# #	cJimmy <- 0
# #	cMitchell <- 0
# 
# #	if ( nrow(hold) > 0 & 
# #	     !is.na(dMData$river[i]) &
# #	     !is.na(dMData$dateForEnv[ i ]) &
# #	     !is.na(dMData$lagDateForEnv[ i ] ) ) {
# 
# #		for ( j in 1 : nrow(hold) ) {
# #			if( hold$river[j] == 'WB JIMMY' &
# #				hold$date2[j] > dMData$dateForEnv[ i ]  &
# #				hold$date2[j] <= dMData$lagDateForEnv[ i ] ) cJimmy <- cJimmy + 1
# #		
# #			if( hold$river[j] == 'WB MITCHELL' &
# #				hold$date2[j] > dMData$dateForEnv[ i ]  &
# #				hold$date2[j] <= dMData$lagDateForEnv[ i ] ) cMitchell <- 
#                                                      cMitchell + 1	
# #     }
# 
# #	}
# 
# #	dMData$antennaCountJimmy[ i ] <- cJimmy
# #	dMData$antennaCountMitchell[ i ] <- cMitchell
# #	if ( i %% 50 == 0 ) print( c( i,nrow(dMData),Sys.time() ) )
# #}	
# 
# #}    
# 
# #thiS work faster
# m <- merge ( x = dMData, y=antennaTribs[,c('river','tagNumberCH','date2')],
#                                          by = c('tagNumberCH'), all.x=TRUE)
# #m <- m[order(m$tagNumberCH,m$sampleNum),]
# m$antennaHit <- ifelse( m$date2.y > m$dateForEnv & 
#                           m$date2.y <= m$lagDateForEnv,
#                         1, 0 )
# 
# m$antennaHit[ is.na( m$antennaHit) ] <- 0		                
# 
# # too Slow
# #mHit <- ddply( m, .(river.y, tagNumberCH, sampleNum), summarise, 
#                   sum( antennaHit ), .progress = "text" )		                
# 
# #mHit <- tapply(m$antennaHit,
#                 list(m$river.y, m$tagNumberCH, m$sampleNum), sum)
# mHit <- aggregate( m$antennaHit , 
#                    by=list( m$river.y, m$tagNumberCH, m$sampleNum ),
#                    FUN=sum, na.rm =TRUE )
# 
# names(mHit) <- c('river','tagNumberCH','sampleNum','antennaCount')
# 
# rm(m)
# 
# mHitJimmy <- subset(mHit, river == 'WB JIMMY')
# mHitMitchell <- subset(mHit, river == 'WB MITCHELL')
# mHitOBear <- subset(mHit, river == 'WB OBEAR')
# 
# names(mHitJimmy) <- c('antennaTrib','tagNumberCH', 
#                       'sampleNum', 'antennaCountJimmy')
# names(mHitMitchell) <- c('antennaTrib','tagNumberCH', 
#                          'sampleNum', 'antennaCountMitchell')
# names(mHitOBear) <- c('antennaTrib','tagNumberCH', 
#                       'sampleNum', 'antennaCountOBear')
# 
# 
# dMData <- merge ( x = dMData, y=mHitJimmy[,2:4], 
#                   by = c('tagNumberCH', 'sampleNum'), all.x=TRUE)
# dMData <- merge ( x = dMData, y=mHitMitchell[,2:4], 
#                   by = c('tagNumberCH', 'sampleNum'), all.x=TRUE)
# dMData <- merge ( x = dMData, y=mHitOBear[,2:4], 
#                   by = c('tagNumberCH', 'sampleNum'), all.x=TRUE)
# 
# 
# dMData$antennaCountJimmy[ is.na( dMData$antennaCountJimmy ) ] <- 0		                
# dMData$antennaCountMitchell[ is.na( dMData$antennaCountMitchell ) ] <- 0
# dMData$antennaCountOBear[ is.na( dMData$antennaCountOBear ) ] <- 0
# 
# dMData$antennaCountJimmyGT1 <- ifelse(dMData$antennaCountJimmy >0, 1, 0)
# dMData$antennaCountMitchellGT1 <- ifelse(dMData$antennaCountMitchell >0, 1, 0)
# dMData$antennaCountOBearGT1 <- ifelse(dMData$antennaCountOBear >0, 1, 0)
# 
# # get list of fish that moved from OBear to WB - 
# #  this is better than fromTo becasue is doesn't rely on consec samples
# #could do the same thing for other movements...
# wBAndOB <- ddply(dMData[!is.na(dMData$river),], .(tagNumberCH), function(x) {
#              any(x$river == c('WEST BROOK')) * any(x$river == c('WB OBEAR'))})
# wBAndOBTN <- wBAndOB[wBAndOB$V1 == T,1]
# 
# dMData$wBAndOB <- ifelse( dMData$tagNumberCH %in% wBAndOBTN, 1,0 )
# 

##################################

#######################################################
# Fill in river observations for missing occasions
# observations per individual in each river


maxRiver<-dMData[!is.na(riverN),
                 length(year),
                 by=list(tag,riverN)][,riverN[which.max(V1)],
                                      by=tag]
setnames(maxRiver,"V1","maxRiver")
setkey(maxRiver,tag)

# NOTE - this lags backwards
dMData <- addLagged(data = dMData, individual = "tag", time = "sampleNum", 
                    lag = c("riverN"), k=-1)
# Fills in most common river per tagNumber (r$maxRiver[ dMData$tagNumber ]) 
#  before first sample
# Keeps river for observed occasions and grabs lagged (backwards)
#  river for first uncaptured occasion
dMData[,riverN:=ifelse(sampleNum<first,riverN[which(!is.na(riverN))[1]],riverN),by=tag]

dMData[,riverN:=
         ifelse(length(unique(na.omit(riverN)))==1,
                unique(na.omit(riverN)),
                riverN),
       by=tag]




dMData[,riverConsec:=ifelse(sampleNum < first,
                            maxRiver[tag,
                                     maxRiver],
                            ifelse(!is.na(riverN),
                                   riverN,
                                   lagRiverN
                            )
)
]

#######Add lagged versions of key variables##############################
dMData <- addLagged(data = dMData, individual = "tag", time = "sampleNum", 
                    lag = c("sampleNum","riverN","length","date"))   
#########################################################################

###################I'm not sure what addEnvironmentalData2 does..
###################so I'll need to rewrite this after figuring that out, 
 #commenting out for now
#dateTime...' are the date cols in the pheno data
# (before <- Sys.time())
# dMData <- addEnvironmentalData2(
#   dMData, envData, lagDateTimeCol = "dateForEnv", 
#   dateTimeCol = "lagDateForEnv", 
#   dateCol = "date2", temperatureColumn = "temperature", 
#   dischargeColumn = "discharge" )
# (after <-Sys.time())     
# difftime(after,before)  
# 
# # fill in 'samples' before the study starts with means
# 
# envMeans <- aggregate((dMData[,c('intervalLength','maxT','meanT',
#                                  'medianT','minT','sdT','skewT',
#                                  'maxD','meanD','medianD','minD',
#                                  'sdD','skewD') ] ),
#              by=list( dMData$drainage,dMData$season ),FUN=mean, na.rm =TRUE )
# names(envMeans)[1:2] <- c('drainage','season')
# 
# ##############################################################################
# ##############################################################################
# #### Temporary fix to fill in mean data for augmented back rows ##############
# ##############################################################################
# dMData$seasonMeanIntLen<-envMeans$intervalLength[match(dMData$season,
#                                                        envMeans$season)]
# dMData$seasonMeanT <- envMeans$meanT[ match(dMData$season,envMeans$season) ]
# dMData$seasonMeanD<-envMeans$meanD[match(dMData$season,envMeans$season)]
# dMData$fullMeanIntLen<-dMData$intervalLength
# dMData$fullMeanT<-dMData$meanT
# dMData$fullMeanD<-dMData$meanD
# dMData$fullMeanIntLen[which(is.na(dMData$intervalLength))]<-
#     dMData$seasonMeanIntLen[which(is.na(dMData$intervalLength))]
# dMData$fullMeanT[which(is.na(dMData$meanT))]<-
#     dMData$seasonMeanT[which(is.na(dMData$meanT))]
# dMData$fullMeanD[which(is.na(dMData$meanD))]<-
#     dMData$seasonMeanD[which(is.na(dMData$meanT))]
# ##############################################################################
# ##############################################################################
# ##############################################################################
# ##############################################################################
# 
# dMData <- dMData[ order(dMData$tagNumber,dMData$sampleNum),]   
# 
# ##############################################################################
if(modelType=='js'){
  dMData[,sampleNumAdj:=sampleNum-min(sampleNum)+1]
}

nSamples <- dMData[,max(sampleNumAdj)]
samples <- dMData[,list(sampleNumAdj,sampleNum,season,year)]
setkey(samples,sampleNumAdj,sampleNum,season,year)
samples<-unique(samples)

nRivers<-dMData[!is.na(riverN),
                length(unique(riverN))]

########################################################################
if(modelType=='js'){
  
  knowns <- function(samplesInData,cohort,last){
    if(cohort<min(samples$year)){born<-2
    } else {
    born <- samples[year==cohort & season == 2, sampleNumAdj]
    }
    unknownAfter <- samples[sampleNum==last,sampleNumAdj]
    
    z<-rep(NA,max(samplesInData))
    z[samplesInData < born] <- 1 #sets state to not entered before birth
    z[samplesInData >= born & samplesInData <= unknownAfter] <- 2 #alive when known
    return(z)
  }
  
  dMData[,zKnown:=knowns(sampleNumAdj,unique(cohort),unique(last)),by=tag]
  dMData[sampleNumAdj==1,zKnown:=1] #everyone starts the study not entered
  
  d<-dMData[,list(sampleNumAdj,
                  riverN,
                  enc,
                  zKnown)]
  
  for(r in 1:nRivers){
    nExtras<-round(dMData[riverN==r,length(unique(tag))]*0.3)
    assign(paste0('aug',r),
           data.table(sampleNumAdj=rep(1:nSamples,nExtras)))
    get(paste0('aug',r))[,c("riverN","enc","zKnown"):=
                           list(r,    0,   as.numeric(NA))]
  }
  aug<-rbind(aug1,aug2,aug3,aug4)
  aug[sampleNumAdj==1,zKnown:=1]
  d<-rbind(d,aug)
  d[,enc:=abs(enc-2)]
  
}


if(modelType=='js'){
  firstObsRows<- d[,which(sampleNumAdj==1)]
  nFirstObsRows <- length( firstObsRows )
  lastObsRows <- c(firstObsRows[2:nFirstObsRows]-1,nrow(d))
  nLastObsRows <- length(lastObsRows)
  
  evalRows <- d[,which(sampleNumAdj!=1)]
  nEvalRows <- length(evalRows)
  
  evalJSRows <- dMData[,which(ageInSamples != lastAIS)]
  nEvalJSRows <- length(evalJSRows) 
}

if(modelType!='js'){
firstObsRows= dMData[,which(first==sampleNum)]
nFirstObsRows <- length( firstObsRows )
lastObsRows <- dMData[,which(is.na(lagDateForEnv))]
nLastObsRows <- length(lastObsRows)

evalRows <- dMData[,which(!is.na(lagDateForEnv) & sampleNum >= first)]
nEvalRows <- length(evalRows)

evalJSRows <- dMData[,which(ageInSamples != lastAIS)]
nEvalJSRows <- length(evalJSRows)   
}


############################################################

# variables for estimating pMat
summerObsRows <- dMData[,which(season == 2)]
nSummerObsRows <- length(summerObsRows)

nonSummerObsRows <- dMData[,which(season!=2)]
nNonSummerObsRows <- length(nonSummerObsRows)

occasions <-unique(dMData[,list(ageInSamples,season)])
summerAIS <- occasions$ageInSamples[occasions$season == 2]
nonSummerAIS <- occasions$ageInSamples[occasions$season != 2]

###################################################################

evalList <- list(firstObsRows      = firstObsRows,
                 nFirstObsRows     = nFirstObsRows,
                 lastObsRows       = lastObsRows,
                 nLastObsRows      = nLastObsRows,
                 evalRows          = evalRows,
                 nEvalRows         = nEvalRows,
                 evalJSRows        = evalJSRows,
                 nEvalJSRows       = nEvalJSRows,
                 summerObsRows     = summerObsRows,
                 nSummerObsRows    = nSummerObsRows,
                 nonSummerObsRows  = nonSummerObsRows, 
                 nNonSummerObsRows = nNonSummerObsRows,
                 summerAIS         = summerAIS,
                 nonSummerAIS      = nonSummerAIS,
                 nSamples          = nSamples,
                 samples           = samples,
                 nRivers           = nRivers)

######################################################################
# # means for standardizing
# lengthStd <- tapply(dMData$length,dMData$ageInSamples,mean, na.rm=TRUE)     
# # need to do this for the SR, so fish in AIS 10,13,14
# 
# stdList <-
#   list(
#     lengthStd = lengthStd
#     #tempStd = tapply(dMData$fullMeanT,dMData$season,mean),
#     #flowStd = tapply(dMData$fullMeanD,dMData$season,mean) 
#   )
# 
# # for cohort by ais means
# nAgeInSamples = length(unique(dMData$ageInSamples))
# nCohorts = length(unique(dMData$cohort))
# cohortIndex<-dMData$cohort
# 
# aisMeans <- dMData[,mean(length,na.rm=T),by=ageInSamples]
# setnames(aisMeans,"V1","aisMeanLength")
# aisCohortMeans <- dMData[,mean(length,na.rm=T),by=list(cohort,ageInSamples)]
# setnames(aisCohortMeans,"V1","aisCohortMeanLength")
# 
# 
# setkey(aisMeans,ageInSamples)
# setkey(aisCohortMeans,ageInSamples)
# aisCohortMeans<-aisMeans[aisCohortMeans]
# aisCohortMeans[is.na(aisCohortMeanLength),aisCohortMeanLength:=aisMeanLength]
# 
# 
# stdList_cohort <-
#   list(
#     lengthStd = aisCohortMeans
#     #tempStd = tapply(dMData$fullMeanT,dMData$season,mean),
#     #flowStd = tapply(dMData$fullMeanD,dMData$season,mean) 
#   )

##########################################################
# do counts for stdN in the growthSurvivalMove model. 
#need counts of fish from all cohorts for estimate of N
if(modelType=='someFutureType'){
scaleVec<-function(x,na.rm=F){(x-mean(x,na.rm=na.rm))/sd(x,na.rm=na.rm)}
nRivers <- length(unique(dMData[!is.na(river),river]))
nYears = max(dMData$year)-min(dMData$year)+1


count2 <-pheno2Long[enc == 1,
                    length(c(tag)),
                    by=list(seasonConsec,yearConsec,riverN)]
setnames(count2,"V1",'count')
count2[,riverN:=riverN+1]



uniqueSamples<-unique(count2[,list(seasonConsec,yearConsec)])
fillIn5 <- cbind(uniqueSamples[rep(1:nrow(uniqueSamples),nRivers+1)],
                 riverN=rep(1:(nRivers+1),each=nrow(uniqueSamples)),
                 key=c("riverN","yearConsec","seasonConsec")) 

setkey(count2,riverN,yearConsec,seasonConsec)
count2<-count2[fillIn5]
count2[,countAdj:=scaleVec(count,na.rm=T),by=list(seasonConsec,riverN)]
count2[is.na(count),count:=0]
#count2[riverN == 1, count:=as.numeric(rbinom(  sum((riverN == 1)),20,0.75 ))] 
# put in dummy numbers so don't get a sd of 0 calculated in bugs
count2[,year2:=yearConsec-min(yearConsec)+1]

countForN <- array(count2[,count],c(4,nYears,nRivers+1))
meanForN <- array(count2[,mean(count),
                         by=list(riverN,seasonConsec)]$V1,
                         c(4,nRivers+1)) #need to check indexing
sdForN <- array(count2[,sd(count),
                       by=list(riverN,seasonConsec)]$V1,c(4,nRivers+1)) 
#need to check indexing to see if it matches

sdForN[sdForN == 0 ]  <- 1 # so we don't divide by 0

statsForN <- list(
  countForN = countForN,
  meanForN  = meanForN,
  sdForN    = sdForN,
  nYears    = nYears,
  minYear   = min(count2$yearConsec),
  maxYear   = max(count2$yearConsec)
  
) 
}

##########################################################
##############################################################################
# output dMData  to species_DMData_river.RData
############################################################################## 
directory <- tempfile( pattern="output-", tmpdir =processedDir, fileext='-dMData')
dir.create(directory)
file.copy(from='./makeDMData.R', to=paste(directory,'makeDMData.R',sep='/'))
writeLines(text=directory, con='./latest_directory')
print(directory)

fileName <- paste('dMDataOut',
                   species,
                   subsetDMdataCohortMin,'_',
                   subsetDMdataCohortMax,
                   '.RData', sep='')

#save.image(paste(directory,fileName, sep=''))
if(modelType=='needToChangeThis'){
save(dMData, evalList, stdList, stdList_cohort, statsForN,
     file = paste(directory,fileName, sep='/'))
save(dMData,evalList,stdList,stdList_cohort,statsForN,
     file= file.path(processedDir,fileName))
print(str(dMData))
}

assign('dMData',dMData,envir=.GlobalEnv)
assign('evalList',evalList,envir=.GlobalEnv)
assign('d',d,envir=.GlobalEnv)
}
##############################################################################
##############################################################################
# end of data prep
##############################################################################
##############################################################################