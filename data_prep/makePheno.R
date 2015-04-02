## as of 11/6/2013
# file location that was on felek got deleted during a switch
# saving this on git now.
# have not updated directories and file sources etc..







###

##########################################################################
# File location on felek: /var/lib/data/share
#
# Files to update on felek before running this file:
#
# fourRiversAllSpp.txt
# file created using access file query  fourRiversAllSpp:fourRivers query
# in C:\PITTAGMAIN\CMR Analyses\Hierach_Bugs\allSpp\fourRiversAllSpp.mdb
# export query as text file without formatting, include names on first row
# copy fourRiversAllSpp.txt to felek (home/ben/allSpp)

# adjSampNums.csv 
# right now, median dates are based on WB only. probably should incude tribs in dates
#      make sure to udpate alignedDates.csv and medianSampleDates.csv in C:\PITTAGMAIN\CMR Analyses\adjustedDates
#        1) update by copying new sample rows from fourRiversAllSpp:Median Date for Each Sample Table and pasting into medianSampleDates.csv
#           will need to copy and paste and delete the river columns before pasting
#        2) rerun sampleDates.r 
#        3) copy adjSampNums.csv to felek (home/ben/allSpp/adjustedDates)

# envData.txt 
# file created using access file query  fourRiversAllSpp:envData query
# in C:\PITTAGMAIN\CMR Analyses\Hierach_Bugs\allSpp
# export query as text file without formatting, include names on first row
# copy to felek home/ben/allSpp

# smolttrapFourRiversAllSpp.txt
# file created using access file query  fourRiversAts:smoltTrapfourRiversAts query
# in C:\PITTAGMAIN\CMR Analyses\Hierach_Bugs\allSpp
#export query as text file without formatting

# antennaTribs.txt
# in C:\PITTAGMAIN\CMR Analyses\Hierach_Bugs\allSpp fourRiversAllSpp:antennaTribs query
# export query as text file without formatting
# then copy to felek home/ben/allSpp

# antennaDuda.txt
# file created using access file query  fourRiversAts:antennaDuda query
# in C:\PITTAGMAIN\CMR Analyses\Hierach_Bugs\allSpp
#export query as text file without formatting
# then copy to felek home/ben/allSpp

# yoySizeLimits
# file made with query of yoy size limits
# file created using access file query  fourRiversAllSpp:stockYearBySizeClasses query
# in C:\PITTAGMAIN\CMR Analyses\Hierach_Bugs\allSpp\fourRiversAllSpp.mdb
# run query [stockYearBySizeClasses], export and copy to
#'/data/projects/jwpcenter/bayesian/allSpp/stockYearBySizeClasses.txt'
# update any visual fixes to the size limits in callMSRiverP.R on Denver computer

# unsampled rivers
# update propSampledDATA and zeroSectionsDATA in callMSRiverP.R for unsampled rivers/years

#
#############################################################################
#############################################################################
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
library(lattice)
library(latticeExtra)
library(ggplot2)
#library(reshape)
library(moments)
#library(multicore) #linux only
library(date) #to get month/day from julian date using date.mdy

# data subsetting
#############################################################################

#choose a species
#speciesSubset <- 'ATS' 
speciesSubset <- 'bkt'

#############################################################################
if (speciesSubset == 'ats'  ) {
  riverSubset <- tolower(c('WEST BROOK'))#,'WB JIMMY') #,'WB MITCHELL',"WB OBEAR") 
  #riverSubset <- c("SHOREY BROOK") # "SAWMILL RIVER","WB JIMMY","WB OBEAR", "WB MITCHELL", "CATAMARAN BROOK", 
  #riverSubset <- c("SAWMILL RIVER")
  areaSubset <- tolower(c('INSIDE', 'ABOVE', 'BELOW'))#, 'TRIB' ) 
  
  # subset variables for dMData
  # if include fish from cohorts < 1997, they will have ageInSamples that
  #don't go back to 1. for now we are leaving out cohort < 1997
  #when we want to include them, we'll need to augment back to ageInsamples 1
  #bay adding in negative smample numbers
  subsetDMdataCohortMin <- 1996 # >=
  subsetDMdataCohortMax <- 2050 # <=
  subsetDMdataAgeInSamples <- 15 # <  
  
  # exclude fish that were captured for the first time after the following ageInSamples
  # set equal to subsetDMdataAgeInSamples - 1 to have no effect
  maxAgeInSamplesFirstCapt <- 4 #subsetDMdataAgeInSamples - 1 #4  
}

if (speciesSubset == 'bkt' | speciesSubset == 'bnt'  ) {
  riverSubset <- tolower(c('WEST BROOK','WB JIMMY','WB MITCHELL',"WB OBEAR")) 
  areaSubset <- tolower(c('INSIDE', 'ABOVE', 'BELOW', 'TRIB','ABOVE ABOVE','aboveabove','BELOW BELOW' )) 
  
  subsetDMdataCohortMin <- 2002 # >=
  subsetDMdataCohortMax <- 2014 # <=
  
  subsetDMdataAgeInSamples <- 15 # <  
  
  maxAgeInSamplesFirstCapt <- subsetDMdataAgeInSamples - 1 #4  
}

# columns to include in  pheno2LongList
pheno2LongList <- c('tag','sample_name','sampleNumAdjConsec',
                    'river','cohort','species',
                    'season','age','measured_length','measured_weight',
                    'section',
                    'sex','mature01','everMat01',
                    'date','medianDate','area', 'riverN'
)
# columns to include in  pheno2LongList - need to update 'names' on next line
dMDataList <-  c('tag','sample_name','sampleNumAdjConsec','measured_length','measured_weight','mature01','section',
                 'enc', 'river', 'speciesConsec',
                 'seasonConsec','ageConsec','ageInSamplesConsec',
                 'yearConsec',
                 'gtFirstOcc','firstConsec','lastConsec','cohortConsec',
                 'date','medianDate',
                 'everMat01Consec','area', 'riverN','sex'
)

dMDataNames <- c('tag','sampleName','sampleNum','length','weight','mature01','section','enc', 'river',
                 'species','season','age','ageInSamples',
                 'year',
                 'gtFirstOcc','first','last','cohort',
                 'date','medianDate',
                 'everMat01','area', 'riverN','sex'
)                  

#############################################################################

#setwd("/var/lib/data/share")
#setwd('C:/PITTAGMAIN/CMR Analyses/Hierach_Bugs/allSpp')

#pheno <- as.data.frame(read.csv(file="fourRiversAllSpp.txt", header=TRUE))                                         
# file created using access query fourRiversAllSpp 
# in C:\PITTAGMAIN\CMR Analyses\Hierach_Bugs\allSpp\fourRiverAllSpp.accdb
# export query as text file without formatting, include names on first row
# then copy to felek in '/home/ben/allSpp/' and 'var/lib/data/share'

# NOW IN ECOPIFFLE, so commented out:
#source('./addLaggedForward.r')     #includes addEnvironmentalData2 (w/ skew)

pheno <- pheno[season!="Summer"]    #get rid of the two summer obs
pheno[,season:=as.numeric(factor(pheno$season,levels=c("PreSmolt","PostSmolt","Fall","PreWinter"), ordered=T))]
#MAKE SURE there are only 4 seasons - otherwise numbering gets screwed up  when merging on season

pheno[,medianDate:=median(date),by=sample_name]

pheno[,year:=year(date)]
pheno[,julian:=as.numeric(format(date,"%j"))]
pheno[,age:=year-cohort]
pheno[,daysOld:=julian + age * 365]

pheno[,mature01 := ifelse(maturity %in% c('p','m','f'),1,0)]
pheno[,everMat01:=max(mature01),by=tag]

pheno$riverN<-as.numeric(factor(pheno$river))  #Jimmy 1, Mitchell 2, WB 3

#rename wB sample 41.8 to 42. 42 was a sampleComple==NO, but there are equivalent samples in the other rivers
pheno[sample_name == 41.8,sample_name:=42 ]

# get rid of duplicate tags on the same occasion
pheno <- pheno[ !duplicated(pheno[,list(tag,sample_name)]), ]

##########################
# End of initial data prep 
##########################