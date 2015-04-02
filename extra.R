

stop('Done') # stop when this file is 'sourced'

# data checks
table(pheno2$river)
length(unique(pheno2$tagNumber))

table(pheno2Long$river[pheno2Long$enc==1])
table(pheno2Long$river[pheno2Long$available==1])
length(unique(pheno2Long$tagNumber))

table(dMData$river[dMData$enc==1])
length(unique(dMData$tagNumber))

table(dMData$river[dMData$enc==1],dMData$ageInSamples[dMData$enc==1])
ftable(dMData$river[dMData$enc==1],
       dMData$cohort[dMData$enc==1],
       dMData$ageInSamples[dMData$enc==1]
)


table(dMData$river[dMData$enc==1],
      dMData$sampleNum[dMData$enc==1]
)


table(dMData$ageInSamples[dMData$enc==1],
      dMData$cohort[dMData$enc==1]      
)

tapply(dMData$length[dMData$enc==1],
       list(dMData$ageInSamples[dMData$enc==1],
            dMData$cohort[dMData$enc==1]),
       function(x){mean(x,na.rm=TRUE)})




################################################################################
# below sets up design matrix - not using for now because it's slow- may return to....
################################################################################

# set intercept levels
#dMData$river <- relevel(dMData$river, ref='WEST')
#dMData$season <- relevel(dMData$season, ref='3')
#dMData$age <- relevel(dMData$age, ref='1')

# get indices for random effects
dMData$fullRandom <- paste(dMData$drainage,dMData$season, dMData$age, dMData$cohort,sep=":")
dMData$fullRandomIndex <- as.numeric(as.factor(dMData$fullRandom))

randomIndex <- unique(cbind(dMData$fullRandomIndex,dMData$drainage,dMData$season, dMData$age, dMData$cohort))
randomIndex <- randomIndex[order(randomIndex[,1]),]

#if put length in dM, it'll exclude all the unobserved length rows
dM1 <- model.matrix(enc ~ ( season * age )^2 , dMData )

dM <- cbind(dM1,dMData$fullRandomIndex)
dimnames(dM)[[2]][ncol(dM)] <- "randomIndex"

#exclude colSums ==0
dM <- dM[ , colSums(dM) > 0 ]

#get length vector for input - this is ordered same as dM
lengthIn <- as.numeric(dMData$length)
#replace NAs with 0s. this will mult sizeBetas by 0 when not observed
lengthIn[ is.na( lengthIn )] <- 0

table(dMData$age)
