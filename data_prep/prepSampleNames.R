#load raw file exported from access database
sampleNames<-fread(file.path(originalDir,"SampleNames.txt"))

#reduce columns and redundnant rows and rename stuff
sampleNames<-sampleNames[,c(2,5,6,10),with=F]
setnames(sampleNames,c("drainage","sample_name","sampleComplete","season"))
sampleNames[,drainage:=tolower(drainage)]
setkey(sampleNames,drainage,sample_name)
sampleNames<-sampleNames[!duplicated(sampleNames[,list(drainage,sample_name,season)])]
sampleNames[season=="Fall_Winter",season:="Fall"]



#save new version
saveRDS(sampleNames,file.path(processedDir,"sampleNames.rds"))