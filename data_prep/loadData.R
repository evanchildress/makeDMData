pheno<-data.table(dbGetQuery(link$conn,"SELECT * FROM data_trout WHERE tag != 'NA';"))
pheno[,sample_name:=as.numeric(sample_name)]

sampleNames<-readRDS(file.path(processedDir,"sampleNames.rds"))
sampleNames<-sampleNames[drainage=='west',list(sample_name,season)]
sampleNames[sample_name==89,season:="PostSmolt"]

setkey(pheno,sample_name)
setkey(sampleNames,sample_name)

pheno<-sampleNames[pheno]

assign('pheno',sampleNames[pheno],env=shared_data)

