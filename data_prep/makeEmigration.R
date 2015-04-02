load(file.path(processedDir,"dMDataOutbkt2002_2014.rDATA"))

boundaryDetections<-data.table(dbGetQuery(link$conn,"SELECT * FROM data_boundary_detections;"))
boundaryDetections<-boundaryDetections[,list(lastBoundaryDetection=max(detection_date)),by=tag]
setkey(boundaryDetections,tag)

dMData[,lastRecap:=date[which(last==sampleNum)],by=tag]
dMData<-boundaryDetections[dMData]
dMData[is.na(lastBoundaryDetection) | 
         lastRecap>lastBoundaryDetection |
         medianDateSampleNum > lastBoundaryDetection,emPerm:=0]
dMData[is.na(emPerm),
       emPerm:=ifelse(sampleNum==max(sampleNum),1,0),by=tag]
