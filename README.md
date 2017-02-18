# Classification
##L8_Iro_2016=raster
##trainData=vector
##responseCol=field trainData
##nsamples=number 100
##output=output raster
install.packages(repos=NULL, choose.files(caption = "C://packages/rgdal/", multi= T))
library(sp)
library(raster)
library(caret)
library(rgdal)

L8_Iro_2016<-stack("L8_Iro_2016.tif")
names(L8_Iro_2016) <- c(paste0("B",1:3))
L8_Iro_2016_1<-L8_Iro_2016*(L8_Iro_2016>=0)
names(L8_Iro_2016_1)<-c(paste0("B",1:3))
dfAll = data.frame(matrix(vector(), 0, length(names(L8_Iro_2016)) + 1))

for (i in 1:length(unique(trainData[[responseCol1]]))){
  category <- unique(trainData[[responseCol1]])[i]
  categorymap <- trainData[trainData[[responseCol1]] == category,]
  dataSet <- extract(L8_Iro_2016, categorymap)
  dataSet <- lapply(dataSet, function(x){cbind(x, class = as.numeric(rep(category, nrow(x))))})
  df <- do.call("rbind", dataSet)
  dfAll <- rbind(dfAll, df)
}

sdfAll <- subset(dfAll[sample(1:nrow(dfAll), nsamples), ])

modFit_rf <- train(as.factor(class) ~ B1 + B2 + B3, method = "rf", data = sdfAll)

beginCluster()
preds_rf <- clusterR(L8_Iro_2016, raster::predict, args = list(model = modFit_rf))
endCluster()

output = preds_rf
