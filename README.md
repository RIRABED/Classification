##L8_Iro_2016=raster
##trainData=vector
##responseCol=field trainData
##nsamples=number 100
##output=output raster
##install.packages(repos=NULL, choose.files(caption = "C://packages/rgdal/", multi= T))
##install.packages("e1071"): pas besoin de le charger dans la liste de librairie
##install.packages("snow"):pas besoin de le charger dans la liste de librairie
library(sp)
library(raster)
library(caret)
library(rgdal)
library(randomForest)
L8_Iro_2016<-stack("L8_Iro_2016.tif")
names(L8_Iro_2016) <- c(paste0("B",1:3))
L8_Iro_2016_1<-L8_Iro_2016*(L8_Iro_2016>=0)
names(L8_Iro_2016_1)<-c(paste0("B",1:3))
#writeRaster(L8_Iro_2016_1, filename="L8_Iro_2016.tif",overwrite=TRUE)
plotRGB(L8_Iro_2016_1, r=1,g=2, b=3, stretch="hist",scale=10000)
trainData<-shapefile("C://Séminaire_R/Classification_randomForest/training9.shp")
reponseCol1<-"CLASS_ID"
trueData<-shapefile("C://Séminaire_R/Classification_randomForest/trainTrue9.shp")
reponseCol2<-"CLASS_ID"

dfAll = data.frame(matrix(vector(), 0, length(names(L8_Iro_2016_1)) + 1))
for (i in 1:length(unique(trainData[[reponseCol1]]))){
  category <- unique(trainData[[reponseCol1]])[i]
  categorymap <- trainData[trainData[[reponseCol1]] == category,]
  dataSet <- extract(L8_Iro_2016_1, categorymap)
  dataSet <- lapply(dataSet, function(x){cbind(x, class = as.numeric(rep(category, nrow(x))))})
  df <- do.call("rbind", dataSet)
  dfAll <- rbind(dfAll, df)
}

nsamples = 100
sdfAll <- subset(dfAll[sample(1:nrow(dfAll), nsamples), ])

modFit_rf <- train(as.factor(class) ~ B1 + B2 + B3, method = "rf", data = dfAll)

beginCluster()
preds_rf <- clusterR(L8_Iro_2016_1, raster::predict, args = list(model = modFit_rf))
endCluster()

output = preds_rf


dfAll1 = data.frame(matrix(vector(), 0, length(names(L8_Iro_2016_1)) + 1))
for (i in 1:length(unique(trueData[[reponseCol2]]))){
  category <- unique(trueData[[reponseCol2]])[i]
  categorymap <- trainData[trueData[[reponseCol2]] == category,]
  dataSet <- extract(L8_Iro_2016_1, categorymap)
  dataSet <- lapply(dataSet, function(x){cbind(x, class = as.numeric(rep(category, nrow(x))))})
  df <- do.call("rbind", dataSet)
  dfAll1 <- rbind(dfAll1, df)
}

predicted <- predict(modFit_rf, dfAll1)
confusionMatrix(predicted, dfAll1$CLASS_ID)
