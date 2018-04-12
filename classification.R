# Load required packages
library(rgdal)
library(raster)
library(caret)

# Load the Landsat image and the training data
img=brick(file.choose())
names(img) <- c(paste0("B", 1:7, coll = ""))  
head(img)
tail(img)

trainData = shapefile(file.choose())
trainData <- spTransform(trainData, crs(img))
responseCol <- "class"


# Extract training data values from the image bands

dfAll = data.frame(matrix(vector(), nrow = 0, ncol = length(names(img)) + 1))
for (i in 1:length(unique(trainData[[responseCol]]))){
  category <- unique(trainData[[responseCol]])[i]
  categorymap <- trainData[trainData[[responseCol]] == category,]
  dataSet <- extract(img, categorymap)
  
  if(is(trainData, "SpatialPointsDataFrame")){
    dataSet <- cbind(dataSet, class = as.numeric(category))
    dfAll <- rbind(dfAll, dataSet)
  }
  if(is(trainData, "SpatialPolygonsDataFrame")){
    dataSet <- lapply(dataSet, function(x){cbind(x, class = as.numeric(rep(category, nrow(x))))})
    df <- do.call("rbind", dataSet)
    dfAll <- rbind(dfAll, df)
  }
}

# Create training and testing datasets

inTrain <- createDataPartition(y = dfAll$class, p = 0.7, list = FALSE)
training <- dfAll[inTrain,]
testing <- dfAll[-inTrain,]


# balance training dataset
undersample_ds <- function(x, classCol, nsamples_class){
  for (i in 1:length(unique(x[, classCol]))){
    class.i <- unique(x[, classCol])[i]
    if((sum(x[, classCol] == class.i) - nsamples_class) != 0){
      x <- x[-sample(which(x[, classCol] == class.i), 
                     sum(x[, classCol] == class.i) - nsamples_class), ]
    }
  }
  return(x)
}
nsamples_class <- 600
training_bc <- undersample_ds(training, "class", nsamples_class)


# Random Forests model
set.seed(123)
mod.rf <- train(as.factor(class) ~ B1+ B2+ B3 + B4 + B5+ B7, method = "rf", data = training)
pred.rf <- predict(mod.rf, testing)

beginCluster()
preds_rf <- clusterR(img, raster::predict, args = list(model = mod.rf))
endCluster()
plot(preds_rf)
colors <- c("red", "black", "blue", "black")
plot(preds_rf, col = colors, legend = FALSE, axes = FALSE, box = F)

#save the class image
writeRaster(preds_rf, "class_lan5_RF.tif", overwrite=TRUE)

# SVM model
set.seed(123)
mod.svm <- train(as.factor(class) ~ B1+ B2+ B3 + B4 + B5+ B7, method = "svmRadial", data = training)
pred.svm <- predict(mod.svm, testing)

beginCluster()
preds_svm <- clusterR(img, raster::predict, args = list(model = mod.svm))
endCluster()


colors <- c("red", "black", "blue", "black")
plot(preds_svm, col = colors, legend = FALSE, axes = FALSE, box = F)
legend("bottomright", legend=c("Built Up", "Forest", "Water", "Soil"), 
       fill=colors, bg="white")

#save the class image
writeRaster(preds_rf, "class_lan5_svm.tif", overwrite=TRUE)

# RF model accuracy
OA_RF = confusionMatrix(pred.rf, testing$class)$overall[1]
kappa_Rf = confusionMatrix(pred.rf, testing$class)$overall[2]
# SVM model accuracy
OA_svm =  confusionMatrix(pred.svm, testing$class)$overall[1]
kappa_svm =  confusionMatrix(pred.svm, testing$class)$overall[2]

accuracy <- rbind(OA_RF, OA_svm)
row.names(accuracy) <- c("RF", "SVM")
overall_acc=(round(accuracy, 4))
write.table(overall_acc, "OA.txt")

kappa = rbind(kappa_Rf, kappa_svm)
row.names(kappa) <- c("RF", "SVM")
Cohen_kappa=(round(kappa, 4))
write.table(Cohen_kappa, "Cohen_kappa.txt")





