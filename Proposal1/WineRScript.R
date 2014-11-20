
wine <- read.csv("../Datasets/Wine/wine.data", header=FALSE)
colnames(wine) <- c("class","Alcohol","Malic Acid","Ash","Alcalinity of Ash","Magnesium","Total Phenols","Flavanoids","Nonflavanoid Phenols","Proanthocyanins","Color Intensity","Hue","0D280/OD315 of Diluted Wines","Proline")

# log transform 
log.wine <- log(wine[, 2:14])


# apply PCA - scale. = TRUE is highly 
# advisable, but default is FALSE. 
wine.pca <- princomp(log.wine,
                     center = TRUE,
                     scale. = TRUE,cor=TRUE,scores=TRUE) 

summary(wine.pca)
plot(wine.pca, type = "l")

plot3d(wine.pca$scores[,1:3], col=wine[,1])

library(rgl)
library(caret)
library(e1071)
library(caret)
library(MASS)

computeEuclideanDissimilarities <- function (sampMatrix,prototypesMatrix)
{      
        distances <- as.matrix(dist(rbind(sampMatrix,prototypesMatrix),method="euclidean"))
        elements <- nrow(sampMatrix)*nrow(prototypesMatrix)
        dissimMatrix<-distances[1:nrow(sampMatrix),(nrow(sampMatrix)+1):(nrow(sampMatrix)+nrow(prototypesMatrix))]
        return (dissimMatrix)
}


runAnalysis <- function(numPrototypes)
{
        print(c('Iterating with ',numPrototypes,' prototypes'))

         inTrain <- createDataPartition(wine$class, p=0.6, list=FALSE)
         trainingSet <- wine[inTrain,]
         testSet <- wine[-inTrain,]
                 
         svmfit=svm(class~., data=trainingSet, kernel="linear", cost=10,scale=FALSE)
         prototypes<-trainingSet[svmfit$index,]
 
         #prototypes<-prototypes[1:numPrototypes,]
         prototyp<-prototypes[1:numPrototypes,]
        trainSetDissimilarities <- computeEuclideanDissimilarities (trainingSet[,-1],prototyp[,-1])

        dissSpace<-as.data.frame(cbind(trainingSet$class,trainSetDissimilarities))
        colnames(dissSpace)[1]<-"class"
        qda.fit <-qda(class~.,data=dissSpace)
        
        testSetDissimilarities <- computeEuclideanDissimilarities (testSet[,-1],prototyp[,-1])
        testSetDissSpace <- as.data.frame(cbind(testSet$class,testSetDissimilarities))
        colnames(testSetDissSpace)<-colnames(dissSpace)
        qda.testpred <- predict(qda.fit, testSetDissSpace)
        print(table(qda.testpred$class,testSet$class))
        cf<-confusionMatrix(qda.testpred$class,testSet$class)
        acc <- cf$overall['Accuracy']
        print(acc)
        return(acc)
}
 
runAnalysisGaussianKernel <- function(numPrototypes)
{
        print(c('Iterating with ',numPrototypes,' prototypes'))
        
        inTrain <- createDataPartition(wine$class, p=0.6, list=FALSE)
        trainingSet <- wine[inTrain,]
        testSet <- wine[-inTrain,]
        
        svmfit=svm(class~., data=trainingSet, kernel="radial", cost=10,scale=FALSE)
        prototypes<-trainingSet[svmfit$index,]
        
        #prototypes<-prototypes[1:numPrototypes,]
        prototyp<-prototypes[1:numPrototypes,]
        trainSetDissimilarities <- computeEuclideanDissimilarities (trainingSet[,-1],prototyp[,-1])
        
        dissSpace<-as.data.frame(cbind(trainingSet$class,trainSetDissimilarities))
        colnames(dissSpace)[1]<-"class"
        qda.fit <-qda(class~.,data=dissSpace)
        
        testSetDissimilarities <- computeEuclideanDissimilarities (testSet[,-1],prototyp[,-1])
        testSetDissSpace <- as.data.frame(cbind(testSet$class,testSetDissimilarities))
        colnames(testSetDissSpace)<-colnames(dissSpace)
        qda.testpred <- predict(qda.fit, testSetDissSpace)
        print(table(qda.testpred$class,testSet$class))
        cf<-confusionMatrix(qda.testpred$class,testSet$class)
        acc <- cf$overall['Accuracy']
        print(acc)
        return(acc)
}

#         inTrain <- createDataPartition(wine$class, p=0.6, list=FALSE)
#         trainingSet <- wine[inTrain,]
#         testSet <- wine[-inTrain,]
        # We need to improve this to provide alpha = 0.6 per class, otherways qda does not work correctly                
         smp_size <- floor(0.60 * length(which(wine$class==1)))
         set.seed(123)
         inTrain1 <- sample(length(which(wine$class==1)),, size = smp_size)
         trainingSet1 <- wine[which(wine$class==1),][inTrain1,]
         testSet1 <- wine[-which(wine$class==1),][inTrain1,]

         smp_size <- floor(0.60 * length(which(wine$class==2)))
         set.seed(123)
        inTrain2 <- sample(length(which(wine$class==2)),, size = smp_size)
        trainingSet2 <- wine[which(wine$class==2),][inTrain2,]
        testSet2 <- wine[-which(wine$class==2),][inTrain2,]

        smp_size <- floor(0.60 * length(which(wine$class==3)))
        set.seed(123)
        inTrain3 <- sample(length(which(wine$class==3)),, size = smp_size)
        trainingSet3 <- wine[which(wine$class==3),][inTrain3,]
        testSet3 <- wine[-which(wine$class==3),][inTrain3,] 

        trainingSet <- rbind(trainingSet1,trainingSet2,trainingSet3)
        testSet <- rbind(testSet1,testSet2,testSet3)

         length(which(trainingSet$class==1))
         length(which(trainingSet$class==2))
         length(which(trainingSet$class==3))
                 
         svmfit=svm(class~., data=trainingSet, kernel="linear", cost=10,scale=FALSE)
         prototypes<-trainingSet[svmfit$index,]
 

        
protoRange <- 3:25
accuraciesLinear <- lapply(protoRange,runAnalysis)
plot(protoRange,1-as.numeric(accuracies),type="l",main='Classification error',ylab='Classification error')
accuraciesGaussian <- lapply(protoRange,runAnalysisGaussianKernel)

which.max(as.numeric(accuraciesLinear))
max(as.numeric(accuraciesLinear))
which.max(as.numeric(accuraciesGaussian))
max(as.numeric(accuraciesGaussian))