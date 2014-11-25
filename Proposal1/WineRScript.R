#Read the dataset
wine <- read.csv("../Datasets/Wine/wine.data", header=FALSE)
colnames(wine) <- c("class","Alcohol","Malic Acid","Ash","Alcalinity of Ash","Magnesium","Total Phenols","Flavanoids","Nonflavanoid Phenols","Proanthocyanins","Color Intensity","Hue","0D280/OD315 of Diluted Wines","Proline")
#Load required libraries
#Libraries
library(rgl)
library(caret)
library(e1071)
library(caret)
library(MASS)


# Log transform, just for plotting with PCA
log.wine <- log(wine[, 2:14])
# apply PCA - scale. = TRUE is highly 
# advisable, but default is FALSE. 
wine.pca <- princomp(log.wine,
                     center = TRUE,
                     scale. = TRUE,cor=TRUE,scores=TRUE) 
summary(wine.pca)
plot(wine.pca, type = "l")
plot3d(wine.pca$scores[,1:3], col=wine[,1])

#Compute distance between samples and each prototype
computeEuclideanDissimilarities <- function (sampMatrix,prototypesMatrix)
{      
        distances <- as.matrix(dist(rbind(sampMatrix,prototypesMatrix),method="euclidean"))
        elements <- nrow(sampMatrix)*nrow(prototypesMatrix)
        dissimMatrix<-distances[1:nrow(sampMatrix),(nrow(sampMatrix)+1):(nrow(sampMatrix)+nrow(prototypesMatrix))]
        return (dissimMatrix)
}

#Run analysis, basically:
#Take the train and test set (they are generated outside this function)
#Fit an SVM
#Choose the prototypes
#Compute dissimilarity matrix
#Fit QDA with dissimilarity matrix
#Compute accuracy on the test set
runAnalysis <- function(numPrototypes,useSV)
{
         print(c('Iterating with ',numPrototypes,' prototypes'))

         svmfit <- tune.svm(class~.,data=trainingSet)
         svmfit$best.model
        if (useSV == TRUE)
        { 
                #Here we use the support vectors as prototypes
                print ("Using support vectors")
                prototypes <- svmfit$best.model$ind
        }
        else
        {       #Here we use the non-support vectors as prototypes
                print ("Using non-support vectors as prototypes")
                allSamples<- 1:nrow(wine)
                prototypes <- allSamples[-svmfit$best.model$ind]

        }

        prototypes<-prototypes[1:numPrototypes]
        prototyp<-trainingSet[prototypes,]
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
 

        #All this code is used to achieve a 60% train/test per class
         smp_size <- floor(0.60 * length(which(wine$class==1)))
         set.seed(123)
         inTrain1 <- sample(length(which(wine$class==1)),, size = smp_size)
         trainingSet1 <- wine[which(wine$class==1),][inTrain1,]
         testSet1 <- wine[which(wine$class==1),][-inTrain1,]

         smp_size <- floor(0.60 * length(which(wine$class==2)))
         set.seed(123)
        inTrain2 <- sample(length(which(wine$class==2)),, size = smp_size)
        trainingSet2 <- wine[which(wine$class==2),][inTrain2,]
        testSet2 <- wine[which(wine$class==2),][-inTrain2,]

        smp_size <- floor(0.60 * length(which(wine$class==3)))
        set.seed(123)
        inTrain3 <- sample(length(which(wine$class==3)),, size = smp_size)
        trainingSet3 <- wine[which(wine$class==3),][inTrain3,]
        testSet3 <- wine[which(wine$class==3),][-inTrain3,] 
        #Now we have train/test sets with 60% train/test per class
        trainingSet <- rbind(trainingSet1,trainingSet2,trainingSet3)
        testSet <- rbind(testSet1,testSet2,testSet3)
        #We print the number of samples per class just to be sure it works ok
        length(which(trainingSet$class==1))
        length(which(testSet$class==1))

        length(which(trainingSet$class==2))
        length(which(testSet$class==2))
        
        length(which(trainingSet$class==3))
        length(which(testSet$class==3))
 
#Now we will run the analysis for different number of prototypes
#Using the support vectors as prototypes and the non-support vectors as prototypes
#Finally plot the classification errors obtained for both cases
protoRange <- 3:27
accuraciesNonSV <- lapply(protoRange,runAnalysis, useSV=FALSE)
plot(protoRange,100*(1-as.numeric(accuraciesNonSV)),type="l",col="blue",main='Classification error',ylab='Classification error',xlab='Number of prototypes')
max(as.numeric(accuraciesNonSV))

accuraciesSV <- lapply(protoRange,runAnalysis,useSV=TRUE)
lines(protoRange,100*(1-as.numeric(accuraciesSV)),type="l",col="red",main='Classification error',ylab='Classification error',xlab='Number of prototypes')
legend("topright", legend =c("Non support vectors as prototypes","Support Vectors as prototypes"), col=c("blue","red"),pch=1)
max(as.numeric(accuraciesSV))

