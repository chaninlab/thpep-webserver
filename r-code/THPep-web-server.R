#######set directory
setwd("~/Downloads/THP")
#######Load package
library(protr)
library(seqinr)
library(randomForest)

#######Building RF model
x <- read.fasta('main.fasta', seqtype="AA", as.string = TRUE)
D = read.csv("Label.csv", header = TRUE) 
m = length(x)
aac <- t(sapply(x, extractAAC))
training = data.frame(aac, Class = D[,ncol(D)])
Model = randomForest(Class ~ ., training, ntree= 400,mtry =4)

#######Feature extraction
xtest <- read.fasta('Example file.fasta', seqtype="AA", as.string = TRUE)###read data
xtest2 <- xtest[(sapply(xtest, protcheck))]###check special symbol
aactest <- t(sapply(xtest2, extractAAC))
data <- data.frame(aactest)

#######Predicting unknown sequences
data.frame(Prediction= predict(Model,data))




saveRDS(Model, "Model.rds")
