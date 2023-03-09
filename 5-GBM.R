#install.packages("gbm")
library("gbm")
library(caret)
library(pROC)



GBM100 <- gbm(as.character(ROTULO)~., 
                 data=banco_treino[,-M], 
                 distribution="bernoulli", 
                 n.trees=100,
                 shrinkage = 0.1,
                 verbose=FALSE)

GBM300 <- gbm(as.character(ROTULO)~., 
                     data=banco_treino[,-M], 
                     distribution="bernoulli", 
                     n.trees=300,
                     shrinkage = 0.1,
                     verbose=FALSE)

GBM500 <- gbm(as.character(ROTULO)~., 
              data=banco_treino[,-M], 
              distribution="bernoulli", 
              n.trees=500,
              shrinkage = 0.1,
              verbose=FALSE)


# Salvando modelo
save(GBM100,GBM300,GBM500, file="GBM_obj.RData")

