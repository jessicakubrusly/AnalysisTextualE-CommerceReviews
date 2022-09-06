#install.packages("gbm")
library("gbm")
library(caret)
library(pROC)



model_gbm_100 <- gbm(as.character(ROTULO)~., 
                 data=banco_treino[,-M], 
                 distribution="bernoulli", 
                 n.trees=100,
                 shrinkage = 0.1,
                 verbose=FALSE)

model_gbm_300 <- gbm(as.character(ROTULO)~., 
                     data=banco_treino[,-M], 
                     distribution="bernoulli", 
                     n.trees=300,
                     shrinkage = 0.1,
                     verbose=FALSE)

# Salvando modelo
save(model_gbm_100, file="GBM100_obj.RData")
save(model_gbm_300, file="GBM300_obj.RData")
