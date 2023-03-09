library(randomForest)
library(caret)
library(rpart)



load("matriz_td_treino_fator.Rdata")
M = dim(matriz_td_df_fator)[2]
banco_treino = matriz_td_df_fator
dim(banco_treino) #5618  102

CT = rpart(ROTULO ~ .,data=banco_treino[,-M])
save(CT,file="CT_obj.Rdata")

set.seed(52145631)
RF100 = randomForest(ROTULO ~ .,
                  data=banco_treino[,-M],
                  importance=TRUE,
                  ntree=100)

RF300 = randomForest(ROTULO ~ .,
                     data=banco_treino[,-M],
                     importance=TRUE,
                     ntree=300)

RF500 = randomForest(ROTULO ~ .,
                     data=banco_treino[,-M],
                     importance=TRUE,
                     ntree=500)

save(RF100,RF300,RF500,file="RF_obj.Rdata")

