library(randomForest)
library(caret)
library(rpart)



load("matriz_td_treino_fator.Rdata")
matriz_td_df_fator
M = dim(matriz_td_df_fator)[2]
banco_treino = matriz_td_df_fator
class(banco_treino)
dim(banco_treino) #5634  103
names(banco_treino)
class(banco_treino[,1])
#o banco ja esta como fator

CT = rpart(ROTULO ~ .,data=banco_treino[,-M])
summary(CT)


set.seed(52145631)
RF100 = randomForest(ROTULO ~ .,
                  data=banco_treino[,-M],
                  importance=TRUE,
                  ntree=100)
print(RF)

save(RF100,file="RF100_obj.Rdata")
save(CT,file="CT_obj.Rdata")
