#install.packages("xgboost")
library(xgboost)
library(caret)
library(pROC)

load("matriz_td_treino_fator.Rdata")
matriz_td_df <- matriz_td_df_fator

# Ultima coluna apenas para referencia
M <- ncol(matriz_td_df_fator)

# Remocao da coluna de referencia para treino
banco_treino <- matriz_td_df_fator[-M]

# Bancos devem ser em formato de matriz para o XGBOOST e com a variavel-resposta 
# separada das covariaveis
banco_treino_xgboost <- as.matrix(banco_treino[,-(ncol(banco_treino))])
label_xgboost <- as.matrix(banco_treino[,ncol(banco_treino)])

# O treino do XGBOOST espera que tudo seja matriz numerica
storage.mode(banco_treino_xgboost) <- "numeric"
storage.mode(label_xgboost) <- "numeric"

XGBoost_100 = xgboost::xgboost(data  = banco_treino_xgboost, 
                               label = label_xgboost, 
                               gamma = 0, 
                               eta   = 0.3, 
                               objective = "binary:logistic", 
                               verbose   = FALSE, 
                               nrounds   = 100)

XGBoost_300 <- xgboost::xgboost(data = banco_treino_xgboost, 
                             label = label_xgboost,
                             gamma = 0, 
                             eta = 0.3, 
                             objective = "binary:logistic", 
                             verbose = FALSE, 
                             nrounds=300)

XGBoost_500 <- xgboost::xgboost(data = banco_treino_xgboost, 
                       label = label_xgboost,
                       gamma = 0, 
                       eta = 0.3, 
                       objective = "binary:logistic", 
                       verbose = FALSE, 
                       nrounds=500)

# Salvando modelo
save(XGBoost_100,XGBoost_300,XGBoost_500, file="XGBOOST_obj.RData")
