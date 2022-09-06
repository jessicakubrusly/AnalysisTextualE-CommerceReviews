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
banco_treino_xgboost <- as.matrix(banco_treino[-ncol(banco_treino)])
label_xgboost <- as.matrix(banco_treino[ncol(banco_treino)])

# O treino do XGBOOST espera que tudo seja matriz numerica
storage.mode(banco_treino_xgboost) <- "numeric"
storage.mode(label_xgboost) <- "numeric"

# Treino GBM
# model_xgboost <- xgboost(data = banco_treino_xgboost, 
#                          label = label_xgboost,
#                          gamma = 0, 
#                          eta = 0.3, 
#                          objective = "binary:logistic", 
#                          verbose = FALSE, 
#                          nrounds=100)

model_xgboost_100 <- xgboost(data = banco_treino_xgboost, 
                         label = label_xgboost,
                         gamma = 0, 
                         eta = 0.3, 
                         objective = "binary:logistic", 
                         verbose = FALSE, 
                         nrounds=100)

model_xgboost_300 <- xgboost(data = banco_treino_xgboost, 
                             label = label_xgboost,
                             gamma = 0, 
                             eta = 0.3, 
                             objective = "binary:logistic", 
                             verbose = FALSE, 
                             nrounds=300)


# Salvando modelo
save(model_xgboost_100, file="XGBOOST100_obj.RData")
save(model_xgboost_300, file="XGBOOST300_obj.RData")





# #variaveis importantes
# importance_matrix = xgb.importance(
#   colnames(banco_treino_xgboost), model = model_xgboost)
# 
# y = importance_matrix[1:10,1][[1]]
# x = importance_matrix[1:10,2][[1]]
# windows(50,100)
# par(mar=c(10,10,4,2))
# plot(x=x,y=10:1,cex=2,cex.lab=2,
#      axes=F,xlab = "Gain",ylab = "")
# box()
# abline(h = 0:10, col = "lightgray", lty = 3)
# #axis(1,at = 2:11,cex.axis=2)
# axis(2,at = 10:1, labels = y,las=1,cex.axis=2)
# 
# 
# pred_xgboost <- predict(model_xgboost, banco_treino_xgboost) 
# roc_xgboost <- roc(banco_treino$ROTULO, pred_xgboost)
# xgboost_threshold <- coords(roc_xgboost, "best", ret = "threshold")
# plot(roc_xgboost)
# pred_xgboost_classe <- ifelse(pred_xgboost < rep(xgboost_threshold,length(pred_xgboost)), 0, 1)
# tab <- table(pred_xgboost_classe,banco_treino$ROTULO)
# CM_xgboost = confusionMatrix(tab)
# 
# 
# # Carrega dados de teste
# load("matriz_td_teste_fator.Rdata")
# 
# # Remove variaveis que estao no banco de teste e nao no de treino
# rm_vars <- c()
# for(i in 1:ncol(banco_teste)){
#   if(!(colnames(banco_teste)[i] %in% colnames(banco_treino))){
#     rm_vars <- c(rm_vars, i)
#   }
# }
# # Colunas removidas
# # colnames(banco_teste)[rm_vars]
# banco_teste <- banco_teste[-rm_vars]
# 
# # Transformando em matrizes numericas os dados de teste (variavel-resposta separada)
# banco_teste_xgboost <- as.matrix(banco_teste[-ncol(banco_teste)])
# label_teste_xgboost <- as.matrix(banco_teste[ncol(banco_teste)])
# 
# # XGBOOST espera que seja matriz numerica
# storage.mode(banco_teste_xgboost) <- "numeric"
# storage.mode(label_teste_xgboost) <- "numeric"
# 
# # Previsao
# pred_xgboost <- predict(model_xgboost, banco_treino_xgboost) 
# 
# # Ponto de corte da classificacao = limite da curva ROC
# roc_xgboost <- roc(banco_treino$ROTULO, pred_xgboost)
# plot(roc_xgboost)
# xgboost_threshold <- coords(roc_xgboost, "best", ret = "threshold")
# 
# # Classificando
# pred_xgboost_classe <- ifelse(pred_xgboost < rep(xgboost_threshold,length(pred_xgboost)), 0, 1)
# 
# 
# # Matriz de confusao
# 
# confusionMatrix(as.factor(pred_xgboost_classe), as.factor(banco_treino$ROTULO))


