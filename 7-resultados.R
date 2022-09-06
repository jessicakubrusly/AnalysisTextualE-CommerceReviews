library(tidyverse)
library(graphics)
library(RColorBrewer)
library(yarrr)
library(randomForest)
library(caret)
library(rpart)
library(pROC)
library("gbm")
library(xgboost)
library("rattle") #para plotar arvores
library(rpart.plot)


load("CT_obj.Rdata")
load("RF_obj.Rdata")
load("RF100_obj.Rdata")
load("RF300_obj.Rdata")
load("GBM_obj.RData")
load("GBM100_obj.RData")
load("GBM300_obj.RData")
load("XGBOOST_obj.RData")
load("XGBOOST100_obj.RData")
load("XGBOOST300_obj.RData")

google.cols <- piratepal(palette = "google", 
                         trans = 0.6)
minhas_cores = c("#3D79F3CC", #blue
                 "#E6352FFF", #red0
                 "#E6352FB2", #red.3
                 "#E6352F66", #red.6
                 "#F9B90AFF", #yellow0
                 "#F9B90AB2", #yellow.3
                 "#F9B90A66" , #yellow.6
                 "#34A74BFF", #green0
                 "#34A74BB2",#green.3
                 "#34A74B66") #green.6
                 
                 
                 
                 

#variaveis importantes
#windows(190,100)
#fancyRpartPlot(CT,cex=0.6)
#rpart.plot(CT,cex=1.2)

#10 variaveis mais importantes
x0 = CT$variable.importance[1:10]
y0 = names(x0)

IMP=sort(RF$importance[,3])
x1_500 = IMP[(length(IMP)):(length(IMP)-9)]
y1_500 = names(x1_500)

IMP=sort(RF100$importance[,3])
x1_100 = IMP[(length(IMP)):(length(IMP)-9)]
y1_100 = names(x1_100)

IMP=sort(RF300$importance[,3])
x1_300 = IMP[(length(IMP)):(length(IMP)-9)]
y1_300 = names(x1_300)


x2_500 = summary.gbm(model_gbm)$rel.inf[1:10]
y2_500 = summary.gbm(model_gbm)$var[1:10]

x2_300 = summary.gbm(model_gbm_300)$rel.inf[1:10]
y2_300 = summary.gbm(model_gbm_300)$var[1:10]

x2_100 = summary.gbm(model_gbm_100)$rel.inf[1:10]
y2_100 = summary.gbm(model_gbm_100)$var[1:10]


# O treino do XGBOOST espera que tudo seja matriz numerica
importance_matrix = xgb.importance(
        model_xgboost$feature_names, model = model_xgboost)
y3_500 = importance_matrix[1:10,1][[1]]
x3_500 = importance_matrix[1:10,2][[1]]

importance_matrix = xgb.importance(
        model_xgboost_100$feature_names, model = model_xgboost)
y3_100 = importance_matrix[1:10,1][[1]]
x3_100 = importance_matrix[1:10,2][[1]]

importance_matrix = xgb.importance(
        model_xgboost_300$feature_names, model = model_xgboost)
y3_300 = importance_matrix[1:10,1][[1]]
x3_300 = importance_matrix[1:10,2][[1]]


windows(150,100)
par(mar=c(10,14,4,2),mfrow = c(2, 5))

plot(x=x0,y=10:1,cex=2,cex.lab=2,
     axes=F,xlab = "Importance",ylab = "")
box()
abline(h = 0:10, col = "lightgray", lty = 3)
#axis(1,at = 2:11,cex.axis=2)
axis(2,at = 10:1, labels = y0,las=1,cex.axis=2)
title("CT",cex.main=2)


plot(x=x1_100,y=10:1,cex=2,cex.lab=2,
     axes=F,xlab = "Importance",ylab = "")
box()
abline(h = 0:10, col = "lightgray", lty = 3)
#axis(1,at = 2:11,cex.axis=2)
axis(2,at = 10:1, labels = y1_100,las=1,cex.axis=2)
title("RF 100",cex.main=2)


plot(x=x1_300,y=10:1,cex=2,cex.lab=2,
     axes=F,xlab = "Importance",ylab = "")
box()
abline(h = 0:10, col = "lightgray", lty = 3)
#axis(1,at = 2:11,cex.axis=2)
axis(2,at = 10:1, labels = y1_300,las=1,cex.axis=2)
title("RF 300",cex.main=2)


plot(x=x1_500,y=10:1,cex=2,cex.lab=2,
     axes=F,xlab = "Importance",ylab = "")
box()
abline(h = 0:10, col = "lightgray", lty = 3)
#axis(1,at = 2:11,cex.axis=2)
axis(2,at = 10:1, labels = y1_500,las=1,cex.axis=2)
title("RF 500",cex.main=2)

plot(x=x2_100,y=10:1,cex=2,cex.lab=2,
     axes=F,xlab = "Importance",ylab = "")
box()
abline(h = 0:10, col = "lightgray", lty = 3)
#axis(1,at = 2:11,cex.axis=2)
axis(2,at = 10:1, labels = y2_100,las=1,cex.axis=2)
title("GB 100",cex.main=2)


plot(x=x2_300,y=10:1,cex=2,cex.lab=2,
     axes=F,xlab = "Importance",ylab = "")
box()
abline(h = 0:10, col = "lightgray", lty = 3)
#axis(1,at = 2:11,cex.axis=2)
axis(2,at = 10:1, labels = y2_300,las=1,cex.axis=2)
title("GB 300",cex.main=2)


plot(x=x2_500,y=10:1,cex=2,cex.lab=2,
     axes=F,xlab = "Importance",ylab = "")
box()
abline(h = 0:10, col = "lightgray", lty = 3)
#axis(1,at = 2:11,cex.axis=2)
axis(2,at = 10:1, labels = y2_500,las=1,cex.axis=2)
title("GB 500",cex.main=2)


plot(x=x3_100,y=10:1,cex=2,cex.lab=2,
     axes=F,xlab = "Importance",ylab = "")
box()
abline(h = 0:10, col = "lightgray", lty = 3)
#axis(1,at = 2:11,cex.axis=2)
axis(2,at = 10:1, labels = y3_100,las=1,cex.axis=2)
title("XGB 100",cex.main=2)


plot(x=x3_300,y=10:1,cex=2,cex.lab=2,
     axes=F,xlab = "Importance",ylab = "")
box()
abline(h = 0:10, col = "lightgray", lty = 3)
#axis(1,at = 2:11,cex.axis=2)
axis(2,at = 10:1, labels = y3_300,las=1,cex.axis=2)
title("XGB 300",cex.main=2)

plot(x=x3_500,y=10:1,cex=2,cex.lab=2,
     axes=F,xlab = "Importance",ylab = "")
box()
abline(h = 0:10, col = "lightgray", lty = 3)
#axis(1,at = 2:11,cex.axis=2)
axis(2,at = 10:1, labels = y3_500,las=1,cex.axis=2)
title("XGB 500",cex.main=2)


par(mfrow = c(1,1))



load("matriz_td_treino_fator.Rdata")
matriz_td_df_fator
M = dim(matriz_td_df_fator)[2]
banco_treino = matriz_td_df_fator

lista_modelos = list(CT,RF100,RF300,RF,
                     model_gbm_100,model_gbm_300,model_gbm,
                     model_xgboost_100,model_xgboost_300,model_xgboost)
Name = c("CT","RF100","RF300","RF500",
         "GB100","GB300","GB500",
         "XGB100","XGB300","XGB500")

results_treino = data.frame(0,0,0,0,0,0)
names(results_treino) = c("Method","A","P","TPR","TNR","F1")



threshold = NULL

for(i in 1:4){

Mod = lista_modelos[[i]] 
NOME = Name[i]

#testes dentro do banco de treino
pred = predict(Mod,newdata=banco_treino[,-c(M-1,M)],type="prob")
roc  = roc(response=banco_treino$ROTULO, predictor=pred[,2])
threshold[i] <- coords(roc, "best", ret = "threshold")
pred_classe <- ifelse(pred[,2] < rep(threshold[i],length(pred[,2])), 0, 1)
CM = confusionMatrix(table(pred_classe,banco_treino$ROTULO))

TN = CM$table[1,1]
FN = CM$table[1,2]
FP = CM$table[2,1]
TP = CM$table[2,2]

(A = (TP + TN)/(TN + FP + FN + TP))
(P = (TP/(FP+TP)))
(TPR = TP/(FN + TP))
(TNR = TN/(TN + FP))
(F1 = 2*P*TPR/(P + TPR))

results_treino[i,] = c(NOME,A,P,TPR,TNR,F1)
}

for(i in 5:7){
        
        Mod = lista_modelos[[i]] 
        NOME = Name[i]
        
        #testes dentro do banco de treino
        pred = predict(Mod,newdata=banco_treino[,-c(M-1,M)],type="response")
        roc  = roc(response=banco_treino$ROTULO, predictor=pred)
        threshold[i] <- coords(roc, "best", ret = "threshold")
        pred_classe <- ifelse(pred < rep(threshold[i],length(pred)), 0, 1)
        CM = confusionMatrix(table(pred_classe,banco_treino$ROTULO))
        
        TN = CM$table[1,1]
        FN = CM$table[1,2]
        FP = CM$table[2,1]
        TP = CM$table[2,2]
        
        (A = (TP + TN)/(TN + FP + FN + TP))
        (P = (TP/(FP+TP)))
        (TPR = TP/(FN + TP))
        (TNR = TN/(TN + FP))
        (F1 = 2*P*TPR/(P + TPR))
        
        results_treino[i,] = c(NOME,A,P,TPR,TNR,F1)
}


for(i in 8:10){
        
        Mod = lista_modelos[[i]] 
        NOME = Name[i]
        
        #testes dentro do banco de treino
        banco = as.matrix(banco_treino[,-c(M-1,M)])
        storage.mode(banco) <- "numeric"
        pred = predict(Mod,banco)
        roc  = roc(response=banco_treino$ROTULO, predictor=pred)
        threshold[i] <- coords(roc, "best", ret = "threshold")
        pred_classe <- ifelse(pred < rep(threshold[i],length(pred)), 0, 1)
        CM = confusionMatrix(table(pred_classe,banco_treino$ROTULO))
        
        TN = CM$table[1,1]
        FN = CM$table[1,2]
        FP = CM$table[2,1]
        TP = CM$table[2,2]
        
        (A = (TP + TN)/(TN + FP + FN + TP))
        (P = (TP/(FP+TP)))
        (TPR = TP/(FN + TP))
        (TNR = TN/(TN + FP))
        (F1 = 2*P*TPR/(P + TPR))
        
        results_treino[i,] = c(NOME,A,P,TPR,TNR,F1)
}

save(results_treino,file="results_treino.Rdata")

data = as.matrix(results_treino[,-1])
storage.mode(data) <- "numeric"
row.names(data) = results_treino[,1]
colnames(data) = c("Accuracy","Precision","Recall","Specificity","F1")
data

cores <- piratepal(palette = "basel",trans = .1)

windows(180,90)
par(mar=c(5,4,4,10), xpd=TRUE)
barplot(data,
        beside = T,
        col = minhas_cores,
        ylim = c(0,1),
        #legend=rownames(data),
        #args.legend = list(x = "bottomright",bg="white",cex=2),
        cex.axis = 3,
        cex.names = 2.6,
        border = "white")
legend(legend=rownames(data),
       "bottomright",
       bg="white",
       cex=2,
       col = minhas_cores,
       pch = 15,
       inset = c(-0.12,0))







load("matriz_td_teste_fator.Rdata")
matriz_td_teste_fator
M = dim(matriz_td_teste_fator)[2]
banco_teste = matriz_td_teste_fator

results_teste = data.frame(0,0,0,0,0,0)
names(results_teste) = c("Method","A","P","TPR","TNR","F1")

for(i in 1:4){
        
        Mod = lista_modelos[[i]] 
        NOME = Name[i]
        
        #testes dentro do banco de teste
        pred = predict(Mod,newdata=banco_teste[,-c(M-1,M)],type="prob")
        #roc  = roc(response=banco_teste$ROTULO, predictor=pred[,2])
        #threshold <- coords(roc, "best", ret = "threshold")
        pred_classe <- ifelse(pred[,2] < rep(threshold[i],length(pred[,2])), 0, 1)
        CM = confusionMatrix(table(pred_classe,banco_teste$ROTULO))
        
        TN = CM$table[1,1]
        FN = CM$table[1,2]
        FP = CM$table[2,1]
        TP = CM$table[2,2]
        
        (A = (TP + TN)/(TN + FP + FN + TP))
        (P = (TP/(FP+TP)))
        (TPR = TP/(FN + TP))
        (TNR = TN/(TN + FP))
        (F1 = 2*P*TPR/(P + TPR))
        
        results_teste[i,] = c(NOME,A,P,TPR,TNR,F1)
}

for(i in 5:7){
        
        Mod = lista_modelos[[i]] 
        NOME = Name[i]
        
        #testes dentro do banco de teste
        pred = predict(Mod,newdata=banco_teste[,-c(M-1,M)],type="response")
        #roc  = roc(response=banco_teste$ROTULO, predictor=pred)
        #threshold <- coords(roc, "best", ret = "threshold")
        pred_classe <- ifelse(pred < rep(threshold[i],length(pred)), 0, 1)
        CM = confusionMatrix(table(pred_classe,banco_teste$ROTULO))
        
        TN = CM$table[1,1]
        FN = CM$table[1,2]
        FP = CM$table[2,1]
        TP = CM$table[2,2]
        
        (A = (TP + TN)/(TN + FP + FN + TP))
        (P = (TP/(FP+TP)))
        (TPR = TP/(FN + TP))
        (TNR = TN/(TN + FP))
        (F1 = 2*P*TPR/(P + TPR))
        
        results_teste[i,] = c(NOME,A,P,TPR,TNR,F1)
}


for(i in 8:10){
        
        Mod = lista_modelos[[i]] 
        NOME = Name[i]
        
        #testes dentro do banco de teste
        banco = as.matrix(banco_teste[,-c(M-1,M)])
        storage.mode(banco) <- "numeric"
        pred = predict(Mod,banco)
        #roc  = roc(response=banco_teste$ROTULO, predictor=pred)
        #threshold <- coords(roc, "best", ret = "threshold")
        pred_classe <- ifelse(pred < rep(threshold[i],length(pred)), 0, 1)
        CM = confusionMatrix(table(pred_classe,banco_teste$ROTULO))
        
        TN = CM$table[1,1]
        FN = CM$table[1,2]
        FP = CM$table[2,1]
        TP = CM$table[2,2]
        
        (A = (TP + TN)/(TN + FP + FN + TP))
        (P = (TP/(FP+TP)))
        (TPR = TP/(FN + TP))
        (TNR = TN/(TN + FP))
        (F1 = 2*P*TPR/(P + TPR))
        
        results_teste[i,] = c(NOME,A,P,TPR,TNR,F1)
}

save(results_teste,file="results_teste.Rdata")

data = as.matrix(results_teste[,-1])
storage.mode(data) <- "numeric"
row.names(data) = results_teste[,1]
colnames(data) = c("Accuracy","Precision","Recall","Specificity","F1")


windows(180,90)
par(mar=c(5,4,4,10), xpd=TRUE)
barplot(data,
        beside = T,
        col = minhas_cores,
        ylim = c(0,1),
        #legend=rownames(data),
        #args.legend = list(x = "bottomright",bg="white",cex=2),
        cex.axis = 3,
        cex.names = 2.6,
        border = "white")
legend(legend=rownames(data),
       "bottomright",
       bg="white",
       cex=2,
       col = minhas_cores,
       pch = 15,
       inset = c(-0.12,0))

