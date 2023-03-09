#### A Statistical Analysis of Textual E-Commerce Reviews Using Tree-Based Methods
#### Open Journal of Statistics > Vol.12 No.3, June 2022
#### https://scirp.org/journal/paperinformation.aspx?paperid=117776

# install.packages("tidytext")
# install.packages("tidyverse")
# install.packages("textstem")
# install.packages("installr")
# install.packages("spelling")
# install.packages("tm")
# install.packages("hunspell")
# install.packages("igraph")
# install.packages("ggraph")
# install.packages("ggplot2")
# install.packages("wordcloud")
# install.packages("lexicon")

library(tidytext)
library(readr)
library(tidyverse)
library(textstem)
library(installr)
library(spelling)
library(tm)
library(hunspell)
library(igraph)
library(ggraph)
library(wordcloud)
library(lexicon)


############################## read data #######################################
banco <- read.csv("Womens Clothing E-Commerce Reviews.csv")

#############################################################################################
################################### PR?-PROCESSAMENTO #######################################
#############################################################################################

###concat title and review
titulo_texto=NULL
for(i in 1:nrow(banco)){
  if(!is.na(banco$Title[i])){
    titulo_texto[i]=paste(banco$Title[i],banco$Review.Text[i])  
  } else {titulo_texto[i]=banco$Review.Text[i]}
    
}
titulo_texto=as.character(titulo_texto)
banco=cbind(banco,titulo_texto)
dim(banco) #23486    12

#new index
names(banco)
names(banco)[1] = "INDICE"
X1=banco$INDICE+1
banco[1]=X1
names(banco)[7] = "ROTULO"
names(banco)

n_pos = sum(banco$ROTULO==1) #19314
n_neg = sum(banco$ROTULO==0) #4172


#number of each lable in training dataset (70%)
n_treino_por_rotulo = (n_neg*0.7)%/%1  #2920 
#number of each lable in test dataset (30%)
n_teste_por_rotulo = n_neg - n_treino_por_rotulo #1252

#sample negative lables for training dataset
set.seed(1234)
sub_banco_neg = banco[banco$ROTULO==0,]
dim(sub_banco_neg) #4172 12
linhas_neg = sub_banco_neg$INDICE
#verification
sum((banco[linhas_neg,"ROTULO"])==0) #4172
#random selection
linhas_neg_treino = sample(linhas_neg,n_treino_por_rotulo)
length(linhas_neg_treino) #2920
#sample positive lables for training dataset
sub_banco_pos = banco[banco$ROTULO==1,]
dim(sub_banco_pos) #19314 12
linhas_pos = sub_banco_pos$INDICE
#verification
sum(banco[linhas_pos,"ROTULO"]==1) #19314
#random selection
linhas_pos_treino = sample(linhas_pos,n_treino_por_rotulo)
length(linhas_pos_treino) # 2920
#montando o banco de treino
linhas_treino = sort(c(linhas_neg_treino,linhas_pos_treino))
banco_treino = banco[linhas_treino,]
class(banco_treino)
#View(banco_treino)
#verificando se o banco est? na dim correta e balanceado
dim(banco_treino)
names(banco_treino)
table(banco_treino$ROTULO)


#balancing the base
banco_aux = banco[-linhas_treino,]
dim(banco_aux) #17646 12
banco_aux_neg = banco_aux[banco_aux$ROTULO==0,]
dim(banco_aux_neg) #1252 12
linhas_neg_teste = banco_aux_neg$INDICE
length(linhas_neg_teste) #1252
#verification
sum(banco[linhas_neg_teste,"ROTULO"]==0) #1252
#agora para os rotulos positivos
banco_aux_pos = banco_aux[banco_aux$ROTULO==1,]
dim(banco_aux_pos) #16394 12
linhas_aux_pos = banco_aux_pos$INDICE
#verification
sum(banco[linhas_aux_pos,"ROTULO"]==1) #16394 
linhas_pos_teste = sample(linhas_aux_pos,n_teste_por_rotulo)
length(linhas_pos_teste) #1252


linhas_teste = sort(c(linhas_neg_teste,linhas_pos_teste))
banco_teste = banco[linhas_teste,]
#verification
dim(banco_teste) #2504 12
names(banco_teste)
table(banco_teste$ROTULO) #1252 1252


banco_bruto_treino = banco_treino
banco_bruto_teste  = banco_teste
  
#salvar os bancos brutos - teste e treino
save(banco_bruto_treino,file="banco-bruto-treino.Rdata")
save(banco_bruto_teste,file="banco-bruto-teste.Rdata")

