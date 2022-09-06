# #Pacotes Usados
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


############################## Lendo o banco de dados #######################################
banco <- read.csv("Womens Clothing E-Commerce Reviews.csv")
View(banco)
#############################################################################################
################################### PRÉ-PROCESSAMENTO #######################################
#############################################################################################

###Concatenando o titulo da review com o texto da review
titulo_texto=NULL
for(i in 1:nrow(banco)){
  if(!is.na(banco$Title[i])){
    titulo_texto[i]=paste(banco$Title[i],banco$`Review Text`[i])  
  } else {titulo_texto[i]=banco$`Review Text`[i]}
    
}

titulo_texto=as.character(titulo_texto)
banco=cbind(banco,titulo_texto)
dim(banco) #23486    12
#View(banco)

#Renumerando o vetor X1 (Índice) para começar em 1 ao invés de 0
names(banco)
names(banco)[1] = "INDICE"
X1=banco$INDICE+1
banco[1]=X1
names(banco)[7] = "ROTULO"
names(banco)
#View(banco)

n_pos = sum(banco$ROTULO==1)
n_neg = sum(banco$ROTULO==0)


#num de documentos de cada rotulo no banco de treino (70%)
n_treino_por_rotulo = (n_neg*0.7)%/%1  #2920 
#num de documentos de cada rotulo no banco de teste (30%)
n_teste_por_rotulo = n_neg - n_treino_por_rotulo #1252

sub_banco_neg = banco[banco$ROTULO==0,]
dim(sub_banco_neg)
linhas_neg = sub_banco_neg$INDICE
#verificando se de fato linhas_neg guarda as linhas com rotulo=0
sum((banco[linhas_neg,"ROTULO"])==0)
#sorteando n_treino_por_rotulo linhas
set.seed(1234)
linhas_neg_treino = sample(linhas_neg,n_treino_por_rotulo)
length(linhas_neg_treino)
#repetindo o processo para os rotulos positivos
sub_banco_pos = banco[banco$ROTULO==1,]
dim(sub_banco_pos)
linhas_pos = sub_banco_pos$INDICE
#verificando se de fato linhas_pos guarda as linhas com rotulo=1
sum(banco[linhas_pos,"ROTULO"]==1)
#sorteando n_treino_por_rotulo linhas
linhas_pos_treino = sample(linhas_pos,n_treino_por_rotulo)
length(linhas_pos_treino)
#montando o banco de treino
linhas_treino = sort(c(linhas_neg_treino,linhas_pos_treino))
banco_treino = banco[linhas_treino,]
class(banco_treino)
#View(banco_treino)
#verificando se o banco está na dim correta e balanceado
dim(banco_treino)
names(banco_treino)
table(banco_treino$ROTULO)


#sorteando n_teste_por_rotulo linhas (diferente das linhas sorteadas para o treino)
#pra isso primeiro vamos criar um banco aux que e o original se as 
#linhas que foram sorteadas para o treino
banco_aux = banco[-linhas_treino,]
dim(banco_aux)
#vamos separar esse banco como neg e pos, como fizemos antes
banco_aux_neg = banco_aux[banco_aux$ROTULO==0,]
dim(banco_aux_neg)
#como o rotulo neg era o com menor quantidade, todos as linhas restantes 
#com rotulo neg vao para o teste, ou seja, banco_aux_neg sera a parte com
#rotulo neg do banco de teste
linhas_neg_teste = banco_aux_neg$INDICE
length(linhas_neg_teste)
#verificando se de fato linhas_neg guarda as linhas com rotulo=0
sum(banco[linhas_neg_teste,"ROTULO"]==0)
#agora para os rotulos positivos
banco_aux_pos = banco_aux[banco_aux$ROTULO==1,]
dim(banco_aux_pos)
#aqui temos mais que n_teste_por_rotulo linhas, entao vamos sortear
linhas_aux_pos = banco_aux_pos$INDICE
#verificando se de fato linhas_pos guarda as linhas com rotulo=1
sum(banco[linhas_aux_pos,"ROTULO"]==1)
#sorteando n_teste_por_rotulo linhas
linhas_pos_teste = sample(linhas_aux_pos,n_teste_por_rotulo)
length(linhas_pos_teste)
#montando o banco de teste
linhas_teste = sort(c(linhas_neg_teste,linhas_pos_teste))
banco_teste = banco[linhas_teste,]
#verificando se o banco está na dim correta e balanceado
dim(banco_teste)
names(banco_teste)
table(banco_teste$ROTULO)


banco_bruto_treino = banco_treino
banco_bruto_teste  = banco_teste
  
#salvar os bancos brutos - teste e treino
save(banco_bruto_treino,file="banco-bruto-treino.Rdata")
save(banco_bruto_teste,file="banco-bruto-teste.Rdata")

