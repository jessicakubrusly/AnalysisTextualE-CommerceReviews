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
library("SnowballC")
library("RColorBrewer")

set.seed(123)

load("banco-bruto-teste.Rdata")
dim(banco_bruto_teste) #2504 12
names(banco_bruto_teste)
banco = banco_bruto_teste

#replicate in test dataset

banco_tibble = as_tibble(banco)
banco_tibble %>% mutate_if(is.factor, as.character) -> 
  banco_tibble

names(banco_tibble)
banco_sub=subset(banco_tibble,
                 select = c(
                   INDICE,
                   ROTULO,
                   titulo_texto))

banco_tidy=banco_sub %>%
  unnest_tokens(word,titulo_texto)

dim(banco_tidy) #157127      3
#concate not+word
i = 1
tam = dim(banco_tidy)[1]
while(i < tam){
  if(banco_tidy[[3]][i]=="not" && !is.na(banco_tidy[[3]][i])){
    palavra_seguinte=banco_tidy[[3]][i+1]
    banco_tidy[[3]][i]=paste("not",palavra_seguinte)
    indice_excluir=i+1
    banco_tidy=banco_tidy[-indice_excluir,]
    #print(i)
  }
  i = i + 1
  tam = dim(banco_tidy)[1]
}
dim(banco_tidy) #155718      3


#stopwords
data("stop_words")
banco_tidy=
  banco_tidy%>%
  anti_join(stop_words)
dim(banco_tidy) #55760     3


#numbers
dim(banco_tidy) 
banco_tidy_sem_numeros=removeNumbers(banco_tidy[[3]])
banco_tidy[[3]]=banco_tidy_sem_numeros
dim(banco_tidy) 

#removing weird tokens
i = 1
tam = dim(banco_tidy)[1]
while(i < tam){
  if((is.na(banco_tidy[[3]][i])|
      banco_tidy[[3]][i]==""|
      banco_tidy[[3]][i]=="'"|
      banco_tidy[[3]][i]=="',"|
      banco_tidy[[3]][i]=="'."|
      banco_tidy[[3]][i]==","|
      banco_tidy[[3]][i]=="."|
      banco_tidy[[3]][i]==";"|
      banco_tidy[[3]][i]=="."|
      banco_tidy[[3]][i]==".."|
      banco_tidy[[3]][i]==";.") #&& !is.na(banco_tidy[[3]][i])
  ){
    indice_excluir=i
    banco_tidy=banco_tidy[-indice_excluir,]
    #print(i)
    tam = tam - 1
  } else {
    i = i + 1
  }
}
dim(banco_tidy) #53827     3
sort(banco_tidy[[3]])[1:100]
sum(is.na(banco_tidy[[3]])) 

#############################################################################################
##################################### lematization ###########################################
#############################################################################################

#Criando um vetor com as palavras lematizadas
a=lemmatize_words(banco_tidy[[3]])

#Substituindo as palavras lematizadas no banco original
banco_tidy[[3]]=a


##############################################################################################
############################ document-term matrix ###############################################
#############################################################################################

load("matriz_td_treino.Rdata")

termos = names(matriz_td_treino)
M = dim(matriz_td_treino)[2]

tab=table(banco_tidy[[1]])
N = length(tab)

matriz_termo_docuemnto = matrix(0,N,M)
colnames(matriz_termo_docuemnto) <- termos
rownames(matriz_termo_docuemnto) <- names(tab)

rotulos = matrix(NA,N,1) 
rownames(rotulos) <- names(tab)
for(i in 1:nrow(banco_tidy)){
  teste_termos = (termos == banco_tidy[[3]][i])
  if(sum(teste_termos) == 1){
    j = 1
    while(!teste_termos[j]){
      j = j + 1
    }
    #j guarda a coluna da matriz onde vamos incrementar o valor
    k = banco_tidy[[1]][i]  
    matriz_termo_docuemnto[as.character(k),j] = 
      matriz_termo_docuemnto[as.character(k),j] + 1
    
    #guardar o rotulo desse documento aproveitando o indice
    rotulos[as.character(k),1] = banco_tidy[[2]][i]
  }
}
#verification
matriz_termo_docuemnto["3",1:20]
sort(banco_tidy[[3]][banco_tidy$INDICE==3])

table(rotulos[,1]) # 1213 1187
dim(rotulos) #2446 1
sum(is.na(rotulos)) #46

linhas_so_com_zero = NULL
for(i in 1:nrow(matriz_termo_docuemnto)){
  linha = matriz_termo_docuemnto[i,]
  if(sum(linha)==0)
    linhas_so_com_zero = c(linhas_so_com_zero,i)
}
linhas_so_com_zero
matriz_td_df_teste = data.frame(matriz_termo_docuemnto)

class(matriz_td_df_teste)
dim(matriz_td_df_teste)
colnames(matriz_td_df_teste)
rownames(matriz_td_df_teste)
matriz_td_df_teste$ROTULO = rotulos
matriz_td_df_teste$INDICE = names(tab)

matriz_td_df_teste[linhas_so_com_zero,]
matriz_td_df_teste = matriz_td_df_teste[-linhas_so_com_zero,]

linhas_so_com_zero = NULL
for(i in 1:nrow(matriz_td_df_teste)){
  if(sum(matriz_td_df_teste[i,-c(M-1,M)])==0)
    linhas_so_com_zero = c(linhas_so_com_zero,i)
}
linhas_so_com_zero
sum(is.na(matriz_td_df_teste$ROTULO))


matriz_td_teste = matriz_td_df_teste
save(matriz_td_teste,file="matriz_td_teste.Rdata")


troca = function(x,y){
  if(x==y)
    return(x-1)
  return(x)
}


load("matriz_td_treino_fator.Rdata")
matriz_td_df_teste = matriz_td_teste
M = dim(matriz_td_df_fator)[2]
matriz_td_df = matriz_td_df_fator
j = 1
while(j < M){
  niveis_treino = levels(matriz_td_df[,names(matriz_td_df)[j]])
  niveis_teste  = levels(as.factor(matriz_td_df_teste[,names(matriz_td_df_teste)[j]]))
  if(all(niveis_teste %in% niveis_treino)){
    matriz_td_df_teste[,names(matriz_td_df_teste)[j]] = 
      as.factor(matriz_td_df_teste[,names(matriz_td_df_teste)[j]])
    matriz_td_df_teste[,names(matriz_td_df_teste)[j]] = 
      factor(matriz_td_df_teste[,names(matriz_td_df_teste)[j]],levels = niveis_treino)
    j = j + 1
  } else {
    k = 1
    repeat{
      if(!(niveis_teste %in% niveis_treino)[k]){
        #achei a posicao do erro
        intruso = niveis_teste[k]
        matriz_td_df_teste[,names(matriz_td_df_teste)[j]] = 
          sapply(
            matriz_td_df_teste[,names(matriz_td_df_teste)[j]],
            troca,
            y=intruso)
        break
      } else {
        k = k + 1
      }
    }
  }
}

dim(matriz_td_df_teste) # 2400  103

matriz_td_teste_fator = matriz_td_df_teste
save(matriz_td_teste_fator,file="matriz_td_teste_fator.Rdata")
