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

#load 
load("banco-bruto-treino.Rdata")
dim(banco_bruto_treino)
names(banco_bruto_treino)
banco = banco_bruto_treino

#as tibble
banco_tibble = as_tibble(banco)
banco_tibble %>% mutate_if(is.factor, as.character) -> 
  banco_tibble

#selecting variables 
names(banco_tibble)
banco_sub=subset(banco_tibble,
                 select = c(
                   INDICE,
                   ROTULO,
                   titulo_texto))

#tokens
banco_tidy=banco_sub %>%
 unnest_tokens(word,titulo_texto)


dim(banco_tidy) #363722      3
#concat not + word
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
dim(banco_tidy) #360412      3

#stopwords
data("stop_words")

banco_tidy=
  banco_tidy%>%
  anti_join(stop_words)
dim(banco_tidy) #128038      3


#number removing
banco_tidy_sem_numeros=removeNumbers(banco_tidy[[3]])
banco_tidy[[3]]=banco_tidy_sem_numeros
dim(banco_tidy) #128038 3


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
dim(banco_tidy) #123769      3
sort(banco_tidy[[3]])[1:100]
sum(is.na(banco_tidy[[3]])) #no na

#############################################################################################
##################################### lematization ##########################################
#############################################################################################

#Criando um vetor com as palavras lematizadas
a=lemmatize_words(banco_tidy[[3]])

#Substituindo as palavras lematizadas no banco original
banco_tidy[[3]]=a

#Ap?s esse passo o banco est? lematizado

#salvar banco_tidy

banco_indice_doc_token = banco_tidy

save(banco_indice_doc_token,file="banco-indice-doc-token.Rdata")

