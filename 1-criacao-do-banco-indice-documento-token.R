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

#carregando o banco de treino
load("banco-bruto-treino.Rdata")
dim(banco_bruto_treino)
names(banco_bruto_treino)
banco = banco_bruto_treino
#View(banco_bruto_treino)

#Transformando o banco de dados em tibble
banco_tibble = as_tibble(banco)
banco_tibble %>% mutate_if(is.factor, as.character) -> 
  banco_tibble

banco_tibble

#Selecionando as variáveis de interesse (subset)
names(banco_tibble)
banco_sub=subset(banco_tibble,
                 select = c(
                   INDICE,
                   ROTULO,
                   titulo_texto))
banco_sub
#Separando o banco de dados em tokens
banco_tidy=banco_sub %>%
 unnest_tokens(word,titulo_texto)


dim(banco_tidy) #363887      3
banco_tidy
banco_tidy[[3]]
#Concatenando not com a palavra posterior
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
dim(banco_tidy) #360577      3
### Resultado:
### Foram concatenados 3310 tokens not+palavra posterior
###


#Removendo as stop words
data("stop_words")

dim(banco_tidy) #360577      3
banco_tidy=
  banco_tidy%>%
  anti_join(stop_words)
dim(banco_tidy) #128203      3
#Depois da remcao das stopwords sobraram 128203 tokens


#Removendo Números
dim(banco_tidy) 
banco_tidy_sem_numeros=removeNumbers(banco_tidy[[3]])
banco_tidy[[3]]=banco_tidy_sem_numeros
dim(banco_tidy) 
#o banco nao continha numeros


#Removendo os tokens em branco e estranhos
dim(banco_tidy) #128203      3
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
sum(is.na(banco_tidy[[3]])) #nao restou na

#############################################################################################
##################################### LEMATIZAÇÃO ###########################################
#############################################################################################

#Criando um vetor com as palavras lematizadas
a=lemmatize_words(banco_tidy[[3]])

#Substituindo as palavras lematizadas no banco original
banco_tidy[[3]]=a

#Após esse passo o banco está lematizado

#salvar banco_tidy

banco_indice_doc_token = banco_tidy

save(banco_indice_doc_token,file="banco-indice-doc-token.Rdata")

