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

# carregar o data frame contendo a matriz termo documento com uma última coluna 
# com os rotulos

load("banco-bruto-teste.Rdata")
dim(banco_bruto_teste)
names(banco_bruto_teste)
banco = banco_bruto_teste

### Fazendo o pre processamento textual no banco de teste

#Transformando o banco de dados em tibble
banco_tibble = as_tibble(banco)
banco_tibble %>% mutate_if(is.factor, as.character) -> 
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
banco_tidy

dim(banco_tidy) #157185      3
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
dim(banco_tidy) #155776      3


#Removendo as stop words
data("stop_words")

dim(banco_tidy) 
banco_tidy=
  banco_tidy%>%
  anti_join(stop_words)
dim(banco_tidy) #55818     3


#Removendo Números
dim(banco_tidy) 
banco_tidy_sem_numeros=removeNumbers(banco_tidy[[3]])
banco_tidy[[3]]=banco_tidy_sem_numeros
dim(banco_tidy) 
#o banco nao continha numeros


#Removendo os tokens em branco e estranhos
dim(banco_tidy) #55818     3
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
sum(is.na(banco_tidy[[3]])) #nao restou na

#############################################################################################
##################################### LEMATIZAÇÃO ###########################################
#############################################################################################

#Criando um vetor com as palavras lematizadas
a=lemmatize_words(banco_tidy[[3]])

#Substituindo as palavras lematizadas no banco original
banco_tidy[[3]]=a

#Após esse passo o banco está lematizado


##############################################################################################
############################ MATRIZ TERMO DOCUMENTO ###############################################
#############################################################################################

load("matriz_td_treino.Rdata")

termos = names(matriz_td_treino)
M = dim(matriz_td_treino)[2]

tab=table(banco_tidy[[1]])
N = length(tab)

matriz_termo_docuemnto = matrix(0,N,M)
colnames(matriz_termo_docuemnto) <- termos
rownames(matriz_termo_docuemnto) <- names(tab)

rotulos = matrix(NA,N,1) #vai ser util jaja, vou aproveitar esse for
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
#verificando
matriz_termo_docuemnto["3",1:20]
sort(banco_tidy[[3]][banco_tidy$INDICE==3])

#verificando vetor de rotulos
table(rotulos[,1])
dim(rotulos)
sum(is.na(rotulos))
#tem rotulos = NA, mas esses sao correspondentes as 
#linhas em branco e serao removidos. 

#buscando linhas iguais a zero / em branco
linhas_so_com_zero = NULL
for(i in 1:nrow(matriz_termo_docuemnto)){
  linha = matriz_termo_docuemnto[i,]
  if(sum(linha)==0)
    linhas_so_com_zero = c(linhas_so_com_zero,i)
}
linhas_so_com_zero
#eliminando as linhas nulas da matriz termo documento.
#para isso vamos transforma-la em um data.frame
#transformando a matriz em um dataframe
matriz_td_df_teste = data.frame(matriz_termo_docuemnto)
class(matriz_td_df_teste)
dim(matriz_td_df_teste)
colnames(matriz_td_df_teste)
rownames(matriz_td_df_teste)
#antes de eliminar as linhas, colocar os rotulos e os indices dos 
#documentos para nao perder essa informacao
matriz_td_df_teste$ROTULO = rotulos
matriz_td_df_teste$INDICE = names(tab)

#agora vamos tirar as linhas so com zero (e junto tirar o rótulo e 
#os indices dessas linhas somente)
matriz_td_df_teste[linhas_so_com_zero,]
matriz_td_df_teste = matriz_td_df_teste[-linhas_so_com_zero,]

#verificando se sobrou alguma linha so com zero
linhas_so_com_zero = NULL
for(i in 1:nrow(matriz_td_df_teste)){
  if(sum(matriz_td_df_teste[i,-c(M-1,M)])==0)
    linhas_so_com_zero = c(linhas_so_com_zero,i)
}
linhas_so_com_zero
#MARAVILHA!!!

#verificando se sobrou algum rotulo NA
sum(is.na(matriz_td_df_teste$ROTULO))
#MARAVILHA!!!

matriz_td_teste = matriz_td_df_teste
save(matriz_td_teste,file="matriz_td_teste.Rdata")




#vamos salvar tambem o banco ja transformado para fator
#mas precisamos cuidar para que cada termo no banco teste tenha os mesmos niveis do banco treino

#vou usar em seguida
troca = function(x,y){
  if(x==y)
    return(x-1)
  return(x)
}

#testes no banco de teste
load("matriz_td_treino_fator.Rdata")
matriz_td_df_teste = matriz_td_teste
M = dim(matriz_td_df_fator)[2]
matriz_td_df = matriz_td_df_fator
#transformar em fator as colunas de matriz_td_df_teste
#mas cuidade: os levels tem que ser os mesmos que os fatores de matriz_td_df
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

dim(matriz_td_df_teste) # 2420  103

matriz_td_teste_fator = matriz_td_df_teste
save(matriz_td_teste_fator,file="matriz_td_teste_fator.Rdata")
