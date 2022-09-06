library(tidytext)
library(tidyverse)
library(wordcloud)
library(yarrr)
library(dplyr)

# library(spelling)
# library(tm)
# library(hunspell)
# library(igraph)
# library(ggraph)
# library(wordcloud)
# library(lexicon)
#library(readr)
# library(textstem)
#library(installr)

load("banco-indice-doc-token.Rdata")
banco_tidy = banco_indice_doc_token
dim(banco_tidy)
class(banco_tidy)


sum(is.na(banco_tidy[[3]]))

google.cols <- piratepal(palette = "google", 
                         trans = .2)


##############################################################################################
############################ MATRIZ TERMO DOCUMENTO ###############################################
#############################################################################################
#vamos usar 100 termos
#selecao dos termos
#vamos usar M termos
#analisar freq
banco_tidy_copia = banco_tidy
#banco_tidy = banco_tidy_copia

words1 = c("dress","fit","love","size","top","wear","color")
words2 = c("fabric", "cute", "shirt", "run")
words3 = c("pretty")  
words4 = c("beautiful")
words5 = c( "short" ,"sweater")
words6 = c("material","nice")
words7 = c("buy")

dim(banco_tidy)
for(i in 1:length(words1)){
  banco_tidy = banco_tidy %>% filter(word != words1[i])
}
for(i in 1:length(words2)){
  banco_tidy = banco_tidy %>% filter(word != words2[i])
}
for(i in 1:length(words3)){
  banco_tidy = banco_tidy %>% filter(word != words3[i])
}
for(i in 1:length(words4)){
  banco_tidy = banco_tidy %>% filter(word != words4[i])
}
for(i in 1:length(words5)){
  banco_tidy = banco_tidy %>% filter(word != words5[i])
}
for(i in 1:length(words6)){
  banco_tidy = banco_tidy %>% filter(word != words6[i])
}
for(i in 1:length(words7)){
  banco_tidy = banco_tidy %>% filter(word != words7[i])
}
dim(banco_tidy)

freq=banco_tidy%>%
  count(ROTULO,word) #calcular porcentagens

freq%>% arrange(desc(word))
freq%>% arrange(desc(n))

words1;words2;words3;words4;words5

freq = freq %>% mutate(R1 = ifelse(ROTULO == 1,n,0),
                       R0 = n - R1)

freq%>% arrange(desc(word))
freq%>% arrange(desc(n))

freq = freq %>% 
  group_by(word) %>% 
  summarise(across(everything(), sum))%>% 
  select(word,n,R1,R0)

freq%>% arrange(desc(word))

freq%>% arrange(desc(n))
(freq%>%arrange(desc(n)))$word[1:10]

freq%>% arrange(desc(R1))
(freq%>%arrange(desc(R1)))$word[1:10]

freq%>% arrange(desc(R0))
(freq%>%arrange(desc(R0)))$word[1:10]




#vamos ficar com os 100 termos mais frequentes
(freq%>%arrange(desc(n)))[100:110,]
freq = freq%>%filter(n>196)
dim(freq)
M = 100
cte = M/sum(freq$n)

#salvar o objeto freq para futura analise de sentimento
save(freq,file="freq_obj.Rdata")

windows(110,60)
par(mar=c(19,11,4,2))
valores = ((freq%>%arrange(desc(n)))[1:10,]$n)*cte
barplot(valores, 
        las = 2, 
        names.arg = (freq%>%arrange(desc(n)))[1:10,]$word,
        col =google.cols[1], 
        #        main ="Most frequent words (training data)",
        #        ylab = "Word frequencies (%)",
        cex.axis = 4,
        cex.names = 4)
title(#main="Most frequent words (training data - positive label)",
  ylab="Word frequencies(%)",
  cex.main=4,
  cex.lab=3.8,
  mgp=c(7,0,0)
)


windows(110,60)
par(mar=c(19,11,4,2))
valores = ((freq%>%arrange(desc(R1)))[1:10,]$R1)*cte
barplot(valores, 
        las = 2, 
        names.arg = (freq%>%arrange(desc(R1)))[1:10,]$word,
        col =google.cols[1], 
        #        main ="Most frequent words (training data)",
        #        ylab = "Word frequencies (%)",
        cex.axis = 4,
        cex.names = 4)
title(#main="Most frequent words (training data - positive label)",
  ylab="Word frequencies(%)",
  cex.main=4,
  cex.lab=3.8,
  mgp=c(7,0,0)
)


windows(110,60)
par(mar=c(19,11,4,2))
valores = ((freq%>%arrange(desc(R0)))[1:10,]$R0)*cte
barplot(valores, 
        las = 2, 
        names.arg = (freq%>%arrange(desc(R0)))[1:10,]$word,
        col =google.cols[1], 
        #        main ="Most frequent words (training data)",
        #        ylab = "Word frequencies (%)",
        cex.axis = 4,
        cex.names = 4)
title(#main="Most frequent words (training data - positive label)",
  ylab="Word frequencies(%)",
  cex.main=4,
  cex.lab=3.8,
  mgp=c(7,0,0)
)




# 
# 
# 
# set.seed(1234) # for reproducibility 
# windows(60,60)
# wordcloud(words = freq$word, 
#           freq  = freq$n, 
#           min.freq = 1,           
#           max.words=200, 
#           random.order=FALSE, 
#           rot.per=0.35,            
#           colors=brewer.pal(8, "Dark2"))
# 
# 

# 
# set.seed(1234) # for reproducibility 
# 
# windows(60,60)
# wordcloud(words = freq_pos$word, 
#           freq  = freq_pos$n, 
#           min.freq = 1,           
#           max.words=200, 
#           random.order=FALSE, 
#           rot.per=0.35,            
#           colors=brewer.pal(8, "Dark2"))
# 
# 
# freq%>%arrange(desc(n))
# freq%>%arrange((n))
# 
# freq_completo = banco_tidy_copia%>%
#   count(word) #calcular porcentagens
# 
# ((freq_completo%>%arrange(desc(n)))$n)[1:100]
# 
# windows(100,60)
# #par(mar=c(5,4,4,2)+0.1)
# par(mar=c(8,10,4,2))
# plot(x=1:length(freq_porcentagen[,1]),
#      y=freq_porcentagen[,1],
#      ylab="",
#      xlab="",
#      pch=19,
#      cex.axis = 2,
#      cex.names = 2)
# abline(h=0.715,lty=1,col=google.cols[1],lwd=3)
# abline(h=0.165,lty=1,col=google.cols[1],lwd=3)
# title(main="Frequency in Descending Order",
#       ylab="Frequency (%)",
#       xlab="Order",
#       cex.main=3.5,
#       cex.lab=3,
#       mgp=c(5,0,0)
#       )
# 
# windows(100,60)
# plot(x=13:112,
#      y=freq_porcentagen[13:112,1],
#      main="Frequency in Descending Order (zoom)",
#      ylab="Frequency (%)",
#      xlab="Order",
#      pch=19)
# abline(h=0.715,lty=1,col=google.cols[1],lwd=3)
# abline(h=0.165,lty=1,col=google.cols[1],lwd=3)
# 


#termos a serem usados - os M mais citados depois de excluidos os 12 mais
#M=100


# 
# set.seed(1234) # for reproducibility 
# windows(60,60)
# wordcloud(words = obj$word, 
#           freq  = obj$n, 
#           min.freq = 1,           
#           max.words=200, 
#           random.order=FALSE, 
#           rot.per=0.35,            
#           colors=brewer.pal(8, "Dark2"))
# 

# 
# 
# 
# set.seed(1234) # for reproducibility 
# windows(60,60)
# wordcloud(words = obj_neg$word, 
#           freq  = obj_neg$n, 
#           min.freq = 1,           
#           max.words=200, 
#           random.order=FALSE, 
#           rot.per=0.35,            
#           colors=brewer.pal(8, "Dark2"))


dim(freq)
termos = freq$word
M = length(termos)
M

tab=table(banco_tidy[[1]])
tab[1:10]
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
matriz_termo_docuemnto["6",1:20]
sort(banco_tidy[[3]][banco_tidy$INDICE==6])

#verificando vetor de rotulos
table(rotulos[,1])
dim(rotulos)
sum(is.na(rotulos))
rotulos[209,]
banco_tidy[[3]][banco_tidy$INDICE==915]
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
matriz_td_df = data.frame(matriz_termo_docuemnto)
class(matriz_td_df)
dim(matriz_td_df)
colnames(matriz_td_df)
rownames(matriz_td_df)
#antes de eliminar as linhas, colocar os rotulos e os indices dos 
#documentos para nao perder essa informacao
ROTULO = rotulos
INDICE = names(tab)
matriz_td_df = cbind(matriz_td_df,ROTULO,INDICE)
dim(matriz_td_df)

#agora vamos tirar as linhas so com zero (e junto tirar o rótulo e 
#os indices dessas linhas somente)
matriz_td_df[linhas_so_com_zero,]
matriz_td_df = matriz_td_df[-linhas_so_com_zero,]

#verificando se sobrou alguma linha so com zero
linhas_so_com_zero = NULL
for(i in 1:nrow(matriz_td_df)){
  if(sum(matriz_td_df[i,-c(M+1,M+2)])==0)
    linhas_so_com_zero = c(linhas_so_com_zero,i)
}
linhas_so_com_zero
#MARAVILHA!!!

#verificando se sobrou algum rotulo NA
sum(is.na(matriz_td_df$ROTULO))
#MARAVILHA!!!


names(matriz_td_df)
dim(matriz_td_df)

matriz_td_treino = matriz_td_df
#Vamos salvar a matriz termo documento para rodar os metodos em um 
#proximo codigo
save(matriz_td_treino,file="matriz_td_treino.Rdata")

#vamos salvar tambem o banco ja transformado para fator
M = dim(matriz_td_df)[2]
matriz_td_df_fator = matriz_td_df
matriz_td_df_fator[names(matriz_td_df_fator)[1:(M-1)]] <- 
  lapply( matriz_td_df_fator[names(matriz_td_df_fator)[1:(M-1)]], factor) # the "[]" keeps the dataframe structure
class(matriz_td_df_fator[,1])

matriz_td_treino_fator = matriz_td_df_fator

save(matriz_td_df_fator,file="matriz_td_treino_fator.Rdata")

