library(tidytext)
library(tidyverse)
library(wordcloud)
library(yarrr)
library(dplyr)

load("banco-indice-doc-token.Rdata")
banco_tidy = banco_indice_doc_token
dim(banco_tidy) # 123769 3

google.cols <- piratepal(palette = "google", 
                         trans = .2)


##############################################################################################
############################ TERM DOC MATRIX ###############################################
#############################################################################################
freq=banco_tidy%>%
  count(ROTULO,word) #calcular porcentagens

freq = freq %>% mutate(R1 = ifelse(ROTULO == 1,n,0),
                       R0 = n - R1)

freq%>% arrange(desc(n))
freq%>% arrange(desc(R0))
freq%>% arrange(desc(R1))

#remove the terms: “dress’’, “fit’’, “love’’, “size’’, “top’’, “wear’’, “color”
dim(banco_tidy) #123769 3
words1 = c("dress","fit","love","size","top","wear","color")
for(i in 1:length(words1)){
  banco_tidy = banco_tidy %>% filter(word != words1[i])
}
dim(banco_tidy) #105849

freq=banco_tidy%>%
  count(ROTULO,word) #calcular porcentagens

freq = freq %>% mutate(R1 = ifelse(ROTULO == 1,n,0),
                       R0 = n - R1)

freq%>% arrange(desc(n))
freq%>% arrange(desc(R0))
freq%>% arrange(desc(R1))

#remove the terms: “fabric’’, “cute’’, “shirt’’, “run”, 
words2 = c("fabric", "cute", "shirt", "run")
for(i in 1:length(words2)){
  banco_tidy = banco_tidy %>% filter(word != words2[i])
}
dim(banco_tidy) #101196 3


freq=banco_tidy%>%
  count(ROTULO,word) #calcular porcentagens

freq = freq %>% mutate(R1 = ifelse(ROTULO == 1,n,0),
                       R0 = n - R1)

freq%>% arrange(desc(n))
freq%>% arrange(desc(R0))
freq%>% arrange(desc(R1))

#remove the terms: “pretty”, “beautiful”
words3 = c("pretty","beautiful")  
for(i in 1:length(words3)){
  banco_tidy = banco_tidy %>% filter(word != words3[i])
}
dim(banco_tidy) #99431 3

freq=banco_tidy%>%
  count(ROTULO,word) #calcular porcentagens

freq = freq %>% mutate(R1 = ifelse(ROTULO == 1,n,0),
                       R0 = n - R1)

freq%>% arrange(desc(n))
freq%>% arrange(desc(R0))
freq%>% arrange(desc(R1))

#remove the terms: “short”, “sweater”
words4 = c( "short" ,"sweater")
for(i in 1:length(words4)){
  banco_tidy = banco_tidy %>% filter(word != words4[i])
}
dim(banco_tidy) #97802 3

freq=banco_tidy%>%
  count(ROTULO,word) #calcular porcentagens

freq = freq %>% mutate(R1 = ifelse(ROTULO == 1,n,0),
                       R0 = n - R1)

freq%>% arrange(desc(n))
freq%>% arrange(desc(R0))
freq%>% arrange(desc(R1))

#remove the terms:  “material’’, “nice” 
words5 = c("material", "nice")
for(i in 1:length(words5)){
  banco_tidy = banco_tidy %>% filter(word != words5[i])
}
dim(banco_tidy) #96135 3

freq=banco_tidy%>%
  count(ROTULO,word) #calcular porcentagens

freq = freq %>% mutate(R1 = ifelse(ROTULO == 1,n,0),
                       R0 = n - R1)

freq%>% arrange(desc(n))
freq%>% arrange(desc(R0))
freq%>% arrange(desc(R1))

#remove the terms:  “buy”
words6 = c("buy")
for(i in 1:length(words6)){
  banco_tidy = banco_tidy %>% filter(word != words6[i])
}
dim(banco_tidy) #95253  3

freq=banco_tidy%>%
  count(ROTULO,word) #calcular porcentagens

freq = freq %>% mutate(R1 = ifelse(ROTULO == 1,n,0),
                       R0 = n - R1)

freq%>% arrange(desc(n))
freq%>% arrange(desc(R0))
freq%>% arrange(desc(R1))
#no more removes 

freq = freq %>% 
  group_by(word) %>% 
  summarise(across(everything(), sum))%>% 
  select(word,n,R1,R0)



freq%>% arrange(desc(n))
(freq%>%arrange(desc(n)))$word[1:10] #Figure 3

freq%>% arrange(desc(R1))
(freq%>%arrange(desc(R1)))$word[1:10]  #Figure 3

freq%>% arrange(desc(R0))
(freq%>%arrange(desc(R0)))$word[1:10]  #Figure 3



#select the first 100 terms
(freq%>%arrange(desc(n)))[100:110,]
freq = freq%>%filter(n>196)
dim(freq) #100 4
M = 100
cte = M/sum(freq$n)

#salvar o objeto freq para futura analise de sentimento
save(freq,file="freq_obj.Rdata")


par(mar=c(19,11,4,2))
valores = ((freq%>%arrange(desc(n)))[1:10,]$n)*cte
#Figure 3
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





#term documents matrix
termos = freq$word
M = length(termos)
M #100

tab=table(banco_tidy[[1]])
tab[1:10]
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
matriz_termo_docuemnto["6",1:20]
sort(banco_tidy[[3]][banco_tidy$INDICE==6])

table(rotulos[,1]) #2839 2779
dim(rotulos) #5674
sum(is.na(rotulos))
rotulos[209,]
banco_tidy[[3]][banco_tidy$INDICE==915]
#there are empty rows

linhas_so_com_zero = NULL
for(i in 1:nrow(matriz_termo_docuemnto)){
  linha = matriz_termo_docuemnto[i,]
  if(sum(linha)==0)
    linhas_so_com_zero = c(linhas_so_com_zero,i)
}
linhas_so_com_zero
matriz_td_df = data.frame(matriz_termo_docuemnto)
class(matriz_td_df)
dim(matriz_td_df)
colnames(matriz_td_df)
rownames(matriz_td_df)
#first index 
ROTULO = rotulos
INDICE = names(tab)
matriz_td_df = cbind(matriz_td_df,ROTULO,INDICE)
dim(matriz_td_df) #5674

#second remove empty rows
matriz_td_df[linhas_so_com_zero,]
matriz_td_df = matriz_td_df[-linhas_so_com_zero,]


linhas_so_com_zero = NULL
for(i in 1:nrow(matriz_td_df)){
  if(sum(matriz_td_df[i,-c(M+1,M+2)])==0)
    linhas_so_com_zero = c(linhas_so_com_zero,i)
}
linhas_so_com_zero
#no more empty rows

sum(is.na(matriz_td_df$ROTULO))
#no na


names(matriz_td_df)
dim(matriz_td_df) #5618 100

matriz_td_treino = matriz_td_df
save(matriz_td_treino,file="matriz_td_treino.Rdata")

#vamos salvar tambem o banco ja transformado para fator
M = dim(matriz_td_df)[2]
matriz_td_df_fator = matriz_td_df
matriz_td_df_fator[names(matriz_td_df_fator)[1:(M-1)]] <- 
  lapply( matriz_td_df_fator[names(matriz_td_df_fator)[1:(M-1)]], factor) # the "[]" keeps the dataframe structure
class(matriz_td_df_fator[,1])

matriz_td_treino_fator = matriz_td_df_fator

save(matriz_td_df_fator,file="matriz_td_treino_fator.Rdata")
