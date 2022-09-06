library(tidytext)
library(textdata)
library(tidyverse)
library(wordcloud)
library(yarrr)
library(dplyr)


load("freq_obj.Rdata")
freq%>% arrange(desc(word))
freq%>% arrange(desc(n))
freq%>% arrange(desc(R1))
freq%>% arrange(desc(R0))
M = dim(freq)[1]
M
cte = M/sum(freq$n)


get_sentiments("bing")

dim(freq)
dim(freq_sent)


freq_sent <- freq %>%
  inner_join(get_sentiments("bing"))

freq_sent%>% arrange(desc(R1))
freq_sent%>% arrange(desc(R0))

(freq_sent%>% filter(sentiment == "positive"))$word
(freq_sent%>% filter(sentiment == "negative"))$word

