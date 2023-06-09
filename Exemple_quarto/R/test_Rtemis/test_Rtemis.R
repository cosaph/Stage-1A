## ----appel Rtemis-----------------------
library(R.temis)
library(tidyverse)
library(RTextTools)


## ----Import-------------------------------------------------------------------------
corpus1 <- import_corpus("C:/Users/cottet_cor/Documents/Stage/Exemple_quarto/R/test_Rtemis",format="txt",language="fr")





corpus <- import_corpus("C:/Users/cottet_cor/Documents/Stage/Exemple_quarto/R/Nuage_de_mot/genre.csv",format="csv",textcolumn=1, language="fr")
#il faut mettre un lien de fichier et non pas un lien d'un seul document (demander pourquoi)

corpus
inspect (corpus[[1]])

## ----  Import document texte -------------------------------------------------------



## ----DTM-Construction du talbleau Lexical---------------------------------------------------------------------------
dtmsmo <-build_dtm(corpus,remove_stopwords = T, min_length = 0)

## ----Lexique--------------------------------------------------------
dictionary(dtmsmo)


## ----lecture dico-------------------------------------------------------------------
dic <-dictionary(dtmsmo)

dic2 = dic %>%
  rownames_to_column(var="word") %>% 
  mutate(Term = word)

dic2$Term[dic2$word == "analyse"] <- "analyses"
dic2$Term[dic2$word == "application"] <- "applications"
dic2$Term[dic2$word == "traitement"] <- "traitements"

row.names(dic2) <- dic2$word
dic3 <- dic2[,c(2:4)]

#write.csv2(dic, file="dic.csv")
#dic_l <- read.csv2("dic_l.csv",row.names=1)
#dtmlem <-combine_terms(dtmsmo, dic_l)

#setdiff(rownames(dic3), rownames(dic))
dtmlem <-combine_terms(dtmsmo, dic3)

#head(dic3)

## ----Frequence des mots-------------------------------------------------------------
frequent_terms(dtmlem)
frequent_terms(corpus1)

## ----cloud--------------------------------------------------------------------------
cloud<-word_cloud(dtmlem, color="black", min.freq=2)
#cloud<-word_cloud(dtmsmo, color="black", min.freq=2)

## Graphe de mots
terms_graph(dtmlem, interactive=T)
terms_graph(dtmlem, min_occ=1,interactive=T)

## ----concord------------------------------------------------------------------------
concordances(corpus,dtmlem,"forêts")
#concordances(corpus,dtmsmo,"mother")





