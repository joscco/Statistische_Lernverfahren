#############################################################################################################################
#############################################################################################################################
#############################################################################################################################

## Umgebung leeren ## 

rm(list=ls(all=TRUE))

## Daten laden ## 
path_reviews_preprocessed_german  <- "Statistische-Lernverfahren-Seminar/Uniprojekt_Meeting_2/reviews_preprocessed_ALT2.csv"
path_reviews_preprocessed_english  <- "Statistische-Lernverfahren-Seminar/Uniprojekt_Meeting_2/reviewEnglisch_preprocessed2.csv"
reviews_preprocessed <- read.csv(path_reviews_preprocessed_german,colClasses=c("factor","character","character","factor"))

path_reviews_german <- "Statistische-Lernverfahren-Seminar/Uniprojekt_Meeting_2/reviews.csv"
path_reviews_english <- "Statistische-Lernverfahren-Seminar/Uniprojekt_Meeting_2/reviewsEnglisch.csv"
reviews <- read.csv(path_reviews_german,colClasses=c("character","factor"))

# Weitere Variablen/Spalten in der Document-Term-Matrix
n <- nrow(reviews)

# Zählen der Vorkommen von jeder Wortart mithilfe von Python/Spacy

wortarten <- levels(reviews_preprocessed$wortart)
d <- length(wortarten)

for(i in 1:d) {
  count_vector <- rep(1,n)
  current_type <- wortarten[i]
  
  for(j in 1:n) {
    count_vector[j] <- nrow(subset(reviews_preprocessed, doc_id == j & wortart == wortarten[i]))
  }
  column_type <- paste("count_of_", current_type)
  reviews[[column_type]] <- count_vector
}
# View(reviews)

# Wörter zählen nicht mehr nötig, da wir bereits die Anzahl von jedem Worttypen zählen
# count_of_words <- rep(1, n)
# for(i in 1:n) {
#   count_of_words[i] <- nrow(reviews_preprocessed[reviews_preprocessed$doc_id == i,])
# }
# reviews$count_of_words <- count_of_words

library(quanteda) # Paket für das Zählen der Sätze (besser als openNLP)
reviews$count_of_sentences <- nsentence(reviews[,1])

# install.packages("openNLP")
# install.packages("openNLPmodels.de", dependencies=TRUE, repos = "http://datacube.wu.ac.at/")
# library(openNLP)

# german_token_annotator <- Maxent_Sent_Token_Annotator(language = "de")
# test <- annotate(reviews[49,1], german_token_annotator)
# test

# Zähle Vorkommen von Satzzeichen:
library(stringr)
count_of_punctation <- rep(1, n)
for(i in 1:n) {
  count_of_punctation[i] <- str_count(reviews[i,1], "[,.!?:;\"\']")
}
reviews$count_of_punctation <- count_of_punctation


## Pakete laden ## 

library(dplyr)
library(tidytext)
library(caret)

#############################################################################################################################
#############################################################################################################################
#############################################################################################################################

## create dictionary ## 

dictionary <- reviews_preprocessed %>% group_by(preprocessed_text) %>% count()

nrow(dictionary[dictionary$n >= 50,])
nrow(dictionary[dictionary$n < 50,])/nrow(dictionary)

# Mehr als die Hälfte aller Wörter kommt nur ein einziges Mal vor
# Wähle also (erst einmal) 50 als Grenze

remove_those <- which(dictionary$n < 50)
remove_those_words <- dictionary[remove_those,]
dictionary2 <- dictionary[-remove_those,]
nrow(dictionary2)
remove_those_words

# Wörter auch aus dem Datensatz entfernen:

reviews_preprocessed2 <- reviews_preprocessed[!reviews_preprocessed$preprocessed_text %in% remove_those_words$preprocessed_text,]
sum(dictionary2$n) == nrow(reviews_preprocessed2)

#############################################################################################################################
#############################################################################################################################
#############################################################################################################################

## create document term matrix ## 

# add count to dataset
reviews_preprocessed2$count <- rep(1,nrow(reviews_preprocessed2))

# ! add real count
reviews_preprocessed2 <- reviews_preprocessed2 %>% group_by(doc_id, preprocessed_text) %>% mutate(count = n())
# reviews_preprocessed2 <- as.data.frame(reviews_preprocessed2)
# reviews_preprocessed2 <- distinct(reviews_preprocessed2, doc_id, preprocessed_text, .keep_all = TRUE)
# View(reviews_preprocessed2)


# transform dataset to document term matrix
dtmatrix <- reviews_preprocessed2 %>%
  cast_dtm(doc_id, preprocessed_text, count)
# PROBLEM: cast_dtm sortiert nach dem Faktor doc_id (alphabetisch) und macht dadurch die Reihenfolge kaputt

dtmatrix <- as.data.frame(as.matrix(dtmatrix))
dtmatrix <- dtmatrix[order(as.numeric(rownames(dtmatrix))),, drop=FALSE]

# Matrix korrekt erstellt?
ncol(dtmatrix) == nrow(dictionary2)

dtmatrix$doc_id <- as.factor(1:nrow(dtmatrix))

# add target variable to the document term matrix
reviews$doc_id <- as.factor(1:nrow(reviews))
dtmatrix <- inner_join(dtmatrix,reviews,by='doc_id')

#############################################################################################################################
#############################################################################################################################
#############################################################################################################################

## split dataset in trainings- & testdataset ## 


sample_size <- ceiling(0.8 * nrow(dtmatrix))
set.seed(40211)
train_ind <- sample(seq_len(nrow(dtmatrix)), size = sample_size)

# train_ind <- createDataPartition(dtmatrix$type, p = 0.8, list = FALSE)

####
train_ind <- read.csv("Statistische-Lernverfahren-Seminar/Uniprojekt_Meeting_2/doc_id.csv")$Resample1

train <- dtmatrix[train_ind,]
test <- dtmatrix[-train_ind,]

train <- train[order(as.numeric(rownames(train))),, drop=FALSE]
test <- test[order(as.numeric(rownames(test))),, drop=FALSE]

#View(train)

# write.csv2(dtmatrix, file = "Statistische-Lernverfahren-Seminar/Uniprojekt_Meeting_2/test.csv", row.names = FALSE)

# Um festzustellen, ob unsere "Wortvorkommensgrenze" gut gewählt wurde,
# zähle die Anzahl der Zeilen in der DTM, in denen 0 bei jedem Wort steht.
zaehler <- 0
for(i in 1:n) {
  indikator <- 1
  for(j in 1:nrow(dictionary2)) {
    if(dtmatrix[i, j] > 0) {
      indikator <- 0
      break
    }
  }
  if(indikator == 1) {
    zaehler <- zaehler + 1
  }
}
zaehler
zaehler/n
# ca. 2,5% aller Reviews sind hier also ohne "häufig" vorkommende Wörter.
