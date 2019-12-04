library(dplyr)
library(tidytext)
library(caret)
library(e1071)
library(randomForest)

# Erstelle Wörterbücher
path_reviews_preprocessed = "reviews2_new.csv"
reviews_preprocessed = read.csv(path_reviews_preprocessed, fileEncoding="utf-8", colClasses=c("integer", "character", "character", "character", "character"))
reviews_preprocessed$count = rep(1, nrow(reviews_preprocessed))

  # Wörter /Lemmata
  reviews_preprocessed$Lemma = tolower(reviews_preprocessed$Lemma)
  dict_lemma = reviews_preprocessed[c('ID','Lemma')] %>% group_by(ID, Lemma)
  dict_lemma = dict_lemma %>% group_by(Lemma) %>% count()
    ind_lemma = dict_lemma$n > 9
    dict_lemma = dict_lemma[ind_lemma,]
  # Wortart
  dict_pos = reviews_preprocessed %>% group_by(Wortart) %>% count()
  # Wortart verfeinert
  dict_tag = reviews_preprocessed %>% group_by(Tag) %>% count()



# Erstelle DT-Matrix
  # Zusätzliche Features
    # Anzahl Sätze
    satzendezeichen = c('.', '!', '?', '...')
    ind_sez = reviews_preprocessed$Lemma %in% satzendezeichen
    anz_saetze = reviews_preprocessed[ind_sez,] %>% group_by(ID) %>% count()
    anz_saetze$anz_saetze = anz_saetze$n
    anz_saetze = anz_saetze[c('ID','anz_saetze')]
    
    # Anzahl Nebensätze / Trennungen
    satztrennzeichen = c(',', ';', ':', '-', '(', ')')
    ind_stz = reviews_preprocessed$Lemma %in% satztrennzeichen
    anz_nebensaetze = reviews_preprocessed[ind_stz,] %>% group_by(ID) %>% count()
    anz_nebensaetze$anz_nebensaetze = anz_nebensaetze$n
    anz_nebensaetze = anz_nebensaetze[c('ID', 'anz_nebensaetze')]
    
    # Anzahl Wörter
    path_anz_woerter = "Anzahl_Woerter.csv"
    anz_woerter = read.csv(path_anz_woerter, fileEncoding="utf-8", colClasses=c("numeric","integer"))
  
    # Typen
    path_reviews = "reviews2.csv"
    reviews = read.csv(path_reviews, fileEncoding="utf-8", colClasses=c("character","factor"))
    reviews$ID = 1:nrow(reviews)
      # Typenbeschreibung
      typen=data.frame('type' = c('Gewissenhaft','Initiativ','Stetig','Dominant'),
                       'emotional' = c(1,1,-1,-1), 'introvertiert' = c(1,-1,1,-1),
                       'g' = c(1,-1,-1,-1), 'i' = c(-1,1,-1,-1), 's' = c(-1,-1,1,-1), 'd' = c(-1,-1,-1,1),
                       'type_num' = c(1,2,3,4))  
  
dtmatrix = reviews_preprocessed[c('ID','Lemma','count')] %>%
  cast_dtm(ID,Lemma,count)
dtmatrix = as.data.frame(as.matrix(dtmatrix))
#dtmatrix = dtmatrix[order(as.numeric(dtmatrix$ID)),]
cols = dict_lemma$Lemma
dtmatrix = dtmatrix[cols]
dtmatrix_abs = dtmatrix
dtmatrix[dtmatrix > 1] = 1
dtmatrix$ID = 1:nrow(dtmatrix)
dtmatrix_abs$ID = 1:nrow(dtmatrix)
dtmatrix = inner_join(dtmatrix, dtmatrix_abs, by='ID', suffix = c(".ind", ".abs"))

# DT-Matrix für Wortarten
dtmatrix_pos = reviews_preprocessed[c('ID','Wortart','count')] %>%
  cast_dtm(ID,Wortart,count)
dtmatrix_pos = as.data.frame(as.matrix(dtmatrix_pos))
anz = anz_woerter$Anz
for (col in names(dtmatrix_pos)) {
  #print(col)
  dtmatrix_pos[col] = dtmatrix_pos[col] / anz
}
dtmatrix_pos$ID = 1:nrow(dtmatrix_pos)

# Features anreichern
dtmatrix = inner_join(dtmatrix, dtmatrix_pos, by='ID')
dtmatrix = inner_join(dtmatrix, anz_woerter, by='ID')
dtmatrix = left_join(dtmatrix, anz_saetze, by='ID')
dtmatrix$anz_saetze[is.na(dtmatrix$anz_saetze)] = 1
dtmatrix = left_join(dtmatrix, anz_nebensaetze, by='ID')
dtmatrix$anz_nebensaetze[is.na(dtmatrix$anz_nebensaetze)] = 0

reviews = reviews[c('ID','type')]
dtmatrix = inner_join(dtmatrix, reviews, by='ID') 
dtmatrix = inner_join(dtmatrix, typen, by = 'type')

###############################################################################################

exclude = c('ID', 'i', 'd', 's', 'g', 'type', 'emotional', 'introvertiert', 'type_num')
exclude_t = c('ID', 'i', 'd', 's', 'g', 'type', 'emotional', 'introvertiert')
exclude_e = c('ID', 'i', 'd', 's', 'g', 'type', 'introvertiert', 'type_num')
exclude_i = c('ID', 'i', 'd', 's', 'g', 'type', 'emotional', 'type_num')
include = c(dict_pos$Wortart, "Anz", "anz_nebensaetze", "anz_saetze")

X=dtmatrix[,-which(names(dtmatrix) %in% exclude)]


PCA = TRUE
if (PCA){
  X = t(t(X)-colMeans(X))
  Cov=cov(X)
  eig = eigen(Cov)
  blubb = as.matrix(X) %*% as.matrix(eig$vectors[,1:30])
  bla = data.frame(blubb)
} else {
  bla= X
}

bla$ID = 1:nrow(bla)
bla = inner_join(bla, reviews[c('ID','type')], by='ID')
bla = inner_join(bla, typen, by='type')


set.seed(40211)
doc_ids = read.csv("doc_id.csv", encoding = "utf-8")
train_ind = doc_ids$Resample1
#train_ind <- createDataPartition(bla$type, p = 0.8, list = FALSE)
train_bla <- bla[train_ind,]
test_bla <- bla[-train_ind,]
train_ind = createDataPartition(train_bla$type, p = 0.5, list = FALSE)
train1_bla = train_bla[train_ind,]
train2_bla = train_bla[-train_ind,]



for (i in 1:4){
for (j in (i+1):(i+1)){
#plot(bla[,i], bla[,j], col=c("red","blue","green","black")[bla$type])
  
  plot(bla[bla$type=="Initiativ",i],bla[bla$type=="Initiativ",j], col = "green", xlab=paste("X",i), ylab = paste("X",j))
  points(bla[bla$type=="Stetig",i],bla[bla$type=="Stetig",j], col = "black")
  points(bla[bla$type=="Gewissenhaft",i],bla[bla$type=="Gewissenhaft",j], col = "blue")
  points(bla[bla$type=="Dominant",i],bla[bla$type=="Dominant",j], col = "red")
  legend("bottomleft",legend = c("d", "i", "s", "g"), text.col = c("red","green","black","blue"))
  readline(prompt=i)
}
}


nb_type = train(train_bla[,-which(names(train_bla) %in% exclude)], as.factor(train_bla$type), 'nb', 
                trControl=trainControl(method='cv', number=10))
nb_type_pred = predict(nb_type, test_bla)
confusionMatrix(nb_type_pred, as.factor(test_bla$type))

nb_emo = train(train_bla[,include], as.factor(train_bla$emotional), 'nb', 
      trControl=trainControl(method='cv', number=10))
nb_emo_pred = predict(nb_emo, test_bla)
confusionMatrix(nb_emo_pred, as.factor(test_bla$emotional))

nb_intro = train(train_bla[,include], as.factor(train_bla$introvertiert), 'nb', 
           trControl=trainControl(method='cv', number=10))
nb_intro_pred = predict(nb_intro, test_bla)
confusionMatrix(nb_intro_pred, as.factor(test_bla$introvertiert))


###########################################################################################
rf_type = randomForest(x=train_bla[,-which(names(train_bla) %in% exclude)], y = train_bla$type, ntree = 3000)
rf_type_pred = predict(rf_type, test_bla)
confusionMatrix(rf_type_pred, test_bla$type)

rf_emo = randomForest(x=train_bla[,-which(names(train_bla) %in% exclude)], y = train_bla$emotional, ntree = 2000)
rf_emo_pred = sign(predict(rf_emo, test_bla))
#rf_emo_pred = inner_join(data.frame('emotional' = rf_emo_pred), typen, by='emotional')
confusionMatrix(as.factor(rf_emo_pred), as.factor(test_bla$emotional))

rf_intro = randomForest(x=train_bla[,-which(names(train_bla) %in% exclude)], y = train_bla$introvertiert, ntree = 1000)
rf_intro_pred = sign(predict(rf_intro, test_bla))
confusionMatrix(as.factor(rf_intro_pred), as.factor(test_bla$introvertiert))


###########################################################################################
# Lineare Modelle
lm_bla_type = lm(type_num ~ . , data = train_bla[,-which(names(train_bla) %in% exclude_t)])
lm_bla_type_pred = predict(lm_bla_type, test_bla)
lm_bla_type_pred = round(lm_bla_type_pred)
lm_bla_type_pred[lm_bla_type_pred > 4] = 4
lm_bla_type_pred[lm_bla_type_pred < 1] = 1
lm_bla_type_pred = inner_join(data.frame(type_num = lm_bla_type_pred), typen, by='type_num')
confusionMatrix(lm_bla_type_pred$type, test_bla$type)



lm_bla_emo = lm(emotional ~ ., data = train_bla[-which(train_bla$ID %in% c(10)),-which(names(train_bla) %in% exclude_e)])
lm_bla_emo_pred = predict(lm_bla_emo, test_bla)
confusionMatrix(as.factor(sign(lm_bla_emo_pred)), as.factor(test_bla$emotional))

lm_bla_intro = lm(introvertiert ~ X1 + X14, data = train_bla[-which(train_bla$ID %in% c(118, 49, 140, 282, 229, 238, 365, 43, 155, 323)),-which(names(train_bla) %in% exclude_i)])
lm_bla_intro_pred = predict(lm_bla_intro, test_bla)
confusionMatrix(as.factor(sign(lm_bla_intro_pred)), as.factor(test_bla$introvertiert))

lm_bla_zus_pred = data.frame('emotional'=sign(lm_bla_emo_pred), 'introvertiert'=sign(lm_bla_intro_pred))
lm_bla_zus_pred = inner_join(lm_bla_zus_pred, typen, by=c('emotional', 'introvertiert'))
confusionMatrix(lm_bla_zus_pred$type, test_bla$type)

######################################################################################
rf_emo = randomForest(x=train1_bla[,-which(names(train_bla) %in% exclude)], y = train1_bla$emotional, ntree = 1000)
rf_emo_pred = sign(predict(rf_emo, test_bla))
confusionMatrix(as.factor(rf_emo_pred), as.factor(test_bla$emotional))

rf_intro = randomForest(x=train1_bla[,-which(names(train_bla) %in% exclude)], y = train1_bla$introvertiert, ntree = 1000)
#confusionMatrix(as.factor(rf_intro_pred), as.factor(train2_bla$introvertiert))
rf_intro_pred = sign(predict(rf_intro, train2_bla))
train3_bla = train2_bla
train3_bla$intro = (rf_intro_pred -1)/2
rf_emo = randomForest(x=train3_bla[,-which(names(train_bla) %in% exclude)], y = train3_bla$type, ntree = 1000)
rf_intro_pred = sign(predict(rf_intro, test_bla))
test2_bla = test_bla
test2_bla$intro = (rf_intro_pred -1)/2
rf_emo_pred = predict(rf_emo, test2_bla)
confusionMatrix(rf_emo_pred, test_bla$type)


rf_bla_zus_pred = data.frame('emotional'=sign(rf_emo_pred), 'introvertiert'=sign(rf_intro_pred))
rf_bla_zus_pred = inner_join(rf_bla_zus_pred, typen, by=c('emotional', 'introvertiert'))
confusionMatrix(rf_bla_zus_pred$type, test_bla$type)


####
nb_intro = train(train1_bla[,include], as.factor(train1_bla$introvertiert), 'nb', 
                 trControl=trainControl(method='cv', number=10))
nb_intro_pred = predict(nb_intro, test_bla)
confusionMatrix(nb_intro_pred, as.factor(test_bla$introvertiert))

nb_intro_pred = as.integer(predict(nb_intro, train2_bla))
train3_bla = train2_bla
train3_bla$intro = (nb_intro_pred -2)
rf_emo = randomForest(x=train3_bla[,-which(names(train_bla) %in% exclude)], y = train3_bla$type, ntree = 3000)
nb_intro_pred = as.integer(predict(nb_intro, test_bla))
test2_bla = test_bla
test2_bla$intro = (nb_intro_pred -2)
rf_emo_pred = predict(rf_emo, test2_bla)
confusionMatrix(rf_emo_pred, test_bla$type)


rf_type = randomForest(x=train_bla[,-which(names(train_bla) %in% exclude)], y = train_bla$type, ntree = 3000)
rf_type_pred = predict(rf_type, test_bla)
confusionMatrix(rf_type_pred, test_bla$type)


#####
library(leaps)
library(faraway)
lm = leaps(train_bla[,dict_pos$Wortart[-15]], train_bla$emotional)
Cpplot(lm)

regsubsets(train_bla[,dict_pos$Wortart], train_bla$emotional)

                     