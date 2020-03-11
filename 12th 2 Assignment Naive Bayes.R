sms <- read.csv(file.choose())
View(sms)
class(sms)
str(sms)
sms$type <- factor(sms$type)
str(sms)
table(sms$type)
#Building a corpus using text mining
library(tm)
install.packages("textcat")
library(textcat)
table(textcat(x=sms$type),sms$type)
#latvian messages are spam
#Creating corpus
sms_corpus <- Corpus(VectorSource(sms$text))
#Clean up the corpus using tm_map()
Corpus_clean<-tm_map(sms_corpus,tolower)
Corpus_clean<-tm_map(Corpus_clean,removeNumbers)
Corpus_clean<-tm_map(Corpus_clean,removeWords,stopwords())
Corpus_clean<-tm_map(Corpus_clean,removePunctuation)
Corpus_clean<-tm_map(Corpus_clean,stripWhitespace)
class(Corpus_clean)
Corpus_clean$content[1:10]
#Creating a Document term matrix
sms_dtm<-DocumentTermMatrix(Corpus_clean)
class(sms_dtm)
#creating training and test datasets
set.seed(101)
split <- sample(1:nrow(sms),nrow(sms)*0.7,F)
tr_sms <- sms[split,]
ts_sms <- sms[-split,]
#Splitting the document term matrix
tr_dtm <- sms_dtm[split,]
ts_dtm <- sms_dtm[-split,]
#splitting the corpus
tr_cor <- Corpus_clean[split]
ts_cor <- Corpus_clean[-split]
round(prop.table(table(tr_sms$type))*100)
install.packages("wordcloud")
library(wordcloud)
windows()
wordcloud(tr_cor,min.freq = 10,max.words = 100,colors = ifelse(sms$type=="spam","red","green"),random.order = F)
#Reducing the number of columns based on lowest frequency
freq<-findMostFreqTerms(x=tr_dtm,lowfreq=10)
tr_dtm=DocumentTermMatrix(tr_cor,list(dictionary=freq))
ts_dtm= DocumentTermMatrix(ts_cor,list(dictionary = freq))
nrow(tr_dtm)
dim(tr_dtm)
nrow(ts_dtm)
dim(ts_dtm)
counts=function(x){
  x= ifelse(x>0,1,0)
  x= factor(x,levels = c(0,1), labels = c("no","yes"))
}
tr_dtm = apply(tr_dtm,MARGIN = 2,counts);table(tr_dtm)
tr_dtm
ts_dtm =apply(ts_dtm,MARGIN = 2,counts);table(ts_dtm)
ts_dtm
#Building naive bayes Model
library(e1071)
model <- naiveBayes(tr_dtm,tr_sms$type)
pred <- predict(model,ts_dtm)
table(pred,ts_sms$type)
mean(pred==ts_sms$type)
#Model Accuracy is 97.78%
library(gmodels)
CrossTable(pred,ts_sms$type,prop.chisq = F,prop.r = F,prop.t = F)
#Build Another Model 
model2 <- naiveBayes(tr_dtm,tr_sms$type,laplace = 1)
pred2 <- predict(model2,ts_dtm)
mean(pred2==ts_sms$type)
library(gmodels)
CrossTable(pred,ts_sms$type,prop.chisq = F,prop.r = F,prop.t = F)

