#Project main script, Big Data for Social Analysis, Horn Péter
#help: https://stackoverflow.com/questions/38119447/using-r-for-webscraping-http-error-503-despite-using-long-pauses-in-program
#-------------------------------------------------RUN From here-------------------------------------------------
setwd("C:/Users/T440s/OneDrive - Central European University/CEU/ECON_Pol 2. semester/Big_data/project")

#used libraries
install.packages('qdap')
install.packages('httr')
install.packages('RColorBrewer')
install.packages('dplyr')
install.packages('rvest')
install.packages('ggmap')
install.packages('leaflet')
install.packages('stringr')
install.packages('xml2')
install.packages('purrr')
install.packages('ggeffects')

library(httr)
library(RColorBrewer)
library(dplyr)
library(rvest)
library(ggmap)
library(leaflet)
library(stringr)
library(xml2)
library(httr)
library(purrr)
library(bitops)
library(RColorBrewer)
library(NLP)
library(RCurl)
library(tm)
library(SnowballC)
library(wordcloud)
library(readr)
library(qdap)
require(quanteda)
require(quanteda.corpora)
library(bitops)
library(RCurl)
library(SnowballC)
library(wordcloud)
library(readr)
require(ggplot2)
library(ggeffects)
require(tm)
require(lubridate)
library(rjson)
require(readtext)
library(ISLR)
library(leaps)
#-------------------------------------------------RUN until here-------------------------------------------------
#getting URL ready (politico.com search: "economy"), scareped: 2019:02.04.

info <- GET('https://www.politico.com/search/1?q=economy&adv=true&c=0000014b-324d-d4f3-a3cb-f3ff415e0035')
print(info)


# ---------------------------------------------Creating article title dataset ----------------------------------------------------------------------------------------
# data collected at: 2019.02....
## problem with potentialy changing source data durring scraping (politico has a prevention against DOS attacks so big lags were needed)
datalist = list()
for (i in 1:1105){
  #print the link you are collecting
  print(paste("https://www.politico.com/search/",toString(1*i),toString("?q=economy&adv=true&c=0000014b-324d-d4f3-a3cb-f3ff415e0035"),sep="")) #we know where the loop is
  #get the link
  url <-read_html(paste("https://www.politico.com/search/",toString(1*i),toString("?q=economy&adv=true&c=0000014b-324d-d4f3-a3cb-f3ff415e0035"),sep="")) 
  
  title <- url %>%
    html_nodes(".summary a") %>%
    html_text()
    table(title)
  
  pol_url <- url %>%
    html_nodes(".summary a") %>%
    html_attr("href")
    table(pol_url)
    Sys.sleep(59)
  
  a <- data.frame(title,pol_url)
  datalist[[i]] <- a

}

datalist2 = list()
for (i in 842:1105){
  #print the link you are collecting
  print(paste("https://www.politico.com/search/",toString(1*i),toString("?q=economy&adv=true&c=0000014b-324d-d4f3-a3cb-f3ff415e0035"),sep="")) #we know where the loop is
  #get the link
  url <-read_html(paste("https://www.politico.com/search/",toString(1*i),toString("?q=economy&adv=true&c=0000014b-324d-d4f3-a3cb-f3ff415e0035"),sep="")) 
  
  title <- url %>%
    html_nodes(".summary a") %>%
    html_text()
  table(title)
  
  pol_url <- url %>%
    html_nodes(".summary a") %>%
    html_attr("href")
  table(pol_url)
  Sys.sleep(35)
  
  a <- data.frame(title,pol_url)
  datalist2[[i]] <- a
  
}

print(datalist2)

datalists=append(datalist, datalist2)

#creating a dataframe for titles and urls
data_title= do.call(rbind, datalists)

write.csv(data_title, file = "data_title.csv")

#removing articles, where subscription would be needed (could be bias)
data_title_nosub <- subset(data_title, grepl('https://www.politico.com/', data_title$pol_url))

write.csv(data_title_nosub, file = "data_title_nosub.csv")

#----------------------------------------------Creating article text dataset----------------------------------------------------------------------
data_title_nosub = read.csv("data_title_nosub.csv", header = TRUE)
data_title_nosub <- subset(data_title_nosub, select = c(title, pol_url))
#----------------------------------------creating a list for urls
urls_list <- list()
for (i in data_title_nosub[3]){
  urls_list <- i
}

#Have to get subsamples because of error 503 and additional bad links (error 401)
#--------------------------------------text sub-data 1
print(urls_list[2405])
urls_list <- urls_list[-2405]
#loop over urls collected above

data_main <- data.frame()
for (i in urls_list){

  link <- i
  print(link)
  url <- read_html(link)
  #print(url)
  
  text <- url %>% 
    html_nodes(".story-text") %>%
    html_text()
  #print(text)
  Sys.sleep(1)
  
  z <- data.frame(link,text)
  data_main <- rbind(data_main,z)
  
}


#--------------------------------------text sub-data 2

data_text = read.csv("data_text.csv", header = TRUE) 
data_text <- subset(data_text, select = c(link, text))

print(urls_list[19525])

data_main2 <- data.frame()
for (i in urls_list[19525:19527]){ 
  
  link <- i
  print(link)
  url <- read_html(link)
  #print(url)
  
  text <- url %>% 
    html_nodes(".story-text") %>%
    html_text()
  #print(text)
  Sys.sleep(sample(1:5,1)*0.356)
  
  z <- data.frame(link,text)
  data_main2 <- rbind(data_main2,z)
  
}
 

data_text = rbind(data_text, data_main2)

print(urls_list[2405])

write.csv(data_text, file = "data_text.csv")
data_text = read.csv('data_text.csv', header = TRUE)
data_title_nosub = read.csv('data_title_nosub.csv', header = TRUE)
#--------------------------------merging title data with text data
total = merge(data_title_nosub, data_text, by.x ="pol_url", by.y = "link")
total = unique(total)
write.csv(total, file = "project-data_total.csv")
write.table(total, file = "project-data_total2.csv")
#-------------------------------trying to clean text
total = read.csv("project-data_total.csv", header = TRUE)
total <- subset(total, select = c(pol_url,title,text))
total = unique(total)
total$text = gsub('\\s+',' ',total$text)
total$text = gsub('.cms-textAlign-center', '', total$text)
total$text = gsub('.cms-textAlign-left', '', total$text)
total$text = gsub('.cms-textAlign-right', '', total$text)
total$text = gsub('[[:punct:]]', ' ', total$text) # removes all speacial caracters
total$text = gsub('text align left   text align center   text align right   cms magazineStyles smallCaps font variant small caps   cms playbookStyle rubric', '', total$text)
total$text = gsub('   color  b70000 font weight bold font family sans serif  ', '', total$text)
total$text = gsub('”', '',total$text)
total$text = gsub('“', '',total$text)
total$text = gsub("’s", '',total$text)
print(total$text[3])
write.csv(total, file = "project-data_total.csv")

#---------------------------------------------------Creating a clean corpus - RUN from here--------------------------------------
total = read.csv("project-data_total.csv", header = TRUE)
total <- subset(total, select = c(pol_url, title, text))
total_subsamp <- sample_n(total, 100)

#creating a corpus out of the list of articles
#articles_corpus <- Corpus(VectorSource(total_subsamp$text))
articles_corpus <- Corpus(VectorSource(total$text))

# Removing extra spaces 

articles_corpus <- tm_map(articles_corpus, stripWhitespace)

# Remove punctuation

articles_corpus <- tm_map(articles_corpus, removePunctuation)

# Remove numbers

articles_corpus <- tm_map(articles_corpus, removeNumbers)

# All lower letters

articles_corpus <- tm_map(articles_corpus, tolower)

# Remove stopwords via functions dictionary

articles_corpus <- tm_map(articles_corpus, removeWords, stopwords("english"))

# Stemming words

articles_corpus <- tm_map(articles_corpus, stemDocument, language= "english")
#------------------------------------------------- Creating document-term matrix and term-document matrix

#creating a document-term matrix form words
articles_dtm <-DocumentTermMatrix(articles_corpus)
#creating a term-document matrix
articles_tdm <- TermDocumentMatrix(articles_corpus)
#articles_dtm <-removeSparseTerms(dtm, sparse=.97) - to remove unique worlds (not sure if good idea)


# View parts of the dtm matrix
inspect(articles_dtm[1:3, 20:30])
View(as.matrix(articles_dtm[1:5, 200:205]))
findFreqTerms(articles_dtm, 30000) # words used more than x times

#zipfs-law distribution of words (stands)
Zipf_plot(articles_dtm)

#----------------------Creating document-term matrix only with words that I have chosen to explain the appearance of inflation 
#creating list of words
econ_words <- c("inflat", "interest", "invest", "job", "financi", "investor", "labor", "gdp", "employ", "unemploy", "deflat", "busi", "tax", "money", "profit", "output", "save")

#creating dtm
econ_dtm <- DocumentTermMatrix(articles_corpus, control=list(dictionary = econ_words))
econ_tdm <- TermDocumentMatrix(articles_corpus, control = list(dictionary = econ_words))

#creating matrix format 
econ_dtm.matrix <- as.matrix(econ_dtm)
econ_tdm.matrix <- as.matrix(econ_tdm)

# creating wordcloud based of freq
econ_tdm.sort <- sort(rowSums(econ_tdm.matrix),decreasing=TRUE)
econ_tdm.last <- data.frame(word = names(econ_tdm.sort),freq=econ_tdm.sort)
table(econ_tdm.last$freq)
pal2 <- brewer.pal(9,"BrBG")
png(file = "szofelho.png", width=1280, height=800)
wordcloud(econ_tdm.last$word,econ_tdm.last$freq, scale=c(8,.2),min.freq=150,max.words=Inf, random.order=FALSE, rot.per=.15, colors=pal2)
dev.off()

#creating dataframe
econ_dataframe <- as.data.frame(econ_dtm.matrix)
econ_dataframe$inflat_init <- ifelse(econ_dataframe$inflat >= 1, 1, 0)
econ_dataframe <- econ_dataframe[c("inflat_init", "inflat", "interest", "invest", "job", "financi", "investor", "labor", "gdp", "employ", "unemploy", "deflat", "busi", "tax", "money", "profit", "output", "save" )]


#--------------------------------------------------------------DATA ANALYSIS-----------------------------------------------------
#help: https://stats.idre.ucla.edu/r/dae/logit-regression/, https://cran.r-project.org/web/packages/margins/vignettes/Introduction.html 
install.packages('margins')
install.packages('ROCR')
install.packages('caTools')
install.packages('caret')
install.packages('AER')
install.packages("texreg", repos = "http://R-Forge.R-project.org")

library(margins)
library(ROCR)
library(caTools)
require(caret)
library(AER)
library(texreg)
#----------------------------- 1st model - simple logit regression with test-train spilit validation ----------
set.seed(30) #seed is the random number the random sample will start with (so we always get the same samples if seed is the same)
#simple test-train split 70 percent is the training 30 percent for test
splt = sample.split(econ_dataframe, SplitRatio = 0.7)
Train = subset(econ_dataframe, splt==TRUE)
Test = subset(econ_dataframe, splt==FALSE)

mylogit <- glm(inflat_init ~ interest + invest + job + financi + investor + labor + gdp + employ + unemploy + deflat + busi + tax + money + profit + output + save, data = Train, family = binomial)
summary(mylogit)
coef(mylogit)

#saving regression output to word file
htmlreg(mylogit, file = 'mylogit.doc')

#make predictions on the held-out data and plot the ROC curve (Receiver operating characteristic)
PredictROC = predict(mylogit, newdata = Test, type="response") # predicting if inflation in text in test data using mylogit model
PredictROC
pred = prediction(PredictROC, Test$inflat_init)
perf = performance(pred, "tpr", "fpr") # checking the performance of the model in the test data
png(filename="tpr.png")
tpr = plot(perf, col = 'blue', lwd = 1, lty = 1, main = "Receiver Operating Characteristic (ROC) curve") + lines(x = c(0,100), y = c(0,100), lwd = 1, lty = 2)
dev.off()

#marginal effects
png(file = 'gdp_marg-eff.png')
gdp_marg <- ggpredict(mylogit, 'gdp')
plot(gdp_marg) + labs(title = "Marginal effect of GDP is in article", y = "prob. of inflation is in article", x = "occurance of GDP in article")
dev.off()

png(file = 'interest_marg-eff.png')
interest_marg <- ggpredict(mylogit, 'interest')
plot(interest_marg) + labs(title = "Marginal effect of interest is in article", y = "prob. of inflation is in article", x = "occurance of interest in article")
dev.off()

png(file = 'unemploy_marg-eff.png')
unemp_marg <- ggpredict(mylogit, 'unemploy')
plot(unemp_marg) + labs(title = "Marginal effect of unemployment (unemploy) is in article", y = "prob. of inflation is in article", x = "occurance of unemploy in article")
dev.off()

#MSE = 0.0346266
econ_dataframe.test = Test$inflat_init
yhat = predict(mylogit, newdata = Test, type = "response")
yhat
mean((yhat - econ_dataframe.test)^2)

#---------------------------------2nd model - simple logit reg. with k-fold cross vaildation----------------------------------------------------------
#help: https://stats.stackexchange.com/questions/52274/how-to-choose-a-predictive-model-after-k-fold-cross-validation

# predict function for the regsubset function (from the internet): source: https://lagunita.stanford.edu/c4x/HumanitiesSciences/StatLearning/asset/ch6.html
predict.regsubsets = function(object, newdata, id, ...) {
  form = as.formula(object$call[[2]])
  mat = model.matrix(form, newdata)
  coefi = coef(object, id = id)
  mat[, names(coefi)] %*% coefi
}

#k-fold validation modeling
set.seed(30)
k = 20
k_folds=sample(1:k,nrow(econ_dataframe),replace=TRUE) #sampling the data t k-fold
cross_val.errors = matrix(NA,k,16, dimnames = list(NULL, paste(1:16))) #creating an empty matrix for cross validation errors

for (j in 1:k){
  logit_best.fit = regsubsets(inflat_init ~interest + invest + job + financi + investor + labor + gdp + employ + unemploy + deflat + busi + tax + money + profit + output + save, data = econ_dataframe[k_folds != j,], nvmax = 16) # family("binomial") not working?
  for (i in 1:16){
    pred = predict(logit_best.fit, econ_dataframe[k_folds == j,], id = i, type = "response")
    cross_val.errors[j,i] = mean( (econ_dataframe$inflat_init[k_folds == j] - pred)^2) #calculating mean RSS (obs-predicted)^2 (residual sum of squares) rss = sum(yi-f(xi))^2 and adding them to the matrix
  }
}
cross_val.errors.mean = apply(cross_val.errors,2, mean) #mean cross-validation errors
cross_val.errors.mean
par(mfrom = c(1,1))
plot(cross_val.errors.mean) #plotting the mean errors

#ploting cross-valid. errors
png(file = "cv_errors.png")
plot(cross_val.errors.mean, xlab = "Number of predictors", ylab = "Cross- validation RSS-s", main = "Residual Sum of Squares with dif. predictor #")
dev.off()

logit_reg.best = regsubsets(inflat_init~interest + invest + job + financi + investor + labor + gdp + employ + unemploy + deflat + busi + tax + money + profit + output + save, data = econ_dataframe, nvmax = 12) # in this case the best model is when I include 12 vars.
coef(logit_reg.best, 12)
#MSE smallest = 0.0333194 
cross_val.errors.mean[12]


#----------------------------------------3rd model - decision tree ---------------------------------------------
library(tree)
library(ISLR)
library(MASS)
library(randomForest)
library(gbm)

set.seed(30)
splt = sample.split(econ_dataframe, SplitRatio = 0.7)
train = subset(econ_dataframe, splt==TRUE)
test = subset(econ_dataframe, splt==FALSE)
tree.econ_dataframe = tree(inflat_init ~ interest + invest + job + financi + investor + labor + gdp + employ + unemploy + deflat + busi + tax + money + profit + output + save, data = train) #same as the simple regression 
summary(tree.econ_dataframe)
plot(tree.econ_dataframe)
text(tree.econ_dataframe,pretty=0)
cross_val.econ_dataframe = cv.tree(tree.econ_dataframe)
plot(cross_val.econ_dataframe$size, cross_val.econ_dataframe$dev, type = 'b')
prune.econ_dataframe = prune.tree(tree.econ_dataframe, best = 4)
plot(prune.econ_dataframe)
text(prune.econ_dataframe)

#ploting decision tree
png(file = "decision_tree.png")
plot(tree.econ_dataframe)
text(tree.econ_dataframe, pretty = 0)
dev.off()

#plotting cross-valid errors
png(file = "cv_tree.png")
plot(cross_val.econ_dataframe$size, cross_val.econ_dataframe$dev, type = 'b', xlab = "Number of nodes", ylab = "Error rate", main = "Residual Sum of Squares with dif. predictor #")
dev.off()

#MSE = 0.0325837
yhat = predict(prune.econ_dataframe, newdata=test)
test.econ_dataframe = test$inflat_init
mean((yhat - test.econ_dataframe)^2)
