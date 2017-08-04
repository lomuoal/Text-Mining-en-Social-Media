library(rJava)
library(qdap)
library(XML)
library(tm)
library(splitstackshape)
library(caret)
library(party)
library(arm)
library(pls)
library(LiblineaR)
library(SnowballC)

GenerateVocabulary <- function(path,stemming=TRUE, n = 1000,acentos=TRUE, lowcase = TRUE, punctuations = TRUE, numbers = TRUE, whitespaces = TRUE, swlang = "", swlist = "", verbose = TRUE) {
  setwd(path)
  
  files = list.files(pattern="*.xml")
  
  corpus.raw <- NULL
  i <- 0
  for (file in files) {
    xmlfile <- xmlTreeParse(file, useInternalNodes = TRUE)
    corpus.raw <- c(corpus.raw, xpathApply(xmlfile, "//document", function(x) xmlValue(x)))
    i <- i + 1
    if (verbose) print(paste(i, " ", file))	
  }
  
  corpus.preprocessed <- corpus.raw
  
  if (acentos){
    if (verbose) print("Eliminado acentos...")
    corpus.preprocessed <- chartr('áéíóúñ','aeioun',corpus.preprocessed)
  }
  
  if (lowcase) {
    if (verbose) print("Tolower...")
    corpus.preprocessed <- tolower(corpus.preprocessed)
  }	
  
  if (punctuations) {
    if (verbose) print("Removing punctuations...")		
    corpus.preprocessed <- removePunctuation(corpus.preprocessed)
  }
  
  if (numbers) {
    if (verbose) print("Removing numbers...")
    corpus.preprocessed <- removeNumbers(corpus.preprocessed)
  }
  
  if (whitespaces) {
    if (verbose) print("Stripping whitestpaces...")
    corpus.preprocessed <- stripWhitespace(corpus.preprocessed)
  }
  
  if (swlang!="")	{
    if (verbose) print(paste("Removing stopwords for language ", swlang , "..."))
    corpus.preprocessed <- removeWords(corpus.preprocessed, stopwords(swlang))
  }
  
  if (swlist!="") {
    if (verbose) print("Removing provided stopwords...")
    corpus.preprocessed <- removeWords(corpus.preprocessed, swlist)
  }
  
  if (verbose) print("Generating frequency terms")
  
  if(stemming){
    corpus.preprocessed<-stemDocument(corpus.preprocessed,language = "spanish")
  }
  
  corpus.frequentterms <- freq_terms(corpus.preprocessed, n)
  if (verbose) plot(corpus.frequentterms)
  
  return (corpus.frequentterms)
}


GenerateBoW <- function(path, vocabulary,acentos=TRUE, n = 1000, lowcase = TRUE, punctuations = TRUE, numbers = TRUE, whitespaces = TRUE, swlang = "", swlist = "", class="variety", verbose = TRUE, stemming=TRUE) {
  setwd(path)
  
  truth <- read.csv("truth.txt", sep=":", header=FALSE)
  truth <- truth[,c(1,4,7)]
  colnames(truth) <- c("author", "gender", "variety")
  
  i <- 0
  bow <- NULL
  files = list.files(pattern="*.xml")
  for (file in files) {
    author <- gsub(".xml", "", file)
    variety <- truth[truth$author==author,"variety"]
    gender <- truth[truth$author==author,"gender"]
    
    xmlfile <- xmlTreeParse(file, useInternalNodes = TRUE)
    txtdata <- xpathApply(xmlfile, "//document", function(x) xmlValue(x))
    

    if (acentos){
      txtdata <- chartr('áéíóúñ','aeioun',txtdata)
    }
    
    if (lowcase) {
      txtdata <- tolower(txtdata)
    }
    
    if (punctuations) {
      txtdata <- removePunctuation(txtdata)
    }
    
    if (numbers) {
      txtdata <- removeNumbers(txtdata)
    }
    
    if (whitespaces) {
      txtdata <- stripWhitespace(txtdata)
    }
    
    if(stemming){
      txtdata<-stemDocument(txtdata,language = "spanish")
    }
    
    line <- author
    freq <- freq_terms(txtdata, n)
    for (word in vocabulary$WORD) {
      thefreq <- 0
      if (length(freq[freq$WORD==word,"FREQ"])>0) {
        thefreq <- freq[freq$WORD==word,"FREQ"]
      } 
      line <- paste(line, ",", thefreq, sep="")
    }
    if (class=="variety") {
      line <- paste(line, ",", variety, sep="")
    } else {
      line <- paste(line, ",", gender, sep="")
    }
    
    bow <- rbind(bow, line)
    
    i <- i + 1
    
    if (verbose) {
      if (class=="variety") {
        print(paste(i, author, variety))
      } else {
        print(paste(i, author, gender))
      }
    }
  }
  
  return (bow)
}


n <- 1000
path_training <- "/Users/Lorena/Downloads/pan-ap17-bigdata/training/"	# Your training path
path_test <- "/Users/Lorena/Downloads/pan-ap17-bigdata/test/"			# Your test path

vocabulary <- GenerateVocabulary(path_training, n, swlang="es")
vocabulary_nostem <- GenerateVocabulary(path_training, n, swlang="es", stemming=FALSE)

bow_training <- GenerateBoW(path_training, vocabulary, n, class="variety")
bow_test <- GenerateBoW(path_test, vocabulary, n, class="variety")
bow_training_nostem <- GenerateBoW(path_training, vocabulary_nostem, n, class="variety")
bow_test_nostem <- GenerateBoW(path_test, vocabulary_nostem, n, class="variety")

training <- concat.split(bow_training, "V1", ",")
test <- concat.split(bow_test, "V1", ",")

training_nostem <- concat.split(bow_training_nostem, "V1", ",")
test_nostem <- concat.split(bow_test_nostem, "V1", ",")

training <- training[,3:ncol(training)]
names(training)[ncol(training)] <- "class"
truth  <- unlist(test[,ncol(test):ncol(test)])
test <- test[,3:(ncol(test)-1)]

training_nostem <- training_nostem[,3:ncol(training_nostem)]
names(training_nostem)[ncol(training_nostem)] <- "class"
truth_nostem  <- unlist(test_nostem[,ncol(test_nostem):ncol(test_nostem)])
test_nostem <- test_nostem[,3:(ncol(test_nostem)-1)]

#train_control <- trainControl( method="repeatedcv", number = 10 , repeats = 3) 
#model_SVM <- train( class~., data= training, trControl = train_control, method = "svmLinear")
#print(model_SVM)

train_control <- trainControl(method="none")
############################################
#### Random Forest #########################
############################################
model_cforest_ns <- train( class~., data= training_nostem, trControl = train_control, method = "cforest")
pred_cforest_ns <- predict(model_cforest_ns, test_nostem)
confusionMatrix(pred_cforest_ns, truth)

model_cforest <- train( class~., data= training, trControl = train_control, method = "cforest")
pred_cforest <- predict(model_cforest, test)
confusionMatrix(pred_cforest, truth)

############################################
#### Bayesian Generalized Linear Model #####
############################################
model_bglm <- train( class~., data= training, trControl = train_control, method = "bayesglm")
pred_bglm <- predict(model_bglm, test)
confusionMatrix(pred_bglm, truth)

############################################
#### Minimos cuadrados #####
############################################
model_logic <- train( class~., data= training, trControl = train_control, method = "pls")
pred_logic <- predict(model_logic, test)
confusionMatrix(pred_logic, truth)

############################################
#### Regularized Linear Support Vector Machines with Class Weights #####
############################################
model_SVM <- train( class~., data= training, trControl = train_control, method = "LogitBoost")
pred_SVM <- predict(model_SVM, test)
confusionMatrix(pred_SVM, truth)


