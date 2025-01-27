library("stringr")
############Folders
#folders <- list.dirs("C:/Users/User/Downloads/2015-10-27-apache-jena-v2-11-0/153849-v1.0.0/src/jena-iri/src/main/java/org/apache/jena/iri/impl/")
folders <- list.dirs("C:/Users/User/Downloads/2017-09-18-iarpa-stonesoup-phase-3-asterisk-v10-2-0")

j <- 1
while(j<=length(folders))
{
	folders[j] <- paste(folders[j],"/") 
folders[j] <- str_replace_all(folders[j], " ", "")
j <- j+1
}
####################
allFiles <- c()
i <- 1
while(i<=length(folders)){


dosyalar <- list.files(path=folders[i], pattern="\\.java", all.files=FALSE,
    full.names=TRUE)
uzunluk <- length(dosyalar)

k <- 1
while(k <= length(dosyalar)){
allFiles <- append(allFiles, dosyalar[k])
k <- k+1
}

i <- i+1
}
################################
dosyalar <- allFiles
uzunluk <- length(dosyalar)
#################################
##########2. kısım#################
library("tm")
k <- 1
while(k<uzunluk)
{
yazi <- read.delim(dosyalar[k])
yazi <- unlist(yazi)
yazi <- as.character(yazi)
data<- removePunctuation(yazi)
yazi <- data
yazi <- str_replace_all(yazi, "[\r\n]" , "")
write.table(yazi,file=dosyalar[k],sep="\t")
k <- k+1
}
####################################
list3 <- list("")
library('rlist')
k <-1 
while(k<uzunluk){
file1 <- read.table(dosyalar[k], sep = '=', quote = '')
list3 <- list.append(list3,file1)
print("##################")
print(k)
print("##################")
yazi <- list3
k <- k+1
}
yazi <- unlist(yazi)
yazi <- as.character(yazi)
###Bu kod gereksiz satırları "yazi" dan siler
i <- 2
uzunluk <- length(yazi)
vektor <- c(1)
while(i < uzunluk)
{
	vektor <- append(vektor,i)
	i <- i+2
print("##################")
print(i)
print("##################")
}
yazi <- yazi[-vektor]
library(tm)
yazi <- removeNumbers(yazi)


##################################
library(word2vec)
set.seed(123456789)
model <- word2vec(x = yazi, type = "cbow", dim = 15, iter = 5)
embedding <- as.matrix(model)
write.csv(embedding, file="D:/makaleler/vulnerability/results/asterisk.txt")
#####################################
labels <- rbinom( length(embedding[,1]), 1, 0.5)
########################################
library(text2vec)
library(data.table)
library(magrittr)




#########training phase
library(glmnet)
NFOLDS = 4
t1 = Sys.time()
glmnet_classifier = cv.glmnet(x = embedding, y = labels, 
                              family = 'binomial', 
                              # L1 penalty
                              alpha = 1,
                              # interested in the area under ROC curve
                              type.measure = "deviance",
                              # 5-fold cross-validation
                              nfolds = NFOLDS,
                              # high value is less accurate, but has faster training
                              thresh = 1e-9,
                              # again lower number of iterations for faster training
                              maxit = 1e3)
print(difftime(Sys.time(), t1, units = 'sec'))
plot(glmnet_classifier)


preds = predict(glmnet_classifier, embedding, type = 'response')[,1]
glmnet:::auc(labels, preds)
###################################
########label function##########################

