##################PARTI I
library("stringr")
yazi <- read.table("C:/makaleler/makaleler/vulnerability/paper2/dataset/gnugrep/gnugrep.txt",header=TRUE,sep = ',')
###we intend to create a center list vector consisting of every 20 words
##find number of words
wordLength <- length(yazi[,1])
averageList <- c()
###detect center word average
i <- 1
sumAverage <- 0
averageUtf <- 0
k <- 20
while(k < wordLength){
sumAverage <- 0
averageUtf <- 0
while(i <= k)
{
	utfResult <- utf8ToInt(yazi[i,1])
	j <- 1
	while(j<=length(utfResult))
	{
	averageUtf <- utfResult[j]+averageUtf
	j <- j+1
	}
	averageUtf <- averageUtf/length(utfResult)
	sumAverage <- sumAverage +averageUtf
	i <- i+1
}
tmp <- sumAverage/20
print(sumAverage/20)
averageList <- append(averageList,tmp)
k <- k+20
}
####end of detecting center word average

######################replace inf with real value
averageList[is.infinite(averageList)] = 108.6237
#######################
 k <- 1501
sum <- 0
while(k<=1520){
utfResult <- utf8ToInt(yazi[k,1])
averageUtf <- 0
i <- 1
while(i<=length(utfResult))
{
	averageUtf <- utfResult[i]+averageUtf
	i <- i+1
}

averageUtf <- averageUtf/length(utfResult)
sum <- sum+averageUtf
k <- k+1
}
print(sum/20)
########calculate cosinus values depending on the center values############
wordLength <- length(yazi[,1])
library(matlib)
matA <- matrix(c(3, 1), nrow = 2)  ##column vectors
matB <- matrix(c(5, 5), nrow = 2)
matA[1,1] <- 1
matB[1,1] <- 1
sum <- 0
i <- 1
k <- 1
m <- 1
cosVector <- c()
while(k<=wordLength)
{
	j <- 1
	while(j <= 20)
	{
		utfResult <- utf8ToInt(yazi[k,1])
		while(m <= length(utfResult))
		{
		sum <- sum + utfResult[m]
		m <- m+1
		}
		averageUtf <- sum/length(utfResult)
		#matA mat B koy
		matA[2,1] <- averageUtf
		matB[2,1] <- averageList[i]
		cosAngle <- angle(as.vector(matA), as.vector(matB)) 
		cosVector <- append(cosVector,cosAngle)
		j <- j+1
		k <- k+1
		sum <- 0
		m <- 1
	}
	i <- i+1
}
if(is.na(cosVector[5829])){
print("Na bulundu")
}
cosVector <- cosVector %>% replace(is.na(.), runif(1,0,0.04))
cosVector <- cosVector[1:length(yazi[,1])]
###convert from 0.00 to real angle values
cosVector <- cosVector*1000
##################PARTI II
####library(aspace)
####acos_d(theta = 90) is used to convert from angle to value
######create labels with vulnerability matrix
yazi <- read.table("C:/makaleler/makaleler/vulnerability/paper2/code/keyword.txt",header=TRUE,sep = ',')
i <- 1
lengthM <- length(yazi[,1])
sum <- 0
while(i<=lengthM)
{
		utfResult <- utf8ToInt(yazi[i,1])
		j <- 1
		tmpResult <- 0
		while(j <= length(utfResult))
		{
					tmpResult <- tmpResult+utfResult[j]
					print(tmpResult)
					j <- j+1
		}

		sum <- sum + (tmpResult/length(utfResult))
		#print(sum)
		i <- i+1
}
####average uft of vulnerability keyword matrix was found
averageUtf <- sum/length(yazi[,1])
#####################
##############
#####calculate difference between vulnerability and code word columns
yazi <- read.table("C:/makaleler/makaleler/vulnerability/paper2/dataset/gnugrep/gnugrep.txt",header=TRUE,sep = ',')
lengthM <- length(yazi[,1])
tmpResult <- 0
sum <- 0
i <- 1
j <- 1
utfList <- c()
while(i<=lengthM)
{
		utfResult <- utf8ToInt(yazi[i,1])
		j <- 1
		tmpResult <- 0
		while(j <= length(utfResult))
		{
					tmpResult <- tmpResult+ utfResult[j]
					j <- j+1
		}

		utfRow <- tmpResult/length(utfResult)
		utfList <- append(utfList, utfRow)
		print(utfRow)
		i <- i+1
}
####create vulnerability label column, thereby seeing difference between utf scores
i <- 1
labels <- c()
while(i<=length(utfList))
{
	tmpResult <- abs(utfList[i]-averageUtf)	
ifelse(tmpResult <= 0.1, labels <- append(labels, 1), labels <- append(labels, 0))
i <- i+1
}





