
################################

yazi <-c("permitall","exe","select","access","allowhtml","update","pattern","from","gets","strcmp","regex","process","hash","strcat",
"strcpy","digest","diagnostics","password","array","equal","index","final","readobject","byte","buffer","user","logged",
"cmd","name","sendattack","command")


##################################
library("stringr")
library("stringi")
x <- yazi
x <- unlist(strsplit(x, split = " "))
x <- strsplit(x,split=" ")
x <- x[lengths(x)!=0]
###remove alpha characters
#x <- gsub("[^[:alpha:]]", " ", x)
###remove spaces in strings
x <- str_trim(x)
#####tolower all characters
x <- tolower(x)
cwe125Flag1 <-0
cwe125Flag2 <-0
cwe22Flag1 <-0
cwe22Flag2 <-0
cwe352Flag1 <-0
cwe352Flag2 <-0
cwe476Flag1 <- 0
cwe476Flag2 <- 0
cwe287Flag1 <- 0
cwe287Flag2 <- 0
cwe502Flag1 <- 0
cwe502Flag2 <- 0
cwe119Flag1 <- 0
cwe119Flag2 <- 0
cwe798Flag1 <- 0
cwe798Flag2 <- 0
###remove dublicate words from string
x <- unique(x)
####split strings into characters

###create data frame
j <- 1
generalFrame <- c()
while(j<=length(x)){

numericList <- c()
wordExtracted <- str_extract_all(x[j], boundary("character"))
lengthWord <- length(wordExtracted[[1]])
i <- 1
sqlFlag <- 0
parFlag <- 0
deger <- 0
#####cwe-119


cwe798Flag <-0

while(i<=lengthWord)
{
if(wordExtracted[[1]][i]=="p"||wordExtracted[[1]][i]=="a"||wordExtracted[[1]][i]=="s"||wordExtracted[[1]][i]=="s"||wordExtracted[[1]][i]=="w")
	cwe798Flag <- cwe798Flag+1
	else
	deger <- 0
numericList <- append(numericList,deger)
i <- i+1
}
i <-1
if(cwe798Flag==5){
cwe798Flag1 <-1
}
while(i<=lengthWord)
{
if(wordExtracted[[1]][i]=="e"||wordExtracted[[1]][i]=="q"||wordExtracted[[1]][i]=="u"||wordExtracted[[1]][i]=="a"||wordExtracted[[1]][i]=="l")
	cwe798Flag <- cwe798Flag+1
	else
	deger <- 0
numericList <- append(numericList,deger)
i <- i+1
}
if(cwe798Flag==5){
cwe798Flag2 <-1
}
i <-1
####end of cwe-798
#####cwe-119


cwe119Flag <-0

while(i<=lengthWord)
{
if(wordExtracted[[1]][i]=="a"||wordExtracted[[1]][i]=="r"||wordExtracted[[1]][i]=="r"||wordExtracted[[1]][i]=="a"||wordExtracted[[1]][i]=="y")
	cwe119Flag <- cwe119Flag+1
	else
	deger <- 0
numericList <- append(numericList,deger)
i <- i+1
}
i <-1
if(cwe119Flag==5){
cwe119Flag1 <-1
}
while(i<=lengthWord)
{
if(wordExtracted[[1]][i]=="i"||wordExtracted[[1]][i]=="n"||wordExtracted[[1]][i]=="d"||wordExtracted[[1]][i]=="e"||wordExtracted[[1]][i]=="x")
	cwe119Flag <- cwe119Flag+1
	else
	deger <- 0
numericList <- append(numericList,deger)
i <- i+1
}
if(cwe119Flag==5){
cwe119Flag2 <-1
}
i <-1
####end of cwe-119
#####cwe-502


cwe502Flag <-0

while(i<=lengthWord)
{
if(wordExtracted[[1]][i]=="f"||wordExtracted[[1]][i]=="i"||wordExtracted[[1]][i]=="n"||wordExtracted[[1]][i]=="a"||wordExtracted[[1]][i]=="l")
	cwe502Flag <- cwe502Flag+1
	else
	deger <- 0
numericList <- append(numericList,deger)
i <- i+1
}
i <-1
if(cwe502Flag==5){
cwe502Flag1 <-1
}
while(i<=lengthWord)
{
if(wordExtracted[[1]][i]=="r"||wordExtracted[[1]][i]=="e"||wordExtracted[[1]][i]=="a"||wordExtracted[[1]][i]=="d"||wordExtracted[[1]][i]=="o"||wordExtracted[[1]][i]=="b"||wordExtracted[[1]][i]=="j"||wordExtracted[[1]][i]=="e")
	cwe502Flag <- cwe502Flag+1
	else
	deger <- 0
numericList <- append(numericList,deger)
i <- i+1
}
if(cwe502Flag==8){
cwe502Flag2 <-1
}
i <-1
####end of cwe-502
#####cwe-190


cwe190Flag <-0

while(i<=lengthWord)
{
if(wordExtracted[[1]][i]=="b"||wordExtracted[[1]][i]=="y"||wordExtracted[[1]][i]=="t"||wordExtracted[[1]][i]=="e")
	cwe190Flag <- cwe190Flag+1
	else
	deger <- 0
numericList <- append(numericList,deger)
i <- i+1
}
i <-1

while(i<=lengthWord)
{
if(wordExtracted[[1]][i]=="b"||wordExtracted[[1]][i]=="u"||wordExtracted[[1]][i]=="f")
	cwe190Flag <- cwe190Flag+1
	else
	deger <- 0
numericList <- append(numericList,deger)
i <- i+1
}


i <-1
####end of cwe-190
#####cwe-287


cwe287Flag <-0

while(i<=lengthWord)
{
if(wordExtracted[[1]][i]=="u"||wordExtracted[[1]][i]=="s"||wordExtracted[[1]][i]=="e"||wordExtracted[[1]][i]=="r")
	cwe287Flag <- cwe287Flag+1
	else
	deger <- 0
numericList <- append(numericList,deger)
i <- i+1
}
i <-1
if(cwe287Flag==4){
print("cwe287Flag1")
cwe287Flag1 <-1
}
while(i<=lengthWord)
{
if(wordExtracted[[1]][i]=="l"||wordExtracted[[1]][i]=="o"||wordExtracted[[1]][i]=="g"||wordExtracted[[1]][i]=="g"||wordExtracted[[1]][i]=="e"||wordExtracted[[1]][i]=="d"||wordExtracted[[1]][i]=="i"||wordExtracted[[1]][i]=="n")
	cwe287Flag <- cwe287Flag+1
	else
	deger <- 0
numericList <- append(numericList,deger)
i <- i+1
}
if(cwe287Flag==8){
print("cwe287Flag2")
cwe287Flag2 <-1
}
i <-1
####end of cwe-287
#####cwe-476


cwe476Flag <-0

while(i<=lengthWord)
{
if(wordExtracted[[1]][i]=="c"||wordExtracted[[1]][i]=="m"||wordExtracted[[1]][i]=="d")
	cwe476Flag <- cwe476Flag+1
	else
	deger <- 0
numericList <- append(numericList,deger)
i <- i+1
}
i <-1
if(cwe476Flag==3)
cwe476Flag1 <-1
while(i<=lengthWord)
{
if(wordExtracted[[1]][i]=="s"||wordExtracted[[1]][i]=="y"||wordExtracted[[1]][i]=="s"||wordExtracted[[1]][i]=="t"||wordExtracted[[1]][i]=="e"||wordExtracted[[1]][i]=="m"||wordExtracted[[1]][i]=="."||wordExtracted[[1]][i]=="g")
	cwe476Flag <- cwe476Flag+1
	else
	deger <- 0
numericList <- append(numericList,deger)
i <- i+1
}
if(cwe476Flag==8)
cwe476Flag2 <-1
i <-1
####end of cwe-476
#####cwe-352


cwe352Flag <-0

while(i<=lengthWord)
{
if(wordExtracted[[1]][i]=="n"||wordExtracted[[1]][i]=="a"||wordExtracted[[1]][i]=="m"||wordExtracted[[1]][i]=="e")
	cwe352Flag <- cwe352Flag+1
	else
	deger <- 0
numericList <- append(numericList,deger)
i <- i+1
}
i <-1
if(cwe352Flag==4)
cwe352Flag1 <-1
while(i<=lengthWord)
{
if(wordExtracted[[1]][i]=="s"||wordExtracted[[1]][i]=="e"||wordExtracted[[1]][i]=="n"||wordExtracted[[1]][i]=="d"||wordExtracted[[1]][i]=="a"||wordExtracted[[1]][i]=="t"||wordExtracted[[1]][i]=="t"||wordExtracted[[1]][i]=="a")
	cwe352Flag <- cwe352Flag+1
	else
	deger <- 0
numericList <- append(numericList,deger)
i <- i+1
}
if(cwe352Flag==8)
cwe352Flag2 <-1
i <-1
####end of cwe-352
#####cwe-22
cwe22Flag <-0

while(i<=lengthWord)
{
if(wordExtracted[[1]][i]=="u"||wordExtracted[[1]][i]=="s"||wordExtracted[[1]][i]=="e"||wordExtracted[[1]][i]=="r"||wordExtracted[[1]][i]=="n")
	cwe22Flag <- cwe22Flag+1
	else
	deger <- 0
numericList <- append(numericList,deger)
i <- i+1
}
i <-1
if(cwe22Flag==5)
cwe22Flag1 <-1
while(i<=lengthWord)
{
if(wordExtracted[[1]][i]=="m"||wordExtracted[[1]][i]=="y")
	cwe22Flag <- cwe22Flag+1
	else
	deger <- 0
numericList <- append(numericList,deger)
i <- i+1
}
if(cwe22Flag==2)
cwe22Flag2 <-1
i <-1
#####cwe-20
cwe125Flag <-0

while(i<=lengthWord)
{
if(wordExtracted[[1]][i]=="a"||wordExtracted[[1]][i]=="r"||wordExtracted[[1]][i]=="r"||wordExtracted[[1]][i]=="a"||wordExtracted[[1]][i]=="y")
	cwe125Flag <- cwe125Flag+1
	else
	deger <- 0
numericList <- append(numericList,deger)
i <- i+1
}
i <-1
if(cwe125Flag==5)
cwe125Flag1 <-1
while(i<=lengthWord)
{
if(wordExtracted[[1]][i]==">"||wordExtracted[[1]][i]=="=")
	cwe125Flag <- cwe125Flag+1
	else
	deger <- 0
numericList <- append(numericList,deger)
i <- i+1
}
if(cwe125Flag==2)
cwe125Flag2 <-1
i <-1

#####cwe-20
cwe20Flag <-0
while(i<=lengthWord)
{
if(wordExtracted[[1]][i]=="t"||wordExtracted[[1]][i]=="o"||wordExtracted[[1]][i]=="t"||wordExtracted[[1]][i]=="a"||wordExtracted[[1]][i]=="l")
	cwe20Flag <- cwe20Flag+1
	else
	deger <- 0
numericList <- append(numericList,deger)
i <- i+1
}
i <-1

#####cwe-78
cwe78Flag <- 0
while(i<=lengthWord)
{
if(wordExtracted[[1]][i]=="c"||wordExtracted[[1]][i]=="m"||wordExtracted[[1]][i]=="d")
	cwe78Flag <- cwe78Flag+1
	else
	deger <- 0
numericList <- append(numericList,deger)
i <- i+1
}
i <-1
while(i<=lengthWord)
{
if(wordExtracted[[1]][i]=="s"||wordExtracted[[1]][i]=="c"||wordExtracted[[1]][i]=="r")
	cwe78Flag <- cwe78Flag+1
	else
	deger <- 0
numericList <- append(numericList,deger)
i <- i+1
}
i <-1
while(i<=lengthWord)
{
if(wordExtracted[[1]][i]=="c"||wordExtracted[[1]][i]=="o"||wordExtracted[[1]][i]=="m")
	cwe78Flag <- cwe78Flag+1
	else
	deger <- 0
numericList <- append(numericList,deger)
i <- i+1
}
i <-1


#####################cwe78Flag end

#####cwe-416
cwe416Flag <- 0
while(i<=lengthWord)
{
if(wordExtracted[[1]][i]=="f"||wordExtracted[[1]][i]=="r"||wordExtracted[[1]][i]=="e"||wordExtracted[[1]][i]=="e"||wordExtracted[[1]][i]=="(")
	cwe416Flag <- cwe416Flag+1
	else
	deger <- 0
	numericList <- append(numericList,deger)
i <- i+1
}
i <-1

####parameter control
while(i<=lengthWord)
{
if(wordExtracted[[1]][i]=="@")
	parFlag <- 1
	else
	deger <- 0
	numericList <- append(numericList,deger)
i <- i+1
}
i <-1
###sql injection check
sqlFlag <-0
while(i<=lengthWord)
{
	if(wordExtracted[[1]][i]=="s"||wordExtracted[[1]][i]=="e"||wordExtracted[[1]][i]=="c"||wordExtracted[[1]][i]=="l"||wordExtracted[[1]][i]=="t")
	sqlFlag <- sqlFlag+1
	else 
	deger <- 0
	numericList <- append(numericList,deger)
	i <- i+1
}
###cwe-787 out of bounds check
i <- 1
cwe787Flag <- 0
while(i<=lengthWord)
{
	if(wordExtracted[[1]][i]=="s"||wordExtracted[[1]][i]=="t"||wordExtracted[[1]][i]=="r"||wordExtracted[[1]][i]=="c"||wordExtracted[[1]][i]=="p"||wordExtracted[[1]][i]=="y")
	cwe787Flag <- cwe787Flag+1
	else 
	deger <- 0
	numericList <- append(numericList,deger)
	i <- i+1
}
###cwe-79 XSS
i <- 1
cwe79Flag <- 0
while(i<=lengthWord)
{
	if(wordExtracted[[1]][i]=="a"||wordExtracted[[1]][i]=="l"||wordExtracted[[1]][i]=="e"||wordExtracted[[1]][i]=="r"||wordExtracted[[1]][i]=="t")
	cwe79Flag <- cwe79Flag+1
	else 
	deger <- 0
	numericList <- append(numericList,deger)
	i <- i+1
}
###cwe-89 XSS
i <- 1
cwe89Flag <- 0
while(i<=lengthWord)
{
	if(wordExtracted[[1]][i]=="s"||wordExtracted[[1]][i]=="e"||wordExtracted[[1]][i]=="l"||wordExtracted[[1]][i]=="e"||wordExtracted[[1]][i]=="c")
	cwe89Flag <- cwe89Flag+1
	else 
	deger <- 0
	numericList <- append(numericList,deger)
	i <- i+1
}
###assign specific values for vowel numbers
vowel <- as.integer(str_detect(wordExtracted, "e|i|a|o|u"))

numericList <- runif(6,0.45,0.5)


generalFrame <- rbind(generalFrame,numericList)
j <- j+1

}	


####assign row names
namesRow <- rownames(generalFrame)
i <- 1
while(i<=length(namesRow))
{
	rownames(generalFrame)[i] <- x[i]
	i <- i+1 
}





write.csv(generalFrame, file="C:/makaleler/makaleler/vulnerability/paper2/code/keyword.txt")
#####################################






