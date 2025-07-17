library("ManifoldOptim")
set.seed(1234)
p <- 4; n <- 4
B <- matrix(rnorm(n*n), nrow=n)
vector1 <- runif(16,0.1,0.8)
###for hidden and batchsize
vector1 <- round(vector1)
i<- 1
j <- 1
liste1 <- c()
vc <- c(4,1:3)
framex <- data.frame()
framex <- rbind(framex,vc)
rownames(framex) <- NULL
while(i<=16)
{
	liste1 <- append(liste1,vector1[i])
			
		if(j%%4==0){
	print(liste1)
		framex <- rbind(framex,liste1)		
			liste1 <- c()
		}

		i <- i+1
j <- j+1
}
framex <- framex[-1,]
B <- framex
rownames(B) <- NULL
colnames(B) <- NULL
B <- as.matrix(B)
########################
##########create D matrix###################
vector2 <- accuFunction(vector1)
i<- 1
j <- 1
liste1 <- c()
vc <- c(4,1:3)
flag <-0
framex <- data.frame()
framex <- rbind(framex,vc)
rownames(framex) <- NULL
while(i<=16)
{
	liste1 <- append(liste1,vector2[i])
		if(j%%4==0){
		framex <- rbind(framex,liste1)		
			liste1 <- c()
		flag <- flag+1
		}

	
		i <- i+1
j <- j+1
}
framex <- framex[-1,]
D <- framex
rownames(D) <- NULL
colnames(D) <- NULL
D <- as.matrix(D)
###########################

##############################
############################
Function generate accuracy
#################################
##******before running this part, you should run deepnet.R
#################################
########################
##################PARTI I
############################
accuFunction <- function(x){
library(deepnet)
yazi <- read.table(dataPath,header=TRUE,sep = ',')
yazi <- yazi[,-1]
yazi = as.matrix(yazi)
yazi <- matrix(as.numeric(yazi),ncol=6)
#labels[5830]<- 0

resultLength <- length(x)
i <- 1
resultList <- c()
while(i<=resultLength){
nn <- nn.train(yazi, labels,learningrate=x[i])
#,momentum=0.33,learningrate_scale=3,batchsize=21,numepochs=3,learningrate=0.57
yy = nn.predict(nn, yazi)
#print(head(yy))

yhat = matrix(0,length(yy),1)
yhat[which(yy > mean(yy))] = 1
yhat[which(yy <= mean(yy))] = 0
cm = table(labels,yhat)
accuracy <- (cm[1]+cm[4])/(cm[2]+cm[3]+cm[1]+cm[4])
resultList <- append(resultList, accuracy)
i <- i+1
}
return(resultList)
}#end of function 
#############################
###############################
################################
hyperparameters <- c(1e-1,1e-2,1e-3,1e-4,1e-5,1e-6,1e-8,1e-8)
resultList <- vector("list", 8) 
i <- 1
while(i<=8){
tx <- function(x) { matrix(x, n, p) }
f <- function(x) { X <- tx(x); Trace( t(X) %*% B %*% X %*% D ) }
g <- function(x) { X <- tx(x); 2 * B %*% X %*% D }
mod <- Module("ManifoldOptim_module", PACKAGE = "ManifoldOptim")
prob <- new(mod$RProblem, f, g)
x0 <- as.numeric(orthonorm(matrix(rnorm(n*p), nrow=n, ncol=p)))
mani.params <- get.manifold.params(IsCheckParams = TRUE)
solver.params <- get.solver.params(
Tolerance=hyperparameters[i],
Max_Iteration=50,
DeltaBar=10,
Delta0=1e-3
)
mani.defn <- get.stiefel.defn(n, p)
res <- manifold.optim(prob, mani.defn, method = "RTRSR1",
mani.params = mani.params, solver.params = solver.params, x0 = x0)
print(res)
resultList[[i]] <- res
i <- i+1
}

