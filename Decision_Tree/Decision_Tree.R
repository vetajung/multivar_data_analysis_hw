#Assignment6_2015170378 정은영

# Performance Evaluation Function -----------------------------------------
perf_eval <- function(cm){
  
  # True positive rate: TPR (Recall)
  TPR <- cm[2,2]/sum(cm[2,])
  # Precision
  PRE <- cm[2,2]/sum(cm[,2])
  # True negative rate: TNR
  TNR <- cm[1,1]/sum(cm[1,])
  # Simple Accuracy
  ACC <- (cm[1,1]+cm[2,2])/sum(cm)
  # Balanced Correction Rate
  BCR <- sqrt(TPR*TNR)
  # F1-Measure
  F1 <- 2*TPR*PRE/(TPR+PRE)
  
  return(c(TPR, PRE, TNR, ACC, BCR, F1))
}

# Performance table
Perf.Table <- matrix(0, nrow = 2, ncol = 6)
rownames(Perf.Table) <- c("CART(pruned 'x')","CART(pruned 'o')")
colnames(Perf.Table) <- c("TPR", "Precision", "TNR", "Accuracy", "BCR", "F1-Measure")

# Load the data & Preprocessing
df <- read.csv("heart.csv")
input.idx <- c(1:13)
target.idx <- 14

df.input <- df[,input.idx]
df.target <- as.factor(df[,target.idx])

df.data <- data.frame(df.input, df.target)

nR <- nrow(df)
nR

set.seed(12345) #랜덤 알고리즘 지정
trn.idx <- sample(1:nR, 200) #train data 를 임의로 200개 뽑기

#[Q1]
install.packages("tree")
library(tree)

tree.trn <- data.frame(df.input[trn.idx,], HeartYN = df.target[trn.idx])
tree.tst <- data.frame(df.input[-trn.idx,], HeartYN = df.target[-trn.idx])

# Training the tree
CART.model <- tree(HeartYN ~ ., tree.trn)
summary(CART.model)

# Plot the tree
plot(CART.model) 
text(CART.model, pretty = 1)

# Prediction
CART.prey <- predict(CART.model, tree.tst, type = "class")
CART.cfm <- table(tree.tst$HeartYN, CART.prey)
CART.cfm

# Evaluation
Perf.Table[1,] <- perf_eval(CART.cfm)
Perf.Table

#[Q2]
# Find the best tree
set.seed(12345)
CART.model.cv <- cv.tree(CART.model, FUN = prune.misclass)

# Plot the pruning result
plot(CART.model.cv$size, CART.model.cv$dev, type = "b")
CART.model.cv #best 찾기

# Select the final model
CART.model.pruned <- prune.misclass(CART.model, best = 4)
plot(CART.model.pruned) # 4개만 나타남!
text(CART.model.pruned, pretty = 1)

# Prediction
CART.prey <- predict(CART.model.pruned, tree.tst, type = "class")
CART.cfm <- table(tree.tst$HeartYN, CART.prey)
CART.cfm

Perf.Table[2,] <- perf_eval(CART.cfm)
Perf.Table

#[Q3]

#Preference Matrix
Perf.Table_2 <- matrix(0, nrow = 4, ncol = 6)
rownames(Perf.Table_2) <- c("tree","rpart","RWeka","party")
colnames(Perf.Table_2) <- c("TPR", "Precision", "TNR", "Accuracy", "BCR", "F1-Measure")

Perf.Table_2[1,] <- perf_eval(CART.cfm)
Perf.Table_2

install.packages("RWeka")
install.packages("rpart")
install.packages("party")
install.packages("partykit") #J48모델 그릴때 필요
library(rpart)
library(RWeka)
library(party)
library(partykit)

#####rpart#####
#rpart model [Q3-1] package name
rpart.model <- rpart(HeartYN ~ ., data = tree.trn, method = "class")
plot(rpart.model)
text(rpart.model)

#pruning rpart
printcp(rpart.model)
plotcp(rpart.model)

#[Q3-2] option_ 교수님께서 다른 방식으로 변환해 보면 좋다고 하셨다, [Q3-3]비교,best
rpart.model.pruned <- prune(rpart.model, cp= rpart.model$cptable[which.min(rpart.model$cptable[,"xerror"]),"CP"])

#[Q3-4] plot
plot(rpart.model.pruned)
text(rpart.model.pruned)

#rpart prediction & evaluation [Q3-5] 분류성능
rpart.prey <- predict(rpart.model.pruned, tree.tst, type = 'class')
rpart.cfm <- table(tree.tst$HeartYN, rpart.prey)
rpart.cfm

Perf.Table_2[2,] <- perf_eval(rpart.cfm)
Perf.Table_2

#####RWeka#####
#RWeka model [Q3-1] package name
RWeka.model <- J48(HeartYN ~ ., data = tree.trn)
if(require("party",quietly = TRUE)){plot(RWeka.model)}

#[Q3-2] option
C = c(0.4, 0.25, 0.05) #C: <pruning confidence>, Set confidence threshold for pruning. (default 0.25)
M = c(2,10,20) # M: <minimum number of instances> Set minimum number of instances per leaf. (default 2)

Perf.Table_3 <- matrix(0, nrow = 9, ncol = 6)
rNames <- c()
colnames(Perf.Table_3) <- c("TPR", "Precision", "TNR", "Accuracy", "BCR", "F1-Measure")

for(i in 1:3){
  for(j in 1:3){
    rNames <- c(rNames, paste("C:",C[i],",M:",M[j]))
    RWeka.model.pruned <- J48(HeartYN ~ ., data = tree.trn, control = Weka_control(C = C[i], M = M[j]))
    if(require("party",quietly = TRUE)){plot(RWeka.model.pruned)}
    RWeka.prey <- predict(RWeka.model.pruned, tree.tst)
    RWeka.cfm <- table(tree.tst$HeartYN, RWeka.prey)
    RWeka.cfm
    Perf.Table_3[(3*(i-1)+j),] <- perf_eval(RWeka.cfm)
  }
}
rownames(Perf.Table_3) <- rNames
Perf.Table_3 
#[Q3-3]비교,best
#C: 0.25, M:10 일 때 가장 높음.

#best model
RWeka.model.pruned <- J48(HeartYN ~ ., data = tree.trn, control = Weka_control(C = 0.25, M = 10))
#[Q3-4] plot
if(require("party",quietly = TRUE)){plot(RWeka.model.pruned)}
RWeka.prey <- predict(RWeka.model.pruned, tree.tst)
RWeka.cfm <- table(tree.tst$HeartYN, RWeka.prey)
RWeka.cfm

#[Q3-5] 분류성능
Perf.Table_2[3,] <- perf_eval(RWeka.cfm)
Perf.Table_2

#####party#####
#party model [Q3-1] package name
party.model <- ctree(HeartYN ~ ., data = tree.trn)
#[Q3-2] option, [Q3-3]비교,best
# party패키지는 가지치기를 significance를 사용해서 하기 때문에 별도의 pruning 과정이 필요 없다.
#[Q3-4] plot
plot(party.model)

#party prediction & evaluation
party.prey <- predict(party.model, tree.tst)
party.cfm <- table(tree.tst$HeartYN, party.prey)
party.cfm

#[Q3-5] 분류성능
Perf.Table_2[4,] <- perf_eval(party.cfm)
Perf.Table_2

