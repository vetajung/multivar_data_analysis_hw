#Assignment4_2015170378 정은영

install.packages("moments") 
install.packages("corrplot")
library(moments)
library(corrplot)

df <- read.csv('Admission_Predict.csv')
str(df)
head(df,3)  #데이터 확인
length(unique(df$Serial.No.)) == nrow(df) #Serial number 가 중복되는 것이 없는지 확인
sum(is.na(df)) #missing value 있는지 확인
#중복이나 결측값 없음.

#[Q1]

df[1] <- NULL #Serial number 는 admission의 변화에 영향 주지 않기 때문에 제거.

#[Q2]

par(mfrow = c(2,2))
for(i in 1:(length(colnames(df))-1)){
  plot(df[,i], df$Chance.of.Admit, main = names(df[i]), ylab = names(df$Chance.of.Admit), xlab = "", col = 'green')
}

# 데이터프레임을 참고하고 그래프를 그려본 결과 변수 University.Rating, SOP, LOR, Research 가 discrete 변수라고 생각됨.
#또한 그래프를 그려본 결과 input 변수들이 종속변수와 양의 상관관계를 가질 것이라고 예상.
#따라서 이 변수들의 column index를 구해보면 3,4,5,7 임.

cateIdx <- c(3,4,5,7) #categorical index
conIdx = c(1:(length(colnames(df))-1))[!(c(1:(length(colnames(df))-1)) %in% cateIdx)] #non categorical index

# mean, standard deviation, skewness, kurtosis 포함 matrix 정의
nMat <- matrix(c(1:length(conIdx)*4),nrow=length(conIdx),ncol=4)
colnames(nMat)<- c("mean", "std","skewness","kurtosis")
rownames(nMat)<- colnames(df[conIdx])

#Continuous value 에 대해 histogram그리고, matrix 채우기.
par(mfrow=c(1,2))
for(i in 1:length(conIdx)){
  hist(df[,conIdx[i]], main = paste("Histogram of" , colnames(df[conIdx[i]])))
  boxplot(df[,i])
  nMat[i,1] <- mean(unlist(df[,conIdx[i]]))
  nMat[i,2] <- sqrt(var(df[,conIdx[i]]))
  nMat[i,3] <- skewness(df[,conIdx[i]])
  nMat[i,4] <- kurtosis(df[,conIdx[i]])
}
#non categorical value 들의 histogram을 그려본 결과 대부분 정규분포와 같이 퍼져있고 outlier도 거의 관측되지 않음을 확인할 수 있다.
#matrix의 결과를 봐도 skewness 가 거의 0에 가깝고 kurtosis가 2와 3 사이임을 확인하여 정규분포를 따른다고 볼 수 있다.

#Categorical Value에 대해 Summary 확인
for(i in 1:length(cateIdx)){
  print(colnames(df[cateIdx[i]]))
  print(summary(factor(df[,cateIdx[i]])))
  hist(df[,cateIdx[i]], main = paste("Histogram of" , colnames(df[cateIdx[i]])))
}
#summary와 histogram을 참고했을 때, University Rating, SOP, LOR이 정규분포의 모양과 유사함을 확인할 수 있다.

#[Q3]
#categorical 변수들에 대해서는 summary를 확인했을 때 이상치가 없다고 볼 수 있다.
#Continuous 변수들은 Box plot 근거로 upper_range, lower_range 정하여 outliers 탐색.

for(i in conIdx){
  print(colnames(df[i]))
  summary(df[,i])
  q1 <- quantile(df[,i], c(0.25))
  q3 <- quantile(df[,i], c(0.75))
  IQR <- q3 - q1  
  upper_range <- q3 + 1.5*IQR
  lower_range <- q1 - 1.5*IQR
  print(nrow(df[df[,i] > upper_range,])+nrow(df[df[,i] < lower_range,])) #outliers 개수
  df[,i]<- ifelse(df[,i] > upper_range, NA, df[,i]) #outlier 제거(윗부분)
  df[,i]<- ifelse(df[,i] > upper_range, NA, df[,i]) #outlier 제거(윗부분)
  df<- na.omit(df) #outlier 해당 데이터 데이터셋에서 제거
}
#GRE.Score, TOEFL.Score은 outliers 없고, CGPA는 1개의 outlier 가 있어 제거.

#[Q4] 
# Basic Scatterplot Matrix
pairs(~.,data=df, 
      main="Scatterplot Matrix")

# correlation plot
corr <- cor(df) 
corrplot(corr, method = "color", outline = T, cl.pos = 'n', rect.col = "black",  tl.col = "indianred4", addCoef.col = "black", number.digits = 2, number.cex = 0.60, tl.cex = 0.7, cl.cex = 1, col = colorRampPalette(c("green4","white","red"))(100))

#numeric variables 인 GRE.Score, TOEFL.Score, CGPA, Chance of Admit 가 강한 상관관계를 보인다.

#Discrete value 에 대해 y축을 chance of admit로 boxplot 그리기.
par(mfrow=c(2,2))
for(i in cateIdx){
  boxplot(df[,length(colnames(df))]~df[,i], xlab='', main=names(df[i]), col=c("yellow","green"))
}

#box plot을 그렸을 때 데이터의 트렌드를 보면 대학수준(University Rating), 학업계획서(SOP), 추천서(LOR) 가 수준이 높아질수록 입학기회가 높아짐(높은 상관관계)을 확인할 수 있다. 또한 연구를 할수록 입학기회가 높아진다는 것을 확인할 수 있다.


#[Q5] 

table(df$Chance.of.Admit > 0.8) #False=283, True=117, 즉 입학기회 0.8보다 적은 것이 높은 것보다 많다. 
df$Chance.of.Admit = as.factor(ifelse(df$Chance.of.Admit > 0.8,1,0)) #0,1을 갖는 값으로 target variable 변환.

# Conduct the normalization
input_idx <- c(1:7) 
target_idx <- 8 #Chance of admit

df_input <- df[,input_idx]
df_input <- scale(df_input, center = TRUE, scale = TRUE) #round error 발생할 수 있어 SCALE 맞춤. 
df_target <- df[,target_idx]
df_scaled <- data.frame(df_input, df_target)

# Split the data into the training/validation sets
set.seed(12345)
trn_idx <- sample(1:nrow(df_scaled), round(0.7*nrow(df_scaled)))
df_trn <- df_scaled[trn_idx,]
df_tst <- df_scaled[-trn_idx,]

# Train the Logistic Regression Model with all variables
full_lr <- glm(df_target ~ ., family=binomial, df_trn) #GLM_generalize linear model
summary(full_lr) #유의수준. 이상
#CGPA 의 p-value가 0.01보다 작고, TOEFL점수의 p-value가 0.1보다 작다.

#[Q6] 
#prediction
lr_response <- predict(full_lr, type = "response", newdata = df_tst) #예측, 숫자_sample번호, 값들의 CUTOFF를 결정해서 이걸 나눔. 
lr_target <- df_tst$df_target
lr_predicted <- rep(0, length(lr_target))
lr_predicted[which(lr_response >= 0.5)] <- 1 #로지스틱 cutoff를 0.5로 결정하겠다. 
cm_full <- table(lr_target, lr_predicted) #실제정답, 예측 CONFUSION MATRIX 생성. 
cm_full
#logistic cutoff 0.5로 했을 시 타겟을 제대로 예측하는 개수가 0-0은 76개, 1-1은 37개임, 또한 제대로 못한 경우 0-1은 4개, 1-0은 3개임

#perference evaluation funtion
perf_eval2 <- function(cm){
  
  # True positive rate: TPR (Recall)  
  TPR <- cm[2,2]/sum(cm[2,]) #cm=>confusion matrix,R-script는 오름차순이어서 TPR이 2행.
  # True negative rate: TNR
  TNR <- cm[1,1]/sum(cm[1,])
  # False Positive Rate: FPR
  FPR <- cm[1,2]/sum(cm[1,])
  # False Negative Rate: FNR
  FNR <- cm[2,1]/sum(cm[2,])
  
  # Precision
  PRE <- cm[2,2]/sum(cm[,2])
  # Simple Accuracy
  ACC <- (cm[1,1]+cm[2,2])/sum(cm)
  # Balanced Correction Rate
  BCR <- sqrt(TPR*TNR)
  # F1-Measure
  F1 <- 2*TPR*PRE/(TPR+PRE)
  
  return(c(TPR, TNR, FPR, FNR, PRE, ACC, BCR, F1))
}

# Initialize the performance matrix
perf_mat <- matrix(0, 1, 8)
colnames(perf_mat) <- c("TPR (Recall)", "TNR","FPR","FNR","Precision", "ACC", "BCR", "F1")
rownames(perf_mat) <- "Logstic Regression"

#결과
perf_mat[1,] <- perf_eval2(cm_full)
perf_mat

#[Q7]

#AUROC 산출 함수_교수님 프린트를 기반으로 x,y축에 평행한 선들로 구성된 ROC 커브로 간주.
AUROC <- function(seed_num){
  
  # Split the data into the training/validation sets
  set.seed(seed_num)
  trn_idx <- sample(1:nrow(df_scaled), round(0.7*nrow(df_scaled)))
  df_trn <- df_scaled[trn_idx,]
  df_tst <- df_scaled[-trn_idx,]
  
  # 학습
  full_lr <- glm(df_target ~ ., family=binomial, df_trn) #GLM_generalize linear model
  
  # 예측
  lr_response <- predict(full_lr, type = "response", newdata = df_tst)
  lr_target <- df_tst$df_target
  lr_predicted <- rep(0, length(lr_target))
  
  #TPR, FPR 담을 벡터 생성 및 필요한 변수 초기화
  TPR_vec <- c(length(lr_response))
  FPR_vec <- c(length(lr_response))
  dummy <- matrix(0,nrow = 2,ncol = 1)
  
  dFPR <- 0
  dTPR <- 0
  AUROC <- 0
  
  #TPR, FPR 기반으로 AUROC 계산
  for(i in 1:length(lr_response)){
    lr_predicted[which(lr_response >= lr_response[i])] <- 1 #cutoff를 하나씩 옮기기
    cm_1 <- cbind(table(lr_target, lr_predicted),dummy) 
    #confusion matrix의 2열이 나오지 않을 때를 대비하여(2열이 0,0 일때 표시안됨) dummy로 0,0을 추가.
    TPR_vec[i] <- cm_1[2,2]/sum(cm_1[2,]) #TPR
    FPR_vec[i] <- cm_1[1,2]/sum(cm_1[1,]) #FPR
    if(i>1){
      dFPR <- FPR_vec[i]-FPR_vec[(i-1)] #FPR이 처음에 0, FPR의 차이 계산
    }
    AUROC <- AUROC + TPR_vec[i] * dFPR #막대들 더하기
  }
  
  return(AUROC)
}

#AUROC(seed number) 로 계산한 AOROC 값.
AUROC(12345)
AUROC(11111)
AUROC(1000)
AUROC(123)
AUROC(54321)


#[Q8] 이 외 웹이나 기타 자료들을 통해 재미있는 데이터셋(fun dataset)을 찾아 나름대로의 로지스틱 회귀 분석 모형 구축 및 결과 해석을 수행하시오.

# 직접 크롤링해서 긁어온 쇼핑몰 '미쳐라'의 바지 사이즈를 포함한 정보데이터이다.
# 사이즈가 small, medium, large 로 구분됨. 허리둘레, 허벅지 둘레 등에 따른 사이즈를 알아봄.

install.packages("nnet")
library(nnet)

#데이터 불러오기
pants <- read.csv('Michyeora_Pants_Info_UTF_8.csv')
str(pants)
head(pants,3)  #데이터 확인

pants[,c(1,2,8,9)] <- NULL #ID, name, Image, URL 은 size에 영향을 주지 않기 때문에 제거

# 평가지표 구하는 함수
perf_eval3 <- function(cm){
  
  # Simple accuracy
  ACC <- sum(diag(cm))/sum(cm)
  
  # ACC for each class, 대각선 따로 계산한 것. 
  A1 <- cm[1,1]/sum(cm[1,])
  A2 <- cm[2,2]/sum(cm[2,])
  A3 <- cm[3,3]/sum(cm[3,])
  BCR <- (A1*A2*A3)^(1/3) #세제곱 루트 
  
  return(c(ACC, BCR))
}

# Define the baseline class
pants$size <- as.factor(pants$size) #CLASS가 종속변수, 문자로 되어있는데 범주로 바꾸어줘야함.
pants$size <- relevel(pants$size, ref = "L") #기준 범주를 선택해주는 작업, L로 선택.

# 학습-평가 데이터 분류
trn_idx <- sample(1:nrow(pants), round(0.7*nrow(pants)))
pants_trn <- pants[trn_idx,]
pants_tst <- pants[-trn_idx,]

# Train multinomial logistic regression
ml_logit <- multinom(size ~ ., data = pants_trn)

# Check the coefficients
summary(ml_logit) #S,M 각각은 p(Y=S)/p(Y=L), p(Y=M)/P(Y=L)
t(summary(ml_logit)$coefficients)

# Conduct 2-tailed z-test to compute the p-values
z_stats <- summary(ml_logit)$coefficients/summary(ml_logit)$standard.errors #P-VALUE를 따로 계산하기 위해
t(z_stats)

p_value <- (1-pnorm(abs(z_stats), 0, 1))*2
options(scipen=10)
t(p_value) #P-VALUE

cbind(t(summary(ml_logit)$coefficients), t(p_value)) #회귀계수랑 P-VALUE를 S,M에 대해 보여줌(L을 기준으로 확률)

# Predict the class probability
ml_logit_haty <- predict(ml_logit, type="probs", newdata = pants_tst)
ml_logit_haty[1:10,]

# Predict the class label
ml_logit_prey <- predict(ml_logit, newdata = pants_tst)

cfmatrix <- table(pants_tst$size, ml_logit_prey)
cfmatrix
perf_eval3(cfmatrix) #단순 정확도, 균형정확도


