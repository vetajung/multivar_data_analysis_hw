#Assignment3_2015170378 정은영

install.packages("moments") 
install.packages("corrplot")
library(moments)
library(corrplot)

df <- read.csv('kc_house_data.csv')
str(df)
head(df,3)  #데이터 확인
summary(df)

#[Q1]

df[1:2] <- NULL #id, date 제거

#[Q2]
#column 첫번째는 Price(종속변수)
#Price를 y축, 나머지 변수들을 x 축으로 각각 그래프 그려보기
par(mfrow = c(2,2))
for(i in 2:length(colnames(df))){
  plot(df[,i], df$price, main = names(df[i]), ylab = names(df$price), xlab = "", col = 'green')
}

#데이터프레임을 참고하고 그래프를 그려본 결과 변수 bedrooms, bathrooms, floors, waterfront, view, condition, grade,  zipcode 가 discrete 변수라고 생각됨.
#따라서 이 변수들의 column index를 구해보면 2,3,6,7,8,9,10,15 임.

cateIdx <- c(2,3,6,7,8,9,10,15) #categorical Var index
conIdx = c(2:19)[!(c(2:19) %in% cateIdx)] #non categorical index

nMat <- matrix(c(1:10*4),nrow=10,ncol=4)
colnames(nMat)<- c("mean", "std","skewness","kurtosis")
rownames(nMat)<- colnames(df[conIdx])

#continuous value
par(mfrow=c(2,2))
for(i in 1:length(conIdx)){
  hist(df[,conIdx[i]], main = paste("Histogram of" , colnames(df[conIdx[i]])))
  nMat[i,1] <- mean(unlist(df[,conIdx[i]]))
  nMat[i,2] <- sqrt(var(df[,conIdx[i]]))
  nMat[i,3] <- skewness(df[,conIdx[i]])
  nMat[i,4] <- kurtosis(df[,conIdx[i]])
}

#discrete value boxplot
par(mfrow=c(2,2))
for(i in cateIdx){
  boxplot(df[,1]~df[,i], xlab='', main=names(df[i]), col=c("yellow","green"))
}


#[Q3]
#bedroom의 경우 0-11 까지와 갑자기 33이 등장한다. 10보다 큰 값 제거

df$bedrooms<- ifelse(df$bedrooms> 10, NA, df$bedrooms) #outlier 제거

df<- na.omit(df) #결측치 제거

#11,33 모두 제거됨.

#[Q4] 
corr <- cor(df) #correlation
corrplot(corr, method = "color", outline = T, cl.pos = 'n', rect.col = "black",  tl.col = "indianred4", addCoef.col = "black", number.digits = 2, number.cex = 0.60, tl.cex = 0.7, cl.cex = 1, col = colorRampPalette(c("green4","white","red"))(100)) #Corrlation Plot

# Price와의 correlation 
#sqft_living(0.7), grade(0.67), sqft_above(0.6), sqft_living15(0.59), #bathrooms(0.52)

#Price 제외한 두 변수의 상관관계가 높은 것들
#sqft_living & sqft_above(0.87), sqft_living & sqft_living15(0.76), sqft_living & grade(0.77), sqft_living & bathrooms(0.75)


#[Q5] 
# Split the data into the training/validation sets

nR <- nrow(df) #행의 갯수 계산하는 명령어
nC <- ncol(df) #열의 갯수 계산하는 명령어

set.seed(12345) #랜덤 알고리즘 지정
df_trn_idx <- sample(1:nR, round(0.7*nR)) #70%를 샘플링
df_trn_data <- df[df_trn_idx,]
df_val_data <- df[-df_trn_idx,] #training 아닌것들

mlr_df <- lm(price ~ ., data = df_trn_data)
summary(mlr_df)
plot(mlr_df)

#[Q6] 

uColNames <- colnames(df)[-12] #sqft_basement는 NA 이므로 제외
for (i in 2:(length(summary(mlr_df)$coefficient[,1]))){
  if(summary(mlr_df)$coefficient[(i),4] <= 0.01){
    print(paste(uColNames[i],"//", summary(mlr_df)$coefficient[(i),1]))
  }
}

#[Q7] Test 데이터셋에 대하여 MAE, MAPE, RMSE를 계산하고 그에 대한 해석을 해 보시오.

perf_eval_reg <- function(tgt_y, pre_y){ #tgt=정답, pre=예측 제공
  
  # RMSE
  rmse <- sqrt(mean((tgt_y - pre_y)^2))
  # MAE
  mae <- mean(abs(tgt_y - pre_y))
  # MAPE
  mape <- 100*mean(abs((tgt_y - pre_y)/tgt_y))
  
  return(c(rmse, mae, mape))
  
}

perf_mat <- matrix(0, nrow = 2, ncol = 3)

# Initialize a performance summary
rownames(perf_mat) <- c("House Data(full model)","House Data(7-Var model)")
colnames(perf_mat) <- c("RMSE", "MAE", "MAPE")

#함수 사용하여 MAE, MAPE, RMSE 계산
mlr_df_haty <- predict(mlr_df, newdata = df_val_data)
perf_mat[1,] <- perf_eval_reg(df_val_data$price, mlr_df_haty)
perf_mat #MAE는 가격 오차, MAPE는 % 오차


#[Q8] 

install.packages("car")
library(car)

mlr2_df <- lm(price ~ bedrooms + bathrooms + sqft_living + waterfront + view + grade + sqft_above + yr_renovated + lat + sqft_living15, data = df_trn_data)
summary(mlr2_df)
plot(mlr2_df)
vif(mlr2_df)

mlr3_df <- lm(price ~ bedrooms + bathrooms + waterfront +  sqft_living  + view + grade + yr_renovated + lat , data = df_trn_data)
summary(mlr3_df)
plot(mlr3_df)
vif(mlr3_df)

#[Q9] 

mlr4_df <- lm(price ~ bedrooms + bathrooms + waterfront +  sqft_living  + view + grade + lat , data = df_trn_data)
summary(mlr4_df)
plot(mlr4_df)
vif(mlr4_df)

#함수 사용하여 MAE, MAPE, RMSE 계산
mlr4_df_haty <- predict(mlr4_df, newdata = df_val_data)
perf_mat[2,] <- perf_eval_reg(df_val_data$price, mlr4_df_haty)
perf_mat #MAE는 절대적 오차, MAPE는 % 오차

#[Extra Question]

hist(df[,1], main = paste("Histogram of" , colnames(df[1])))
df$price <- log(df$price)
hist(df[,1], main = paste("Histogram of" , colnames(df[1])))

skewness(df[,1])
kurtosis(df[,1])

corr <- cor(df) #correlation
corrplot(corr, method = "color", outline = T, cl.pos = 'n', rect.col = "black",  tl.col = "indianred4", addCoef.col = "black", number.digits = 2, number.cex = 0.60, tl.cex = 0.7, cl.cex = 1, col = colorRampPalette(c("green4","white","red"))(100)) #Corrlation Plot

# Split the data into the training/validation sets

nR <- nrow(df) #행의 갯수 계산하는 명령어
nC <- ncol(df) #열의 갯수 계산하는 명령어

set.seed(12345) #랜덤 알고리즘 지정
df_trn_idx <- sample(1:nR, round(0.7*nR)) #70%를 샘플링
df_trn_data <- df[df_trn_idx,]
df_val_data <- df[-df_trn_idx,] #training 아닌것들

mlr5_df <- lm(price ~ ., data = df_trn_data)
summary(mlr5_df)
plot(mlr5_df)

#perf_mat
perf_mat2 <- matrix(0, nrow = 2, ncol = 3)
rownames(perf_mat2) <- c("House Data(full model)","House Data(9-Var model)")
colnames(perf_mat2) <- c("RMSE", "MAE", "MAPE")

#함수 사용하여 MAE, MAPE, RMSE 계산
mlr5_df_haty <- predict(mlr5_df, newdata = df_val_data)
perf_mat2[1,]<-perf_eval_reg(df_val_data$price, mlr5_df_haty)
perf_mat2

#
mlr6_df <- lm(price ~ bedrooms + bathrooms + sqft_living + sqft_lot + floors + waterfront + view + grade + yr_renovated + lat +sqft_living15 , data = df_trn_data)
summary(mlr6_df)
plot(mlr6_df)
vif(mlr6_df)

mlr7_df <- lm(price ~ bathrooms + sqft_living + sqft_lot + waterfront + view + grade + yr_renovated + lat +sqft_living15 , data = df_trn_data)
summary(mlr7_df)
plot(mlr7_df)
vif(mlr7_df)

#함수 사용하여 MAE, MAPE, RMSE 계산
mlr7_df_haty <- predict(mlr7_df, newdata = df_val_data)
perf_mat2[2,]<-perf_eval_reg(df_val_data$price, mlr7_df_haty)
perf_mat2
