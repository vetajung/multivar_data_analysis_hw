#Assignment5_2015170378정은영

# Install necessary packages
# glmnet: Ridge, Lasso, Elastic Net Logistic Regression 
install.packages("glmnet")
install.packages("GA")

library(glmnet)
library(GA)

# Load the data & Preprocessing
df <- read.csv("Weather_Ankara.csv")
df_input <- df[,-10]
df_input_scaled <- scale(df_input, center = TRUE, scale = TRUE) #scaling,정규화 수행
df_target <- df$Mean_temperature
df_data_scaled <- data.frame(df_input_scaled, df_target)

nR <- nrow(df_data_scaled)
nC <- ncol(df_data_scaled)

set.seed(12345) #랜덤 알고리즘 지정
df_trn_idx <- sample(1:nR, 250) 
df_trn <- df_data_scaled[df_trn_idx,] #250개를 training dataset으로
df_val <- df_data_scaled[-df_trn_idx,] #training 아닌것들


#[Q1] 
# RMSE, MAE, MAPE 산출 함수
perf_eval_reg <- function(tgt_y, pre_y){ #tgt=정답, pre=예측 제공
  
  # RMSE
  rmse <- sqrt(mean((tgt_y - pre_y)^2))
  # MAE
  mae <- mean(abs(tgt_y - pre_y))
  # MAPE
  mape <- 100*mean(abs((tgt_y - pre_y)/tgt_y))
  
  return(c(rmse, mae, mape))
}
perf_mat <- matrix(0, nrow = 6, ncol = 6)
# Initialize a performance summary
rownames(perf_mat) <- c("Weather data(full model)","Weather Data(Exhaustive Search)","Weather Data(Forward)","Weather Data(Backward)","Weather Data(Stepwise)","Weather Data(GA)")
colnames(perf_mat) <- c("input_formula","Adj.R^2","Time","RMSE", "MAE", "MAPE")

#MLR
full_model <- lm(df_target ~ ., data = df_trn)
summary(full_model)
#adjusted R^2 = 0.9882, **(0.01) or ***(0.001) 을 고르면 Max_tem, Min_tem, Dewpoint, Sea_level_pressure
perf_mat[1,1] <- as.character(summary(full_model)$call)[2]
perf_mat[1,2] <- summary(full_model)$adj.r.squared
perf_mat[1,3] <- NA

#함수 사용하여 MAE, MAPE, RMSE 계산
mlr_df_haty <- predict(full_model, newdata = df_val)
perf_mat[1,4:6] <- perf_eval_reg(df_val$df_target, mlr_df_haty)
perf_mat

#[Q2]
es <- function(trn_data){
  target <- c(0,0,0,0,0,0,0,0,0)
  dfs <- function(len, idx, target,trn_data,adjR2_max){
    a <- c(1,2,3,4,5,6,7,8,9)
    if(len == l){
      tmp_x <- paste(colnames(df_trn)[target[1:l]], collapse=" + ")
      tmp_xy <- paste("df_target ~ ", tmp_x, collapse = "")
      es_model <- lm(as.formula(tmp_xy), data = df_trn)
      adjR2 <- round(summary(es_model)$adj.r.squared,digits = 5)
      print(paste("adj.R^2:",adjR2,",formular: ",tmp_xy))
      return()
    }
    nxt <- a[(idx+1)]
    target[(len+1)] <- nxt
    dfs((len + 1), (idx + 1),target,trn_data,adjR2_max)
    target[len+1] <- 0
    if((9-idx)>(l-len)) {
      return(dfs(len, (idx+1),target,trn_data,adjR2_max))
    }
  }
  for(l in 1:9){
    dfs(0,0,target, trn_data,0)
  }
}

start_time <- proc.time()
es(df_trn)
end_time <- proc.time()
perf_mat[2,3] <- (end_time - start_time)[3]

es_Var <-   "df_target ~  Max_termperature + Min_temperature + Dewpoint + Sea_level_pressure + Standard_pressure + Visibility + Wind_speed"

es_model <- lm(as.formula(es_Var), data = df_trn)
summary(es_model)
perf_mat[2,2] <- summary(es_model)$adj.r.squared
perf_mat[2,1] <- es_Var
# Make prediction
mlr_df_haty2 <- predict(es_model, newdata = df_val)
perf_mat[2,4:6] <- perf_eval_reg(df_val$df_target, mlr_df_haty2)
perf_mat

#[Q3] 
#첫번째 - Foward, 두번째- Backward, 세번째- Stepwise
model_v <- c("df_target~1", "full_model", "full_model")
model_d <- c("forward", "backward", "both")

# Variable selection method: Forward Selection, Backward Elimination, Stepwise Selection

for(i in 1:3){
  start_time <- proc.time()
  print(model_d[i])
  i_model <- step(lm(as.formula(model_v[i]), data =df_trn), 
                  scope = list(upper = full_model, lower = df_target ~ 1), 
                  direction= model_d[i], trace = 1) 
  end_time <- proc.time()
  perf_mat[i+2,3] <- (end_time - start_time)[3]
  summary(i_model)
  perf_mat[i+2,2] <- summary(i_model)$adj.r.squared
  perf_mat[i+2,1] <- as.character(summary(i_model)$call)[2]
  # Make prediction
  mlr_df_haty2 <- predict(i_model, newdata = df_val)
  perf_mat[i+2,4:6] <- perf_eval_reg(df_val$df_target, mlr_df_haty2)
}
perf_mat


#[Q4] 
fit_F1 <- function(string){
  sel_var_idx <- which(string == 1) #사용하는 변수의 index 항에 집어넣기.
  # Use variables whose gene value is 1
  sel_x <- x[, sel_var_idx] #사용하라고 지정된 것만 사용
  xy <- data.frame(sel_x, y)
  # Training the model
  GA_lr <- lm(y ~ ., data = xy)
  return(summary(GA_lr)$adj.r.squared)
}

x <- as.matrix(df_trn[,-10])
y <- df_trn[,10]

start_time <- proc.time()
#maxiter 150,elitism //maxiter 50,elitism 0 으로 두번 수행해봄
GA_F1 <- ga(type = "binary", fitness = fit_F1, nBits = ncol(x), 
            names = colnames(x), popSize = 100, pcrossover = 0.5, 
            pmutation = 0.01, maxiter = 50, elitism = 0, seed = 123)
end_time <- proc.time()
perf_mat[6,3] <- (end_time - start_time)[3]
best_var_idx <- which(GA_F1@solution == 1)
colnames(df_trn)[best_var_idx]

tmp_x <- paste(colnames(df_trn)[best_var_idx], collapse=" + ")
tmp_xy <- paste("df_target ~ ", tmp_x, collapse = "") 
perf_mat[6,1] <- tmp_xy
as.formula(tmp_xy) #string을 formula라는 새로운 데이터 형태로 바꿈(작동시키기위해)

ga_model <- lm(tmp_xy, data = df_trn)
summary(ga_model)
perf_mat[6,2] <- summary(ga_model)$adj.r.squared

mlr_df_haty5 <- predict(ga_model, newdata = df_val)
perf_mat[6,4:6] <- perf_eval_reg(df_val$df_target, mlr_df_haty5)
perf_mat


#[Q5] 
gmat <- matrix(0, nrow = 10, ncol = 6)
rownames(gmat) <- c("population Size(100)","population Size(20)","Cross-over rate(0.5)","Cross-over rate(0.2)","mutation(0.005)","mutation(0.08)","maxiter(150)","maxiter(50)","elitism(0)","elitism(2)")
colnames(gmat) <- c("input_formula","Adj.R^2","Time","RMSE", "MAE", "MAPE")

pS <- c(100,20,100,100,100,100,100,100,100,100)
pco <- c(0.5,0.5,0.5,0.2,0.5,0.5,0.5,0.5,0.5,0.5)
pm <- c(0.005,0.005,0.005,0.005,0.005,0.08,0.005,0.005,0.005,0.005)
mt <- c(150,150,150,150,150,150,150,50,150,150)
el <- c(0,0,0,0,0,0,0,0,0,2)

for(i in 1:10){
  start_time <- proc.time()
  GA_F1 <- ga(type = "binary", fitness = fit_F1, nBits = ncol(x), 
                names = colnames(x), popSize = pS[i], pcrossover = pco[i], 
                pmutation = pm[i], maxiter = mt[i], elitism = el[i], seed = 123)
  end_time <- proc.time()
  gmat[i,3] <- (end_time - start_time)[3]
  best_var_idx <- which(GA_F1@solution == 1)
  colnames(df_trn)[best_var_idx]
  
  tmp_x <- paste(colnames(df_trn)[best_var_idx], collapse=" + ")
  tmp_xy <- paste("df_target ~ ", tmp_x, collapse = "") 
  gmat[i,1] <- tmp_xy
  as.formula(tmp_xy) #string을 formula라는 새로운 데이터 형태로 바꿈(작동시키기위해)
  
  ga_model <- lm(tmp_xy, data = df_trn)
  summary(ga_model)
  gmat[i,2] <- summary(ga_model)$adj.r.squared
  
  mlr_df_haty5 <- predict(ga_model, newdata = df_val)
  gmat[i,4:6] <- perf_eval_reg(df_val$df_target, mlr_df_haty5)
}

gmat

