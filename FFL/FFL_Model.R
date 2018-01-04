#### FFL Model #####
# Aug 31, 2017


# set_env -----------------------------------------------------------------

library(rIA)
set_env(clear_env = T, dir = "C:/Users/AblMi001/Desktop/FFL", extra_pkgs = c("caret", "randomForest", "gbm", "xgboost", "xlsx"))


# pull data ---------------------------------------------------------------

team_ref <- read.csv("Team_Ref.csv")

play_11 <- read.csv("2011_PlayerStats.csv")
play_12 <- read.csv("2012_PlayerStats.csv")
play_13 <- read.csv("2013_PlayerStats.csv")
play_14 <- read.csv("2014_PlayerStats.csv")
play_15 <- read.csv("2015_PlayerStats.csv")
play_16 <- read.csv("2016_PlayerStats.csv")

players <- rbind(play_11, play_12, play_13, play_14, play_15, play_16) %>% mutate(FantPt = ifelse(is.na(FantPt), 0, FantPt))
play_misc <- read.csv("2011-2016_MiscStats.csv")

team_11 <- read.csv("2011_TeamOff.csv")
team_12 <- read.csv("2012_TeamOff.csv")
team_13 <- read.csv("2013_TeamOff.csv")
team_14 <- read.csv("2014_TeamOff.csv")
team_15 <- read.csv("2015_TeamOff.csv")
team_16 <- read.csv("2016_TeamOff.csv")

teams <- rbind(team_11, team_12, team_13, team_14, team_15, team_16)
team_records <- read.csv("2011-2016_TeamRecords.csv")
team_stats <- read.csv("2011-2016_TeamStats.csv")

# clean data --------------------------------------------------------------

players1 <- players %>% 
  dplyr::mutate(YR_plus1 = YR + 1) %>% 
  left_join(select(.data = players, YR, PlayerId, FantPt), by = c("YR_plus1" = "YR", "PlayerId")) %>%
  dplyr::rename(FantPt = FantPt.x, FantPt_proj = FantPt.y) %>%
  select(-YR_plus1, -OvRank) 

teams1 <- inner_join(teams, team_ref, by = c("Tm_Name"))

play_team <- inner_join(players1, teams1, by = c("YR", "Tm"))

pos_trans <- play_team %>%
  dplyr::mutate(ct = 1) %>%
  dcast(YR + PlayerId ~FantPos,  fill = 0, value.var = "ct")

df <- inner_join(play_team, pos_trans, by = c("YR", "PlayerId")) %>%
  select(YR, PlayerId, Name,  FantPos, Tm, Tm_Name, FantPt_proj, FantPt, QB, RB, WR, TE, Age, everything()) %>%
  dplyr::mutate(RY.A = ifelse(is.na(RY.A), 0, RY.A),
         Tgt = ifelse(is.na(Tgt), 0, Tgt),
         ReY.R = ifelse(is.na(ReY.R), 0, ReY.R)) %>%
  dplyr::filter(!is.na(Age)) %>%
  #dplyr::filter(G >= 12) %>%
  inner_join(select(.data = play_misc, - Name, -X2PM), by = c("PlayerId", "YR")) %>%
  inner_join(team_stats, by = c("YR", "Tm_Name")) %>%
  inner_join(team_records, by = c("YR", "Tm_Name")) 

df[is.na(df)] <- 0

df_pergame <- as.data.frame(sapply(select(.data = df, FantPt , PCmp:ReTD , -RY.A, -ReY.R, Total_TD, Total_Pts), function(x) x/df$G))
df1 <- select(.data = df, -FantPt, -(PCmp:ReTD), -Total_TD, -Total_Pts) %>% cbind(df_pergame) %>% filter(G >= 6) %>% select(-G, -GS)

df_hist <- df1 %>% dplyr::filter(YR != 2016) %>% dplyr::filter(!is.na(FantPt_proj))
df_2017 <- df1 %>% dplyr::filter(YR == 2016)
#df_hist <- df %>% dplyr::filter(YR != 2016) %>% dplyr::filter(!is.na(FantPt_proj))
#df_2017 <- df %>% dplyr::filter(YR == 2016)

# prep data ---------------------------------------------------------------

#split data
set.seed(83)
training <- createDataPartition(df_hist$FantPt_proj, p = .6, list = F)
df_train <- df_hist[training, -c(1:6)] #remove nominal data: [1] "YR"          "PlayerId"    "Name"        "FantPos"     "Tm"          "Tm_Name"
df_test <- df_hist[-training, -c(1:6)]

# model -------------------------------------------------------------------

#linear regression
lm.model <- train(FantPt_proj~., df_train, method = "lm")
summary(lm.model)
varImp(lm.model)

lm.model.pred <- predict(lm.model, df_test)
RMSE(lm.model.pred, df_test$FantPt_proj) #47.9
plot(lm.model.pred, df_test$FantPt_proj, col = "red", main = "LM Model", xlim = c(0,500), ylim = c(0,500))
abline(0,1)

#cv linear regression
ctr <- trainControl(method = "cv", number = 10)
cv.model <- train(FantPt_proj~., df_hist[,-c(1:6)], trControl = ctr, method = "lm")
summary(cv.model)
varImp(cv.model)

cv.model.pred <- predict(cv.model, df_test)
RMSE(cv.model.pred, df_test$FantPt_proj) #46.6
plot(cv.model.pred, df_test$FantPt_proj, col = "red", main = "CV Model", xlim = c(0,500), ylim = c(0,500))
abline(0,1)

#rf
set.seed(83)
rf.model <- randomForest(FantPt_proj~., df_train, ntree = 2500, mtry = 17, importance = T)


rf.model.pred <- predict(rf.model, df_test)
RMSE(rf.model.pred, df_test$FantPt_proj) #47.7 2500/17
plot(rf.model.pred, df_test$FantPt_proj, col = "red", main = "RF Model", xlim = c(0,500), ylim = c(0,500))
abline(0,1)
varImpPlot(rf.model)

#gbm
gbmGrid <- expand.grid(
  n.trees = c(100, 175, 250),
  shrinkage = c(0.1, 0.15, 0.2),
  interaction.depth = c(2, 4, 6),
  n.minobsinnode = c(10))
gbmCtrl <- trainControl(method = "cv", number = 8)

set.seed(83)
gbm.model <- train(as.matrix(df_train[, -c(1)]), df_train$FantPt_proj, tuneGrid = gbmGrid, trControl = gbmCtrl, method = "gbm")
varImp(gbm.model)

head(gbm.model$results[with(gbm.model$results, order(RMSE)),]) #get the top 5 models
#    shrinkage interaction.depth n.minobsinnode n.trees     RMSE  Rsquared   RMSESD RsquaredSD
#1       0.10                 2             10     100 49.93067 0.5542623 1.583764 0.06120063
#19      0.20                 2             10     100 50.17523 0.5539047 2.039507 0.06452302
#4       0.10                 4             10     100 51.01560 0.5384620 1.297813 0.05904259
#2       0.10                 2             10     175 51.04656 0.5357207 1.818982 0.06694802
#10      0.15                 2             10     100 51.09855 0.5372372 1.621631 0.05914257
#20      0.20                 2             10     175 51.10587 0.5401385 2.141261 0.06595684

gbm.model.pred <- predict(gbm.model, df_test)
RMSE(gbm.model.pred, df_test$FantPt_proj) #47.8
plot(gbm.model.pred, df_test$FantPt_proj, col = "red", main = "GBM Model", xlim = c(0,500), ylim = c(0,500))
abline(0,1)

#xgboost (non-caret)

xgb.model <- xgboost(data = as.matrix(df_train[,-c(1)]),
                     label = df_train$FantPt_proj,
                     objective = "reg:linear",
                     seed = 83,
                     eval_metric = "rmse",
                     nrounds = 100,
                     eta = 0.1175,
                     max_depth = 2,
                     nthread = 2)

xgb.model.pred <- predict(xgb.model, as.matrix(df_test[,-1]))
RMSE(xgb.model.pred, df_test$FantPt_proj) #47.6 100/.1175
plot(xgb.model.pred, df_test$FantPt_proj, col = "red", main = "XGBOOST Model", xlim = c(0,500), ylim = c(0,500))
abline(0,1)
xgb.importance(feature_names = names(df_train[,-1]) ,model = xgb.model)

# final predictions -------------------------------------------------------

df_2017_matx <- df_2017[, -c(1:6)] # for gbm/xgboost if used

final_pred <- predict(cv.model, df_2017)
final_pred1 <- predict(xgb.model, data.matrix(df_2017_matx[, -1]))
#final_pred <- predict(gbm.model, df_2017_matx)

final_drfts <- df_2017 %>% dplyr::mutate(FantPt_proj = final_pred)
final_drfts1 <- df_2017 %>% dplyr::mutate(FantPt_proj = final_pred1) #xgboost
final_drfts2 <- df_2017 %>% dplyr::mutate(FantPt_proj_cv = final_pred,
                                          FantPt_proj_xg = final_pred1,
                                          FantPt_proj = (FantPt_proj_cv + FantPt_proj_xg)/2) #ensemble


# output ------------------------------------------------------------------

write.xlsx(final_drfts, "FFL_2017_DraftPcks.xlsx", sheetName = "Best CV LM", row.names = F)
write.xlsx(final_drfts1, "FFL_2017_DraftPcks.xlsx", sheetName = "XGBoost", row.names = F, append = T )
write.xlsx(final_drfts2, "FFL_2017_DraftPcks.xlsx", sheetName = "Ensemble", row.names = F, append = T )

