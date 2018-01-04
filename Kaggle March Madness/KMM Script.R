################ KMM 2017 ####################

#NOTES:


##############################################


# Load and Setup Packages -------------------------------------------------

library(rIA)
library(xlsx)
library(openxlsx)
library(rpart)
library(rpart.plot)
library(ROSE)
library(caret)
library(randomForest)
library(e1071)
library(neuralnet)
library(kernlab)
library(nnet)
library(reshape)
library(dplyr)

set_env(clear_env = T, dir = "C:/Users/AblMi001/Desktop/Kaggle March Madness" )


# Load Data ---------------------------------------------------------------
  
  #training data
  regularseason_detailed <- read.csv("RegularSeasonDetailedResults.csv", header = T)
  tourney_detailed <- read.csv("TourneyDetailedResults.csv", header = T)
  tourney_locations <- read.csv("TourneyGeog.csv", header = T)
  
  #references
  final4regions <- read.csv("Seasons.csv", header = T)
  team_IDs <- read.csv("Teams.csv", header = T)
  seedteam_IDs <- read.csv("TourneySeeds.csv", header = T)
  final4seedmatchups <- read.csv("TourneySlots.csv", header = T)
  team_locations <- read.csv("TeamGeog.csv", header = T)
 
  
  #sample submission
  samplesubmission <- read.csv("SampleSubmission.csv", header = T)


# Build Regular Season Data Set -------------------------------------------
  
  #rollup regular season data
  regularseason_detailed_wins <- regularseason_detailed %>%
    group_by(Season, Wteam) %>%
    summarise(wins = n(),
              wscore = sum(Wscore, na.rm = T),
              wfgm = sum(Wfgm, na.rm = T),
              wfga = sum(Wfga, na.rm = T),
              wgfm3 = sum(Wfgm3, na.rm = T),
              wgfa3 = sum(Wfga3, na.rm = T),
              wftm = sum(Wftm, na.rm = T),
              wfta = sum(Wfta, na.rm = T),
              wor = sum(Wor, na.rm = T),
              wdr = sum(Wdr, na.rm = T),
              wast = sum(Wast, na.rm = T),
              wto = sum(Wto, na.rm = T),
              wstl = sum(Wstl, na.rm = T),
              wblk = sum(Wblk, na.rm = T),
              wpf = sum(Wpf, na.rm = T)) %>%
    rename(team = Wteam)
    
  regularseason_detailed_losses <- regularseason_detailed %>%
    group_by(Season, Lteam) %>%
    summarise(losses = n(),
              lscore = sum(Lscore, na.rm = T),
              lfgm = sum(Lfgm, na.rm = T),
              lfga = sum(Lfga, na.rm = T),
              lgfm3 = sum(Lfgm3, na.rm = T),
              lgfa3 = sum(Lfga3, na.rm = T),
              lftm = sum(Lftm, na.rm = T),
              lfta = sum(Lfta, na.rm = T),
              lor = sum(Lor, na.rm = T),
              ldr = sum(Ldr, na.rm = T),
              last = sum(Last, na.rm = T),
              lto = sum(Lto, na.rm = T),
              lstl = sum(Lstl, na.rm = T),
              lblk = sum(Lblk, na.rm = T),
              lpf = sum(Lpf, na.rm = T)) %>%
    rename(team = Lteam)
  
    regularseason_final <- inner_join(regularseason_detailed_wins, regularseason_detailed_losses, by = c("Season", "team")) %>%
      mutate(vscore = wscore - lscore,
             vfgm = wfgm - lfgm,
             vfga =   wfga - lfga,       
             vgfa3 = wgfa3 - lgfa3,
             vftm =  wftm - lftm,
             vfta =  wfta - lfta,
             vor =  wor - lor,
             vdr =  wdr - ldr,
             vast =  wast - last,
             vto =  wto - lto,
             vstl =  wstl - lstl,
             vblk =  wblk - lblk,
             vpf = wpf - lpf,
             games = wins + losses) %>%
      mutate(ascore = (wscore + lscore)/games,
             afgm = (wfgm + lfgm)/games,
             afga =   (wfga + lfga)/games,       
             agfa3 = (wgfa3 + lgfa3)/games,
             aftm =  (wftm + lftm)/games,
             afta =  (wfta + lfta)/games,
             aor =  (wor + lor)/games,
             adr =  (wdr + ldr)/games,
             aast =  (wast + last)/games,
             ato =  (wto + lto)/games,
             astl =  (wstl + lstl)/games,
             ablk =  (wblk + lblk)/games,
             apf = (wpf + lpf)/games) %>%
      inner_join(team_locations, by = c("team" = "team_id"))
      
    
# Build Tourney Dataset ------------------------------------------


  #tourney data
  tourney <- tourney_locations %>%
      select(season, wteam, lteam) %>%
      inner_join(regularseason_final, by = c("season" = "Season", "wteam" = "team")) %>%
      inner_join(regularseason_final, by = c("season" = "Season", "lteam" = "team")) %>%
      left_join(seedteam_IDs, by = c("season" = "Season", "wteam" = "Team")) %>%
      left_join(seedteam_IDs, by = c("season" = "Season", "lteam" = "Team")) %>%
      mutate(dseed = as.numeric(substring(Seed.x,2,3)) - as.numeric(substring(Seed.y,2,3)),
             dwins = wins.x - wins.y,
             dgames = games.x - games.y,
             dscore = (wscore.x + lscore.x) - (wscore.y + lscore.y),
             dfgm = (wfgm.x + lfgm.x) - (wfgm.y + lfgm.y),
             dgfm3 = (wgfm3.x + lgfm3.x) - (wgfm3.y + lgfm3.y),
             dftm = (wftm.x + lftm.x) - (wftm.y + lftm.y),
             dor = (wor.x + lor.x) - (wor.y + lor.y),
             ddr = (wdr.x + ldr.x) - (wdr.y + ldr.y),
             dast = (wast.x + last.x) - (wast.y + last.y),
             dto = (wto.x +lto.x ) - (wto.y + lto.y),
             dblk = (wblk.x + lblk.x) - (wblk.y + lblk.y),
             dascore = ascore.x - ascore.y,
             dafgm = afgm.x -  afgm.y,
             dafga = afga.x -  afga.y,  
             dagfa3 = agfa3.x - agfa3.y,
             daftm = aftm.x - aftm.y,
             dafta = afta.x - afta.y,  
             daor =  aor.x -  aor.y,  
             dadr =  adr.x -  adr.y,   
             daast =  aast.x - aast.y, 
             dato = ato.x - ato.y,
             dastl = astl.x - astl.y, 
             dablk = ablk.x - ablk.y,    
             dapf =  apf.x -  apf.y)%>%
      select( -(wins.x:lng.x), -(wins.y:lng.y), -Seed.x, -Seed.y) %>%
      mutate(FirstTeam = ifelse(wteam > lteam, wteam, lteam),
             SecondTeam = ifelse(wteam > lteam, lteam, wteam),
             FirstTeam_Win = ifelse(wteam == FirstTeam, 1,0))

    
    tourney1 <- apply(tourney[,c(4:28)], 2, function(x) ifelse(tourney$FirstTeam == tourney$wteam, x, x*-1 ))
    tourney2 <- tourney %>%
      select(season:lteam, FirstTeam:FirstTeam_Win) 
    tourney2 <- cbind(tourney2, tourney1) %>%
      left_join(team_IDs, by = c("FirstTeam" = "Team_Id")) %>%
      left_join(team_IDs, by = c("SecondTeam" = "Team_Id")) %>%
      select(-Team_Name.x, -Conference.x, -Team_Name.y, -Conference.y)
      
    #-P5, -Mid.Major, - Conference
    
# Split Train and Test Data -----------------------------------------------

    
    tourney_final <- tourney2 %>% 
      select(-(season:lteam),-(FirstTeam:SecondTeam)) %>%
      mutate(FirstTeam_Win = as.factor(FirstTeam_Win))
    
    set.seed(85)
    intrain <- createDataPartition(tourney_final$FirstTeam_Win, p = 0.75, list = F)
    
    d.train <- tourney_final[intrain,]
    d.test <- tourney_final[-intrain,]
    

# Logistic Model ----------------------------------------------------------------
    
    #model 
    set.seed(39)
    logistic.model <- train(FirstTeam_Win~., d.train, method = "glm")
    #logistic.model$finalModel #look at coeff
    plot(varImp(logistic.model))
    
    
    #prediction
    logistic.pred <- predict(logistic.model, d.test)
    confusionMatrix(logistic.pred, d.test$FirstTeam_Win, positive = "1") #Accuracy = 0.708, 
    roc.curve(d.test$FirstTeam_Win,logistic.pred)
    
    #logistic.pred.out <- predict(logistic.model, d.test, type = "prob")

# Random Forest Model -----------------------------------------------------

  #model
  set.seed(39)
  rtree.model <- train(FirstTeam_Win~., d.train, method = "rf")
  plot(varImp(rtree.model))
  
  
  #prediction
  rtree.pred <- predict(rtree.model,d.test)
  confusionMatrix(rtree.pred, d.test$FirstTeam_Win) #Accuracy = 0.712
  roc.curve(d.test$FirstTeam_Win, rtree.pred)
    

# Decision Tree Model -----------------------------------------------------
  
  #model
  set.seed(39)
  tree.model <- train(FirstTeam_Win~., d.train, method = "rpart")
  #plot(tree.model)
  
  #prediction
  tree.pred <- predict(tree.model, d.test)
  confusionMatrix(tree.pred, d.test$FirstTeam_Win) #Accuracy = 0.67
  roc.curve(d.test$FirstTeam_Win, tree.pred)
  
  #tree.pred.out <- predict(tree.model, d.test, type = "prob") #output prediction probabilities


# SVM Model ---------------------------------------------------------------

  #model
  set.seed(39)
  svm.model <- train(FirstTeam_Win~., d.train, method = "svmRadial")
  plot(varImp(svm.model))
  
  #prediction
  svm.pred <- predict(svm.model, d.test, probability=TRUE)
  confusionMatrix(svm.pred, d.test$FirstTeam_Win) #Accuracy 0.717
  roc.curve(d.test$FirstTeam_Win, svm.pred)
    
  
# Neural Network ----------------------------------------------------------

  #model
  set.seed(39)
  nn.model <- train(FirstTeam_Win~., d.train, method = "nnet",  lineout = F, trace = F)
  plot(varImp(nn.model))
  
  #predict
  nn.pred <- predict(nn.model, d.test)
  confusionMatrix(nn.pred, d.test$FirstTeam_Win, positive = "1") #Accuracy 0.714
  roc.curve(d.test$FirstTeam_Win, nn.pred)

# Compare Models ----------------------------------------------------------

  modelvalues <- resamples(list(logmod = logistic.model, 
                                dtreemod = tree.model, 
                                rtmod = rtree.model,
                                svmmod = svm.model,
                                nnmod = nn.model))

  summary(modelvalues)

  

# Kaggle Submission File Step 1-------------------------------------------------------

  samplesubmission1 <- colsplit(as.character(samplesubmission$Id),split =  "_", names = c("Season", "FirstTeam", "SecondTeam"))
  samplesubmission2 <- cbind(samplesubmission, samplesubmission1)
  samplesubmission2 <- samplesubmission2[,-2]
  
  #submission
  submission <- samplesubmission2 %>%
    inner_join(regularseason_final, by = c("Season" = "Season", "FirstTeam" = "team")) %>%
    inner_join(regularseason_final, by = c("Season" = "Season", "SecondTeam" = "team")) %>%
    left_join(seedteam_IDs, by = c("Season" = "Season", "FirstTeam" = "Team")) %>%
    left_join(seedteam_IDs, by = c("Season" = "Season", "SecondTeam" = "Team")) %>%
    mutate(dseed = as.numeric(substring(Seed.x,2,3)) - as.numeric(substring(Seed.y,2,3)),
           dwins = wins.x - wins.y,
           dgames = games.x - games.y,
           dscore = (wscore.x + lscore.x) - (wscore.y + lscore.y),
           dfgm = (wfgm.x + lfgm.x) - (wfgm.y + lfgm.y),
           dgfm3 = (wgfm3.x + lgfm3.x) - (wgfm3.y + lgfm3.y),
           dftm = (wftm.x + lftm.x) - (wftm.y + lftm.y),
           dor = (wor.x + lor.x) - (wor.y + lor.y),
           ddr = (wdr.x + ldr.x) - (wdr.y + ldr.y),
           dast = (wast.x + last.x) - (wast.y + last.y),
           dto = (wto.x +lto.x ) - (wto.y + lto.y),
           dblk = (wblk.x + lblk.x) - (wblk.y + lblk.y),
           dascore = ascore.x - ascore.y,
           dafgm = afgm.x -  afgm.y,
           dafga = afga.x -  afga.y,  
           dagfa3 = agfa3.x - agfa3.y,
           daftm = aftm.x - aftm.y,
           dafta = afta.x - afta.y,  
           daor =  aor.x -  aor.y,  
           dadr =  adr.x -  adr.y,   
           daast =  aast.x - aast.y, 
           dato = ato.x - ato.y,
           dastl = astl.x - astl.y, 
           dablk = ablk.x - ablk.y,    
           dapf =  apf.x -  apf.y)%>%
    select( -(wins.x:lng.x), -(wins.y:lng.y), -Seed.x, -Seed.y) 


# Kaggle Submission File Step 2 -------------------------------------------
  
  submission1 <- submission %>%
    left_join(team_IDs, by = c("FirstTeam" = "Team_Id")) %>%
    left_join(team_IDs, by = c("SecondTeam" = "Team_Id"))
  
  #logistic model 
  Pred <- predict(logistic.model, submission1, type = "prob" )
  submission1$Pred <- Pred[,2]
  write.csv(submission1, "2017 Bracket Reference.csv", row.names = F)
  
  #svm model
  Pred2 <- predict(svm.model, submission1)
  submission1$Pred2 <- Pred2
  
# Export ------------------------------------------------------------------

  
  #log model
  submission_final <- submission1 %>%
    select(Id, Pred)
  write.csv(submission_final, "Kaggle_MarchMad_1.csv", row.names = F)
  
  #svm model
  submission_final2 <- submission1 %>%
    select(Id, Pred2) 
  write.csv(submission_final2, "Kaggle_MarchMad_2.csv", row.names = F)

  