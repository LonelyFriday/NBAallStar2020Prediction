nba_final
NBA2020

library(tidyverse)
library(rpart)
library(nnet)

data_nba <- nba_final%>%
  select(Player.x,Pos1,Age,Tm,PTS,TRB,AST,STL,BLK,TOV,PF,Play)  ## we will use only Age, Points, Rebounds, Assists, Steals, Blocks, TurnOver, and PersonalFoul to predict whether he will be played in NBA AllStar 2020 or not. 

## Logistic regression
predict_allstar <- glm(Play~PTS+TRB+AST+STL+BLK+TOV+PF,data = data_nba, family = "binomial")
predict(predict_allstar,type = "response")>=0.5

## decision tree
dt_allstar <- rpart(Play~PTS+TRB+AST+STL+BLK+TOV+PF,data = data_nba,method = "class")

## Neural net
nn_allstar2 <- nnet(Play~PTS+TRB+AST+STL+BLK+TOV+PF,data = data_nba,size = 4)

## See prediction from the model
data_nba %>%
  select(Player.x) %>%
  filter(predict(predict_allstar,type = "response")>=0.5)

## Let's predict with the data from NBA 2020 - 21
data_nba2020 <- NBA2020 %>%
  select(c(
    Player.x = PLAYER,
    Age = AGE,
    PTS,
    TRB = REB,
    AST,
    STL,
    BLK,
    TOV,
    PF,
    Play = Play
  ))

nba_allstar <- data_nba2020 %>%  ## NBA ALL-Star 2020
  select(Player.x,Play) %>%
  filter(Play == "Yes") %>%
  arrange(Player.x)

predict(predict_allstar,newdata = data_nba2020, type = "response")  ## logistics reg

predict(dt_allstar,newdata= data_nba2020,type = "class") ## decision tree


##Logistic regression
Logistic_nba<- data_nba2020 %>%
  select(Player.x) %>%
  filter(predict(predict_allstar,newdata = data_nba2020, type = "response")>0.409) %>%
  arrange(Player.x)

sum(diag(table(predict(predict_allstar,newdata = data_nba2020, type = "response")>0.409),data_nba2020$Play))/sum(table(predict(predict_allstar,newdata = data_nba2020, type = "response")>0.5),data_nba2020$Play)

Logistic_nba

## decision tree
tree_nba <- data_nba2020 %>%
  select(Player.x) %>% 
  filter(predict(dt_allstar,newdata= data_nba2020,type = "class") == "Yes")

mean(predict(dt_allstar,newdata= data_nba2020,type = "class") == data_nba2020$Play)

tree_nba

## Neural net
nn_nba <- data_nba2020 %>%
  select(Player.x) %>%
  mutate(all_star = predict(nn_allstar2,newdata=data_nba2020,type="class")) %>%
  filter(all_star == "Yes") %>%
  arrange(Player.x)

mean(predict(nn_allstar2,newdata=data_nba2020,type="class") == data_nba2020$Play,na.rm = T)

nn_nba

## Combine the result in in 1 table
predict <- cbind(nba_allstar,Logistic_nba,nn_nba)
colnames(predict)
predict[,c(2,5)] <- NULL
colnames(predict) <- c('allStar2020','Logistic2020','nn2020')
predict

write.csv(predict,'nbaAllStar_prediction2020.csv')
