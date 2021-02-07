#install.packages("BallMapper")
#install.packages("readxl")
#install.packages("DescTools")
#install.packages("dplyr")
library(BallMapper)
library(readxl)
library(DescTools)
library(dplyr)

my_table <- data.frame(
  read_excel('D:/Temp/R file for homework/data_year.xlsx',sheet = 2)
  )                     # root of data
class(my_table)
colnames(my_table) <- c(
  'Securities code','year','a. current ratio','b. quick ratio','c. cash ratio','d. asset-liability ratio',
  'e. account receivable turnover ratio','f. inventory turnover ratio',
  'g. Liquid assets turnover rate','h. Total assets turnover rate','i. Total assets net interest rate',
  'j. Rate of net assets','k. Operating net interest rate','l. Operating income growth rate',
  'm. Total assets growth rate','n. Net profit growth rate','o. Annual closing price' 
)
wins_vars <- function(x, pct_level = 0.01){
  if(is.numeric(x)){
    Winsorize(x, probs = c(pct_level, 1-pct_level), na.rm = F)
  } else {x}
}

my_table <- bind_cols(lapply(my_table, wins_vars))

par(mfrow=c(1,1))
eps = 1
test_2 <- matrix(rep(0,dim(my_table)[1]*dim(my_table)[2]),dim(my_table)[1], dim(my_table)[2])

n <- c(3:17)
for(j in n){
  test_value <- as.data.frame(my_table[,j])
  test_value <- normalize_to_min_0_max_1(test_value)
  for(i in 1:1509){
    test_2[i,j] <- test_value[i,]
  }
}
test_value <- test_2
for(i in n){
  value <- as.data.frame(test_value[,i])
  point <- cbind(test_value[,c(3:17)])
  ballm <- BallMapper(point, value, epsilon = eps)
  ColorIgraphPlot(ballm)
  title(main = colnames(my_table)[i])
}

#love your rabbit!!!


