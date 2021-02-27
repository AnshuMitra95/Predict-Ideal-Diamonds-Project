# Predict-Ideal-Diamonds-Project
My Website
Hello, Website!

For more information about simple R Markdown websites, please read the documentation at https://bookdown.org/yihui/rmarkdown/rmarkdown-site.html.

Please also note that simple R Markdown sites are not based on blogdown. They are probably good for websites with only a few Rmd documents. For larger-scale and more sophisticated websites (such as blogs), you may want to use blogdown instead: https://github.com/rstudio/blogdown.

Loading A File
library(neuralnet)
library(caret)
## Loading required package: lattice
## Loading required package: ggplot2
library(tidyverse)
## -- Attaching packages ---------------------------------------------------------------------------- tidyverse 1.3.0 --
## v tibble  3.0.1     v dplyr   0.8.5
## v tidyr   1.1.0     v stringr 1.4.0
## v readr   1.3.1     v forcats 0.5.0
## v purrr   0.3.4
## -- Conflicts ------------------------------------------------------------------------------- tidyverse_conflicts() --
## x dplyr::compute() masks neuralnet::compute()
## x dplyr::filter()  masks stats::filter()
## x dplyr::lag()     masks stats::lag()
## x purrr::lift()    masks caret::lift()
#Checking The data from data frame

head(diamonds)
## # A tibble: 6 x 10
##   carat cut       color clarity depth table price     x     y     z
##   <dbl> <ord>     <ord> <ord>   <dbl> <dbl> <int> <dbl> <dbl> <dbl>
## 1 0.23  Ideal     E     SI2      61.5    55   326  3.95  3.98  2.43
## 2 0.21  Premium   E     SI1      59.8    61   326  3.89  3.84  2.31
## 3 0.23  Good      E     VS1      56.9    65   327  4.05  4.07  2.31
## 4 0.290 Premium   I     VS2      62.4    58   334  4.2   4.23  2.63
## 5 0.31  Good      J     SI2      63.3    58   335  4.34  4.35  2.75
## 6 0.24  Very Good J     VVS2     62.8    57   336  3.94  3.96  2.48
df<-diamonds %>%
    filter(cut %in% c("Ideal","Good"))
#head(df)
df<-sample_n(df,5000)
#dim(df)
df$binary<- ifelse(df$cut=="Ideal", 1,0)
head(df)
## # A tibble: 6 x 11
##   carat cut   color clarity depth table price     x     y     z binary
##   <dbl> <ord> <ord> <ord>   <dbl> <dbl> <int> <dbl> <dbl> <dbl>  <dbl>
## 1  0.34 Ideal H     SI1      61.3    56   516  4.48  4.52  2.76      1
## 2  0.52 Ideal D     VVS2     59.9    57  2217  5.21  5.28  3.14      1
## 3  1.29 Ideal J     SI1      61.6    58  5607  6.96  6.99  4.3       1
## 4  0.73 Ideal E     VS1      61.2    57  3620  5.85  5.8   3.57      1
## 5  0.71 Good  D     VS2      63.3    56  2788  5.64  5.68  3.58      0
## 6  0.55 Ideal F     VS2      60.9    56  1709  5.3   5.34  3.24      1
df$binary<-as.factor(df$binary)
names(df)
##  [1] "carat"   "cut"     "color"   "clarity" "depth"   "table"   "price"  
##  [8] "x"       "y"       "z"       "binary"
df<-df[,-2]
head(df)
## # A tibble: 6 x 10
##   carat color clarity depth table price     x     y     z binary
##   <dbl> <ord> <ord>   <dbl> <dbl> <int> <dbl> <dbl> <dbl> <fct> 
## 1  0.34 H     SI1      61.3    56   516  4.48  4.52  2.76 1     
## 2  0.52 D     VVS2     59.9    57  2217  5.21  5.28  3.14 1     
## 3  1.29 J     SI1      61.6    58  5607  6.96  6.99  4.3  1     
## 4  0.73 E     VS1      61.2    57  3620  5.85  5.8   3.57 1     
## 5  0.71 D     VS2      63.3    56  2788  5.64  5.68  3.58 0     
## 6  0.55 F     VS2      60.9    56  1709  5.3   5.34  3.24 1
rows<-createDataPartition(df$binary,p=.7, list = F, times = 1)
head(rows)
##      Resample1
## [1,]         2
## [2,]         3
## [3,]         4
## [4,]         5
## [5,]         6
## [6,]         7
train<-df[rows,]
## Warning: The `i` argument of ``[`()` can't be a matrix as of tibble 3.0.0.
## Convert to a vector.
## This warning is displayed once every 8 hours.
## Call `lifecycle::last_warnings()` to see where this warning was generated.
head(train)
## # A tibble: 6 x 10
##   carat color clarity depth table price     x     y     z binary
##   <dbl> <ord> <ord>   <dbl> <dbl> <int> <dbl> <dbl> <dbl> <fct> 
## 1  0.52 D     VVS2     59.9    57  2217  5.21  5.28  3.14 1     
## 2  1.29 J     SI1      61.6    58  5607  6.96  6.99  4.3  1     
## 3  0.73 E     VS1      61.2    57  3620  5.85  5.8   3.57 1     
## 4  0.71 D     VS2      63.3    56  2788  5.64  5.68  3.58 0     
## 5  0.55 F     VS2      60.9    56  1709  5.3   5.34  3.24 1     
## 6  0.3  G     VS1      62.9    57   776  4.31  4.28  2.7  1
test<-df[-rows,]
head(test)
## # A tibble: 6 x 10
##   carat color clarity depth table price     x     y     z binary
##   <dbl> <ord> <ord>   <dbl> <dbl> <int> <dbl> <dbl> <dbl> <fct> 
## 1  0.34 H     SI1      61.3    56   516  4.48  4.52  2.76 1     
## 2  1.61 G     SI1      61.2    55 11192  7.57  7.62  4.65 1     
## 3  0.31 H     VS2      62.4    55   628  4.36  4.33  2.71 1     
## 4  0.82 E     SI2      62.6    55  3131  5.94  5.98  3.73 1     
## 5  1.01 E     VS2      63.7    56  6335  6.29  6.27  4    0     
## 6  1.31 H     VS1      62.5    56  8774  7.03  6.95  4.37 1
library(ranger)
control<-trainControl(method = "repeatedcv" ,number = 2, repeats = 2)
model<-train(binary~ .,data= train, method ="ranger" ,trControl= control)
model
## Random Forest 
## 
## 3501 samples
##    9 predictor
##    2 classes: '0', '1' 
## 
## No pre-processing
## Resampling: Cross-Validated (2 fold, repeated 2 times) 
## Summary of sample sizes: 1751, 1750, 1751, 1750 
## Resampling results across tuning parameters:
## 
##   mtry  splitrule   Accuracy   Kappa    
##    2    gini        0.9734371  0.9061775
##    2    extratrees  0.8516136  0.2658382
##   11    gini        0.9831483  0.9427250
##   11    extratrees  0.9802924  0.9329502
##   20    gini        0.9824336  0.9402917
##   20    extratrees  0.9840052  0.9459166
## 
## Tuning parameter 'min.node.size' was held constant at a value of 1
## Accuracy was used to select the optimal model using the largest value.
## The final values used for the model were mtry = 20, splitrule = extratrees
##  and min.node.size = 1.
pred<- predict(model,test)
head(pred)
## [1] 1 1 1 1 0 1
## Levels: 0 1
confusionMatrix(pred,test$binary)
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    0    1
##          0  256    8
##          1   14 1221
##                                           
##                Accuracy : 0.9853          
##                  95% CI : (0.9779, 0.9908)
##     No Information Rate : 0.8199          
##     P-Value [Acc > NIR] : <2e-16          
##                                           
##                   Kappa : 0.9499          
##                                           
##  Mcnemar's Test P-Value : 0.2864          
##                                           
##             Sensitivity : 0.9481          
##             Specificity : 0.9935          
##          Pos Pred Value : 0.9697          
##          Neg Pred Value : 0.9887          
##              Prevalence : 0.1801          
##          Detection Rate : 0.1708          
##    Detection Prevalence : 0.1761          
##       Balanced Accuracy : 0.9708          
##                                           
##        'Positive' Class : 0               
## 
