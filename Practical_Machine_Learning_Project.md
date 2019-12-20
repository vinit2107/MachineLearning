Practical\_Machine\_Learning\_Project
================
Vinit Deshbhratar
19/12/2019

## Summary

The dataset contains the quantifiable readings of the accelerometers.
The aim of the project is to create a model which can identify whether
the exercises performed by the person was correct or not. The target
variable of the dataset has 10 categories which identifies the five
correct and five incorrect exercises performed by the person.

## Loading the libraries

``` r
library(dplyr)
library(corrplot)
library(caret)
```

## Loading the training and test dataset

``` r
training = read.csv('pml-training.csv')
testing = read.csv('pml-testing.csv')
```

## Data Description

Dimensions of the dataset are:

``` r
dim(training)
```

    ## [1] 19622   160

Columns names are:

``` r
names(training)
```

    ##   [1] "X"                        "user_name"               
    ##   [3] "raw_timestamp_part_1"     "raw_timestamp_part_2"    
    ##   [5] "cvtd_timestamp"           "new_window"              
    ##   [7] "num_window"               "roll_belt"               
    ##   [9] "pitch_belt"               "yaw_belt"                
    ##  [11] "total_accel_belt"         "kurtosis_roll_belt"      
    ##  [13] "kurtosis_picth_belt"      "kurtosis_yaw_belt"       
    ##  [15] "skewness_roll_belt"       "skewness_roll_belt.1"    
    ##  [17] "skewness_yaw_belt"        "max_roll_belt"           
    ##  [19] "max_picth_belt"           "max_yaw_belt"            
    ##  [21] "min_roll_belt"            "min_pitch_belt"          
    ##  [23] "min_yaw_belt"             "amplitude_roll_belt"     
    ##  [25] "amplitude_pitch_belt"     "amplitude_yaw_belt"      
    ##  [27] "var_total_accel_belt"     "avg_roll_belt"           
    ##  [29] "stddev_roll_belt"         "var_roll_belt"           
    ##  [31] "avg_pitch_belt"           "stddev_pitch_belt"       
    ##  [33] "var_pitch_belt"           "avg_yaw_belt"            
    ##  [35] "stddev_yaw_belt"          "var_yaw_belt"            
    ##  [37] "gyros_belt_x"             "gyros_belt_y"            
    ##  [39] "gyros_belt_z"             "accel_belt_x"            
    ##  [41] "accel_belt_y"             "accel_belt_z"            
    ##  [43] "magnet_belt_x"            "magnet_belt_y"           
    ##  [45] "magnet_belt_z"            "roll_arm"                
    ##  [47] "pitch_arm"                "yaw_arm"                 
    ##  [49] "total_accel_arm"          "var_accel_arm"           
    ##  [51] "avg_roll_arm"             "stddev_roll_arm"         
    ##  [53] "var_roll_arm"             "avg_pitch_arm"           
    ##  [55] "stddev_pitch_arm"         "var_pitch_arm"           
    ##  [57] "avg_yaw_arm"              "stddev_yaw_arm"          
    ##  [59] "var_yaw_arm"              "gyros_arm_x"             
    ##  [61] "gyros_arm_y"              "gyros_arm_z"             
    ##  [63] "accel_arm_x"              "accel_arm_y"             
    ##  [65] "accel_arm_z"              "magnet_arm_x"            
    ##  [67] "magnet_arm_y"             "magnet_arm_z"            
    ##  [69] "kurtosis_roll_arm"        "kurtosis_picth_arm"      
    ##  [71] "kurtosis_yaw_arm"         "skewness_roll_arm"       
    ##  [73] "skewness_pitch_arm"       "skewness_yaw_arm"        
    ##  [75] "max_roll_arm"             "max_picth_arm"           
    ##  [77] "max_yaw_arm"              "min_roll_arm"            
    ##  [79] "min_pitch_arm"            "min_yaw_arm"             
    ##  [81] "amplitude_roll_arm"       "amplitude_pitch_arm"     
    ##  [83] "amplitude_yaw_arm"        "roll_dumbbell"           
    ##  [85] "pitch_dumbbell"           "yaw_dumbbell"            
    ##  [87] "kurtosis_roll_dumbbell"   "kurtosis_picth_dumbbell" 
    ##  [89] "kurtosis_yaw_dumbbell"    "skewness_roll_dumbbell"  
    ##  [91] "skewness_pitch_dumbbell"  "skewness_yaw_dumbbell"   
    ##  [93] "max_roll_dumbbell"        "max_picth_dumbbell"      
    ##  [95] "max_yaw_dumbbell"         "min_roll_dumbbell"       
    ##  [97] "min_pitch_dumbbell"       "min_yaw_dumbbell"        
    ##  [99] "amplitude_roll_dumbbell"  "amplitude_pitch_dumbbell"
    ## [101] "amplitude_yaw_dumbbell"   "total_accel_dumbbell"    
    ## [103] "var_accel_dumbbell"       "avg_roll_dumbbell"       
    ## [105] "stddev_roll_dumbbell"     "var_roll_dumbbell"       
    ## [107] "avg_pitch_dumbbell"       "stddev_pitch_dumbbell"   
    ## [109] "var_pitch_dumbbell"       "avg_yaw_dumbbell"        
    ## [111] "stddev_yaw_dumbbell"      "var_yaw_dumbbell"        
    ## [113] "gyros_dumbbell_x"         "gyros_dumbbell_y"        
    ## [115] "gyros_dumbbell_z"         "accel_dumbbell_x"        
    ## [117] "accel_dumbbell_y"         "accel_dumbbell_z"        
    ## [119] "magnet_dumbbell_x"        "magnet_dumbbell_y"       
    ## [121] "magnet_dumbbell_z"        "roll_forearm"            
    ## [123] "pitch_forearm"            "yaw_forearm"             
    ## [125] "kurtosis_roll_forearm"    "kurtosis_picth_forearm"  
    ## [127] "kurtosis_yaw_forearm"     "skewness_roll_forearm"   
    ## [129] "skewness_pitch_forearm"   "skewness_yaw_forearm"    
    ## [131] "max_roll_forearm"         "max_picth_forearm"       
    ## [133] "max_yaw_forearm"          "min_roll_forearm"        
    ## [135] "min_pitch_forearm"        "min_yaw_forearm"         
    ## [137] "amplitude_roll_forearm"   "amplitude_pitch_forearm" 
    ## [139] "amplitude_yaw_forearm"    "total_accel_forearm"     
    ## [141] "var_accel_forearm"        "avg_roll_forearm"        
    ## [143] "stddev_roll_forearm"      "var_roll_forearm"        
    ## [145] "avg_pitch_forearm"        "stddev_pitch_forearm"    
    ## [147] "var_pitch_forearm"        "avg_yaw_forearm"         
    ## [149] "stddev_yaw_forearm"       "var_yaw_forearm"         
    ## [151] "gyros_forearm_x"          "gyros_forearm_y"         
    ## [153] "gyros_forearm_z"          "accel_forearm_x"         
    ## [155] "accel_forearm_y"          "accel_forearm_z"         
    ## [157] "magnet_forearm_x"         "magnet_forearm_y"        
    ## [159] "magnet_forearm_z"         "classe"

## Feature Selection

In order to go forward with feature selection, we need to determine how
much data is missing in a particulare column. There is no point in
imputing the values in a column if most of the data in the column is
missing. Hence we will first determine which columns have sufficient
amount of data.

``` r
missing_values = c()
for(col in names(training)){
        missing_values = c(missing_values, sum(training[, col] == '' | is.na(training[, col]))/nrow(training))
}
missing_values_df = data.frame(names(training), Missing_Values = missing_values*100)
```

We have a dataframe which contains the amount of missing values in the
column in percentage. The dataframe is as follows.

``` r
head(missing_values_df)
```

    ##        names.training. Missing_Values
    ## 1                    X              0
    ## 2            user_name              0
    ## 3 raw_timestamp_part_1              0
    ## 4 raw_timestamp_part_2              0
    ## 5       cvtd_timestamp              0
    ## 6           new_window              0

We’ll remove the columns which have missing values greater than 70%.

``` r
missing_values_df = missing_values_df %>% filter(Missing_Values < 70)
```

Dimensions of the filtered dataframe are as follows:

``` r
dim(missing_values_df)
```

    ## [1] 60  2

So we have 60 columns that can be used for building a model. Selecting
the columns that we have got from the training dataset.

``` r
training = training[, as.character(missing_values_df$names.training.)]
```

We do not need the X, username and timestamp related columns for
building the model. Hence dropping the columns.

``` r
training = training[,  !names(training) %in% c('X', 'user_name', 'raw_timestamp_part_1', 'raw_timestamp_part_2', 'cvtd_timestamp', 'new_window')]
```

## Building a correlation matrix

``` r
corr_mat = cor(training[-c(54)])
corrplot(corr_mat, method = 'color', tl.cex = 0.5)
```

![](Practical_Machine_Learning_Project_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

We can see from the correaltion plot that there are columns which are
highly correalted, positively and negatively too. We need to filter out
the columns which are contributing to it. Firstly setting the upper
triangle to 0. And then setting the diagnonal of the matrix to 0.

``` r
corr_mat[upper.tri(corr_mat)] = 0
diag(corr_mat) = 0
training = training[, !apply(corr_mat,2,function(x) any(x > 0.8))]
```

Checking the dimension of the dataframe after filtering out.

``` r
dim(training)
```

    ## [1] 19622    45

# Building a model

We’ll try to build two models using the random forest, one using the
cross-validation and the other not using the cross-validation.

``` r
rf_fit = randomForest::randomForest(classe ~ ., data = training)

rf_fit
```

    ## 
    ## Call:
    ##  randomForest(formula = classe ~ ., data = training) 
    ##                Type of random forest: classification
    ##                      Number of trees: 500
    ## No. of variables tried at each split: 6
    ## 
    ##         OOB estimate of  error rate: 0.11%
    ## Confusion matrix:
    ##      A    B    C    D    E  class.error
    ## A 5579    0    0    0    1 0.0001792115
    ## B    3 3793    1    0    0 0.0010534633
    ## C    0    7 3415    0    0 0.0020455874
    ## D    0    0    7 3208    1 0.0024875622
    ## E    0    0    0    1 3606 0.0002772387

``` r
rf_fit_cv = train(classe~., method = 'rf', data = training, trControl = trainControl(method = 'cv', number = 3))
```

The expected out of sample error rate is 0.22%.

``` r
rf_fit_cv
```

    ## Random Forest 
    ## 
    ## 19622 samples
    ##    44 predictor
    ##     5 classes: 'A', 'B', 'C', 'D', 'E' 
    ## 
    ## No pre-processing
    ## Resampling: Cross-Validated (3 fold) 
    ## Summary of sample sizes: 13081, 13082, 13081 
    ## Resampling results across tuning parameters:
    ## 
    ##   mtry  Accuracy   Kappa    
    ##    2    0.9950056  0.9936824
    ##   23    0.9975537  0.9969058
    ##   44    0.9946998  0.9932960
    ## 
    ## Accuracy was used to select the optimal model using the largest value.
    ## The final value used for the model was mtry = 23.

Here is the confusion matrix for the values in the training dataset and
the values predicted from the training dataset. This is without using
the cross-validation.

``` r
confusionMatrix(training$classe, predict(rf_fit, training[-45]))
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction    A    B    C    D    E
    ##          A 5580    0    0    0    0
    ##          B    0 3797    0    0    0
    ##          C    0    0 3422    0    0
    ##          D    0    0    0 3216    0
    ##          E    0    0    0    0 3607
    ## 
    ## Overall Statistics
    ##                                      
    ##                Accuracy : 1          
    ##                  95% CI : (0.9998, 1)
    ##     No Information Rate : 0.2844     
    ##     P-Value [Acc > NIR] : < 2.2e-16  
    ##                                      
    ##                   Kappa : 1          
    ##                                      
    ##  Mcnemar's Test P-Value : NA         
    ## 
    ## Statistics by Class:
    ## 
    ##                      Class: A Class: B Class: C Class: D Class: E
    ## Sensitivity            1.0000   1.0000   1.0000   1.0000   1.0000
    ## Specificity            1.0000   1.0000   1.0000   1.0000   1.0000
    ## Pos Pred Value         1.0000   1.0000   1.0000   1.0000   1.0000
    ## Neg Pred Value         1.0000   1.0000   1.0000   1.0000   1.0000
    ## Prevalence             0.2844   0.1935   0.1744   0.1639   0.1838
    ## Detection Rate         0.2844   0.1935   0.1744   0.1639   0.1838
    ## Detection Prevalence   0.2844   0.1935   0.1744   0.1639   0.1838
    ## Balanced Accuracy      1.0000   1.0000   1.0000   1.0000   1.0000

Here is the confusion matrix for the calues in the training dataset and
the values predicted from the training dataset. The classifier has been
created using the cross-validation.

``` r
confusionMatrix(training$classe, predict(rf_fit_cv, training[-45]))
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction    A    B    C    D    E
    ##          A 5580    0    0    0    0
    ##          B    0 3797    0    0    0
    ##          C    0    0 3422    0    0
    ##          D    0    0    0 3216    0
    ##          E    0    0    0    0 3607
    ## 
    ## Overall Statistics
    ##                                      
    ##                Accuracy : 1          
    ##                  95% CI : (0.9998, 1)
    ##     No Information Rate : 0.2844     
    ##     P-Value [Acc > NIR] : < 2.2e-16  
    ##                                      
    ##                   Kappa : 1          
    ##                                      
    ##  Mcnemar's Test P-Value : NA         
    ## 
    ## Statistics by Class:
    ## 
    ##                      Class: A Class: B Class: C Class: D Class: E
    ## Sensitivity            1.0000   1.0000   1.0000   1.0000   1.0000
    ## Specificity            1.0000   1.0000   1.0000   1.0000   1.0000
    ## Pos Pred Value         1.0000   1.0000   1.0000   1.0000   1.0000
    ## Neg Pred Value         1.0000   1.0000   1.0000   1.0000   1.0000
    ## Prevalence             0.2844   0.1935   0.1744   0.1639   0.1838
    ## Detection Rate         0.2844   0.1935   0.1744   0.1639   0.1838
    ## Detection Prevalence   0.2844   0.1935   0.1744   0.1639   0.1838
    ## Balanced Accuracy      1.0000   1.0000   1.0000   1.0000   1.0000

These are the values for the test dataset for the quiz.

``` r
columns = names(training[-45])
testing = testing[, columns]
predict(rf_fit, testing)
```

    ##  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 
    ##  B  A  B  A  A  E  D  B  A  A  B  C  B  A  E  E  A  B  B  B 
    ## Levels: A B C D E

``` r
predict(rf_fit_cv, testing)
```

    ##  [1] B A B A A E D B A A B C B A E E A B B B
    ## Levels: A B C D E

Both the models give the same output for the training as well as the
test dataset.
