
setwd("/Users/thoffman/Documents/QRS")

#load all the required libraries
require(xgboost)
library(data.table)
library(mlr)
#library(ggplot2)
library(caret)
library(e1071)
library(DiagrammeR)
library(pROC)
require(randomForest)
require(sqldf)
require(woeBinning)
require(dplyr)
require(gains)


rm(list = ls())

# Read in raw data
raw_data=read.csv("/Users/thoffman/Documents/QRS/QRS_cum_gains_actuals.csv",header=TRUE)

raw_data$label_pred <- ifelse(raw_data$probability_score >.5,1,0)

#query for eda
bp <- sqldf("
            select product
            , avg(probability_score) as prob
            , sum(label_test) as pos_obs
            , sum(label_pred) as pos_pred
            , count(*) as records

            
            from raw_data
            group by 1
            order by 1 desc
            ")

#Define histogram function
histPercent <- function(x, ...) {
  
  h <- hist(x, plot = FALSE, )
  
  h$density <- with(h, 100 * density* diff(breaks)[1])
  labs <- paste(round(h$density), "%", sep="")
  

  plot(h
        , freq = FALSE
        #, labels = labs
        , ylim=c(0, 1.3*max(h$density))
        , main = "Histogram of Predicted Scores"
        , xlab = "Predicted Score"
        ,...)
  
  strh <- strheight('W')
  strw <- strwidth(max(h$counts))
  text(h$mids, strh + h$density, labels = round(h$density,2), adj=c(0, 0.5), srt =90)
  
return(h)
}



#Create scores_df data frame
#scores_df <- athena_data[,c("label_test","probability_score")]

scores_df <- data.frame(label_obs = raw_data$label_test, probability_score = raw_data$probability_score)
#rm(raw_data)


#Plot histogram of predicted scores
par(mfrow=c(2,3))
#histogram <- histPercent(scores_df$probability_score, col="red")


plot(density(scores_df$probability_score)
     , col="red"
     , main = "Density: Probability Score"
     , bty = 'n'
     , las=1
     )
#polygon(d, col = "red")

# Perform calculations needed for cumulative gains and calibration.
scores_df <- scores_df[order(-scores_df$probability_score),]
scores_df$label_pred <- ifelse(scores_df$probability_score >.5,1,0)

scores_df$record <- 1
scores_df$cum_pos_cases <- cumsum(scores_df$label_obs)

#scores_df$cum_tot_cases_test <- seq.int(nrow(scores_df))
scores_df$cum_tot_cases <- cumsum(scores_df$record)
#scores_df$pct_cum_pos_cases <-scores_df$cum_pos_cases/max(scores_df$cum_pos_cases)
#scores_df$pct_cum_tot_cases <-scores_df$cum_tot_cases/max(scores_df$cum_tot_cases)
scores_df$bin <- as.numeric(as.character(cut(scores_df$cum_tot_cases,10,seq(1,10, by=1)))) #equalsize bins
#scores_df$bin_cal <- as.numeric(as.character(cut(scores_df$probability_score,seq(0,1,.1),labels = seq(0.1,1,.1))))#specified bins


#rm(cum_gains)
cum_gains <- sqldf("with df as (
                  select bin
                  , sum(record) as records
                  , sum(label_obs) as label_obs
                  , sum(label_pred) as label_pred
                  , avg(probability_score) as probability_score
                  from scores_df
                  group by 1)
                  
                  select *
                  from df
                
                   ")
                 

# Create cumulative gains data.
#cum_gains <- data.frame(aggregate(cbind(record,label_obs) ~ bin, data = scores_df,FUN = sum))

#cum_gains$gt_pos_cases <- sum(cum_gains$label_obs)

cum_gains$obs_rate <- cum_gains$label_obs / sum(cum_gains$label_obs)

cum_gains$cum_tot_cases <- cumsum(cum_gains$records)
cum_gains$cum_pos_cases <- cumsum(cum_gains$label_obs)

cum_gains$pct_cum_tot_cases <- round(cum_gains$cum_tot_cases/max(cum_gains$cum_tot_cases),2)
cum_gains$pct_cum_pos_cases <- round(cum_gains$cum_pos_cases/max(cum_gains$cum_pos_cases),2)

cum_gains$lift <- round(cum_gains$pct_cum_pos_cases / cum_gains$pct_cum_tot_cases,2)
cum_gains$cum_pos_cases_rand <- round(max(cum_gains$cum_pos_cases)*cum_gains$pct_cum_tot_cases,0)
#cum_gains$pct_pos_cases <- cum_gains$label_obs / sum(cum_gains$label_obs)

plot (x=cum_gains$bin, y=cum_gains$probability_score
      , col="red"
      , type="b"
      , pch=c(19)
      , main = "Predicted vs Actual"
      , xlab="Bin"
      , ylab="Prevalence Rate"
      , bty = 'n'
      , ylim = c(0,ceiling(max(cum_gains$probability_score))/2)
      , xlim = c(0,max(cum_gains$bin))
      , las=1
      )

points(x=cum_gains$bin, y=cum_gains$obs_rate, col="red", type = "b", lty=3)

text(x=cum_gains$bin
     , y=cum_gains$obs_rate
     ,labels=cum_gains$cum_pos_cases
     , pos=3)



#Cumulative Gains Plot
plot(cum_gains$pct_cum_tot_cases,cum_gains$pct_cum_pos_cases
     , xlab="Percent Total Cases"
     , ylab="Percent Positive Cases"
     , type="b"
     , col="red"
     , pch=c(19)
     , las=1
     , lty=c(1)
     , ylim=c(0,1.1)
     , xlim = c(0,1)
     , main = "Cumulative Gains"
     , bty = 'n'
     #,cex.lab=.75, cex.axis=.75, cex.main=.85, cex.sub=.5
)
text(cum_gains$pct_cum_tot_cases, cum_gains$pct_cum_pos_cases, labels=round(cum_gains$pct_cum_pos_cases,2)
     , cex=.9, pos=3)

points(cum_gains$pct_cum_tot_cases, cum_gains$pct_cum_tot_cases, col="bisque4", pch=c("o"))
lines(cum_gains$pct_cum_tot_cases, cum_gains$pct_cum_tot_cases, col="bisque4",lty=c(1), type="b")
text(cum_gains$pct_cum_tot_cases, cum_gains$pct_cum_tot_cases, labels=round(cum_gains$pct_cum_tot_cases,2), cex=.9, pos=1)


#Lift Chart
plot(cum_gains$pct_cum_tot_cases,cum_gains$lift
     , xlab="Percent Total Cases"
     , ylab="Lift"
     , type="b"
     , col="red"
     , pch=c(19)
     , las=1
     , lty=c(1)
     , main = "Lift"
     , ylim=c(0,2)
     , bty = 'n'
     , xlim = c(0,1)
     
     #,cex.lab=.75, cex.axis=.75, cex.main=.85, cex.sub=.5
)
text(cum_gains$pct_cum_tot_cases, cum_gains$lift, labels=round(cum_gains$lift,2)
     , cex=.9, pos=3)

cum_gains$lift_base <- 1

#points(cum_gains$pct_cum_tot_cases, cum_gains$pct_cum_tot_cases, col="bisque4", pch=c("o"))
lines(cum_gains$pct_cum_tot_cases, cum_gains$lift_base, col="bisque4",lty=c(1), type="b")
#text(cum_gains$pct_cum_tot_cases, cum_gains$pct_cum_tot_cases, labels=round(cum_gains$pct_cum_tot_cases,2), cex=.9, pos=1)



#Create confusion matrix
conf_mat <- confusionMatrix(table(scores_df$label_pred, scores_df$label_obs), positive="1")

conf_mat_ct <- data.frame(true_pos = conf_mat$table[2,2]
                          , true_neg = conf_mat$table[1,1]
                          , false_pos = conf_mat$table[2,1]
                          , false_neg = conf_mat$table[1,2]
                          )

#Plot Confusion Matrix
plot(x=c(1,0,0,1)
     , y=c(1,0,1,0)
     , xlim = c(-.5,1.5)
     , ylim = c(-.5,1.5)
     , xlab="Actual Class"
     , ylab="Predicted Class"
     , axes=FALSE
     , bty = 'n' # Removes box around plot.
     , pch=""
     )

    # Custom axes
    axis(1,col="white", at=c(0,1), col.axis="black")
    axis(2,col="white", at=c(0,1), col.axis="black",lty=2)
    title("Confusion Matrix", cex.main=1.2)
    rect(xleft = -.5, ybottom =-.5, xright=.5, ytop=.5)
    rect(xleft = .5, ybottom =-.5, xright=1.5, ytop=.5)
    rect(xleft = .5, ybottom =.5, xright=1.5, ytop=1.5)
    rect(xleft = -.5, ybottom =.5, xright=.5, ytop=1.5)

    text(x=c(1,0,0,1), y=c(1,0,1,0), labels=format(conf_mat_ct,big.mark = ","), pos=1)
    text(x=c(1,0,0,1)
     , y=c(1,0,1,0)
     , labels=c("True Positives","True Negatives","False Positives","False Negatives")
     , font=2
     , col= c("green4","green4","red4","red4")
     , pos=3
     #, cex=1.2
     )



#Calculate AUC and get performance measures from confusion matrix object.
auc_mod <- roc(scores_df$label_obs,scores_df$label_pred)

auc_label <- "AUC"

measures_auc <- data.frame(measure = auc_label, value = as.numeric(auc_mod$auc))

conf_mat_byClass <- data.frame(measure = row.names(data.frame(conf_mat$byClass))
                                   , value = conf_mat$byClass
                                   , row.names = NULL
                               )

conf_mat_overall <- data.frame(measure = row.names(data.frame(conf_mat$overall))
                               , value = conf_mat$overall
                               , row.names = NULL
                               )

conf_mat_measures <- rbind(measures_auc, conf_mat_byClass, conf_mat_overall)

keep_measures <- c("Prevalence"
                  ,"AUC"
                  ,"Kappa"
                  ,"Pos Pred Value"
                  ,"Neg Pred Value"
                  ,"Specificity"
                  ,"Sensitivity"
                  , "Recall"
                  ,"Precision")

conf_mat_measures <- conf_mat_measures[conf_mat_measures$measure %in% keep_measures,]





#Plot the performance measures for the model (makes a chart look like a table).
conf_mat_measures$plot_index <- 1.2
conf_mat_measures$plot_pos <- seq(1,length(conf_mat_measures$measure), by=1)

plot(conf_mat_measures$plot_index
     , conf_mat_measures$plot_pos
     , main = "Performance Measures"
     , xlim=c(0,2.5)
     , ylim=c(1,max(conf_mat_measures$plot_pos))
     , las = 1
     , axes = FALSE
     , type = "n"
     , ylab=""
     , yaxt="n"
     , xlab=""
     , xaxt="n"
     )

    text(conf_mat_measures$plot_index
      , conf_mat_measures$plot_pos 
      , labels = round(conf_mat_measures$value,4)
      , pos = 4
      , offset = 1
      )

    text(conf_mat_measures$plot_index -1
      , conf_mat_measures$plot_pos 
      , labels = conf_mat_measures$measure
      , pos = 4
      , offset = 1
      )



# Calculate calibration data
scores_df <- scores_df[order(scores_df$probability_score),]
scores_df$bin_cal <- as.numeric(as.character(cut(scores_df$probability_score,seq(0,1,.1),labels = seq(0.1,1,.1))))#specified cut point


eda <- sqldf("

             select bin
             , max(probability_score) as max_prob
             , Min(probability_score) as min_prob
             , count(*) as count
             , sum(label_obs) as pos
             from scores_df
             group by 1
             
             ")


cal <- sqldf("select bin_cal
      , sum(record) as records
      , sum(label_obs) as label_obs
      , sum(label_pred) as label_pred
      , avg(probability_score) as pred_rate

      from scores_df
      group by bin_cal
      "
      )

#cal$obs_sum <- sum(cal$label_obs)
cal$obs_rate <- cal$label_obs / sum(cal$label_obs)


#Plot calibration plot
plot(cal$pred_rate
     , cal$obs_rate
     , xlab = "Predicted Rate"
     , ylab = "Observed Rate"
     , col="red"
     , ylim = c(0,max(cal$bin_cal))
     , xlim = c(0,max(cal$bin_cal))
     , pch=c(19)
     , type="b"
     , bty = 'n'
     , main = "Calibration"
     , axes = FALSE
     
     )

    #axis(side = 1, at = c(0,cal$bin_cal))
    axis(side = 2, at = c(0,cal$bin_cal), las=1)

    text(cal$pred_rate, cal$obs_rate, labels=round(cal$obs_rate,2), cex=.9, pos=3)
    lines(cal$bin_cal, cal$bin_cal, col="bisque4",lty=c(1), type="b")

x <- scores_df$bin_cal
y <- scores_df$probability_score

#plot(y ~ jitter(x,2), col="red")

x1 <- cum_gains$probability_score
y1 <- cum_gains$pct_pos_cases

#plot(x1,y1)




