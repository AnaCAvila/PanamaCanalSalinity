
df <- read.csv('all_data.csv')
###################################

df <- subset(df, df$data != 15)
df <- subset(df, df$data != 4.11)
#outliers
df <- subset(df, df$site != 'TMH')
#this station is right outside of a river (Coco Solo), but we don't have any flow data for it. The salinity of it is very low, although it is near the lock.
df <- df[complete.cases(df), ] #Selected only where we have traffic data.

plot(data ~ min_dist_traffic, data = df, main="All variables", ylab = "salinity")

#what explains the variation?

fit1 <- lm(data ~ Gatun_flow_dist + Cano_Quebrado_flow_dist + Trinidad_flow_dist + Ciri_Grande_flow_dist + min_dist_neo_traffic + min_dist_traffic, data=df)

summary(fit1)


#relative importance per predictor
# Calculate Relative Importance for Each Predictor
library(relaimpo)
library(MLmetrics)


calc.relimp(fit1,type=c("lmg","last","first","pratt"),
            rela=TRUE)

pred <- predict(fit1, newdata = data.frame(Gatun_flow_dist = df$Gatun_flow_dist, Cano_Quebrado_flow_dist = df$Cano_Quebrado_flow_dist, Trinidad_flow_dist = df$Trinidad_flow_dist, Ciri_Grande_flow_dist=df$Ciri_Grande_flow_dist, min_dist_neo_traffic=df$min_dist_neo_traffic, min_dist_traffic=df$min_dist_traffic))



plot(df$data, pred, xlab = 'Observed', ylab = 'Predicted')
abline(0, 1, col='red')

MSE(pred, df$data)

mse <- mean(fit1$residuals^2)

t.test(df$data, pred)

hist(fit1$residuals, main="Histogram of Residuals", xlab = "residuals")


plot(pred, df$data)
plot(as.yearmon(df$date), pred)

ggplot(df, aes(as.yearmon(date)), aes(y = "salinity", x = "date")) +
  geom_point(aes(y=data), colour="red") +  # first layer
  geom_point(aes(y=pred), colour="green") +
  geom_hline(yintercept=0.5, linetype="dashed", color = "red") +
  geom_vline(xintercept=as.yearmon('06-2016'), linetype="dashed", color = "red")
  

################################################## Try to predict the model with optim

sumSqMin_salt <- function(par, data) {
  with(data, sum((par[1] + par[2] * min_dist_traffic + par[3] * min_dist_neo_traffic + par[4] * Gatun_flow_dist + par[5] * Cano_Quebrado_flow_dist + par[6] * Trinidad_flow_dist + par[7] * Ciri_Grande_flow_dist- data)^2))
}


result <- optim(par = c(0, 1, 1, 1, 1, 1, 1), fn = sumSqMin_salt, data = df)
result

plot(data ~ min_dist_traffic, data = df, main="main")
abline(a = result$par[1], b = result$par[2], col = "red")



#########################



