dau <- read.csv("section4-dau.csv", header = T, stringsAsFactors = F)
user_info <- read.csv("section4-user_info.csv", header = T, stringsAsFactors = F)
head(user_info)
dau.user.info <- merge(dau, user_info , by = c("user_id", "app_name"))
head(dau.user.info)
dau.user.info$log_month <- substr(dau.user.info$log_date, 1, 7)
table(dau.user.info[, c("log_month", "gender")])
table(dau.user.info[,c("log_month", "generation")])
library(reshape2)
dcast(dau.user.info, log_month ~ gender + generation, value.var = "user_id", length)
table(dau.user.info[, c("log_month", "device_type")])
library(plyr)
dau.user.info.device.summary <- ddply(dau.user.info, .(log_date, device_type), summarise, dau = length(user_id))
head(dau.user.info.device.summary)
dau.user.info.device.summary$log_date <- as.Date(dau.user.info.device.summary$log_date)
library(ggplot2)
library(scales)
limits <- c(0, max(dau.user.info.device.summary$dau))
ggplot(dau.user.info.device.summary, aes(x= log_date, y = dau, col=device_type, lty = device_type, shape = device_type))+
  geom_line(lwd = 1) +
  geom_point(size=4)+
  scale_y_continuous(label = comma, limits=limits)

#section5

ab.test.imp <- read.csv("section5-ab_test_imp.csv", header = T, stringsAsFactors = F)
head(ab.test.imp)
ab.test.goal <- read.csv("section5-ab_test_goal.csv", header = T, stringsAsFactors = F)
head(ab.test.goal)
ab.test.imp.imp <- merge(ab.test.imp, ab.test.goal, by = c("transaction_id"), all.x = T, suffixes=c("",".g"))
head(ab.test.imp.imp)
ab.test.imp.imp$is.goal <- ifelse(is.na(ab.test.imp.imp$user_id.g), 0, 1)
head(ab.test.imp.imp)
ddply(ab.test.imp.imp, .(test_case), summarize, cvr = sum(is.goal)/length(user_id))
chisq.test(ab.test.imp.imp$test_case, ab.test.imp.imp$is.goal)
ab.test.imp.summary <- ddply(ab.test.imp.imp, .(log_date, test_case), summarize, 
                             imp = length(user_id),
                             cv = sum(is.goal),
                             cvr = sum(is.goal)/length(user_id))
head(ab.test.imp.summary)
ab.test.imp.summary <- ddply(ab.test.imp.summary, .(test_case), transform,
                             cvr.avg = sum(cv)/sum(imp))
ab.test.imp.summary$log_date <- as.Date(ab.test.imp.summary$log_date)
limits <- c(0, max(ab.test.imp.summary$cvr))
ggplot(ab.test.imp.summary, aes( x= log_date, y = cvr, col=test_case, lty=test_case, shape=test_case)) +
  geom_line(lwd = 1)+
  geom_point(size =4)+
  geom_line(aes(y=cvr.avg, col=test_case))+
  scale_y_continuous(label = percent, limits = limits)
