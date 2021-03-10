dau <- read.csv("section3-dau.csv", header = T, stringsAsFactors = F)
getwd()
head(dau)
dpu <- read.csv("section3-dpu.csv", header = T, stringsAsFactors = F)
head(dpu)
install <- read.csv("section3-install.csv", header = T,stringsAsFactors = F )
head(install)
library(dplyr)
dau.install <- merge(dau, install, by = c("user_id", "app_name"))
head(dau.install)
head(dau)
dau.install.payment <- merge(dau.install, dpu, by = c("log_date", "app_name", "user_id"), all.x =T)
head(dau.install.payment)
head(na.omit(dau.install.payment))
dau.install.payment$payment[is.na(dau.install.payment$payment)] <- 0 
x[is.na(x)]
x <- c(1,2,NA, 3, NA,5,NA)
head(dau.install.payment)
dau.install.payment$log_month <- substr(dau.install.payment$log_date, 1,7)
dau.install.payment$install_month <- substr(dau.install.payment$install_date, 1,7)
install.packages("plyr")
library(plyr)
mau.payment <- ddply(dau.install.payment, .(log_month, user_id, install_month),
                     summarize,
                     payment = sum(payment))
head(mau.payment)
mau.payment$user.type <- ifelse(mau.payment$install_month == mau.payment$log_month, "install", "existing")
mau.payment.summary <- ddply(mau.payment,
                             .(log_month, user.type),
                             summarize,
                             total.payment = sum(payment))
head(mau.payment)                            
head(mau.payment.summary)
mau.payment.summary
library(ggplot2)
library(scales)
ggplot(mau.payment.summary, aes(x=log_month, y=total.payment, fill=user.type, order=user.type)) +
         geom_bar(stat = "identity") +
         scale_y_continuous(label = comma)
ggplot(mau.payment[mau.payment$payment > 0 & mau.payment$user.type == "install", ], aes(x= payment, fill = log_month)) +
  geom_histogram(position = "dodge", binwidth = 20000) +  scale_x_continuous(label = comma)

ssas
