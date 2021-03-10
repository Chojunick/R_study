ad.data <- read.csv("ad_result.csv", header = T, stringsAsFactors = F)
ad.data
library(ggplot2)
library(scales)
ggplot(data = ad.data, aes(x= tvcm, y=install))+geom_point()+xlab("TV 광고비")+ylab("신규 유저수")+
  scale_x_continuous(label = comma) + scale_y_continuous(label=comma)
ggplot(data = ad.data, aes(x=magazine, y=install))+geom_point() + xlab("잡지 광고비") +ylab("신규 유저수")+ scale_x_continuous(label = comma)+ scale_y_continuous(label =comma)
fit <- lm(install ~ ., data = ad.data[,c("install", "tvcm", "magazine")])
fit
summary(fit)

# chapter7
dau <- read.csv("section7-dau.csv", header = T, stringsAsFactors = F)
dau
mau <- unique(dau[, c("region_month", "device", "user_id")])
head(mau)
fp.mau <- unique(dau[dau$device=="FP", c("region_month", "device", "user_id")])
sp.mau <- unique(dau[dau$device == "SP", c("region_month", "device", "user_id")])
fp.mau1 <- fp.mau[fp.mau$region_month =="2013-01",]
fp.mau2 <- fp.mau[fp.mau$region_month == "2013-02",]
head(fp.mau2)
sp.mau1 <- sp.mau[sp.mau$region_month == "2013-01",]
sp.mau2 <- sp.mau[sp.mau$region_month == "2013-02",]
mau$is_access <-1
fp.mau1 <- merge(fp.mau1, mau[mau$region_month == "2013-02", 
                              c("user_id", "is_access")], by = "user_id", all.x =T)
head(fp.mau1)
fp.mau1$is_access[is.na(fp.mau1$is_access)] <- 0
head(fp.mau1)
fp.mau2$is_fp <-1
fp.mau1 <- merge(fp.mau1, fp.mau2[, c("user_id", "is_fp")], by = "user_id", all.x = T)
fp.mau1$is_fp[is.na(fp.mau1$is_fp)] <- 0
head(fp.mau1)
sp.mau2$is_sp <- 1
fp.mau1 <- merge(fp.mau1, sp.mau2[,c("user_id", "is_sp")],
                 by = "user_id", all.x = T)
fp.mau1$is_sp[is.na(fp.mau1$is_sp)] <- 0
head(fp.mau1)
fp.mau1 <- fp.mau1[fp.mau1$is_access == 0 | fp.mau1$is_sp ==1, ]
head(fp.mau1)
library(reshape2)
fp.dau1 <- dau[dau$device == "FP" & dau$region_month == "2013-01", ]
fp.dau1$is_access <-1
fp.dau1.cast <- dcast(fp.dau1, user_id ~ region_day, value.var = "is_access", function(x) as.character(length(x)))
names(fp.dau1.cast)[-1] <- paste0("X", 1:31, "day")
head(fp.dau1.cast)
fp.dau1.cast <- merge(fp.dau1.cast, fp.mau1[, c("user_id", "is_sp")], by = "user_id")
head(fp.dau1.cast)
table(fp.dau1.cast$is_sp)
fit.logit <- step(glm(is_sp ~., data = fp.dau1.cast[, -1],
                      family = binomial))
summary(fit.logit)
fp.dau1.cast$prob <- round(fitted(fit.logit), 2)
fp.dau1.cast$pred <- ifelse(fp.dau1.cast$prob >0.5, 1,0)
head(fp.dau1.cast)
table(fp.dau1.cast[, c("is_sp", "pred")])
fp.dau1.cast1 <- fp.dau1.cast[fp.dau1.cast$is_sp == 1 & fp.dau1.cast$pred == 1,]
head(fp.dau1.cast1[order(fp.dau1.cast1$prob, decreasing = T), ])
fp.dau1.cast2 <- fp.dau1.cast[fp.dau1.cast$is_sp == 0 & fp.dau1.cast$pred ==1,]
head(fp.dau1.cast2[order(fp.dau1.cast2$prob, decreasing = T), ])
fp.dau1.cast3 <- fp.dau1.cast[fp.dau1.cast$is_sp == 0 & fp.dau1.cast$pred == 0, ]
head(fp.dau1.cast3[order(fp.dau1.cast3$prob), ])
