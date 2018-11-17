library(data.table)
library(readr)
library(foreign)
library(haven)
setwd("~/Dropbox/HSE/Ming/dataset -- Ming")

##############
#             BSc Population Health Dissertation (18/19)  --  Draft
##############

##########################################################################################################################

# Read in 2004-2014 datasets

hse04 = read_dta("hse04gpa.dta")
write.csv(hse04, file = "hse04.csv")
hse04 <-read.csv("hse04.csv")

hse05 = read_dta("hse05ai.dta")
write.csv(hse05, file = "hse05.csv")
hse05 <-read.csv("hse05.csv")

hse06 = read_dta("hse06ai.dta")
write.csv(hse06, file = "hse06.csv")
hse06 <-read.csv("hse06.csv")

hse07 = read_dta("hse07ai.dta")
write.csv(hse07, file = "hse07.csv")
hse07 <-read.csv("hse07.csv")

hse08 = read_dta("hse08ai.dta")
write.csv(hse08, file = "hse08.csv")
hse08 <-read.csv("hse08.csv")

hse09 = read_dta("hse09ai.dta")
write.csv(hse09, file = "hse09.csv")
hse09 <-read.csv("hse09.csv")

hse10 = read_dta("hse10ai.dta")
write.csv(hse10, file = "hse10.csv")
hse10 <-read.csv("hse10.csv")

hse11 = read_dta("hse2011ai.dta")
write.csv(hse11, file = "hse11.csv")
hse11 <-read.csv("hse11.csv")

hse12 = read_dta("hse2012ai.dta")
write.csv(hse12, file = "hse12.csv")
hse12 <-read.csv("hse12.csv")

hse13 = read_dta("hse2013ai.dta")
write.csv(hse13, file = "hse13.csv")
hse13 <-read.csv("hse13.csv")

hse14 = read_dta("hse2014ai.dta")
write.csv(hse14, file = "hse14.csv")
hse14 <-read.csv("hse14.csv")

View(head(hse14)[,1:5])
write.csv(colnames(hse14), "names.csv")

table(hse14$gor1)
SM04 <-  pairs(~sex + ethcind + totinc + topqual3 + hse04$birthwt + wtval + htval + clotbf + sysavg + diaavg + porftvg
               + ch00tot + hse04$genhelf + limitill + smkevr,
               data=hse04,main="Scatterplot Matrix for 2004")
print(SM04, vp=viewport(layout.pos.col = (plot.index - 1) %% 2 + 1, 
                        layout.pos.row = (plot.index - 1) %/% 2 + 1))

####################################################################################################################
####################################################################################################################
####################################################################################################################
####################################################################################################################
# Add year for identification

hse04$year <- 04
hse05$year <- 05
hse06$year <- 06
hse07$year <- 07
hse08$year <- 08
hse09$year <- 09
hse10$year <- 10
hse11$year <- 11
hse12$year <- 12
hse13$year <- 13
hse14$year <- 14

hse04$sysavg <- (hse04$sys1om + hse04$sys2om + hse04$sys3om) / 3
hse04$diaavg <- (hse04$dias1om + hse04$dias2om + hse04$dias3om) / 3

pca1 <- prcomp(hse04.new, scale = TRUE)

hse04.2 <- c("sex", "ethcind","totinc","topqual3","birthwt","wtval","htval","clotbf","sysavg","diaavg","porftvg",
             "ch00tot","genhelf","limitill","gor","urban","natpr1","hhsize","hhdtypb","imd2004","famcvd","smkevr")
"urban"，"wtval"，"htval"，"limitill"，
hse04.2 <- c("sysavg","diaavg","age","ethcind","hhsize","hhdtypb","urban","addnum","imd2004","topqual3","porftvg","porftvg")

hse04$addnum

quantile(hse04$sysavg, c(.05, .95)) 
quantile(hse04$sysavg, .05)
quantile(hse04$sysavg, .95) 

hse04.new <- hse04[,hse04.2]

pca1 <- prcomp(hse04.new, scale = TRUE)

# The eigen values (explained variances) and the scree plot:
summary(pca1)
pca1.1 <-  princomp(hse04.new, cor = T, scores = TRUE)
fviz_eig(pca1.1)

# 图1
pca1.1.1 <- data.frame(pca1.1$loadings[,1:5])
ggplot(pca1.1.1,aes(x = Comp.1, y = Comp.2)) + geom_point() + geom_text(aes(label=rownames(pca1.1.1)),hjust=0, vjust=0)

# 图2 
biplot(pca1.1, scale = 0)
prediction <- data.frame(predict(pca1.1))
ggplot(prediction,aes(x = Comp.1, y = Comp.2)) + geom_point() + geom_text(aes(label=rownames(prediction)),hjust=0, vjust=0)

reg2 <- lm(sysavg~sex+ethcind+totinc+topqual3+birthwt+wtval+htval+clotbf+sysavg+diaavg+porftvg+ch00tot+genhelf+limitill+gor+urban+natpr1+hhsize+hhdtypb+imd2004+famcvd+smkevr, data=hse04.new)

summary(reg2)
```

```{r}
hse05$sysavg <- (hse05$sys1om + hse05$sys2om + hse05$sys3om) / 3
hse05$diaavg <- (hse05$dias1om + hse05$dias2om + hse05$dias3om) / 3
hse05.2 <- c("sysavg","diaavg","age")
hse05.new <- hse05[,hse05.2]

# 导入数据
plot(hse04.new)
# 把数据scale 用来计算distance
d_us <- scale(hse04.new)
# 计算distance
di_us <- dist(d_us)

#################
#                 Hierachical clustering
#################

####################################################################
# 创建一个一行三列的图（用来显示接下来要画的cluster）
par(mfrow=c(1,3)) 
# 画三种cluster
plot(cs <- hclust(di_us, method="single"), main = "single")
plot(cc <- hclust(di_us, method="complete"), main = "complete")
plot(ca <- hclust(di_us, method="average"), main = "average")
# 根据不同的需求可以根据h制定不同的cluster！
# 三幅图中（尤其是后两幅）最少可以只有两个cluster （第一幅图是把新点加到已有的cluster中）

####################################################################
# 先plot出一个坐标系 (type是n，为了不让点显示出来，因为后面有text)
par(mfrow=c(1,3)) 
plot(hse04.new[, 1:2], type = "n", xlab = "sysavg"
     , ylab = "diaavg", main = "2-cluster (average linkage)")
# 用cutree来砍树得到点
labs <- cutree(ca, h = 2.5)
# 画砍树得来的点
text(hse04.new[, 1:2], labels = labs, cex = 0.9)

####################################################################
# 另外两幅图 - 1
plot(USArrests[, 1],USArrests[, 4], type = "n", xlab = "Murder"
     , ylab = "Rape", main = "2-cluster (average linkage)")
labs <- cutree(ca, h = 2.5)
text(USArrests[, 1], USArrests[,4], labels = labs, cex = 0.9)
# 另外两幅图 - 2
plot(USArrests[, 2],USArrests[, 4], type = "n", xlab = "Assault"
     , ylab = "Rape", main = "2-cluster (average linkage)")
labs <- cutree(ca, h = 2.5)
text(USArrests[, 2], USArrests[,4], labels = labs, cex = 0.9)

####################################################################
# 用名字来代替点：颜色后加砍树得到的labs变量
par(mfrow=c(1,1)) 
plot(USArrests[, 1:2], type = "n", xlab = "Murder"
     , ylab = "Assault", main = "2-cluster (average linkage)")
text(USArrests[, 1:2], labels = labels(di_us), cex = 0.9, col 
     = c("black", "red")[labs])
```

####################################################################################################################
####################################################################################################################
####################################################################################################################
####################################################################################################################
####################################################################################################################




hse04.pc <- c("sysavg","diaavg","age","ethcind","natpr1","hhsize","hhdtypb","urindew","addnum","imd2004",
              "topqual3","porftvg","gor")
hse04.mk2 <- hse04[,hse04.pc]
hse04.mk2$year <- 04

hse05.pc <- c("sysavg","diaavg","age","ethinda","natpr1","hhsize","hhdtypb","urindew","addnum","imd2004",
              "topqual3","porftvg","gor")
hse05.mk2 <- hse05[,hse05.pc]
hse05.mk2$year <- 05

hse06.pc <- c("sysavg","diaavg","age","ethinda","natpr1","hhsize","hhdtypb","urindew","addnum","imd2004",
              "topqual3","porftvg","gor06")
hse06.mk2 <- hse06[,hse06.pc]
hse06.mk2$year <- 06

hse07.pc <- c("sysavg","diaavg","age","ethinda","natpr1","hhsized","hhdtypb","urindew","addnum","imd2007",
              "topqual3","porftvg","gor07")
hse07.mk2 <- hse07[,hse07.pc]
hse07.mk2$year <- 07

hse08.pc <- c("sysavg","diaavg","age","origin","natpr1","hhsize","hhdtypb","Urban","addnum",
              "topqual3","porftvg","GOR")
hse08.mk2 <- hse08[,hse08.pc]
range(hse08$Urban)
hse08.mk2$year <- 08


####################################################################################################################
####################################################################################################################
####################################################################################################################
####################################################################################################################
####################################################################################################################

quantile(x, ...)


