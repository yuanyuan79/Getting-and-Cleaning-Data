url <- 'https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/spider_wolff_gorb_2013.csv'
filename <- 'spider_wolff_gorb_2013.csv'
library(downloader)
if (!file.exists(filename)) download(url, filename)
spider <- read.csv(filename, skip = 1)
fit6 <- lm(friction ~ type+leg, data = spider)
betahat <- coef(fit6)
Y <- matrix(spider$friction, ncol=1)
X <- model.matrix(~type+leg, data=spider)
Q <- qr.Q(qr(X)) 
R <- qr.R(qr(X)) 



head(spider)
boxplot(spider$friction ~ spider$type * spider$leg, col = c('grey93','grey40'),las =2, main = 'Comparison of friction coefficients of different lef pairs')
spider.sub <- spider[spider$leg == 'L1',]
fit <- lm(friction ~ type, data = spider.sub)
summary(fit)
X <- model.matrix(~type, data = spider.sub)
colnames(X)
head(X)
library(rafalib)
imagemat(X, main = 'Model matrix for linear model with interactions')
stripchart(split(spider.sub$friction, spider.sub$type), vertical = T, pch =1, method = 'jitter',las =2, xlim = c(0,3), ylim =c(0,2))
a <- -0.25
lgth <- .1
library(RColorBrewer)
cols <- brewer.pal(3, 'Dark2')
abline(h=0)
coefs <- coef(fit)
arrows(1+a, 0, 1+a,coefs[1],lwd =3, col = cols[1], length = lgth)
abline(h=coefs[1],col=cols[1])
arrows(2+a,coefs[1], 2+a, coefs[1]+coefs[2],lwd = 3, col=cols[2],length=lgth)
abline(h=coefs[1]+coefs[2],col=cols[2])
legend('right',names(coefs),fill = cols, cex = .75, bg = 'white')


X <- model.matrix(~type+leg, data= spider)
colnames(X)
imagemat(X, main = 'Model matrix for linear model with 2 factors')
fit2 <- lm(friction ~type+leg, data = spider)
summary(fit2)

spider$group <- factor(paste0(spider$leg, spider$type))
stripchart(split(spider$friction,spider$group),vertical = T, pch=1, method = 'jitter', las =2, xlim = c(0,11), ylim = c(0,2))
cols <- brewer.pal(5, 'Dark2')
abline(h=0)
coefs <- coef(fit2)
arrows(1+a, 0, 1+a, coefs[1], lwd = 3, col = cols[1],length = lgth)
abline(h=coefs[1],col=cols[1])
arrows(3+a, coefs[1],3+a, coefs[1]+coefs[3],lwd =3, col = cols[3], length = lgth)
arrows(5+a, coefs[1],5+a, coefs[1]+coefs[4],lwd =3, col = cols[4], length = lgth)
arrows(7+a, coefs[1],7+a, coefs[1]+coefs[5],lwd =3, col = cols[5], length = lgth)

arrows(2+a, coefs[1],2+a, coefs[1]+coefs[2],lwd =3, col = cols[2], length = lgth)
segments(3+a, coefs[1]+coefs[3],4+a, coefs[1]+coefs[3], lwd =3, col=cols[3])
arrows(4+a, coefs[1]+coefs[3],4+a, coefs[1]+coefs[3]+coefs[2],lwd =3, col = cols[2], length = lgth)
segments(5+a, coefs[1]+coefs[4],6+a, coefs[1]+coefs[4], lwd =3, col=cols[4])
arrows(6+a, coefs[1]+coefs[4],6+a, coefs[1]+coefs[4]+coefs[2],lwd =3, col = cols[2], length = lgth)
segments(7+a, coefs[1]+coefs[5],8+a, coefs[1]+coefs[5], lwd =3, col=cols[5])
arrows(8+a, coefs[1]+coefs[5],8+a, coefs[1]+coefs[5]+coefs[2],lwd =3, col = cols[2], length = lgth)
legend('right',names(coefs),fill = cols, cex = .75, bg = 'white')

library(contrast)
L3vsL2 <- contrast(fit2, list(leg='L3',type='pull'), list(leg='L2',type='pull'))
L3vsL2
L4vsL2 <- contrast(fit2, list(leg='L4',type='pull'), list(leg='L2',type='pull'))
L4vsL2



X <- model.matrix(~type*leg, data= spider)
colnames(X)
imagemat(X, main = 'Model matrix for linear model with 2 factors')
fit3 <- lm(friction ~type*leg, data = spider)
summary(fit3)
coefs <- coef(fit3)

stripchart(split(spider$friction,spider$group),vertical = T, pch=1, method = 'jitter', las =2, xlim = c(0,11), ylim = c(0,2))
cols <- brewer.pal(8, 'Dark2')
abline(h=0)

arrows(1+a, 0, 1+a, coefs[1], lwd = 3, col = cols[1],length = lgth)
abline(h=coefs[1],col=cols[1])
arrows(3+a, coefs[1],3+a, coefs[1]+coefs[3],lwd =3, col = cols[3], length = lgth)
arrows(5+a, coefs[1],5+a, coefs[1]+coefs[4],lwd =3, col = cols[4], length = lgth)
arrows(7+a, coefs[1],7+a, coefs[1]+coefs[5],lwd =3, col = cols[5], length = lgth)

arrows(2+a, coefs[1],2+a, coefs[1]+coefs[2],lwd =3, col = cols[2], length = lgth)

segments(3+a, coefs[1]+coefs[3],4+a, coefs[1]+coefs[3], lwd =3, col=cols[3])
arrows(4+a, coefs[1]+coefs[3],4+a, coefs[1]+coefs[3]+coefs[2],lwd =3, col = cols[2], length = lgth)
arrows(4+a, coefs[1]+coefs[3]+coefs[2],4+a, coefs[1]+coefs[3]+coefs[2]+coefs[6],lwd =3, col = cols[6], length = lgth)

segments(5+a, coefs[1]+coefs[4],6+a, coefs[1]+coefs[4], lwd =3, col=cols[4])
arrows(6+a, coefs[1]+coefs[4],6+a, coefs[1]+coefs[4]+coefs[2],lwd =3, col = cols[2], length = lgth)
arrows(6+a, coefs[1]+coefs[4]+coefs[2],6+a, coefs[1]+coefs[4]+coefs[2]+coefs[7],lwd =3, col = cols[7], length = lgth)

segments(7+a, coefs[1]+coefs[5],8+a, coefs[1]+coefs[5], lwd =3, col=cols[5])
arrows(8+a, coefs[1]+coefs[5],8+a, coefs[1]+coefs[5]+coefs[2],lwd =3, col = cols[2], length = lgth)
arrows(8+a, coefs[1]+coefs[5]+coefs[2],8+a, coefs[1]+coefs[5]+coefs[2]+coefs[8],lwd =3, col = cols[8], length = lgth)

legend('right',names(coefs),fill = cols, cex = .75, bg = 'white')
L2push.vs.pull <- contrast(fit3, 
                           list(leg='L2',type='push'), 
                           list(leg='L2',type='pull'))
L2push.vs.pull
L3vsL2interaction <- glht(fit3, linfct=C)

spider$log2friction <- log2(spider$friction)
boxplot(log2friction ~type*leg, data = spider)
fit4 <- lm(log2friction ~type*leg, data = spider)
summary(fit4)
coefs <- coef(fit4)

anova(fit4)

C <- matrix(c(0,0,0,0,0,-1,1,0),1)
L3vsL2interaction <- glht(fit4, linfct=C)


X <- model.matrix(~0+group, data= spider)
colnames(X)
imagemat(X, main = 'Model matrix for linear model with interaction')
library(rafalib)
fit5 <- lm(friction ~0+group, data = spider)
summary(fit5)
coefs <- coef(fit5)
stripchart(split(spider$friction,spider$group),vertical = T, pch=1, method = 'jitter', las =2, xlim = c(0,11), ylim = c(0,2))
cols <- brewer.pal(8, 'Dark2')
abline(h=0)
for (i in 1:8) {
  arrows(i+a, 0, i+a, coefs[i],lwd =3, col = cols[i],length=lgth)
}
legend('right', names(coefs),fill=cols,cex = .75, bg = 'white')
library(contrast)
groupL2push.vs.pull <- contrast(fit5, list(group = 'L2push'), list(group = 'L2pull'))
groupL2push.vs.pull
coefs[4]-coefs[3]
C <- matrix(c(0,0,1,-1,-1,1,0,0),1)
groupL3vsL2interaction <- glht(fit5, linfct=C)
summary(groupL3vsL2interaction)


Sex <- c(0,0,0,0,1,1,1,1)
A <- c(1,1,0,0,0,0,0,0)
B <- c(0,0,1,1,0,0,0,0)
C <- c(0,0,0,0,1,1,0,0)
D <- c(0,0,0,0,0,0,1,1)
X <- model.matrix(~Sex +A+B+C+D-1)
cat('ncol=',ncol(X),'rank=',qr(X)$rank, '\n')

sex <- factor(rep(c('female','male'),each=4))
trt <- factor(c('A','A','B','B','C','C','D','D'))
X <- model.matrix(~sex+trt)
X
qr(X)$rank
Y <- 1:8


makeYstar <- function(a,b) Y-X[,2]*a-X[,5]*b
a = 1
b =2
fitTheRest <- function(a,b){
  Ystar <- makeYstar(a,b)
  Xrest <- X[,-c(2,5)]
  betarest <- solve(t(Xrest)%*%Xrest)%*%t(Xrest)%*%Ystar
  residuals <- Ystar-Xrest%*%betarest
  sum(residuals^2)
}
fitTheRest(a=1,b=2)

betas <- expand.grid(-2:8,-2:8)
rss = apply(betas, 1, function(x) fitTheRest(x[1],x[2]))
min(rss)
fitTheRest(6,0) 
betas[which(rss==min(rss)),]
