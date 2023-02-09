1.
sato1 <- c(5.0, 4.3, 5.9)
sato2 <- c(6.1, 5.7, 7.0)
sato <- c(sato1, sato2)
fung <- gl(2,3,6, labels=c("ei", "kyllä"));fung
tila <- gl(3,1,6) ; tila
data.frame(sato, fung, tila)
interaction.plot(fung, tila, sato, type = 'b', lty=1, pch=0:2)
ero <- sato2 - sato1 ; ero
mean(ero); sd(ero)
t.test(ero)
t.test(sato2, sato1, paired=T)
t.test(sato2, sato1, var.equal=T)
2.
va2 <- lm(sato ~ fung + tila)
round(cbind(summary(va2)$coef, confint(va2), 4))
sovite <- fitted(va2)
jaannos <- resid(va2)
data.frame(fung, tila, sato, sovite, jaannos)
anova(va2)
3.
sipuli.g <- c(371, 378, 464, 463,
410, 373, 498, 654,
642, 536, 507, 588)
library(gmodels)
Y <- sipuli.g/1000
rivit <- gl(3,4,12)
levels(rivit) <- c("3r", "4r", "5r")
lohko <- gl(4,1,12)
data.frame(rivit, lohko, Y)
Ym.r <- tapply(Y, rivit, mean)
Ysd.r <- tapply(Y, rivit, sd)
round(cbind(keskiarvo=Ym.r, hajonta=Ysd.r), 3)
round(tapply(Y, lohko, mean), 3)
round(tapply(Y, lohko, sd), 3)
round(mean(Y), 3)
interaction.plot(rivit, lohko, Y, type='o', pch=1,
xlab="Rivien lkm", ylab="Satotulos (kg/m^2)")
lines(1:3, Ym.r, type="o", pch=4, cex = 1.5, lwd=2, col="blue")
4.
m1 <- lm(Y ~ rivit)
summary(m1)
confint(m1)
anova(m1)
5.
m2 <- lm(Y ~ rivit + lohko)
summary(m2)
confint(m2)
anova(m2)
6.
library(gmodels)
fit.contrast(m1, rivit, c(-1, 0, 1), conf.int=0.95)
fit.contrast(m2, rivit, c(-1, 0, 1), conf.int=0.95)