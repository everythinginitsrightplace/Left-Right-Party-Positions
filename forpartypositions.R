library(plm)
majrelat<- read.table(file = "majrelat.txt", header = TRUE)
attach(majrelat)
Y <- cbind(relat)
X <- cbind(medvotimpr, electoraldef, elecparty, lagged, gdp, inflgdp)

rdata<- plm.data(majrelat, index=c("party_code","date"))


#Сначала ordinary least squares
ols<-lm(Y ~ X + officeexcl  + country - 1, data=rdata)
summary(ols)
#тут есть график, 
yhat <- ols$fitted
plot(rdata$lagged, rdata$relat, pch=19, xlab="предыдущее направление", ylab="модульное значение")
abline(lm(rdata$lagged~rdata$relat),lwd=3, col="red")





#Далее изучим метод наименьших квадратов с дамми переменными
fixed.dum <-lm(Y ~ X + officeexcl + country - 1, data=rdata)
summary(fixed.dum)
yhat <- fixed.dum$fitted
library(car)
scatterplot(yhat~rdata$relat|rdata$country, boxplots=FALSE, xlab="предыдущее направление", ylab="Изменение на шкале левые-правые",smooth=FALSE)
abline(lm(rdata$relat~rdata$lagged),lwd=3, col="red")

scatterplot(Y~date|country, boxplots=FALSE, smooth=TRUE, reg.line=FALSE, data=rdata)
coplot(Y ~ date|country, type="l", data=rdata)
coplot(Y ~ date|country, type="b", data=rdata)

library(gplots)
plotmeans(Y ~ country, main="Heterogeineity across countries", data=rdata)
plotmeans(Y ~ date, main="Heterogeineity across years", data=rdata)




library(apsrtable)
apsrtable(ols,fixed.dum, model.names = c("OLS", "OLS_DUM")) # Displays a table in Latex 



fixed <- plm(Y ~ X + officeexcl + country - 1, data=rdata, index=c("party_code", "date"), model="within")
summary(fixed)


#Далее идет тест на то, какая именно модель лучше: с фиксированными эффектами или  ОЛС.
#Если р-значение меньше 0.05, модель с фиксированными эффектами лучше
pFtest(fixed, ols) # Testing for fixed effects, null: OLS better than fixed


#Теперь модель со случайными эффектами
random <- plm(Y ~ X + officeexcl + country - 1, data=rdata, index=c("party_code", "date"), model="random")
summary(random)

#Проверяем, какая модель лучше: фикси или рэндом. Если р-значение меньше 0.05, ты выбираем модель с фиксированными эффектами
phtest(fixed, random)



pool <- plm(Y ~ X + officeexcl + country - 1, data=rdata, index=c("party_code", "date"), model="pooling")
summary(pool)


#Если р-значение меньше 0.05, то делаем выбор в пользу random, nor OLS
plmtest(pool, type=c("bp"))
pbgtest(fixed)




#Далее тест Бройша-Пагана. Если р-значение меньше 0.05, то есть гетероскедастичность
library(lmtest)
bptest(Y ~ X + officeexcl + country - 1, data = rdata, studentize=F)
pcdtest(fixed, test = c("cd"))


#Если снова р-значение меньше 0.05, то есть корреляция
pbgtest(fixed)


#Мы выбираем модель с фиксированными эффектами, потому что того требуют результаты теста
fixed <- plm(Y ~ X + officeexcl + country - 1, data=rdata, index=c("party_code", "date"), model="within")
coeftest(fixed) # Original coefficients
coeftest(fixed, vcovHC) # Heteroskedasticity consistent coefficients
coeftest(fixed, vcovHC(fixed, method = "arellano")) # Heteroskedasticity consistent coefficients (Arellano)
coeftest(fixed, vcovHC(fixed, type = "HC3")) # Heteroskedasticity consistent coefficients, type 3

#Функция vcovHAC снова не работает в панельной регрессии
vcovHAC(fixed)  
coeftest(model, vcov. = vcovHAC(fixed))

coeftest(pool, vcov=vcovBK(pool, type="HC1"))
coeftest(pool, vcov = vcovBK(pool, cluster="time",type = "HC3"))
coeftest(pool, vcov = vcovBK(pool, cluster="group",type = "HC3"))



random <-plm(Y ~ X + officeexcl + country - 1, data=rdata, index=c("party_code", "date"), model="random")
coeftest(random) # Original coefficients
coeftest(random, vcovHC) # Heteroskedasticityconsistent coefficients
coeftest(random, vcovHC(random, type = "HC3")) # Heteroskedasticityconsistent coefficients


