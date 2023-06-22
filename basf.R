

kupci <- table(Customer)
length(kupci)
kupci[which(kupci == max(kupci))]
kupci[which(kupci == min(kupci))]
length(kupci[which(kupci == min(kupci))])
length(kupci[which(kupci > 1000)])


table(Distributor)
table(Distributor[which(Customer == "Botanichem CC")])
popis <- c()
for (i in Customer) {
  if (is.na(table(Distributor[which(Customer == i)])[2]) == FALSE){
    popis <- c(popis, i)
  }
}
# all customers are consistently with or without a distributor
imaju <- 0
nemaju <- 0
for (i in names(table(Customer))) {
  if (Distributor[which(Customer == i)][1] == "Distributor yes") {
    imaju <- imaju + 1
  }
  else {
    nemaju <- nemaju + 1
  }
}
(dist_distributor <- c(imaju, nemaju))
chisq.test(dist_distributor, p = c(1/2, 1/2))


length(table(Invoice))
max(table(Invoice))
table(Invoice)[which(table(Invoice) == max(table(Invoice)))]
names(table(Customer[which(Invoice == 547063)]))
table(Invoice)[which(table(Invoice) == min(table(Invoice)))]
length(table(Invoice)[which(table(Invoice) == min(table(Invoice)))])

maksi <- Quantity[1]
niz_maksi <- c(maksi)
for (i in 1:length(Quantity)){
  if (Quantity[i]>maksi){
    maksi <- Quantity[i]
  }
  niz_maksi <- c(niz_maksi, maksi)
}

library("qqplotr")
library("VGAM")
df <- data.frame(y=Quantity)
ggplot(df, aes(sample=y)) +
  stat_qq(distribution = qfrechet, dparams = list(shape = 1)) +
  xlab("") + ylab("") +  theme_classic(base_size = 12)
#not a good fit

df <- data.frame(y=niz_maksi)
ggplot(df, aes(sample=y)) +
  stat_qq(distribution = qfrechet, dparams = list(shape = 1)) +
  xlab("") + ylab("") +  theme_classic(base_size = 12)
# to nema smislaaaa


table(`Operating Division`)
chisq.test(table(`Operating Division`), p = rep(1/6, 6))
division <- names(table(`Operating Division`))
freq_divition <- rep(0, 6)
for (i in names(table(Customer))) {
  for (j in 1:6) {
    if (`Operating Division`[which(Customer == i)][1] == division[j]) {
      freq_divition[j] <- freq_divition[j] + 1
    }
  }
  
}
chisq.test(table(`Operating Division`), p = c(1/7, 2/7, 1/7, 1/7, 1/7, 1/7))
chisq.test(table(`Operating Division`), p = c(3/18, 6/18, 2/18, 2/18, 3/18, 2/18))
chisq.test(c(104, 97), p = c(1/2, 1/2))


summary(invoice_date)
max(invoice_date) - min(invoice_date)
#dates_for_products <- c()
#for(i in 1:length(Invoice)){
#  dates_for_products <- c(dates_for_products, rep(`Invoice Date`[i], Quantity[i]))
#}
#rm(dates_for_products)
datumi <- names(table(invoice_date))
datumi <- as.POSIXct(datumi, format="%Y-%m-%d %H:%M", tz="UTC")

table(`Invoice Month`)
plot(table(`Invoice Month`), cex = 0.2)
months <- table(`Invoice Month`)[names = c("December", "January","February", "March", 
                                               "April", "May", "June", "July", "August")]
plot(months)
months_freq <- c()
different_invoices <- names(table(Invoice))
for(i in names(months)){
  new_freq <- 0
  for(j in different_invoices){
    if(`Invoice Month`[which(Invoice == j)][1] == i){
      new_freq <- new_freq + 1
    }
  }
  months_freq <- c(months_freq, new_freq)
}
(names(months_freq) <- names(months))

quant_forinvoice <- c()
for(i in different_invoices){
  quant_forinvoice <- c(quant_forinvoice, sum(Quantity[which(Invoice == i)]))
}
quant_forinvoice

quant_formonths <- c()
for(i in names(months)){
  quant_formonths <- c(quant_formonths, sum(Quantity[which(`Invoice Month` == i)]))
}
quant_formonths
max(quant_formonths)
names(months)[which(quant_formonths == max(quant_formonths))]

dates <- c()
for (i in different_invoices) {
  dates <- c(dates, `Invoice Date`[which(Invoice == i)][1])
}
dates <- as.POSIXct(sort(dates, decreasing = FALSE), 
                    format="%Y-%m-%d %H:%M", tz="UTC")

price_per_invoice <- c()
for(i in dates){
  price_per_invoice <- c(price_per_invoice, sum(`Final Price`[which(invoice_date == i)]))
}
summary(price_per_invoice)
plot(dates, price_per_invoice, type = "l")


price_per_invoice_ts <- ts(price_per_invoice)
plot(price_per_invoice_ts)
library("tseries")
# test stationarity with augmented dickey-fuller test
# H0: there exist a unit root (non-stationary), H1: stationary
adf.test(price_per_invoice_ts)
acf(price_per_invoice_ts)
acf(price_per_invoice_ts, lag = 10000)
install.packages("TSA")
library(TSA)
acf <- stats::acf
#BoxCox.ar(price_per_invoice_ts)
#BoxCox.ar(price_per_invoice_ts)$mle
#bc_price_per_invoice_ts <- (price_per_invoice_ts^(-0.1)-1)/-0.1
#plot(bc_price_per_invoice_ts)
#adf.test(bc_price_per_invoice_ts)
#plot(diff(bc_price_per_invoice_ts))
#adf.test(diff(bc_price_per_invoice_ts))
#acf(diff(bc_price_per_invoice_ts), lag.max = 150)


najboljiAIC <- function(armax, mamax, podaci) {
  redAR <- c()
  redMA <- c()
  AICvrijednost <- c()
  for (i in 0:armax) {
    for (j in 0:mamax) {
      fitaic <- tryCatch(AIC(arima(podaci, c(i, 0, j))), error = function(e) NaN)
      redAR <- c(redAR,i)
      redMA <- c(redMA, j)
      AICvrijednost <- c(AICvrijednost, fitaic)
    }
  }
  rez <- data.frame(p = redAR, q = redMA, AICvrijednosti = AICvrijednost)
  rez <- rez[order(rez$AICvrijednosti), ]
  return(rez[1:min(10,length(redAR)), ])
}
najboljiBIC <- function(armax, mamax, podaci) {
  redAR <- c()
  redMA <- c()
  BICvrijednost <- c()
  for (i in 0:armax) {
    for (j in 0:mamax) {
      fitbic <- tryCatch(BIC(arima(podaci, c(i, 0, j))), error = function(e) NaN)
      redAR <- c(redAR,i)
      redMA <- c(redMA, j)
      BICvrijednost <- c(BICvrijednost, fitbic)
    }
  }
  rez <- data.frame(p = redAR, q = redMA, BICvrijednosti = BICvrijednost)
  rez <- rez[order(rez$BICvrijednosti), ]
  return(rez[1:min(10,length(redAR)), ])
}
najboljiAIC(5, 5, price_per_invoice_ts)
najboljiBIC(5, 5, price_per_invoice_ts)

#use bc only if adf.test rejects stationarity, either bc or differentiate

shapiro.test(bc_price_per_invoice_ts) #does not work, too large sample
library("nortest")
ad.test(bc_price_per_invoice_ts)
jarque.bera.test(bc_price_per_invoice_ts)
# rejected normality - or do i need normality for differences?? to be continued...

library(astsa)
model <- sarima(price_per_invoice_ts, 2, 0, 0, no.constant = TRUE)
model$fit
coef(model$fit)
confint(model$fit)
# 95% conf intervals don't contain zero so all coefficients are statistically significant
ad.test(model$fit$residuals)
# problem with p-values for Ljung-box statistics -> all p-values are 0 so we reject 
# null hypothesis about non correlated residuals -> not good
# also, residuals not normal...
# find another model
model <- sarima(price_per_invoice_ts[1:(length(price_per_invoice_ts)-10)], 2, 0, 0)
model$fit
coef(model$fit)
confint(model$fit)

#this works, p values for Ljung Box statistic are all high
# we have no reason to think there is correlation between residuals

ad.test(model$fit$residuals)
#however, they are not normal
# I cannot find parameters so that normality is not rejected
m1 <- sarima(price_per_invoice_ts, 0, 0, 4)


analiza.modela <- function(podaci, modeli) {
  AICvrijednosti <- c()
  BICvrijednosti <- c()
  varijance <- c()
  brojparametara <- c()
  pvrij <- c()
  rez <- matrix(data = rep(0, 5*length(modeli)), nrow = 5, ncol = length(modeli))
  rownames(rez) <- c("AIC", "BIC", "Var", "Number of parameters", "p-value ADtest")
  colnames(rez) <- paste("ARIMA", as.character(modeli), sep = "")
  for (i in 1:length(modeli)) {
    par(mar=c(1,1,1,1))
    model <- invisible(sarima(podaci, modeli[[i]][[1]], modeli[[i]][[2]], modeli[[i]][[3]], details=F))
    rez[1,i] <- model$AIC
    rez[2,i] <- model$BIC
    rez[3,i] <- model$fit$sigma2
    rez[4,i] <- round(length(model$fit$coef))
    rez[5,i] <- round(ad.test(model$fit$residuals)$p.value, 4)
  }
  return(rez)
}

redovi <- list(c(0,0,2), c(2,0,0), c(1,0,1), c(0,0,1), c(0,0,3), c(3,0,0), c(2,0,1), c(1,0,2))
analiza.modela(price_per_invoice_ts[1:(length(price_per_invoice_ts)-10)], redovi)


forecast(model)

library(forecast)
moj_model <- Arima(price_per_invoice_ts[1:(length(price_per_invoice_ts)-10)], c(2,0,0))
plot(moj_model)
prede <- forecast(moj_model)

library(vars)
ts.plot(price_per_invoice_ts[1:(length(price_per_invoice_ts)-10)])
plot(window(price_per_invoice_ts, start = 10200, end = length(price_per_invoice_ts)-10), 
     type = "l", xlim = c(10205, 10293), ylim = c(0, 2800))  
     #main = "Stvarne i predvidene vrijednosti",  
     #xlab = "Mjerenje (vrijeme)", ylab = "Box Coxova transf. vrijedn. Ethera", 
     #ylim = c(1.6, 1.64))
polygon(c(time(prede$lower), rev(time(prede$upper))), 
        c(prede$lower[, 2], rev(prede$upper[, 2])), 
        col = rgb(0,0,1,0.1), border = FALSE)
polygon(c(time(prede$lower), rev(time(prede$upper))), 
        c(prede$lower[, 1], rev(prede$upper[, 1])), 
        col = "light blue", border = FALSE)
points(prede$mean, col = "black", pch = 16, cex = 0.6)
points(price_per_invoice_ts, pch=16, cex=0.6, col = "orange")
legend("bottomleft", c("stvarno", "predvideno"), cex=0.8, col = c("orange", "light yellow"), pch = c(16, 16))


plot(window(price_per_invoice_ts, start = 10200, end = length(price_per_invoice_ts)-10), 
     type = "l", xlim = c(10275, 10293), ylim = c(0, 2800))
polygon(c(time(prede$lower), rev(time(prede$upper))), 
        c(prede$lower[, 2], rev(prede$upper[, 2])), 
        col = rgb(0,0,1,0.1), border = FALSE)
polygon(c(time(prede$lower), rev(time(prede$upper))), 
        c(prede$lower[, 1], rev(prede$upper[, 1])), 
        col = "light blue", border = FALSE)
points(prede$mean, col = "black", pch = 16, cex = 0.8)
points(price_per_invoice_ts, pch=16, cex=1.2, col = "orange")
