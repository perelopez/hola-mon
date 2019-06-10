library(faraway)
View(crawl)
help(crawl)
summary(crawl$SD)
crawl$SD*sqrt(crawl$n)
summary(crawl$SD*sqrt(crawl$n))
sd(crawl$SD)/mean(crawl$SD)
sd(crawl$SD*sqrt(crawl$n))/mean(crawl$SD*sqrt(crawl$n))
model <- lm(crawling~temperature, data=crawl)
summary(model)
plot(crawling~temperature, data=crawl)
plot(model)

# test de Bartlett de variàncies
si <- crawl$SD^2
ni <- crawl$n
N <- sum(ni)
k <- length(ni)
sp2 <- 1/(N-k)*sum((ni-1)*si)
ec <- 
  ((N-k)*log(sp2)-sum((ni-1)*log(si)))/
  (1+1/3/(k-1)*(sum(1/(ni-1))-1/(N-k)))
pchisq(ec, k-1, lower.tail = FALSE)

wi <- 1/(crawl$SD^2/crawl$n)
data.frame(ni, wi)
plot(ni, wi)
summary(lm(wi~ni))

model1 <- (lm(crawling~temperature, data=crawl))
model2 <- (lm(crawling~temperature, data=crawl, weights=crawl$n))
model3 <- (lm(crawling~temperature, data=crawl, weights=1/(crawl$SD^2/crawl$n)))
summary(model1)
summary(model2)
summary(model3)
plot(crawling~temperature, data=crawl)
text(crawl$temperature,crawl$crawling,crawl$n)
abline(model1, col="red")
abline(model2, col="blue")
abline(model3, col="green")

crexp <- data.frame(crawling=rep(crawl$crawling,times=crawl$n),
                    temperature=rep(crawl$temperature,times=crawl$n))
modeld <- (lm(crawling~temperature, data=crexp))
summary(modeld)
