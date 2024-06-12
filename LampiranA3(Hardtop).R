testing11 <- subset(testing, testing$VehBody == "Hardtop")

#GLM Binomial CLAIM OCC
binom = glm(ClaimOcc~VehValue+VehAge+VehBody+Gender+DrivAge, family = binomial(link = "logit"), data = training, offset = log(Exposure))
summary(binom)
predbinom = predict(binom, newdata = testing11, type = "link") 
muhatbinom = (exp(predbinom)/(1+exp(predbinom)))

#GLM Gamma CLAIM AMOUNT  
gsev = glm(ClaimAmount~VehValue+VehAge+VehBody+Gender+DrivAge, family = Gamma(link = "log"), data = training01)
summary(gsev)
prdgam1 = predict(gsev, newdata = testing11, type = "link") 
mugam1 = exp(prdgam1)

#GLM Gamma CLAIM AMOUNT BARU
gamsev = glm(ClaimAmountBaru~VehValue+VehAge+VehBody+Gender+DrivAge, family = Gamma(link = "log"), data = training01)
summary(gamsev)
prdgam = predict(gamsev, newdata = testing11, type = "link") 
mugam = exp(prdgam)

#GLMM Poisson CLAIM FREQ
library(lme4)
pois <- glmer(ClaimNb ~ (1|VehBody) + VehValue + VehAge + Gender + DrivAge 
              + offset(log(Exposure)), family = poisson(link = "log"), data = training, nAGQ = 25) 
summary(pois)
predps = predict(pois, newdata = testing11, type = "link") 
muhatps = exp(predps)

#GLMM Inverse Gaussian CLAIM AMOUNT BARU 
library(lme4)
inversegaus = glmer(ClaimAmountBaru ~ (1|VehBody) +VehAge+Gender+DrivAge, family = inverse.gaussian(link = "identity"), data = training01, offset = log(Exposure), control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000)))
summary(inversegaus)
predinversegauss = predict(inversegaus, newdata = testing11, type = "link") 
muhatinversegaus = predinversegauss

#GLMM Inverse Gaussian CLAIM AMOUNT 
library(lme4)
inversegaus1 = glmer(ClaimAmount ~ (1|VehBody) +VehValue+VehAge+Gender+DrivAge, family = inverse.gaussian(link = "identity"), data = training01, offset = log(Exposure), control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000)))
summary(inversegaus1)
predinversegauss1 = predict(inversegaus1, newdata = testing11, type = "link") 
muhatinversegaus1 = predinversegauss1

#Cek MSE, MAE
#GLM Binom - GLM Gamma Claim Amount 
mod1 = muhatbinom*mugam1
mean((testing11$ClaimAmount-mod1)^2)
sum(abs(testing11$ClaimAmount-mod1))/326

#GLMM Poisson - GLMM Inverse Gauss Claim Amount Baru 
mod2 = muhatps*muhatinversegaus
mean((testing11$ClaimAmount-mod2)^2)
sum(abs(testing11$ClaimAmount-mod2))/326

#GLMM Poisson - GLM Gamma Claim Amount Baru
mod3 = muhatps*mugam
mean((testing11$ClaimAmount-mod3)^2)
sum(abs(testing11$ClaimAmount-mod3))/326

#GLM Binom - GLMM Inverse Gaussian Claim Amount
mod4 = muhatbinom*muhatinversegaus1
mean((testing11$ClaimAmount-mod4)^2)
sum(abs(testing11$ClaimAmount-mod4))/326

group1 = testing11$ClaimAmount
group2 = mod1
group3 = mod2
group4 = mod3
group5 = mod4
data <- list(group1, group2, group3, group4, group5)
boxplot(data,
        col = c("grey", "grey", "grey", "grey", "grey"),  # Set box colors
        border = c("black", "black", "black", "black", "black"),                # Set border colors
        names = c("Grup 1", "Grup 2", "Grup 3", "Grup 4", "Grup 5"),       # Set group names
        main = "Perbandingan Data Asli dengan Hasil Model Terbaik dalam Prediksi Kerugian Jenis Kendaraan Hardtop di Seluruh Data",
        ylab = "ClaimAmount" ,ylim=c(0,1000))

means <- sapply(data, mean)
points(1:length(data), means, col = "red", pch = 18, cex = 2)
text(1:length(data), means + 20, round(means, 2), col = "red", pos = 3)


mean(group1)
mean(group2)
mean(group3)
mean(group4)
mean(group5)
##
sum(group1) 
sum(group2)
sum(group3)
sum(group4)
sum(group5)