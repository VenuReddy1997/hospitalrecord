#importing csv file
df <- read.csv("diabetic.csv", header=TRUE)
str(df)
summary((df))
# reaserch question : for the diabetic patience which eye is more affected (chance of getting effected)

# variables considered for hypothesis are right eye and left eye  data.
library("lattice")
histogram(~risk | eye, data = df)
with(df,
     qqplot(risk[eye == "left"],
            risk[eye == "right"], 
            main = "Eye infecting chances with diabetics", 
            xlab = "left",
            ylab =  "right"))
# i am Using a QQ plot to check for normality qqnorm function plots your sample against a normal distribution
with(df, {
  qqnorm(risk[eye == "left"], 
         main = "Left eye infected chances")
})
# We can add normailty line to the plot to evaluate normality for active period.
with(df, {
  qqnorm(risk[eye == "right"], 
         main = "right eye infected chances")
  qqline(risk[eye == "right"])
})

with(df, {
  qqnorm(risk[eye == "left"], 
         main = "left eye infected chances")
  qqline(risk[eye == "left"])
})

#test: normality test is used to check that both groups have disturbed data or not.
normality_test <- shapiro.test(df$risk)
normality_test$p.value

# from above test the p value is 2.2e-16
# H0= both eyes have same infected chances 
# H1= only pirticular eye has more infected chances 
    
install.packages("pwr")
library(pwr)
power_changes <- pwr.p.test(h = ES.h(p1 = 0.75, p2 = 0.50),
                            sig.level = 0.05,
                            power = 0.80)
plot(power_changes)

effect_size <- pwr.2p.test(h = ES.h(p1 = 0.10, p2 = 0.05), sig.level = 0.05,
                           power = .80)
plot(effect_size)
nrow(df)
# form the test it is proved the case study is rejecting the null hipothes 
# the v=77815 and p-value < 2.2e-16 so our case study is rejecting the null hypothies 
# so bothe eyes have saame risk level by the diabetics from our tests.
# hence it is proved 

wilcox.test(df$risk, data = df)
