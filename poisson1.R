### Another Poisson Example
### Explanatory Notes: http://www.ats.ucla.edu/stat/r/dae/poissonreg.htm
library( sandwich )
library( msm )

p <- read.csv("http://www.ats.ucla.edu/stat/data/poisson_sim.csv")
p <- within(p, {
  prog <- factor(prog, levels=1:3, labels=c("General", "Academic", "Vocational"))
  id <- factor(id)
})
summary(p)

# We can use the tapply function to display the summary statistics by program type. 
# The table below shows the average numbers of awards by program type and seems to suggest 
# that program type is a good candidate for predicting the number of awards, our outcome variable, 
# because the mean value of the outcome appears to vary by prog. 
# Additionally, the means and variances within each level of prog--the conditional means and variances--are similar. 

with(p, tapply(num_awards, prog, function(x) {
  sprintf("M (SD) = %1.2f (%1.2f)", mean(x), sd(x))
}))

# A conditional histogram separated out by program type is plotted to show the distribution.
ggplot(p, aes(num_awards, fill = prog)) +
  geom_histogram(binwidth=.5, position="dodge")

# Poisson Regression
summary(m1 <- glm(num_awards ~ prog + math, family="poisson", data=p))

# Cameron and Trivedi (2009) recommended using robust standard errors for the parameter estimates to control for 
# mild violation of the distribution assumption that the variance equals the mean. 
# We use R package sandwich below to obtain the robust standard errors and calculated the p-values accordingly. 
# Together with the p-values, we have also calculated the 95% confidence interval using the parameter estimates 
# and their robust standard errors.

cov.m1 <- vcovHC(m1, type="HC0")
std.err <- sqrt(diag(cov.m1))
r.est <- cbind(Estimate= coef(m1), "Robust SE" = std.err,
               "Pr(>|z|)" = 2 * pnorm(abs(coef(m1)/std.err), lower.tail=FALSE),
               LL = coef(m1) - 1.96 * std.err,
               UL = coef(m1) + 1.96 * std.err)

r.est

# We can use the residual deviance to perform a goodness of fit test for the overall model. 
# The residual deviance is the difference between the deviance of the current model and the maximum deviance 
# of the ideal model where the predicted values are identical to the observed. 
# Therefore, if the residual difference is small enough, the goodness of fit test will not be significant, 
# indicating that the model fits the data. 
# We conclude that the model fits reasonably well because the goodness-of-fit chi-squared test is not statistically 
# significant. If the test had been statistically significant, it would indicate that the data do not fit the 
# model well. In that situation, we may try to determine if there are omitted predictor variables, 
# if our linearity assumption holds and/or if there is an issue of over-dispersion.

with(m1, cbind(res.deviance = deviance, df = df.residual,
               p = pchisq(deviance, df.residual, lower.tail=FALSE)))

# We can also test the overall effect of prog by comparing the deviance of the full model with 
# the deviance of the model excluding prog. The two degree-of-freedom chi-square test indicates that prog, 
# taken together, is a statistically significant predictor of num_awards.

## update m1 model dropping prog
m2 <- update(m1, . ~ . - prog)
## test model differences with chi square test
anova(m2, m1, test="Chisq")

# we might want to present the regression results as incident rate ratios and their standard errors, 
# together with the confidence interval. To compute the standard error for the incident rate ratios, 
# we will use the Delta method
s <- deltamethod(list(~ exp(x1), ~ exp(x2), ~ exp(x3), ~ exp(x4)), coef(m1), cov.m1)

## exponentiate old estimates dropping the p values
rexp.est <- exp(r.est[, -3])
## replace SEs with estimates for exponentiated coefficients
rexp.est[, "Robust SE"] <- s

rexp.est

# The output above indicates that the incident rate for prog = "Academic" is 2.96 times 
# the incident rate for the reference group (prog = "General"). 
# Likewise, the incident rate for prog = "Vocational" is 1.45 times the incident rate for the 
# reference group holding the other variables at constant. 
# The percent change in the incident rate of num_awards is by 7% for every unit increase in math

# We might want to look at the expected marginal means. 
# For example, what are the expected counts for each program type holding math score at its overall mean? 
# To answer this question, we can make use of the predict function. 
# First off, we will make a small data set to apply the predict function to it

(s1 <- data.frame(math = mean(p$math),
                  prog = factor(1:3, levels = 1:3, labels = levels(p$prog))))

predict(m1, s1, type="response", se.fit=TRUE)

# We can also graph the predicted number of events with the commands below. 
# The graph indicates that the most awards are predicted for those in the academic program (prog = 2), 
# especially if the student has a high math score. The lowest number of predicted awards is for those students
# in the general program (prog = 1). The graph overlays the lines of expected values onto the actual points, 
# although a small amount of random noise was added vertically to lessen overplotting.

## calculate and store predicted values
p$phat <- predict(m1, type="response")

## order by program and then by math
p <- p[with(p, order(prog, math)), ]

## create the plot
ggplot(p, aes(x = math, y = phat, colour = prog)) +
  geom_point(aes(y = num_awards), alpha=.5, position=position_jitter(h=.2)) +
  geom_line(size = 1) +
  labs(x = "Math Score", y = "Expected number of awards")