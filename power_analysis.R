library(pwr)
# power analysis


# H0: the coin is fair and lands heads 50% of the time (?? = 0.50).
# Ha: the coin is loaded to land heads more then 50% of the time (?? > 0.50).
# How many times should we flip the coin to have a high probability (or power), say 0.80, of correctly rejecting the null of ?? = 0.5 if our coin is indeed loaded to land heads 75% of the time?

pwr.p.test(h = ES.h(p1 = 0.75, p2 = 0.50), 
           sig.level = 0.05, 
           power = 0.80, 
           alternative = "greater")


#package build-in efffec size at different effect levels

cohen.ES(test = "r", size = "medium")

pwr.p.test(h = c(0.2,0.5,0.8),
           n = 20,
           sig.level = 0.05)

#two-sample test for proportions
# randomly sample male and female college undergraduate students and ask them if they consume alcohol at least once a week.
# H0: no difference in the proportion that answer yes.
# Ha: there is a difference. 
# This is a two-sided alternative; one gender has higher proportion but we don't know which. We would like to detect a difference as small as 5%. How many students do we need to sample in each group if we want 80% power and a significance level of 0.05?

pwr.2p.test(h = ES.h(p1 = 0.55, p2 = 0.50), sig.level = 0.05, power = .80)


#two-sample test for proportions, unequal sample sizes

#The power of our test if we're interested in being able to detect a "small" effect size with 0.05 significance is about 93%.

cohen.ES(test = "p", size = "small")
pwr.2p2n.test(h = 0.2, n1 = 543, n2 = 675, sig.level = 0.05)

pwr.2p2n.test(h = 0.2, n1 = 763, power = 0.8, sig.level = 0.05)

##one-sample and two-sample t tests for means


#  test if there is a difference in the mean price of what male and female students pay at a library coffee shop. Let's say we randomly observe 30 male and 30 female students check out from the coffee shop and calculate the mean purchase price for each gender. 

#test for a difference in means using a two-sample t-test. How powerful is this experiment if we want to detect a "medium" effect in either direction with a significance level of 0.05?

cohen.ES(test = "t", size = "medium")
pwr.t.test(n = 30, d = 0.5, sig.level = 0.05)

pwr.t.test(d = 0.5, power = 0.80, sig.level = 0.05)


#how to use mean of each group and guessed sd of population to calculate effect size d
#sd_guess = (max-min)/4
#d=(m1-m2)/sd_guess

sd_guess <-  (10-1)/4
d <- 0.75/sd_guess
pwr.t.test(d = d, power = 0.80, sig.level = 0.05)

power.t.test(delta = 0.75, sd = 2.25, sig.level = 0.05, power = 0.8)


# one-sample t-tests
# we think the average purchase price at the Library coffee shop is over $3 per student. Our null is $3 or less; our alternative is greater than $3. We can use a one-sample t-test to investigate this hunch. If the true average purchase price is $3.50, we would like to have 90% power to declare the estimated average purchase price is greater than $3.

#How many transactions do we need to observe assuming a significance level of 0.05? Let's say the maximum purchase price is $10 and the minimum is $1. So our guess at a standard deviation is 9/4 = 2.25.
d <- 0.50/2.25
pwr.t.test(d = d, sig.level = 0.05, power = 0.90, alternative = "greater", 
           type = "one.sample")



#"Paired" t-tests are basically the same as one-sample t-tests, except our one sample is usually differences in pairs. The following example should make this clear.

#(From Hogg & Tanis, exercise 6.5-12) 24 high school boys are put on a ultra-heavy rope-jumping program. Does this decrease their 40-yard dash time (i.e., make them faster)? We'll measure their 40 time in seconds before the program and after. We'll use a paired t-test to see if the difference in times is greater than 0 (before - after). Assume the standard deviation of the differences will be about 0.25 seconds. How powerful is the test to detect a difference of about 0.08 seconds with 0.05 significance?

pwr.t.test(n = 24, d = 0.08 / 0.25, 
           type = "paired", alternative = "greater")

pwr.t.test(d = 0.08 / 0.25, power = 0.8,
           type = "paired", alternative = "greater")


# For paired t-tests we sometimes estimate a standard deviation for within pairs instead of for the difference in pairs. In our example, this would mean an estimated standard deviation for each boy's 40-yard dash times. When dealing with this type of estimated standard deviation we need to multiply it by 2-??? in the pwr.t.test function. Let's say we estimate the standard deviation of each boy's 40-yard dash time to be about 0.10 seconds. The sample size needed to detect a difference of 0.08 seconds is now calculated as follows:
  
pwr.t.test(d = 0.08 / (0.1 * sqrt(2)), power = 0.8, 
             type = "paired", alternative = "greater")



#Goodness of fit test

#A market researcher is seeking to determine preference among 4 package designs. He arranges to have a panel of 100 consumers rate their favorite package design. He wants to perform a chi-square goodness of fit test against the null of equal preference (25% for each design) with a significance level of 0.05. What's the power of the test if 3/8 of the population actually prefers one of the designs and the remaining 5/8 are split over the other 3 designs?
null <- rep(0.25, 4)
alt <- c(3/8, rep((5/8)/3, 3))
ES.w1(null,alt)

pwr.chisq.test(w=ES.w1(null,alt), N=100, df=(4-1), sig.level=0.05)

pwr.chisq.test(w=ES.w1(null,alt), df=(4-1), power=0.8, sig.level = 0.05)


#test of association
# if there's an association between gender and flossing teeth among college students. 
prob <- matrix(c(0.1,0.2,0.4,0.3), ncol=2, 
               dimnames = list(c("M","F"),c("Floss","No Floss")))
prob
ES.w2(prob)
pwr.chisq.test(w = ES.w2(prob), N = 100, df = 1, sig.level = 0.01)


#correlation test
#A graduate student is investigating the effectiveness of a fitness program. She wants to see if there is a correlation between the weight of a participant at the beginning of the program and the participant's weight change after 6 months. She suspects there is a "small" positive linear relationship between these two quantities. She will measure this relationship with correlation, r, and conduct a correlation test to determine if the estimated correlation is statistically greater than 0. How many subjects does she need to sample to detect this small positive (i.e., r > 0) relationship with 80% power and 0.01 significance level?

cohen.ES(test = "r", size = "small")
pwr.r.test(r = 0.1, sig.level = 0.01, power = 0.8, alternative = "greater")
pwr.r.test(r = 0.1, sig.level = 0.01, power = 0.8)











LI<- 0.2
csd<- LI/4
del<- 0.05

pwr.t.test(d=del/csd,
           sig.level=0.05,
           power=0.9,
           alternative ='greater',
           type= 'one.sample')
