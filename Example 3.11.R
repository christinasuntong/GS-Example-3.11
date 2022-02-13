### Example 3.11
memberNames <- "Yujia Wang, Tong Sun, Ruining Jia"
## Plot the figure
# For n = 100 and select values of m, the function alpha(p), for p ranging from 0.4 to 1.
n <- 100
p <- seq(0.4, 1, by=0.01)
m <- c(69, 73)

for(i in m){
  if(i==69){
    df1 <- data.frame(amount=paste('m=69'), p, dis=(1-pbinom(i,n,p)))
  }
  else if(i==73){
    df2 <- data.frame(amount=paste('m=73'), p, dis=(1-pbinom(i,n,p)))
  }
}
df <- rbind(df1, df2)
# We define the probability of new drug and old one are 0.8 and 0.6 respectively, and then define the bottom and top lines of the box represent the 5% type I error and (1-95%) type II error respectively.
library(ggplot2)
ggplot()+
  geom_line(aes(x=p, y=dis, color=amount),data=df)+
  geom_rect(aes(xmin=0.6, xmax=0.8, ymin=0.05, ymax=0.95), fill='purple', alpha=0.1)+
  labs(x='real number between 0 and 1', y='cdf of the binomial distribution', title='The power curve')
# From the plot, it is steep enough to cross with the bottom and top lines of the box at the same time, then we change m (number of cured people) to obtain a range.
# The pink curve here represents the smallest integer of m = 69 that starts to cross with the bottom of the box -- 5% type I error.
# The blue curve indicates the largest integer of m = 73 that on the boundary of the box area.
# And between 69 and 73 are suitable to choose.

## Using pwr package -- pwr.p.test(one-sample proportion test)
# Here the question is : What is the power of our test if we choose 100 samples and lower our type I error tolerance to 0.05.
library(pwr)
p.out.1 <- pwr.p.test(h=ES.h(p1 = 0.8, p2 = 0.6), n=100, sig.level=0.05, alternative="greater")
p.out.1
# The power of our test is about 99.7%. We specified 'alternative="greater"' since we assumed the patients were cured with larger probabilities.
plot(p.out)
# This time, sample size will make the more conservative "two-sided" assumption with leaving "alternative" argument out of the function.
p.out.2 <- pwr.p.test(h=ES.h(p1 = 0.8, p2 = 0.6), n=100, sig.level=0.05)
p.out.2
# The power of this test is 99.3%.
