
#For each snippet:
  
#  Change the parameters as needed for each distribution.
#sample_size and num_samples are set to illustrate the effect; 
#you may adjust these to see how the distribution of sample means 
#changes with different sample sizes or numbers of samples.
#Each snippet concludes with a plot showing the distribution of sample means. 
#Despite the original distributions being non-normal, 
#these plots will demonstrate that the distribution of the sample means 
#approaches a normal distribution, thereby illustrating the Central Limit Theorem.



#To demonstrate the Central Limit Theorem (CLT) with different distributions in R, 
#let's proceed with examples using five different distributions: 
#Uniform, Exponential, Binomial, Poisson, and Chi-Squared.
#For each distribution, I'll outline an R script that generates multiple samples, 
#calculates their means, and then plots the distribution of 
#these sample means to illustrate how it approximates a normal distribution 
#as the sample size increases. 
#This process vividly demonstrates the CLT in action.

#Step 1: Setting up the Environment
#First, ensure you have R installed on your computer. Open RStudio or any R environment you prefer to run the following scripts.




#Example 1: Uniform Distribution
#Uniform distribution is where each value within a certain range 
#has an equal probability of occurrence.


# Load ggplot2 for plotting
library(ggplot2)

# Generate sample means
set.seed(42) # Ensure reproducibility
sample_means <- replicate(30000, mean(runif(30, min=0, max=1)))

# Plot
ggplot(data.frame(Means = sample_means), aes(x = Means)) +
  geom_histogram(aes(y=..density..), bins=40, color="black", fill="skyblue") +
  geom_density(color="red", size=1) +
  labs(title="Sample Means of Uniform Distribution", x="Sample Mean", y="Density")



#Example 2: Exponential Distribution
#Exponential distribution is often used to 
#model the time between independent events that happen at a constant average rate.


sample_means <- replicate(10000, mean(rexp(30, rate=1)))

ggplot(data.frame(Means = sample_means), aes(x = Means)) +
  geom_histogram(aes(y=..density..), bins=40, color="black", fill="lightgreen") +
  geom_density(color="red", size=1) +
  labs(title="Sample Means of Exponential Distribution", x="Sample Mean", y="Density")



#Example 3: Binomial Distribution
#Binomial distribution describes the number of 
#successes in a fixed number of independent Bernoulli trials.


sample_means <- replicate(10000, mean(rbinom(30, size=10, prob=0.5)))

ggplot(data.frame(Means = sample_means), aes(x = Means)) +
  geom_histogram(aes(y=..density..), bins=40, color="black", fill="orange") +
  geom_density(color="red", size=1) +
  labs(title="Sample Means of Binomial Distribution", x="Sample Mean", y="Density")


#Example 4: Poisson Distribution
#Poisson distribution is used to model the number 
#of events in fixed intervals of time or space when 
#these events happen with a known constant mean rate 
#independently of the time since the last event.


sample_means <- replicate(10000, mean(rpois(30, lambda=3)))

ggplot(data.frame(Means = sample_means), aes(x = Means)) +
  geom_histogram(aes(y=..density..), bins=40, color="black", fill="purple") +
  geom_density(color="red", size=1) +
  labs(title="Sample Means of Poisson Distribution", x="Sample Mean", y="Density")

#Example 5: Chi-Squared Distribution
#Chi-Squared distribution is used in hypothesis 
#testing and is derived from the sum of the squares 
#of a set of normally distributed data.


sample_means <- replicate(10000, mean(rchisq(30, df=2)))

ggplot(data.frame(Means = sample_means), aes(x = Means)) +
  geom_histogram(aes(y=..density..), bins=40, color="black", fill="pink") +
  geom_density(color="red", size=1) +
  labs(title="Sample Means of Chi-Squared Distribution", x="Sample Mean", y="Density")
#Conclusion
#Running these scripts in R will produce histograms for 
#the sample means of different distributions, 
#showing how the distribution of these means 
#approximates a normal distribution, 
#illustrating the Central Limit Theorem's effect. 
#This phenomenon occurs regardless of the original distribution of the data, 
#provided the sample size is sufficiently large and the random variables are independent.











































































