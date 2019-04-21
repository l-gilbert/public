
### Interpret scale as ranging from conservative to progressive

number_bills = 100
number_thresholds = 1000
body_size = 100
risk_thresholds = seq(0,1,1/number_thresholds)
distribution = c()

for (j in risk_thresholds){
  number_bills_passed = 0
  for (i in 1:number_bills){
    ## Choose distribution of Senator risk profiles here;
    ## parameters 1,1 recover the uniform distribution
    ## parameters 0.5, 0.5 give Jeffrey's prior
    
    ## for Binomial, use e.g. risk_tolerances = rbinom(100,100,0.5)/100
    ## for Pareto, run library(actuar)
    ## then try e.g. rpareto1(100, 1000, 25). Will need to adjust risk_thresholds above.
    ## for two triangle distributions, load library(triangle) and use e.g.
    
    ## risk_tolerances1 = rtriangle(50, 0, 0.25, 0.25)
    ## risk_tolerances2 = rtriangle(50, 0.75, 1, 0.75)
    ## risk_tolerances = c(risk_tolerances1, risk_tolerances2)
    risk_tolerances = rbeta(body_size,5,1)
    if (sum(risk_tolerances > j) > body_size/2){number_bills_passed = number_bills_passed + 1}
  }
  distribution = c(distribution, number_bills_passed)
}
plot(risk_thresholds, distribution, main = "Distribution of Pass Rates as Risk Changes", ylab = "Number of Bills Passed", xlab = "Perceived Risk of Bill")

