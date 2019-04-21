
############### Function: 'count_supporters'
## count_supporters takes a vector of legislator scores (could be conservative/progressive or something else). 
## It then counts how many of the legislators support the bill
## (i.e. a legislator is counted if his/her score lies within the radius of the bill's score)

count_votes <- function(Legislator_Scores, Bill_Score, radius){
  votes = 0
  for (i in Legislator_Scores) {
    ## Set radius = 0 if you want to count votes for any legislation below the legislator's score
    if (radius == 0 && Bill_Score < i){votes = votes + 1}
    ## Otherwise only bills within the given radius will be voted for
    else if (abs(i - Bill_Score) < radius){votes = votes + 1}}
  return(votes)
}

############### Function: 'simulate_voting' accepts a score and a radius. 
## Randomly generates a legislative body with scores according to a normal mixture. 
## Simulates voting by that body on a bill with the given score and tallies 
## how often the bill passes. 

simulate_voting <- function(score, radius, number_bills){
  number_bills_passed = 0
  for (i in 1:number_bills){
    ## Simulate changes in attendance. If desired, fix as a constant. 
    body_size = rbinom(1, 100, 0.97)
    
    ## Setting up a normal mixture to sample from
    components = sample(1:2, prob=c(0.5, 0.5), size=body_size, replace=TRUE)
    mus = c(0.25, 0.75)
    sds = c(0.1, 0.1)
    
    ## Defining politician scores according to the normal mixture distribution.
    ## For a different distribution, delete the preceding three lines and replace
    ## rnorm with the relevant function and parameters. 
    legislator_scores = rnorm(n=body_size, mean=mus[components],sd=sds[components])
    
    ## If more than a simple majority is needed, replace body_size/2 with the desired amount.
    if (count_votes(legislator_scores, score, radius) > body_size/2){number_bills_passed = number_bills_passed + 1}
  }
  return(number_bills_passed/number_bills)
}


############### Function: 'tally_results' accepts several parameters re: the legislative body
## For every score desired, it records how many bills pass at that score.
## The results are presented in a scatterplot. 
tally_results <- function(min_score, max_score, radius, number_bills, granularity){
  
  distribution = c() #vector containing number of bills that passed at each score
  score_list = seq(min_score, max_score, (max_score - min_score)/granularity)
  
  for (i in score_list){
    distribution = c(distribution, simulate_voting(i, radius, number_bills))
  }
  plot(score_list, distribution, main = "Distribution of Pass Rates", ylab = "Proportion of Bills Passed", xlab = "Bill Score")
  
}

############### Run program
tally_results(min_score = 0, max_score = 1, radius = 0.27, number_bills = 100, granularity = 100)

