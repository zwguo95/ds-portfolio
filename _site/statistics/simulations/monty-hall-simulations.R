# The winning probability of sticking to the initial choice
set.seed(123)
doors <- c(1,2,3)

count <- 0
for(i in 1:10000) {
  car <- sample(doors, 1)
  initial_choice <- 1
  if(initial_choice == car){
    count <- count + 1
  }
}

p_stick <- count/10000
p_stick


# The winning probability of switching to another choice 
set.seed(123)
reveal <- function(doors, car, initial_choice) {
  if(car == initial_choice){
    reveal <- sample(doors[-c(car,initial_choice)], 1)
  } else {
    reveal <- doors[-c(car, initial_choice)]
  }
}

count <- 0
for (i in 1:10000) {
  car <- sample(doors,1)
  initial_choice <- 1
  revealed_door <- reveal(doors, car, initial_choice)
  final_choice <- doors[-c(initial_choice, revealed_door)]
  if(final_choice == car){
    count = count + 1
  }
}

p_switch <- count/10000
p_switch

# compare these probabilities
p_switch > p_stick
