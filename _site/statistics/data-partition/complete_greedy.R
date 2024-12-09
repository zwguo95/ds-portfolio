complete_greedy_approach <- function(data) {
  sorted_data <- data[order(-data$Case_Workers), ]
  S <- sorted_data$Case_Workers 
  
  # Helper function for depth-first-search
  dfs <- function(index, subset1_sum, subset2_sum, subset1_ids, subset2_ids) {
    
    # pruning rule: force the largest integer into subset1 at the root node
    if (index == 1) {
      subset1_sum <- subset1_sum + S[index]
      subset1_ids <- c(subset1_ids, sorted_data$agency_id[index])
      return(dfs(index + 1, subset1_sum, subset2_sum, subset1_ids, subset2_ids))
    }
    
    # base case: if all integers are assigned, return the maximum subset sum and subsets
    if (index > length(S)) {
      return(list(
        max_sum = max(subset1_sum, subset2_sum),
        subset1 = subset1_ids,
        subset2 = subset2_ids
      ))
    }
    
    # pruning rule: if remaining integers cannot close the gap, place all in the smaller subset
    remaining_sum <- sum(S[index:length(S)])
    if (remaining_sum < abs(subset1_sum - subset2_sum)) {
      if (subset1_sum < subset2_sum) {
        subset1_sum <- subset1_sum + remaining_sum
        subset1_ids <- c(subset1_ids, sorted_data$agency_id[index:length(S)])
      } else {
        subset2_sum <- subset2_sum + remaining_sum
        subset2_ids <- c(subset2_ids, sorted_data$agency_id[index:length(S)])
      }
      return(list(
        max_sum = max(subset1_sum, subset2_sum),
        subset1 = subset1_ids,
        subset2 = subset2_ids
      ))
    }

    # left branch: add current integer to subset1
    left_result <- dfs(
      index + 1,
      subset1_sum + S[index],
      subset2_sum,
      c(subset1_ids, sorted_data$agency_id[index]),
      subset2_ids
    )
    
    # right branch: add current integer to subset2
    right_result <- dfs(
      index + 1,
      subset1_sum,
      subset2_sum + S[index],
      subset1_ids,
      c(subset2_ids, sorted_data$agency_id[index])
    )
    
    if (left_result$max_sum < right_result$max_sum) {
      return(left_result)
    } else {
      return(right_result)
    }
  }
  
  result <- dfs(1, 0, 0, c(), c())
  
  treatment_df <- sorted_data[sorted_data$agency_id %in% result$subset1, ]
  control_df <- sorted_data[sorted_data$agency_id %in% result$subset2, ]
  
  return(list(
    Treatment = treatment_df,
    Control = control_df
  ))
}

