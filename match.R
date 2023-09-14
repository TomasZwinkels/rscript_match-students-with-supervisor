
# packages e.t.c.
	# install.packages(c("ompr", "ompr.roi", "ROI.plugin.glpk","tidyverse"))
	library(openxlsx)
	library(ompr)
	library(ompr.roi)
	library(ROI.plugin.glpk)
	library(tidyverse)

# set working directory
	setwd("C:/Users/zwinkels/Dropbox/Tilburg Teaching/master thesis coordination/rscript_match-students-with-supervisor")
	getwd()

# import the datafile with supervisor preferences
	QRAW <- read.xlsx("qualtrics_export.xlsx", sheet = 1)
	head(QRAW)
	
# filter 
	
	# data line 2 is stupid
	QRAW <- QRAW[-1,]
	head(QRAW)
	
	SPRF <- QRAW

# some setting stuff

	totalnrstudents <- nrow(SPRF)
	totalnrstudents
	
	totalnrsupervisors <- 9
	totalnrsupervisors
	
	n_slots_per_supervisor <- 3
	n_slots_per_supervisor
	
# OK, lets get some descriptives first




	# per supervisor, distribution of the rankings
		# from the qualtrics documentation: https://www.qualtrics.com/support/survey-platform/survey-module/editing-questions/question-types-guide/specialty-questions/pick-group-and-rank/
			# The downloaded dataset will include 2 columns for each item participants were asked to group
			# The first column will indicate which group the participant placed the item in (labeled numerically, with “0” being the first group, “1” being the second, and so forth).
			# The second column will indicate what rank this item received within the group it was placed in.
		# CONCLUSION: we only need the first column!
		
	
	# overview of first choices per supervisor
	table(SPRF$stud_supervis_prefer_0_1_RANK) 
	table(SPRF$stud_supervis_prefer_0_2_RANK) 
	table(SPRF$stud_supervis_prefer_0_3_RANK) 
	table(SPRF$stud_supervis_prefer_0_4_RANK) 
	table(SPRF$stud_supervis_prefer_0_5_RANK) 
	table(SPRF$stud_supervis_prefer_0_6_RANK) 
	table(SPRF$stud_supervis_prefer_0_7_RANK) 
	table(SPRF$stud_supervis_prefer_0_8_RANK) 
	table(SPRF$stud_supervis_prefer_0_9_RANK) 
	
	# overview of 2nd choices per supervisor
	table(SPRF$stud_supervis_prefer_1_1_RANK) 
	table(SPRF$stud_supervis_prefer_1_2_RANK) 
	table(SPRF$stud_supervis_prefer_1_3_RANK) 
	table(SPRF$stud_supervis_prefer_1_4_RANK) 
	table(SPRF$stud_supervis_prefer_1_5_RANK) 
	table(SPRF$stud_supervis_prefer_1_6_RANK) 
	table(SPRF$stud_supervis_prefer_1_7_RANK) 
	table(SPRF$stud_supervis_prefer_1_8_RANK) 
	table(SPRF$stud_supervis_prefer_1_9_RANK) 
	
	# overview of 3th choices per supervisor
	table(SPRF$stud_supervis_prefer_2_1_RANK) 
	table(SPRF$stud_supervis_prefer_2_2_RANK) 
	table(SPRF$stud_supervis_prefer_2_3_RANK) 
	table(SPRF$stud_supervis_prefer_2_4_RANK) 
	table(SPRF$stud_supervis_prefer_2_5_RANK) 
	table(SPRF$stud_supervis_prefer_2_6_RANK) 
	table(SPRF$stud_supervis_prefer_2_7_RANK) 
	table(SPRF$stud_supervis_prefer_2_8_RANK) 
	table(SPRF$stud_supervis_prefer_2_9_RANK) 
	
	# overview of 4th choices per supervisor
	table(SPRF$stud_supervis_prefer_3_1_RANK) 
	table(SPRF$stud_supervis_prefer_3_2_RANK) 
	table(SPRF$stud_supervis_prefer_3_3_RANK) 
	table(SPRF$stud_supervis_prefer_3_4_RANK) 
	table(SPRF$stud_supervis_prefer_3_5_RANK) 
	table(SPRF$stud_supervis_prefer_3_6_RANK) 
	table(SPRF$stud_supervis_prefer_3_7_RANK) 
	table(SPRF$stud_supervis_prefer_3_8_RANK) 
	table(SPRF$stud_supervis_prefer_3_9_RANK) 
	
	# OK, some cleaning is required here!
	
	# for one case
		SPRF$stud_supervis_prefer_0_1_RANK[SPRF$stud_supervis_prefer_0_1_RANK == ""] <- "0"
		SPRF$stud_supervis_prefer_0_1_RANK <- as.numeric(SPRF$stud_supervis_prefer_0_1_RANK)

	# and for all the cases in a loop
		# Define the preference numbers and ranks
			pref_nums <- c("0", "1", "2", "3")
			ranks <- c("1", "2", "3", "4", "5", "6", "7", "8", "9")

			# Loop through each combination of preference number and rank
			for (pref_num in pref_nums) {
			  for (rank in ranks) {
				# Construct the column name
				col_name <- paste("stud_supervis_prefer_", pref_num, "_", rank, "_RANK", sep = "")
				
				# Replace empty strings with "0"
				SPRF[[col_name]][SPRF[[col_name]] == ""] <- "0"
				
				# Convert to numeric
				SPRF[[col_name]] <- as.numeric(SPRF[[col_name]])
				
				# Optionally, you could also print the table for each column like you were doing before
				print(table(SPRF[[col_name]]))
			  }
			}

# OK, now for interesting part, how to go about and do this.


	# latest chatGTP suggestion

		# Define the number of students, supervisors and their preferences
		n_students <- totalnrstudents
		n_supervisors <- totalnrsupervisors
		n_slots_per_supervisor <- 3

		# Let's say weights are from 4 to 1 for choices from 1st to 4th.
		# This should actually come from your dataset
		
			# generated weights
			weights <- matrix(runif(n_students * n_supervisors, 1, 4), nrow = n_students)
			colnames(weights) <- LETTERS[1:totalnrsupervisors]
			rownames(weights) <- 1:totalnrstudents
			weights
			
			# actual weights
			actweights <- matrix(0, nrow = n_students, ncol = n_supervisors)
			
			# default is a supervisor your did not even mention
			actweights[,] <- 6
			
			# now let's make some loops that write in the actual values
			
			# for the first supervisor, lets get the index of the value(s) that need to be overwritten -- I say values because two people might be first choice(!)
			
				# when they are the 1st choice
				actweights[which(SPRF$stud_supervis_prefer_0_1_RANK > 0),1] <- 0
			
				# when they are the 2nd choice
				actweights[which(SPRF$stud_supervis_prefer_1_1_RANK > 0),1] <- 2 # please note that this is said to two on purpose, as not getting your first choice hurts more than getting your 3rd instead of 2nd choice
				
				# when they are the 3th choice
				actweights[which(SPRF$stud_supervis_prefer_2_1_RANK > 0),1] <- 3
				
				# when they are the 4th choice
				actweights[which(SPRF$stud_supervis_prefer_3_1_RANK > 0),1] <- 4
			
				actweights # lets do a manual check # yes this looks good.
			
		# Create the model
		model <- MIPModel() %>%
		  add_variable(x[i, j], i = 1:n_students, j = 1:n_supervisors, type = "binary") %>%
		  set_objective(sum_expr(weights[i, j] * x[i, j], i = 1:n_students, j = 1:n_supervisors), "max") %>%
		  add_constraint(sum_expr(x[i, j], j = 1:n_supervisors) == 1, i = 1:n_students) %>%
		  add_constraint(sum_expr(x[i, j], i = 1:n_students) <= n_slots_per_supervisor, j = 1:n_supervisors)

		# Solve the model
		result <- solve_model(model, with_ROI(solver = "glpk", verbose = TRUE))

		# Output results
		result$solution
		
		# Given result$solution as the named vector
		solution_vector <- result$solution	

	# Initialize an empty matrix with dimensions [n_students, n_supervisors]
		assignment_matrix <- matrix(0, nrow = n_students, ncol = n_supervisors)

		# Populate the matrix
		for (name in names(solution_vector)) {
		  # Extract student and supervisor indices from the name (e.g., 'x[1,2]')
		  indices <- as.integer(unlist(strsplit(gsub("x\\[|\\]", "", name), split = ",")))
		  i <- indices[1]
		  j <- indices[2]
		  
		  # Set the value in the matrix
		  assignment_matrix[i, j] <- solution_vector[name]
		}

		assignment_matrix

		# Now, assignment_matrix should be a 25x9 matrix with the assignments.



















	# step 1: randomise the student submissions to avoid any first come first serve effects

	# step 2: make a student preference matrix
	
		# initialze an empty matrix
			


		# basic algoritm is: start with a random student, give this student their 1st of possible still, otherwise -- / 2nd / 3rd/ 4th.. choice if possible

# Sample student preference matrix (rows: students, columns: supervisors)
# Here, students = 10, supervisors = 3 for demonstration. 1 indicates a preference.
student_pref <- matrix(c(
  1, 0, 1,
  1, 1, 0,
  0, 1, 1,
  1, 0, 0,
  0, 1, 0,
  0, 0, 1,
  1, 1, 1,
  0, 0, 0,
  1, 1, 1,
  0, 1, 1
), nrow = 10, byrow = TRUE)

# Initialize matching matrix with zeros
n_students <- nrow(student_pref)
n_supervisors <- ncol(student_pref)
matching_matrix <- matrix(0, nrow = n_students, ncol = n_supervisors)




# Initialize supervisor capacity (here, 3 for each supervisor)
supervisor_capacity <- rep(3, n_supervisors)

# Run the algorithm
for (student in 1:n_students) {
  for (supervisor in 1:n_supervisors) {
    if (student_pref[student, supervisor] == 1) { # If the student prefers this supervisor
      if (sum(matching_matrix[, supervisor]) < supervisor_capacity[supervisor]) { # If supervisor has capacity
        matching_matrix[student, supervisor] <- 1 # Assign student to supervisor
        break # Exit the loop for this student
      }
    }
  }
}

# Print the final matching matrix
print(matching_matrix)



# other option that does not quite seem to work.

		# step 1: define the universe of all possible matches
		
			# let's generate all possible combination
			
			# first, one combination
			# Initialize the matrix with zeros
			matching_matrix <- matrix(0, nrow = totalnrstudents, ncol = totalnrsupervisors)
			colnames(matching_matrix) <- LETTERS[1:totalnrsupervisors]
			rownames(matching_matrix) <- 1:totalnrstudents
			matching_matrix
		
		# step 2: calculate the 'pain' score for each possible universe
		
		# step 3: pick the universe with lowest pain score
		

# SOLID start!! -- because now, all possible matrices are all possible orders of this list.
		
	# Create an empty list to store the results
	result_list <- list()

	# Generate all possible combinations of 3 indices out of 25
	combinations <- combn(1:25, 3)

	# Number of combinations generated
	num_combinations <- ncol(combinations)

	# Iterate through all the combinations
	for (i in 1:num_combinations) {
	  # Generate a zero vector of length 25
	  vec <- rep(0, 25)
	  
	  # Set the indices to 1 for each combination
	  vec[combinations[, i]] <- 1
	  
	  # Store the result in the list
	  result_list[[i]] <- vec
	}

	# Convert the list to a matrix for easier viewing
	result_matrix <- do.call(rbind, result_list)

	# Show first 10 rows of the result
	head(result_matrix, 10)

# om result_matrix I would like to get a selection of rows that meet the condition that each columnsum = 1

find_combinations <- function(matrix, current_rows = list()) {
    # If matrix is empty or has no columns left, return the current combination
    if (ncol(matrix) == 0) {
        return(list(current_rows))
    }
  
    # If matrix is not empty but has no rows left, return NULL (no combination found)
    if (nrow(matrix) == 0) {
        return(NULL)
    }
  
    # Check the first row
    row <- matrix[1, , drop = FALSE]
    remaining_matrix <- matrix[-1, , drop = FALSE]  # Exclude the current row for next iteration
  
    # Check which columns this row covers
    covered_columns <- which(row == 1)
  
    # If adding the current row to the current combination maintains the requirement, 
    # proceed with the reduced matrix
    if (all(sapply(current_rows, function(r) sum(r[covered_columns]) == 0))) {
        combined_rows <- c(current_rows, list(row))
        reduced_matrix <- remaining_matrix[, -covered_columns, drop = FALSE]
        with_current_row <- find_combinations(reduced_matrix, combined_rows)
    } else {
        with_current_row <- NULL
    }
  
    # Also consider the possibility without including the current row
    without_current_row <- find_combinations(remaining_matrix, current_rows)
  
    # Combine and return the results
    return(c(with_current_row, without_current_row))
}

# Call the function on result_matrix
combinations <- find_combinations(result_matrix)

# Filter out NULLs and get the row indices from result_matrix for each combination
combinations <- Filter(Negate(is.null), combinations)
combination_indices <- lapply(combinations, function(comb) {
    sapply(comb, function(row) which(apply(result_matrix, 1, function(r) all(r == row))))
})

# Displaying the first 10 combinations of row indices
head(combination_indices, 10)

