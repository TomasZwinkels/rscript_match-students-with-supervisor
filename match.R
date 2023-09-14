
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
			
			# now let's make some a loop that writes in the actual values
			for(i in 1:totalnrsupervisors)	
				{
				# when they are the 1st choice
				col_name_0 <- paste("stud_supervis_prefer_0_",i,"_RANK", sep = "") # for we dynmically define the name of the variable the info should come from
				actweights[which(SPRF[[col_name_0]] > 0),i] <- 0 # we get a vector with the index of all rows that need to be updated and write this to the supervisor specific column
			
				# when they are the 2nd choice
				col_name_1 <- paste("stud_supervis_prefer_1_",i,"_RANK", sep = "")
				actweights[which(SPRF[[col_name_1]] > 0),i] <- 2
				
				# when they are the 3th choice
				col_name_2 <- paste("stud_supervis_prefer_2_",i,"_RANK", sep = "")
				actweights[which(SPRF[[col_name_2]] > 0),i] <- 3
				
				# when they are the 4th choice
				col_name_3 <- paste("stud_supervis_prefer_3_",i,"_RANK", sep = "")
				actweights[which(SPRF[[col_name_3]] > 0),i] <- 4
				print(i)
				}
			
		# Create the model
		
			#  the MIP model is optimizing the assignment of students to supervisors in such a way that:

				# Each student is assigned to exactly one supervisor.
				# Each supervisor does not oversee more than a certain number of students (n_slots_per_supervisor).
				# The total "badness" or unsuitability of all assignments is minimized, based on your actweights matrix.
				# So, by solving this revised MIP model, we will find the most optimal way, under the given constraints, 
				# to match students with supervisors so as to minimize the overall "badness" of the matches, as represented by the higher values in the actweights matrix.
		
		model <- MIPModel() %>%
		  add_variable(x[i, j], i = 1:n_students, j = 1:n_supervisors, type = "binary") %>%
		  set_objective(sum_expr(weights[i, j] * x[i, j], i = 1:n_students, j = 1:n_supervisors), "min") %>%
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
		colnames(assignment_matrix) <- LETTERS[1:totalnrsupervisors]
		rownames(assignment_matrix) <- 1:totalnrstudents
		assignment_matrix	