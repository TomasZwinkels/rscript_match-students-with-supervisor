# packages e.t.c.
	# install.packages(c("ompr", "ompr.roi", "ROI.plugin.glpk","tidyverse"))
	library(openxlsx)
	library(ompr)
	library(ompr.roi)
	library(ROI.plugin.glpk)
	library(tidyverse)
	library(sqldf)

# set working directory
	setwd("C:/Users/zwinkels/Dropbox/Tilburg Teaching/master thesis coordination/rscript_match-students-with-supervisor")
	getwd()

# import the datafile with supervisor preferences
	QRAW <- read.xlsx("qualtrics_export_20230918.xlsx", sheet = 1) # QRAW <- read.xlsx("qualtrics_export_20230918.xlsx", sheet = 1)
	head(QRAW)
	
	# filter 
	# data line 2 is stupid
	QRAW <- QRAW[-1,]
	head(QRAW)
	
	########### SELECT THE RELEVANT COHORT HERE
	nrow(QRAW)
	QRAW <- QRAW[which(QRAW$student_cohort == "coh_sep-2023"),]
	nrow(QRAW)	
	###########
	
	# quick fix for later
	QRAW$SNR <-  as.numeric(QRAW$SNR)
	
# import the datafile with supervisor info
	SUIN <- read.xlsx("sep2023_supervisorinfo.xlsx", sheet = 1)
	head(SUIN)
	
# we need to get rid of students that signed up with HWS as they need to sign up for a circle by email

	table(QRAW$what_master_track)
	nrow(QRAW)
	QRAW <- QRAW[which(!QRAW$what_master_track == "HWS - Master track: ‘Health, Wellbeing and Society’"),]
	nrow(QRAW)	
	
### this is where manual fixes can be done if there are entries that are messing things up, you can also edit the data on qualtrics and export again

	# the student on row 31 is a duplicate in this case, we look up the responseid (R_2qBH833O30RJqQ7) of this double entry and get rid of it.
	nrow(QRAW)
	QRAW <- QRAW[which(!QRAW$ResponseId == "R_2qBH833O30RJqQ7"),]
	nrow(QRAW)	
	
###
	
# CHECK: any students that signed up double?

	# checking for duplicates
		noduplicates_SNR <- function(DF) {
		  if (length(names(table(duplicated(DF$SNR)))) > 1) {
			return(FALSE)
		  } else {
			return(TRUE)
		  }
		}
		noduplicates_SNR(QRAW)
		QRAW[which(duplicated(QRAW$SNR)),]# return them if they are there
		
		QRAW$email <- tolower(QRAW$email)
		noduplicates_email <- function(DF) {
		  if (length(names(table(duplicated(DF$email)))) > 1) {
			return(FALSE)
		  } else {
			return(TRUE)
		  }
		}
		noduplicates_email(QRAW)
		QRAW[which(duplicated(QRAW$email)),]# return them if they are there
		
		noduplicates_full_name <- function(DF) {
		  if (length(names(table(duplicated(DF$full_name)))) > 1) {
			return(FALSE)
		  } else {
			return(TRUE)
		  }
		}
		noduplicates_full_name(QRAW)
		QRAW[which(duplicated(QRAW$full_name)),]# return them if they are there
	
# CHECK did all students sign for a circle? - if one or more students did not (and did not do an attempt later where they did)
# these students NEED TO BE CONTACTED BY EMAIL FIRST BEFORE YOU CONTINUE
	
	
		allstudentssignedup <- function(DF) {
		  if (length(names(table(DF$stud_supervis_prefer_0_GROUP == "" | is.na(DF$stud_supervis_prefer_0_GROUP)))) > 1) {
			return(FALSE)
		  } else {
			return(TRUE)
		  }
		}
		allstudentssignedup(QRAW)
		QRAW[which(QRAW$stud_supervis_prefer_0_GROUP == "" | is.na(QRAW$stud_supervis_prefer_0_GROUP)),] # return the problematic cases, if any
	
	# ONLY if all checks pass, the script can continue
	if(noduplicates_SNR(QRAW) & noduplicates_email(QRAW) & noduplicates_full_name(QRAW))
	{
	SPRF <- QRAW
	}
	nrow(SPRF)

# how many Extended master
	names(SPRF)[which(names(SPRF) == "Extended.master?")] <- "extended_master"

	table(SPRF$extended_master)
	
# SPEM: we fork a dataframe here for Students in the Extended master, they get assigned as a '4th' student later.
	
	# make the ones that will be manually assigned
	SPEM <- SPRF[which(SPRF$extended_master == "I am seriously considering applying for the extended master, or I have already applied."),]
	nrow(SPEM)
	
	# and filter the remaining
	SPRF <- SPRF[which(!SPRF$extended_master == "I am seriously considering applying for the extended master, or I have already applied."),]
	nrow(SPRF)
	
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
		
		# also, if some supervisors are allowed to have more than 3 students, specify this here! Otherwise set all to 3.
			n_slots_per_supervisor_vec <- c(3, 3, 3, 3, 3, 6, 4, 3, 3) # as long as n_supervisors
			sum(n_slots_per_supervisor_vec)
			nrow(SPRF)
			rbind(n_slots_per_supervisor_vec,LETTERS[1:totalnrsupervisors])

	# allright, now lets put this in a function where the input is: n_students, n_supervisors and n_slots_per_supervisor_vec and the output is the assignment_matrix for this group

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
				# for the concersation with chatGPT about this see: https://chat.openai.com/share/281459f6-d6dc-453c-885f-c98cd459f821

				# Each student is assigned to exactly one supervisor.
				# Each supervisor does not oversee more than a certain number of students (n_slots_per_supervisor_vec).
				# The total "badness" or unsuitability of all assignments is minimized, based on your actweights matrix.
				# So, by solving this revised MIP model, we will find the most optimal way, under the given constraints, 
				# to match students with supervisors so as to minimize the overall "badness" of the matches, as represented by the higher values in the actweights matrix.
		
		model <- MIPModel() %>%
		  add_variable(x[i, j], i = 1:n_students, j = 1:n_supervisors, type = "binary") %>%
		  set_objective(sum_expr(actweights[i, j] * x[i, j], i = 1:n_students, j = 1:n_supervisors), "min") %>%
		  add_constraint(sum_expr(x[i, j], j = 1:n_supervisors) == 1, i = 1:n_students) %>%
		  add_constraint(sum_expr(x[i, j], i = 1:n_students) <= n_slots_per_supervisor_vec[j], j = 1:n_supervisors) 

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
		colSums(assignment_matrix)
		sum(colSums(assignment_matrix))
		
	# manual assignment of extended master students
		SPEM <- subset(SPEM, select=c("full_name","email","SNR","what_master_track","extended_master","student_explanation"))
		
		names(SPEM)
		SPEM

		SPEM$my_assigned_supervisor <- NA
		
		SPEM$my_assigned_supervisor[which(SPEM$email == "R.j.barzegar@tilburguniversity.edu")] <- "C"
		SPEM$my_assigned_supervisor[which(SPEM$email == "g.g.greco@tilburguniversity.edu")] <- "E"
		SPEM$my_assigned_supervisor[which(SPEM$email == "r.s.p.hurl@tilburguniversity.edu")] <- "G"
		SPEM$my_assigned_supervisor[which(SPEM$email == "j.m.vdrMeer_1@tilburguniversity.edu")] <- "I"
		SPEM$my_assigned_supervisor[which(SPEM$email == "w.b.p.kwint@tilburguniversity.edu")] <- "B"
		SPEM$my_assigned_supervisor[which(SPEM$email == "t.j.p.vanaert@tilburguniversity.edu")] <- "A"
	
		SPEM$how_painfull <- NA

	# now let's get a dataframe I can export where the unit of analysis is the student and we add the details of their assigned supervisor
		head(SPRF)
		names(SPRF)
		
		# first, lets select the relevant base variables
        STEX <- subset(SPRF, select=c("full_name","email","SNR","what_master_track","extended_master","student_explanation"))
		
		# what supervisor was assigned to each student
		resvec <- NULL
		for(i in 1:totalnrstudents)
		{
		resvec[i] <- names(which(assignment_matrix[i,] == 1))
		}
		resvec
		STEX$my_assigned_supervisor <- resvec
		table(STEX$my_assigned_supervisor)
		
		# how 'painfull' was this decision for me?
		resvec2 <- NULL
		for(i in 1:totalnrstudents)
		{
		resvec2[i] <- actweights[i,which(assignment_matrix[i,] == 1)]
		}
		resvec2 
		length(resvec2)
		
		STEX$how_painfull <- resvec2
		table(STEX$how_painfull)
		
		names(STEX) == names(SPEM) # should be all TRUE

		EXPO <- rbind(STEX,SPEM)
		nrow(STEX)+nrow(SPEM)
		nrow(EXPO)
		
		EXPO
		
		# now, lets merge in some info on the supervisors
		nrow(EXPO)
		names(EXPO)
		
		EXPO <- sqldf("SELECT EXPO.*, SUIN.letter_in_sep2023surv, SUIN.ANR as 'supervisor_anr', SUIN.last_name as 'supervisor_last_name', SUIN.first_name as 'supervisor_first_name', SUIN.email as 'supervisor_email'
					  FROM EXPO LEFT JOIN SUIN
					  ON
					  EXPO.my_assigned_supervisor = SUIN.letter_in_sep2023surv
					")
		nrow(EXPO)
		EXPO
		
		# order by EXPO$my_assigned_supervisor
		EXPO <- EXPO[order(EXPO$my_assigned_supervisor), ]
		EXPO
		
		# export
		
			# Get the current date and time
			current_time <- Sys.time()
		
			# Format it into a string suitable for a file name
			time_str <- format(current_time, "%Y%m%d_%H%M%S")

			# EXPORT THE COMPLETE FILE
				# Create a file name with the timestamp
				file_name <- paste0("suggested_student-to-supervisor_assignments_", time_str, ".xlsx")
				write.xlsx(EXPO, file_name)
				
			# PLEASE NOTE: THIS PART ALWAYS NEEDS TO BE RUN MANUALLY 
			#
			#	OPTION 1: SELECT DATA FROM ABOVE
			#		MIRS <-  EXPO
			#	OPTION 2: SELECT AN EXCEL SHEET IN THE SAME FORMAT THAT WAS MANUALLY EDITED
			#		MIRS <- read.xlsx("suggested_student-to-supervisor_assignments_versiontobeimportedforexporttoMirsim.xlsx", sheet = 1) 
			#
			
			# because manual edits are error-prone, two data integity checks, of they fail, we manual BREAK the data
			
				# do all the supervisors always have the same ANR
					
					# Create a contingency table
						table_AB <- table(MIRS$supervisor_last_name, MIRS$supervisor_anr)
						if(!all(apply(table_AB, 1, max) == table(MIRS$supervisor_last_name)))
						{
							MIRS <- NA# Break Stuff.. It's just one of those days...
						} else {print("Passed")}
				
				# are all the SNR and Student name matches the same as in the origional data (and do the exact same people come back?).
				
					# first, same constistency check
					table_CD <- table(MIRS$full_name,MIRS$SNR)
						if(!all(apply(table_CD, 1, max) == table(MIRS$full_name)))
						{
							MIRS <- NA# Break Stuff.. It's just one of those days...
						} else {print("Passed")}
					
					# full match
						if(!all(table(EXPO$full_name,EXPO$SNR) == table(MIRS$full_name,MIRS$SNR)))
						{
							MIRS <- NA# Break Stuff.. It's just one of those days...
						} else {print("Passed")}
			
				# if you still have data here ('MIRS') all the checks above passed.
			
			# EXPORT THE FILES IN THE FORMAT MIRSIM	 (functioneel beheerder thesisdossier.uvt.nl) NEEDS them for import
				
				# SET the course codes
				PPSD_coursecode <- "441803-M-24"
				PPSinCP_coursecode <- "400991-M-24"
			
				# course codes for the correct filenames
				MIRS$coursecodes <- NA
				MIRS$coursecodes <- ifelse(grepl("GSMI", MIRS$what_master_track), PPSD_coursecode, NA)
				MIRS$coursecodes <- ifelse(grepl("PPSinCP", MIRS$what_master_track), PPSinCP_coursecode, MIRS$coursecodes)
				table(MIRS$coursecodes)
				nrow(MIRS) == sum(table(MIRS$coursecodes)) # should return TRUE
		
				# export PPSD
					EXPO_PPSD <- sqldf(paste0("SELECT MIRS.SNR, MIRS.supervisor_anr as 'ANR' FROM MIRS WHERE coursecodes = '",PPSD_coursecode,"'"))
					nrow(EXPO_PPSD)
					EXPO_PPSD
					
					file_name2 <- paste0(PPSD_coursecode,"_", time_str, ".xlsx")
					write.xlsx(EXPO_PPSD, file_name2)

				# export PPSinCP
					EXPO_PPSinCP <- sqldf(paste0("SELECT MIRS.SNR, MIRS.supervisor_anr as 'ANR' FROM MIRS WHERE coursecodes = '",PPSinCP_coursecode,"'"))
					nrow(EXPO_PPSinCP)
					EXPO_PPSinCP
					
					file_name2 <- paste0(PPSinCP_coursecode,"_", time_str, ".xlsx")
					write.xlsx(EXPO_PPSinCP, file_name2)
					
					