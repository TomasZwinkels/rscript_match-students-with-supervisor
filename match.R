# packages e.t.c.
	# install.packages(c("openxlsx","ompr", "ompr.roi", "ROI.plugin.glpk","tidyverse","sqldf","digest"))
	library(openxlsx)
	library(ompr)
	library(ompr.roi)
	library(ROI.plugin.glpk)
	library(tidyverse)
	library(sqldf)
	library(digest)


# set working directory
	setwd("C:/Users/zwinkels/Dropbox/Tilburg Teaching/master thesis coordination/rscript_match-students-with-supervisor")
	getwd()

# import the datafile with supervisor preferences
	QRAW <- read.xlsx("qualtrics_export_20250217.xlsx", sheet = 1) # QRAW <- read.xlsx("qualtrics_export_20230918.xlsx", sheet = 1)
	head(QRAW)
	
	# filter 
	# data line 2 is stupid
	QRAW <- QRAW[-1,]
	head(QRAW)
	
	########### SELECT THE RELEVANT COHORT HERE
		table(QRAW$student_cohort)
		currentcohort = "Coh:February 2025"
		nrow(QRAW)
		QRAW <- QRAW[which(QRAW$student_cohort == currentcohort),]
		nrow(QRAW)
	
	###########
	
	# quick fix for later
	QRAW$SNR <-  as.numeric(QRAW$SNR)
	
# import the datafile with supervisor info
	SUINRAW <- read.xlsx("feb2025_supervisorinfo.xlsx", sheet = 1)
	SUINRAW
	nrow(SUINRAW)
	
	SUIN <- SUINRAW[which(SUINRAW$group == "us"),]
	
	head(SUIN)
	nrow(SUIN)
	
# we need to get rid of students that signed up with HWS as they need to sign up for a circle by email

	table(QRAW$what_master_track)
	nrow(QRAW)
	QRAW <- QRAW[which(!QRAW$what_master_track == "HWS - Master track: ‘Health, Wellbeing and Society’"),]
	nrow(QRAW)	
	
### this is where manual fixes can be done if there are entries that are messing things up, you can also edit the data on qualtrics and export again

	# the student on row 62 is a duplicate in this case, we look up the responseid (R_2iP9BLwYB99a2S5) of this double entry and get rid of it.
	nrow(QRAW)

	# manual deletes go here

	# Reza Barzegar heeft mogelijk geen nieuwe circle nodig - volgens haar als hij zijn resit haalt? -- Reza heeft besloten aan een nieuwe circle te willen beginnen.
	# QRAW <- QRAW[!(QRAW$ResponseId == "R_8me08mfUVpjSDKN"),]

	# Yana  Elisabeth van den Berg submitted here preferences twice, we will only use the 2nd submission
	QRAW <- QRAW[!(QRAW$ResponseId == "R_2ZTWzeJkO8IlQRv"),]

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
		
		QRAW[which(QRAW$SNR == "2078052"),]# this student submitted here preferences twice, i'll drop the first occurence
		
		
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

# get rid of these that have been accepted into the extended master
	# how many Extended master
		names(QRAW)[which(names(QRAW) == "Extended.master?")] <- "extended_master"

		table(QRAW$extended_master)
		table(QRAW$extended_master)
		
		# for my overview
		
			# student that started the extended master half a year ago
			QRAW[which(QRAW$extended_master == "Yes, I applied for the extended master roughly half a year ago and am currently doing an internship."),]
			
			# students that recently got accepted into the extended Master
			QRAW[which(QRAW$extended_master == "Yes, I applied for the extended master recently."),]
			
		# and filter the remaining
		nrow(QRAW)
		QRAW <- QRAW[which(!QRAW$extended_master == "Yes, I applied for the extended master recently."),] # these people will 95% sure all be accepted and won't be assigned a thesis circle
		nrow(QRAW)
		
		# text for Other
		OTH <- QRAW[which(QRAW$extended_master == "Other (please specify)."),]
		nrow(OTH)
		OTH$Extended master_4_TEXT
		OTH$full_name
		OTH$email
		
		# this student came up, they want to just start their thesis again (I, Tomas checked by email).
		QRAW$extended_master[which(QRAW$ResponseId == "R_8me08mfUVpjSDKN")] <- "No, I did NOT apply for the extended master."
		
		## if any Other, break stuf!
		if("Other (please specify)." %in% names(table(QRAW$extended_master)))
		{
			QRAW <- NA
		}
		nrow(QRAW)
	
# CHECK did all the remaining students sign-up for a circle? - if one or more students did not (and did not do an attempt later where they did)
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
	if(noduplicates_SNR(QRAW) & noduplicates_email(QRAW) & noduplicates_full_name(QRAW) & allstudentssignedup(QRAW))
	{
	SPRF <- QRAW
	}
	nrow(SPRF)
	
## Get rid of all the people that where just worried about their MTP resits

#	table(SPRF$MTP_Resit)
#	nrow(SPRF)
#	SPRF <- SPRF[which(SPRF$MTP_Resit == "No, I am enrolling to start my Thesis for the first time"),]
#	nrow(SPRF)
	
# some setting stuff

	totalnrstudents <- nrow(SPRF)
	totalnrstudents
	
	totalnrsupervisors <- nrow(SUIN) #6
	totalnrsupervisors
	
	n_slots_per_supervisor <- 4
	n_slots_per_supervisor
	
# OK, now for interesting part, how to go about and do this.

		# Define the number of students, supervisors and their preferences
			n_students <- totalnrstudents
			n_supervisors <- totalnrsupervisors
		
		# also, if some supervisors are (only) allowed to have more or less than 4 students, specify this here! Otherwise set all to 4.
			n_slots_per_supervisor_vec <- rep(4, length.out = nrow(SUIN))
			length(n_slots_per_supervisor_vec)
			
			n_slots_per_supervisor_vec <- c(4,4,4,4,4,0) # manual version, needs to as long as n_supervisors
			length(n_slots_per_supervisor_vec)
			
			# some info here on what are (im)polpular supervisors - note: only work after actweights has been generated below (lower values indicated more popular)
				# note for next year, is one supervisor keeps on getting only few students, the optimal way to 'force' student onto that supervisor is by giving the relatively impopular supervisors less students in the list above.
				rbind(n_slots_per_supervisor_vec,LETTERS[1:totalnrsupervisors],unname(colSums(actweights)))
		
			# some inspections, do we have enough supervisors?
				# enough? - should return TRUE
				sum(n_slots_per_supervisor_vec) > nrow(SPRF)
				
				# how many supervisors do we minimally need? - be sure to round up
				ceiling(nrow(SPRF) / n_slots_per_supervisor)
				
	# allright, now lets put this in a function where the input is: n_students, n_supervisors and n_slots_per_supervisor_vec and the output is the assignment_matrix for this group

		# Let's say weights are from 4 to 1 for choices from 1st to 4th.
		# This should actually come from your dataset
			
			# actual weights
			actweights <- matrix(0, nrow = n_students, ncol = n_supervisors)
			
			# default is a supervisor your did not even mention
			actweights[,] <- 6
			
			# now let's make some a loop that writes in the actual values
			
			# NEW - do this with the circle desciptions in the column names? 
			
				# I can take these from SUIN
				colnames(actweights) <- c(SUIN$circle_description)
				
				# and lets make a dataframe with just the four columns that contain the given student preferences as text from SPRF
				PREF <- cbind(SPRF$stud_supervis_prefer_0_GROUP,SPRF$stud_supervis_prefer_1_GROUP,SPRF$stud_supervis_prefer_2_GROUP,SPRF$stud_supervis_prefer_3_GROUP)
				head(PREF)
			
					# OK, I can take these from SUIN, but they do really need to match exactly with the one used in the survey, so lets check
						# how often as a first choice
						a <- colnames(actweights) %in% names(table(PREF[,1]))
						b <- colnames(actweights) %in% names(table(PREF[,2]))
						c <- colnames(actweights) %in% names(table(PREF[,3]))
						d <- colnames(actweights) %in% names(table(PREF[,4]))
						
						CHECK <- rbind(a,b,c,d)
						colnames(CHECK) <- colnames(actweights)
						CHECK # OK, so all but Katya occur at least once, that one was indeed incorrect
						
						
						# if not each column in CHECK contains at least one TRUE, break the script!
						if (!all(apply(CHECK, 2, any)))
						{
							SPRF <- NULL
						}

			# for each row, check if this student mentioned the columnname, which is the supervisor as their preference and update the costs accordingly
			
				# rowloop
				for(i in 1:nrow(actweights))
					{
					# columnloop
					for(j in 1:ncol(actweights))	
						{
						# here, we use the supervisor specific values from colnames(actweights) and check if they occur as substring for the specific student row
						
						# when they are the 1st choice
						actweights[i,j] <- ifelse(grepl(colnames(actweights)[j],SPRF$stud_supervis_prefer_0_GROUP[i],fixed=TRUE),0,actweights[i,j])
						
						# when they are the 2nd choice
						actweights[i,j] <- ifelse(grepl(colnames(actweights)[j],SPRF$stud_supervis_prefer_1_GROUP[i],fixed=TRUE),2,actweights[i,j])
						
						# when they are the 3th choice
						actweights[i,j] <- ifelse(grepl(colnames(actweights)[j],SPRF$stud_supervis_prefer_2_GROUP[i],fixed=TRUE),3,actweights[i,j])
						
						# when they are the 4th choice
						actweights[i,j] <- ifelse(grepl(colnames(actweights)[j],SPRF$stud_supervis_prefer_3_GROUP[i],fixed=TRUE),4,actweights[i,j])
						}
					}
				actweights
			
	# and if you are an extended master student currently doing an intership: double the pain!
		indexvecofexmastu <- which(SPRF$extended_master == "Yes, I applied for the extended master roughly half a year ago and am currently doing an internship.")
		actweights[indexvecofexmastu,] <- actweights[indexvecofexmastu,] * 2
	
	# and if you would like to do a qualitative thesis, but the supervisor is quantitative or vice versa add pentalty 3 
		# (which makes the algorithm in different between getting a supervisor that is your 3th choice or getting your first choice with a missmatch), which means that:
			# - 2nd choice supervisor with match is preferd over first choice with mismatch
			# - random supervisor (pain=6) with a method match is prefered over a 4th choice supervisor with a mismatch (pain=7), but not a lot.
		
			# get the student quant qual prefs simplified
			SPRF$student_quant_qual <- as.character(SPRF$student_quant_qual)
			table(SPRF$student_quant_qual)
			SPRF$student_quant_qual[SPRF$student_quant_qual == "I am quite sure that I want to use quantitative methods in my master's thesis."] <- "quantitative"
			SPRF$student_quant_qual[SPRF$student_quant_qual == "I am quite sure that I want to use qualitative methods in my master's thesis."] <- "qualitative"
			SPRF$student_quant_qual[SPRF$student_quant_qual == "I am quite sure that I want to use both quantitative and qualitative methods (mixed methods) in my master's thesis or am very open to that option."] <- "mixed"
			SPRF$student_quant_qual[SPRF$student_quant_qual == "I am indifferent between quantitative and qualitative methodologies or do not have a strong preference."] <- "indifferent"
			table(SPRF$student_quant_qual)
			
			#  and the supervisor one
			table(SUIN$quant_qual_private)

				# Create the qualquantweights matrix of the same dimensions as actweights
				
					# note: chatGPT conversation about this bit of script here: https://chatgpt.com/share/67af05ce-f2dc-8003-8d57-52d94f6c8138
				
				qualquantweights <- matrix(0, nrow = n_students, ncol = n_supervisors)

				# Loop over each student and supervisor combination
				for (i in 1:n_students) {
				  for (j in 1:n_supervisors) {
					
					# Retrieve the student's and supervisor's method preferences
					student_pref <- SPRF$student_quant_qual[i]
					supervisor_pref <- SUIN$quant_qual_private[j]
					
					# For student "quantitative"
					if (student_pref == "quantitative" && supervisor_pref == "quantitative") {
					  qualquantweights[i, j] <- 0
					}
					if (student_pref == "quantitative" && supervisor_pref == "indifferent") {
					  qualquantweights[i, j] <- 0
					}
					if (student_pref == "quantitative" && supervisor_pref == "qualitative") {
					  qualquantweights[i, j] <- 3
					}
					
					# For student "qualitative"
					if (student_pref == "qualitative" && supervisor_pref == "qualitative") {
					  qualquantweights[i, j] <- 0
					}
					if (student_pref == "qualitative" && supervisor_pref == "indifferent") {
					  qualquantweights[i, j] <- 0
					}
					if (student_pref == "qualitative" && supervisor_pref == "quantitative") {
					  qualquantweights[i, j] <- 3
					}
					
					# For student "mixed"
					if (student_pref == "mixed" && supervisor_pref == "indifferent") {
					  qualquantweights[i, j] <- 0
					}
					if (student_pref == "mixed" && supervisor_pref == "quantitative") {
					  qualquantweights[i, j] <- 3
					}
					if (student_pref == "mixed" && supervisor_pref == "qualitative") {
					  qualquantweights[i, j] <- 3
					}
					
					# For student "indifferent"
					if (student_pref == "indifferent" && supervisor_pref == "indifferent") {
					  qualquantweights[i, j] <- 0
					}
					if (student_pref == "indifferent" && supervisor_pref == "quantitative") {
					  qualquantweights[i, j] <- 0
					}
					if (student_pref == "indifferent" && supervisor_pref == "qualitative") {
					  qualquantweights[i, j] <- 0
					}
				  }
				}

				# Inspect the qualquantweights matrix
				qualquantweights

				# Combine the original actweights with qualquantweights to form the overall cost matrix:
				overall_weights <- actweights + qualquantweights

				# Inspect the overall weights matrix
				overall_weights

		# Create the model
		
			#  the MIP model is optimizing the assignment of students to supervisors in such a way that:
				# for the concersation with chatGPT about this see: https://chat.openai.com/share/281459f6-d6dc-453c-885f-c98cd459f821

				# Each student is assigned to exactly one supervisor.
				# Each supervisor does not oversee more than a certain number of students (n_slots_per_supervisor_vec).
				# The total "badness" or unsuitability of all assignments is minimized, based on your overall_weights matrix.
				# So, by solving this revised MIP model, we will find the most optimal way, under the given constraints, 
				# to match students with supervisors so as to minimize the overall "badness" of the matches, as represented by the higher values in the overall_weights matrix.
		
		model <- MIPModel() %>%
		  add_variable(x[i, j], i = 1:n_students, j = 1:n_supervisors, type = "binary") %>%
		  set_objective(sum_expr(overall_weights[i, j] * x[i, j], i = 1:n_students, j = 1:n_supervisors), "min") %>%
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
		
	# pain and number of supervisors

		# total pain for one solution
		painmatrix <- assignment_matrix*overall_weights
		painmatrix
		sum(painmatrix)
		
	# now let's get a dataframe I can export where the unit of analysis is the student and we add the details of their assigned supervisor
		head(SPRF)
		names(SPRF)
		
		# first, lets select the relevant base variables
        STEX <- subset(SPRF, select=c("full_name","email","SNR","what_master_track","extended_master","double_degree","selfeval_scores_5","selfeval_scores_6"))
		
		# give the quant and qual skills more informative labels
		names(STEX)[names(STEX) == "selfeval_scores_5"] <- "selfeval_quant"
		names(STEX)[names(STEX) == "selfeval_scores_6"] <- "selfeval_qual"
		
		
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
		resvec2[i] <- overall_weights[i,which(assignment_matrix[i,] == 1)]
		}
		resvec2 
		length(resvec2)
		
		STEX$how_painfull <- resvec2
		table(STEX$how_painfull) ## OKI, there are 4 students that did not get any of their first choices.. what would happen if we add Bram. -- if we add Bram - and tweak the numbers per supervisor a bit, we get 

		EXPO <- STEX 

	# now, lets also automatically assign the 2nd readers

		# first of all, import a file with all the people available to be a 2nd reader
			SECR <-	read.xlsx("feb2025_2ndreaderoptions.xlsx", sheet = 1)
			head(SECR)
			
			SECR$letterinsurvey <- as.character(SECR$letterinsurvey)

		# drop everbody that has any students assigned
			nrow(SECR)
			SECR <- SECR[which(!SECR$letterinsurvey %in% names(table(STEX$my_assigned_supervisor))),]
			nrow(SECR) # different with last nrow should be number of supervisors assigned
		
		# order from lowest assigned number of 2nd reader tasks to highest (in a function because we want to do this below again after each assigment)
		
			orderon2ndreadertasks <- function(SECRLOCAL)
				{
					SECRLOCAL[order(SECRLOCAL$current_2nd_reader_tasks),]
				}
			# test
			orderon2ndreadertasks(SECR)
		
		# make a new dataframe, that we can do the random matching in
			# Create a dataframe with those names as a single variable
	#			supervisor_names <- names(table(STEX$my_assigned_supervisor))
	#			DFSU <- data.frame(assigned_supervisor = supervisor_names)
	#			DFSU$assigned_supervisor <- as.character(DFSU$assigned_supervisor)
	#			DFSU

# now, lets merge in some info on the supervisors
		nrow(EXPO)
		names(EXPO)
		head(EXPO)
		
		# adding sub columns that are needed for the thesis dossier import
		EXPO$Category <- currentcohort
		table(EXPO$Category)
		
		## getting the double degree categories all good
		EXPO$Subcategory <- EXPO$double_degree
		EXPO$Subcategory[which(EXPO$Subcategory == "No, I am NOT following the double-degree program.")] <- "Degree in Tilburg"
		EXPO$Subcategory[which(EXPO$Subcategory == "Yes, I am doing the double-degree with Trento")] <- "double-degree with Trento"
		EXPO$Subcategory[which(EXPO$Subcategory == "Yes, I am doing the double-degree with Barcelona")] <- "double-degree with Barcelona"
		EXPO$Subcategory[which(EXPO$Subcategory == "Yes, I am doing the double-degree with Bamberg")] <- "double-degree with Bamberg"
		
		## and the assigned supervisor and subjects as they occur in thesisdossier
		EXPO <- sqldf("SELECT EXPO.*, SUIN.letter_in_sep2024surv, SUIN.ANR as 'supervisor_anr', SUIN.last_name as 'supervisor_last_name', SUIN.first_name as 'supervisor_first_name', SUIN.email as 'supervisor_email', SUIN.circle_description as 'Subject'
					  FROM EXPO LEFT JOIN SUIN
					  ON
					  EXPO.my_assigned_supervisor = SUIN.letter_in_sep2024surv
					")
		nrow(EXPO)
		EXPO
		
		# order by EXPO$my_assigned_supervisor
		EXPO <- EXPO[order(EXPO$my_assigned_supervisor), ]
		EXPO
		
		# here the result file from Melanie can be added!
		
			# first, lets export the format as a template she can fill in
		
				# Get the current date and time
				current_time <- Sys.time()
					
				# Format it into a string suitable for a file name
				time_str <- format(current_time, "%Y%m%d_%H%M%S")

				# Create a file name with the timestamp
				file_name <- paste0("template_HWS_supervisors", time_str, ".xlsx")
			#	write.xlsx(EXPO, file_name)
			
			head(EXPO)
		
		# load this file again, clean it up a bit and append it
		HWSSUP <- read.xlsx("HWS_supervisors20250318.xlsx", sheet = "focused")
	
		HWS_EXPO <- HWSSUP[,c("full_name", "email", "SNR", "extended_master", "double_degree","last_name_of_assigned_supervisor")]
		names(HWS_EXPO)[names(HWS_EXPO) == "last_name_of_assigned_supervisor"] <- "my_assigned_supervisor"

		HWS_EXPO$what_master_track <- "HWS"
		HWS_EXPO$double_degree[is.na(HWS_EXPO$double_degree)] <- "No, I am not following the double degree program."

		HWS_EXPO
		
		# Convert SNR to character in both data frames
			EXPO <- EXPO %>% mutate(SNR = as.character(SNR))
			HWS_EXPO <- HWS_EXPO %>% mutate(SNR = as.character(SNR))

			# Now row-bind them
			EXPO <- bind_rows(EXPO, HWS_EXPO)
			
			EXPO
					
# now randomly assign the 2nd reader
		
		# lets set the seed using the cohort as the input, so that the random selection is the same when we run the script, but different between cohorts.
			# Use digest to create a hash and convert it to a number for set.seed (so, first make a hexadecimal, then convert to a numeric value)
				hashed_value <- strtoi(digest(currentcohort, algo="crc32"),base=16) 

			# Set the seed
				set.seed(hashed_value)
			
			# get a vector with all circles that need to be assigned
				circles_that_still_need_a_2nd_reader <- unique(EXPO$my_assigned_supervisor)
				
			# OPTIONAL - also add the last names of the supervisors from HWS here (as 'letters') if we also want them to be assigned a supervisor (did this now, but not as letters)
			
			# Randomly shuffle this vector per group
			
				# get a vector per group
					circles_that_still_need_a_2nd_reader_forus <- circles_that_still_need_a_2nd_reader[which(nchar(circles_that_still_need_a_2nd_reader) == 1)]
					circles_that_still_need_a_2nd_reader_forHWS <- circles_that_still_need_a_2nd_reader[which(nchar(circles_that_still_need_a_2nd_reader) > 1)]

				# reshuffle each vector
					circles_that_still_need_a_2nd_reader_forus_rand <- circles_that_still_need_a_2nd_reader_forus[sample(length(circles_that_still_need_a_2nd_reader_forus))]
					circles_that_still_need_a_2nd_reader_forHWS_rand <- circles_that_still_need_a_2nd_reader_forHWS[sample(length(circles_that_still_need_a_2nd_reader_forHWS))]
				# bind
					circles_that_still_need_a_2nd_reader_rand <- c(circles_that_still_need_a_2nd_reader_forus_rand,circles_that_still_need_a_2nd_reader_forHWS_rand)
				# circles_that_still_need_a_2nd_reader_rand <- circles_that_still_need_a_2nd_reader[sample(length(circles_that_still_need_a_2nd_reader))]
			
			# loop through each circle of the random list, and assign the person that currently has the lowest number of 2nd reader tasks, until the list is empty.

				resvec <- vector()
				for(i in 1:length(circles_that_still_need_a_2nd_reader_rand))
				{
					# order the dataframe on the current number of assigned 2nd reader tasks
					SECR <- orderon2ndreadertasks(SECR)
					
					# get the ANR of the person on top
					resvec[i] <- SECR[1,"ANR"]
					
					# increase this person its number of assigned tasks (we count the number of peope in this circle in EXPO
					SECR$current_2nd_reader_tasks[1] = SECR$current_2nd_reader_tasks[1] + nrow(EXPO[which(EXPO$my_assigned_supervisor == circles_that_still_need_a_2nd_reader_rand[i]),])
				}
				resvec
				
			# make a little dataframe with the result
				ASSI <- as.data.frame(cbind(circles_that_still_need_a_2nd_reader_rand,resvec))
				
			# get the 2nd reader ANR merged into EXPO, and after that the rest of their info
			
				nrow(EXPO)
				EXPO <- sqldf("SELECT EXPO.*, ASSI.resvec as 'random2ndreader_ANR'
							FROM EXPO
							LEFT JOIN ASSI
							ON EXPO.my_assigned_supervisor = ASSI.circles_that_still_need_a_2nd_reader_rand")
				nrow(EXPO)
				EXPO
			
			# use this ANR to get the rest of the details of this 2nd reader
			
				nrow(EXPO)
				EXPO <- sqldf("SELECT EXPO.*, SECR.last_name as 'random2ndreader_last_name', SECR.first_name as 'random2ndreader_first_name', SECR.email as 'random2ndreader_email'
							FROM EXPO
							LEFT JOIN SECR
							ON EXPO.random2ndreader_ANR = SECR.ANR")
				nrow(EXPO)
				EXPO

			# now, remove this assignment for students that are doing a double degree
			EXPO$random2ndreader_ANR[which(grepl("Yes, I am doing the double-degree",EXPO$double_degree, fixed=TRUE))] <- "Double Degree student, assign manually!"
			EXPO$random2ndreader_last_name[which(grepl("Yes, I am doing the double-degree",EXPO$double_degree, fixed=TRUE))] <- "Double Degree student, assign manually!"
			EXPO$random2ndreader_first_name[which(grepl("Yes, I am doing the double-degree",EXPO$double_degree, fixed=TRUE))] <- NA
			EXPO$random2ndreader_email[which(grepl("Yes, I am doing the double-degree",EXPO$double_degree, fixed=TRUE))] <- NA
			
			EXPO
	
	# merge in more information on the supervisors
				nrow(EXPO)
				EXPO <- sqldf("SELECT EXPO.*, SUINRAW.last_name as 'supervisor_last_name', SUINRAW.first_name as 'supervisor_first_name', SUINRAW.email as 'supervisor_email'
							FROM EXPO
							LEFT JOIN SUINRAW
							ON EXPO.my_assigned_supervisor = SUINRAW.letter_in_feb2025surv")
				nrow(EXPO)
				EXPO	
				
	# do some check sums on the circle sizes
	
		table(HWSSUP$last_name_of_assigned_supervisor)
		table(EXPO$supervisor_last_name)
		
		# Create the two tables
			tbl1 <- table(HWSSUP$last_name_of_assigned_supervisor)
			tbl2 <- table(EXPO$supervisor_last_name)

		# Subset tbl2 to only include names that appear in tbl1,
			# keeping the same order as in tbl1
			tbl2_subset <- tbl2[names(tbl1)]

		# Optionally, replace any NA (if a name in tbl1 is missing from tbl2) with 0
			tbl2_subset[is.na(tbl2_subset)] <- 0

		# Combine the tables by rows
			rbind(tbl1, tbl2_subset)
	
# EXPORT THE COMPLETE FILE with details

	# Get the current date and time
	current_time <- Sys.time()
		
	# Format it into a string suitable for a file name
	time_str <- format(current_time, "%Y%m%d_%H%M%S")

	# Create a file name with the timestamp
	file_name <- paste0("details_suggested_student-to-supervisor_assignments_", time_str, ".xlsx")
	write.xlsx(EXPO, file_name)
				

# EXPORT THE OVERVIEW AS WE USE IT IN THE GOOGLE sheet

	# course codes 
		# SET the course codes
		PPSD_coursecode <- "441803-M-24"
		PPSinCP_coursecode <- "400991-M-24"
		HWS_coursecode <- "400913-M-24"
	
		EXPO$track_code <- NA
		EXPO$track_code <- ifelse(grepl("GSMI", EXPO$what_master_track), PPSD_coursecode, NA)
		EXPO$track_code <- ifelse(grepl("PPSinCP", EXPO$what_master_track), PPSinCP_coursecode, EXPO$track_code)
		table(EXPO$track_code)
		nrow(EXPO) == sum(table(EXPO$track_code)) # should return TRUE


	EXPO2 <- EXPO[,c("SNR","track_code","full_name","email","supervisor_last_name","supervisor_first_name","supervisor_email","random2ndreader_last_name","random2ndreader_first_name","random2ndreader_email"),]

	# Create a file name with the timestamp
	file_name <- paste0("more_focussed_suggested_student-to-supervisor_assignments_", time_str, ".xlsx")
	write.xlsx(EXPO2, file_name)


			# PLEASE NOTE: THIS PART ALWAYS NEEDS TO BE RUN MANUALLY 
			#
			#	OPTION 1: SELECT DATA FROM ABOVE
			#		MIRS <-  EXPO
			#	OPTION 2: SELECT AN EXCEL SHEET IN THE SAME FORMAT THAT WAS MANUALLY EDITED
			#		MIRS <- read.xlsx("suggested_student-to-supervisor_assignments_versiontobeimportedforexporttoMirsim_v2.xlsx", sheet = 1) 
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
				
				head(MIRS)
		
				# export PPSD
					EXPO_PPSD <- sqldf(paste0("SELECT MIRS.SNR, MIRS.supervisor_anr as 'ANR_supervisor',MIRS.random2ndreader_ANR as 'ANR_second-assessor', MIRS.Category, MIRS.Subcategory, MIRS.Subject FROM MIRS WHERE coursecodes = '",PPSD_coursecode,"'"))
					nrow(EXPO_PPSD)
					EXPO_PPSD
					
					file_name2 <- paste0(PPSD_coursecode,"_", time_str, ".xlsx")
					write.xlsx(EXPO_PPSD, file_name2)

				# export PPSinCP
					EXPO_PPSinCP <- sqldf(paste0("SELECT MIRS.SNR, MIRS.supervisor_anr as 'ANR_supervisor', MIRS.random2ndreader_ANR as 'ANR_second-assessor', MIRS.Category, MIRS.Subcategory, MIRS.Subject FROM MIRS WHERE coursecodes = '",PPSinCP_coursecode,"'"))
					nrow(EXPO_PPSinCP)
					EXPO_PPSinCP
					
					file_name2 <- paste0(PPSinCP_coursecode,"_", time_str, ".xlsx")
					write.xlsx(EXPO_PPSinCP, file_name2)
					
					