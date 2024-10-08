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
	QRAW <- read.xlsx("qualtrics_export_20240916.xlsx", sheet = 1) # QRAW <- read.xlsx("qualtrics_export_20230918.xlsx", sheet = 1)
	head(QRAW)
	
	# filter 
	# data line 2 is stupid
	QRAW <- QRAW[-1,]
	head(QRAW)
	
	########### SELECT THE RELEVANT COHORT HERE
		currentcohort = "Coh:September 2024"
		nrow(QRAW)
		QRAW <- QRAW[which(QRAW$student_cohort == currentcohort),]
		nrow(QRAW)
	
	###########
	
	# quick fix for later
	QRAW$SNR <-  as.numeric(QRAW$SNR)
	
# import the datafile with supervisor info
	SUIN <- read.xlsx("sep2024_supervisorinfo.xlsx", sheet = 1)
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
	
	# Kirsten Veerle, does want to do her thesis this semester
	QRAW <- QRAW[which(!QRAW$ResponseId == "R_8ISLBKEbMfLhjik"),]
#	QRAW <- QRAW[which(!QRAW$ResponseId == "R_3gWoHtyh3VRWDdv"),] # 2nd submission suggesting student does an MTP resit?!
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
			
			Yes, I applied for the extended master recently.
		
		# and filter the remaining
		nrow(QRAW)
		QRAW <- QRAW[which(!QRAW$extended_master == "Yes, I applied for the extended master recently."),] # these people will 95% sure all be accepted and won't be assigned a thesis circle
		nrow(QRAW)
		
		# text for Other
		OTH <- QRAW[which(QRAW$extended_master == "Other (please specify)."),]
		nrow(OTH)
		OTH$Extended.master?_4_TEXT
		OTH$full_name
		OTH$email
		
		# this student came up, see email conversation with Christof, lets assume for now she will join
		QRAW$extended_master[which(QRAW$ResponseId == "R_2vSersf12bYNsGt")] <- "No, I did NOT apply for the extended master."
		
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

	table(SPRF$MTP_Resit)
	nrow(SPRF)
	SPRF <- SPRF[which(SPRF$MTP_Resit == "No, I am enrolling to start my Thesis for the first time"),]
	nrow(SPRF)
	
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
		
		# also, if some supervisors are allowed to have more than 3 students, specify this here! Otherwise set all to 4.
			n_slots_per_supervisor_vec <- rep(4, length.out = nrow(SUIN))
			length(n_slots_per_supervisor_vec)
			
			n_slots_per_supervisor_vec <- c(3, 4, 4, 4, 4, 3, 4, 4, 0, 0, 0, 0) # manual version, needs to as long as n_supervisors
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
						CHECK # OK, so all but Katya occur once, that one was indeed incorrect
						
						
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
				
	# OK, so I really don't want Ivar Staps bij Suzanne (method mismatch), dus ik ga dat weight omhoog zetten.
	
		# right person?
			SPRF$full_name[11]
			colnames(actweights)[1]
		
		# yes, so lets set ## I did this different, I just switched his choice 3 and 4 in the qualtrics export file.
			# actweights[11,1] <- actweights[11,1]*2
			
	# and if you are an extended master student currently doing an intership: double the pain!
		indexvecofexmastu <- which(SPRF$extended_master == "Yes, I applied for the extended master roughly half a year ago and am currently doing an internship.")
		actweights[indexvecofexmastu,] <- actweights[indexvecofexmastu,] * 2
	
	# and if you would like to do a qualitative thesis, but the supervisor is quantitative or vice versa add pentalty 3 
		# (which makes the algorithm in different between getting a supervisor that is your 3th choice or getting your first choice with a missmatch), which means that:
			# - 2nd choice supervisor with match is preferd over first choice with mismatch
			# - random supervisor (pain=6) with a method match is prefered over a 4th choice supervisor with a mismatch (pain=7), but not a lot.
		
			## OK, so note that we did not yet actually ask students about this yet this year, so I cannot sort them on this yet. They will ofcourse have already self-sorted.
	
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
		
	# pain and number of supervisors

		# total pain for one solution
		painmatrix <- assignment_matrix*actweights
		painmatrix
		sum(painmatrix)
		
	# manual assignment of extended master students
	#	SPEM <- subset(SPEM, select=c("full_name","email","SNR","what_master_track","extended_master","student_explanation"))
	#	
	#	names(SPEM)
	#	SPEM
	#
	#	SPEM$my_assigned_supervisor <- NA
	#	
	#	SPEM$my_assigned_supervisor[which(SPEM$email == "R.j.barzegar@tilburguniversity.edu")] <- "C"
	#	SPEM$my_assigned_supervisor[which(SPEM$email == "g.g.greco@tilburguniversity.edu")] <- "E"
	#	SPEM$my_assigned_supervisor[which(SPEM$email == "r.s.p.hurl@tilburguniversity.edu")] <- "G"
	#	SPEM$my_assigned_supervisor[which(SPEM$email == "j.m.vdrMeer_1@tilburguniversity.edu")] <- "I"
	#	SPEM$my_assigned_supervisor[which(SPEM$email == "w.b.p.kwint@tilburguniversity.edu")] <- "B"
	#	SPEM$my_assigned_supervisor[which(SPEM$email == "t.j.p.vanaert@tilburguniversity.edu")] <- "A"
	
	#	SPEM$how_painfull <- NA

	# now let's get a dataframe I can export where the unit of analysis is the student and we add the details of their assigned supervisor
		head(SPRF)
		names(SPRF)
		
		# first, lets select the relevant base variables
        STEX <- subset(SPRF, select=c("full_name","email","SNR","what_master_track","extended_master","student_expl_1schoi","double_degree","selfeval_scores_5","selfeval_scores_6"))
		
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
		resvec2[i] <- actweights[i,which(assignment_matrix[i,] == 1)]
		}
		resvec2 
		length(resvec2)
		
		STEX$how_painfull <- resvec2
		table(STEX$how_painfull) ## OKI, there are 4 students that did not get any of their first choices.. what would happen if we add Bram. -- if we add Bram - and tweak the numbers per supervisor a bit, we get 

		EXPO <- STEX 

	# OK, and did Katya get some people that are OK with quant 

		KATJ <- EXPO[which(EXPO$my_assigned_supervisor == "B"),]
		table(KATJ$selfeval_quant) # quite good
	
	# and Nola and Suzanne people that are OK with qualitative
	
		SUSS <- EXPO[which(EXPO$my_assigned_supervisor == "A"),]
		nrow(SUSS)
		table(SUSS$selfeval_qual) # one really NOT very good. 
		table(SUSS$selfeval_quant) # that student feels more comfortable with quant
		SUSS
	
		NOLA <- EXPO[which(EXPO$my_assigned_supervisor == "F"),]
		nrow(NOLA)
		table(NOLA$selfeval_qual) # one really NOT very good. Student Maaike Fukkink should not be with Nola
		table(NOLA$selfeval_quant) # that student feels more comfortable with quant
		NOLA
	
	# now, lets also automatically assign the 2nd readers

		# first of all, import a file with all the people available to be a 2nd reader
			SECR <-	read.xlsx("sep2024_2ndreaderoptions.xlsx", sheet = 1)
			head(SECR)
			SECR
			
			SECR$letterinsurvey <- as.character(SECR$letterinsurvey)

		# drop everbody that has any students assigned
			nrow(SECR)
			SECR <- SECR[which(!SECR$letterinsurvey %in% names(table(STEX$my_assigned_supervisor))),]
			nrow(SECR) # different with last nrow should be number of supervisors assigned
		
		# make a new dataframe, that we can do the random matching in
			# Create a dataframe with those names as a single variable
				supervisor_names <- names(table(STEX$my_assigned_supervisor))
				DFSU <- data.frame(assigned_supervisor = supervisor_names)
				DFSU$assigned_supervisor <- as.character(DFSU$assigned_supervisor)
				DFSU

# now, lets merge in some info on the supervisors
		nrow(EXPO)
		names(EXPO)
		head(EXPO)
		
		# adding sub columns that are needed for the thesis dossier import
		
		## make sure to set this one correctly manually!
		EXPO$Category <- "Coh:February 2024"
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
	
# now randomly assign the 2nd reader
		
			# lets set the seed using the cohort as the input, so that the random selection is the same when we run the script, but different between cohorts.
				# Use digest to create a hash and convert it to a number for set.seed (so, first make a hexadecimal, then convert to a numeric value)
				hashed_value <- strtoi(digest(currentcohort, algo="crc32"),base=16) 

				# Set the seed
				set.seed(hashed_value+3)
				
			# Randomly shuffle the rows of SECR
				SECR <- SECR[sample(nrow(SECR)),]
				
			# And now just select the top 8
				DFSU$random2ndreader_ANR <- SECR$ANR[1:nrow(DFSU)]
				DFSU$random2ndreader_last_name <- SECR$last_name[1:nrow(DFSU)]
				DFSU$random2ndreader_first_name <- SECR$first_name[1:nrow(DFSU)]
				DFSU$random2ndreader_email <- SECR$email[1:nrow(DFSU)]
			
			DFSU
		
		# and merge the result into EXPO
		nrow(EXPO)
			EXPO <- sqldf("SELECT EXPO.*, DFSU.*
                    FROM EXPO
                    LEFT JOIN DFSU
                    ON EXPO.my_assigned_supervisor = DFSU.assigned_supervisor")
		nrow(EXPO)
		EXPO
		
		# now, remove this assignment for students that are doing a double degree
		EXPO$random2ndreader_ANR[which(grepl("Yes, I am doing the double-degree",EXPO$double_degree, fixed=TRUE))] <- NA
		EXPO$random2ndreader_last_name[which(grepl("Yes, I am doing the double-degree",EXPO$double_degree, fixed=TRUE))] <- NA
		EXPO$random2ndreader_first_name[which(grepl("Yes, I am doing the double-degree",EXPO$double_degree, fixed=TRUE))] <- NA
		EXPO$random2ndreader_email[which(grepl("Yes, I am doing the double-degree",EXPO$double_degree, fixed=TRUE))] <- NA
		
		EXPO
	
# EXPORT THE COMPLETE FILE

	# Get the current date and time
	current_time <- Sys.time()
		
	# Format it into a string suitable for a file name
	time_str <- format(current_time, "%Y%m%d_%H%M%S")

	# Create a file name with the timestamp
	file_name <- paste0("suggested_student-to-supervisor_assignments_", time_str, ".xlsx")
	write.xlsx(EXPO, file_name)
				
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
					
					