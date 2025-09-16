# Match Functions
# Functions for student-supervisor matching script

show_duplicates_SNR <- function(DF) {
  dup_SNRs <- DF$SNR[duplicated(DF$SNR) | duplicated(DF$SNR, fromLast = TRUE)]
  if (length(dup_SNRs) > 0) {
	cat("DUPLICATE SNRs found:\n")
	result <- DF[DF$SNR %in% dup_SNRs, c("ResponseId", "SNR", "full_name", "email", "StartDate",
	                                      "stud_supervis_prefer_0_GROUP", "stud_supervis_prefer_1_GROUP",
	                                      "stud_supervis_prefer_2_GROUP", "stud_supervis_prefer_3_GROUP")]
	print(result[order(result$SNR),])
	return(FALSE)
  } else {
	cat("No duplicate SNRs found\n")
	return(TRUE)
  }
}

show_duplicates_email <- function(DF) {
  dup_emails <- DF$email[duplicated(DF$email) | duplicated(DF$email, fromLast = TRUE)]
  if (length(dup_emails) > 0) {
	cat("DUPLICATE emails found:\n")
	result <- DF[DF$email %in% dup_emails, c("SNR", "full_name", "email", "StartDate",
	                                         "stud_supervis_prefer_0_GROUP", "stud_supervis_prefer_1_GROUP",
	                                         "stud_supervis_prefer_2_GROUP", "stud_supervis_prefer_3_GROUP")]
	print(result[order(result$email),])
	return(FALSE)
  } else {
	cat("No duplicate emails found\n")
	return(TRUE)
  }
}

show_duplicates_full_name <- function(DF) {
  dup_names <- DF$full_name[duplicated(DF$full_name) | duplicated(DF$full_name, fromLast = TRUE)]
  if (length(dup_names) > 0) {
	cat("DUPLICATE full names found:\n")
	result <- DF[DF$full_name %in% dup_names, c("SNR", "full_name", "email", "StartDate",
	                                            "stud_supervis_prefer_0_GROUP", "stud_supervis_prefer_1_GROUP",
	                                            "stud_supervis_prefer_2_GROUP", "stud_supervis_prefer_3_GROUP")]
	print(result[order(result$full_name),])
	return(FALSE)
  } else {
	cat("No duplicate full names found\n")
	return(TRUE)
  }
}

allstudentssignedup <- function(DF) {
  if (length(names(table(DF$stud_supervis_prefer_0_GROUP == "" | is.na(DF$stud_supervis_prefer_0_GROUP)))) > 1) {
	cat("NOT all students have signed up yet:\n")
	print(DF[which(DF$stud_supervis_prefer_0_GROUP == "" | is.na(DF$stud_supervis_prefer_0_GROUP)),])
	return(FALSE)
  } else {
	cat("All students have signed up\n")
	return(TRUE)
  }
}

orderon2ndreadertasks <- function(SECRLOCAL) {
  # orders the 2nd reader tasks based on the preferences provided by the 2nd readers

  # only consdier supervisors/2nd readers who have provided their preferences to HWS.
  SECR_PREFS_LOCAL <- SECR_PREFS[which(SECR_PREFS$supervisor %in% SECRLOCAL$supervisor), ]
  SECR_PREFS_LOCAL <- merge(SECR_PREFS_LOCAL, SECRLOCAL[c("supervisor", "max_secr_tasks")], by.x = "supervisor", by.y = "supervisor")

  # check for duplicates
  if (length(unique(SECR_PREFS_LOCAL$supervisor)) != nrow(SECR_PREFS_LOCAL)) {
	# duplicates found
	print("duplicate entry found for a supervisor in the 2nd reader preference file, please fix:")
	print(SECR_PREFS_LOCAL$supervisor[duplicated(SECR_PREFS_LOCAL$supervisor)])
	return(FALSE)
  }

  alloc_2ndr <- MIPModel() %>%
	add_variable(secrtask[i, j], i = 1:nrow(SECR_PREFS_LOCAL), j = 1:nrow(SPRF), type = "binary") %>%
	set_objective(sum_over(secrtask[i, j] * SECR_PREFS_LOCAL$weight_GROUP[i] * (SPRF$stud_supervis_prefer_0_GROUP[j] == SECR_PREFS_LOCAL$GROUP_preference[i]), i = 1:nrow(SECR_PREFS_LOCAL), j = 1:nrow(SPRF)), "max") %>%

	# CONSTRAINT: a student can only be assigned one 2nd reader
	add_constraint(sum_over(secrtask[i, j], i = 1:nrow(SECR_PREFS_LOCAL)) == 1, j = 1:nrow(SPRF)) %>%

	# CONSTRAINT: a supervisor cannot exceed their capacity
	add_constraint(sum_over(secrtask[i, j], j = 1:nrow(SPRF)) <= SECR_PREFS_LOCAL$max_secr_tasks[i], i = 1:nrow(SECR_PREFS_LOCAL))

  # Solve the model and return results
  result <- solve_model(alloc_2ndr, with_ROI(solver = "glpk", verbose = TRUE))

  if (result$status == "optimal") {
	solution <- get_solution(result, secrtask[i, j])
	solution <- solution[solution$value == 1, ]

	if (nrow(solution) == nrow(SPRF)) {
	  cat("Optimal 2nd reader assignment found\n")
	  SPRF$second_reader <- SECR_PREFS_LOCAL$supervisor[solution$i]
	  return(SPRF)
	} else {
	  cat("Warning: Not all students got a 2nd reader\n")
	  return(FALSE)
	}
  } else {
	cat("No optimal solution found for 2nd reader assignment\n")
	return(FALSE)
  }
}