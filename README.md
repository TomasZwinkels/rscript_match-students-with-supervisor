# Student-Supervisor Assignment System

An R script for optimally matching master's thesis students with supervisors using mixed-integer programming (MIP) and also assigns 2nd readers at random, while talking into account how many students a supervisor already has.

## Overview

This system automates the assignment of master's thesis students to supervisors based on:
- Student preferences for supervisors/research circles
- Student research methodology preferences (quantitative/qualitative/mixed)
- Supervisor capacity constraints

The script uses optimization to minimize overall "pain" (dissatisfaction) while respecting all constraints.

## Key Features

- **Data validation**: Multiple integrity checks to prevent assignment errors
- **Preference-based matching**: Students rank their top 4 supervisor choices
- **Methodology matching**: Considers alignment between student and supervisor research methods
- **Capacity constraints**: Respects maximum students per supervisor
- **Extended master handling**: Makes sure that extended master students are almost always assigned their 1st preference (by assigning a higher pentalty when they are not.)
- **Second reader assignment**: Automatically and randomly assigns second readers, balancing this with the already existing number of students.
- **Export formats**: Generates files for various administrative systems

## Dependencies

```r
install.packages(c("openxlsx", "ompr", "ompr.roi", "ROI.plugin.glpk", 
                   "tidyverse", "sqldf", "digest"))
```

## Folder Structure

Each cohort run is stored in its own subfolder under `runs/`:

```
rscript_match-students-with-supervisor/
├── match.R                 # Main script
├── match_functions.R       # Helper functions
├── README.md
└── runs/
    ├── SEP2025/            # September 2025 cohort
    │   ├── qualtrics_export_20250919.xlsx
    │   ├── sep2025_supervisorinfo.xlsx
    │   ├── sep2025_2ndreaderoptions.xlsx
    │   ├── HWS_supervisors20250318.xlsx
    │   └── [output files...]
    └── FEB2026/            # February 2026 cohort (example)
        ├── qualtrics_export_YYYYMMDD.xlsx
        └── ...
```

To set up a new cohort:
1. Create a new folder under `runs/` (e.g., `runs/FEB2026/`)
2. Place input files in that folder
3. Update `cohort_folder` in `match.R` (around line 17)

## Input Files Required

1. `qualtrics_export_YYYYMMDD.xlsx` - Student preference survey data
2. `COHORT_supervisorinfo.xlsx` - Supervisor information and capacities
3. `COHORT_2ndreaderoptions.xlsx` - Available second readers
4. `HWS_supervisorsYYYYMMDD.xlsx` - HWS track assignments (optional)

## Data Structure

### Student Information / Preferences
- Students rank up to 4 supervisors/research circles
- Methodology preference (quantitative/qualitative/mixed/indifferent)
- Extended master status
- Double degree program participation

### Supervisor Information / Preferences
- Available research circles
- Maximum student capacity (typically 4)
- Methodology specialization
- Contact information

## Algorithm

The script uses a Mixed-Integer Programming model that:

1. **Assigns costs** to student-supervisor pairs based on:
   - Preference rank (0 for 1st choice, 2 for 2nd, 3 for 3rd, 4 for 4th, 6 for unranked)
   - Methodology mismatch penalty (+3 points)
   - Extended master penalty (×2 multiplier)

2. **Optimizes** the assignment to minimize total cost while ensuring:
   - Each student gets exactly one supervisor
   - No supervisor exceeds their capacity
   - Preference satisfaction is maximized

3. **Assigns second readers** using load balancing with randomization

## When using for a new cohort, the following needs to happen.

1. **Create cohort folder**: Create a new folder under `runs/` (e.g., `runs/FEB2026/`) and place input files there
2. **Set cohort folder**: Update `cohort_folder` variable (around line 17) to point to the new folder
3. **Set cohort**: Update `currentcohort` variable (around line 34)
4. **Update input filenames**: Update the input file names in the script to match your new files
5. **Configure capacities**: Modify `n_slots_per_supervisor_vec` (around line 235)
6. **Run data validation**: Performs the multiple integrity checks in the script, deal with the cases that pop up.
7. **Execute optimization**: Use the MIP solver, potentially play around with number of slots per supervisor to see if we can drop a supervisor as available without making to many people unhappy.
8. **Generate exports**: Generate the output formats and get them into the other systems.

## Output Files

- `details_suggested_student-to-supervisor_assignments_TIMESTAMP.xlsx` - Complete assignment details
- `more_focussed_suggested_student-to-supervisor_assignments_TIMESTAMP.xlsx` - Administrative overview
- Course-specific files for thesis management system import

## Details on Data Validation

The script includes checks for:
- Duplicate student entries (SNR, email, name)
- Missing supervisor preferences
- Extended master status validation
- Supervisor-ANR consistency
- Student name-SNR consistency

## Configuration Points

### Cohort Folder (line 17)
```r
cohort_folder <- "runs/SEP2025"  # e.g., "runs/SEP2025", "runs/FEB2026"
```

### Manual Adjustments (lines 63-78)
Remove duplicate or problematic entries before processing

### Cohort Selection (line 34)
```r
currentcohort = "Coh:February 2025"
```

### Supervisor Capacities (line 193)
```r
n_slots_per_supervisor_vec <- c(4,4,4,4,4,0)
```

### Course Codes (lines 665-667)
Update for current academic year

## Pain Score Interpretation

- **0**: Student's 1st choice with methodology match
- **2**: Student's 2nd choice with methodology match
- **3**: Student's 3rd choice OR 1st choice with methodology mismatch
- **4**: Student's 4th choice with methodology match
- **6**: Unranked supervisor with methodology match
- **Higher scores**: Combinations of poor preference rank and methodology mismatch

## Notes

- Extended master students receive doubled pain scores
- Double degree students get manual second reader assignment
- Script uses deterministic randomization for reproducible second reader assignments
- HWS track students are processed separately and merged