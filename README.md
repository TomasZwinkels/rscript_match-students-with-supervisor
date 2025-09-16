# Student-Supervisor Assignment System

An R script for optimally matching master's thesis students with supervisors using mixed-integer programming (MIP).

## Overview

This system automates the assignment of master's thesis students to supervisors based on:
- Student preferences for supervisors/research circles
- Student research methodology preferences (quantitative/qualitative/mixed)
- Supervisor capacity constraints
- Automatic second reader assignment

The script uses optimization to minimize overall "pain" (dissatisfaction) while respecting all constraints.

## Key Features

- **Preference-based matching**: Students rank their top 4 supervisor choices
- **Methodology matching**: Considers alignment between student and supervisor research methods
- **Capacity constraints**: Respects maximum students per supervisor
- **Extended master handling**: Applies penalties for extended master students
- **Second reader assignment**: Automatically assigns second readers with load balancing
- **Data validation**: Multiple integrity checks to prevent assignment errors
- **Export formats**: Generates files for various administrative systems

## Dependencies

```r
install.packages(c("openxlsx", "ompr", "ompr.roi", "ROI.plugin.glpk", 
                   "tidyverse", "sqldf", "digest"))
```

## Input Files Required

1. `qualtrics_export_YYYYMMDD.xlsx` - Student preference survey data
2. `feb2025_supervisorinfo.xlsx` - Supervisor information and capacities
3. `feb2025_2ndreaderoptions.xlsx` - Available second readers
4. `HWS_supervisors20250318.xlsx` - HWS track assignments (optional)

## Data Structure

### Student Preferences
- Students rank up to 4 supervisors/research circles
- Methodology preference (quantitative/qualitative/mixed/indifferent)
- Extended master status
- Double degree program participation

### Supervisor Information
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

## Usage

1. **Set cohort**: Update `currentcohort` variable (line 27)
2. **Configure capacities**: Modify `n_slots_per_supervisor_vec` (line 193)
3. **Run data validation**: Script performs multiple integrity checks
4. **Execute optimization**: MIP solver finds optimal assignment
5. **Generate exports**: Multiple output formats for different systems

## Output Files

- `details_suggested_student-to-supervisor_assignments_TIMESTAMP.xlsx` - Complete assignment details
- `more_focussed_suggested_student-to-supervisor_assignments_TIMESTAMP.xlsx` - Administrative overview
- Course-specific files for thesis management system import

## Data Validation

The script includes checks for:
- Duplicate student entries (SNR, email, name)
- Missing supervisor preferences
- Extended master status validation
- Supervisor-ANR consistency
- Student name-SNR consistency

## Configuration Points

### Manual Adjustments (lines 54-68)
Remove duplicate or problematic entries before processing

### Cohort Selection (line 27)
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