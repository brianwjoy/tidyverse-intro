## ---------------------------
##
## Script name: 0_generate_fictitious_data.r
##
## Purpose of script:
##
##
## ---------------------------
##
## Notes:
##   This script does not need to be run.
##   It is for generating ./Data/
##   This data was generated with the assistance of Microsoft Copilot
##   These data are for demonstration purposes only and may not 
##   meet all assumptions of real production data
## ---------------------------

# Load Packages -----------------------------------------------------------

library(tidyverse)


# Set Seed ----------------------------------------------------------------

set.seed(1796)


# Set Parameters ----------------------------------------------------------


n_students <- 2500

colleges <- c("Arts & Sciences", "Business", "Engineering", "Education", "Health Sciences")

departments <- list(
  "Arts & Sciences" = c("Biology", "Chemistry", "History", "Psychology", "Mathematics", "Statistics"),
  "Business"        = c("Accounting", "Finance", "Marketing", "Management", "Information Systems"),
  "Engineering"     = c("Mechanical Engineering", "Electrical Engineering", "Civil Engineering", "Computer Engineering"),
  "Education"       = c("Elementary Education", "Secondary Education", "Special Education", "Educational Leadership"),
  "Health Sciences" = c("Nursing", "Public Health", "Physical Therapy", "Health Administration")
)


term_seq <- tibble(
  EnrollmentTerm = factor(
    c("Fall 2023", "Spring 2024", "Summer 2024",
      "Fall 2024", "Spring 2025", "Summer 2025"),
    levels = c("Fall 2023", "Spring 2024", "Summer 2024",
               "Fall 2024", "Spring 2025", "Summer 2025"),
    ordered = TRUE
  )
) %>%
  mutate(
    TermSeason = str_extract(EnrollmentTerm, "^[A-Za-z]+"),
    TermYear   = as.integer(str_extract(EnrollmentTerm, "\\d{4}"))
  )


# Create a base student roster --------------------------------------------

roster <- tibble(
  StudentID = sprintf("S%05d", 1:n_students),
  AcademicCollege = sample(colleges, n_students, replace = TRUE,
                           prob = c(0.28, 0.18, 0.22, 0.12, 0.20)),
  CohortYear = sample(2023:2025, n_students, replace = TRUE, prob = c(0.45, 0.40, 0.15)),
  # A latent academic ability score to make GPA stable but variable over terms
  ability = rnorm(n_students, mean = 2.8, sd = 0.5)
) %>%
  rowwise() %>%
  mutate(
    AcademicDepartment = sample(departments[[AcademicCollege]], 1),
    AcademicMajor = paste(AcademicDepartment, "Major")
  ) %>%
  ungroup()


# Expand to student-term panel --------------------------------------------


panel <- roster %>%
  crossing(term_seq) %>%
  # First-time status: Fall of the CohortYear
  mutate(
    FirstTimeStatus = if_else(
      EnrollmentTerm == paste("Fall", CohortYear),
      "First-Time", "Continuing"
    ),
    # Credits by term with realistic distributions
    CreditsAttempted = case_when(
      TermSeason %in% c("Fall", "Spring") ~ sample(9:18, size = n(), replace = TRUE,
                                                   prob = c(0.05, 0.05, 0.08, 0.12, 0.15, 0.15, 0.15, 0.15, 0.10, 0.0)),
      TermSeason == "Summer"              ~ sample(3:12, size = n(), replace = TRUE,
                                                   prob = c(0.10, 0.12, 0.14, 0.14, 0.14, 0.12, 0.10, 0.08, 0.04, 0.02))
    ),
    # Full-time rule per your thresholds
    FullTimeStatus = case_when(
      TermSeason %in% c("Fall", "Spring") & CreditsAttempted >= 12 ~ "Full-Time",
      TermSeason == "Summer"              & CreditsAttempted >= 6  ~ "Full-Time",
      TRUE                                                    ~ "Part-Time"
    ),
    # Term GPA: ability + noise, clipped to [0, 4]
    GPA = pmin(4, pmax(0, ability + rnorm(n(), mean = 0.0,
                                          sd = if_else(FullTimeStatus == "Full-Time", 0.35, 0.45)))),
    GPA = round(GPA, 2)
  ) %>%
  # RetainedNextFall: only meaningful on Fall terms—probability influenced by GPA & Full-Time
  mutate(
    RetainedNextFall = case_when(
      TermSeason == "Fall" ~ {
        base_p <- 0.60
        p <- base_p +
          if_else(GPA >= 2.5, 0.12, -0.08) +
          if_else(FullTimeStatus == "Full-Time", 0.10, -0.05)
        p <- pmin(pmax(p, 0.05), 0.95)
        rbinom(n(), 1, p)
      },
      TRUE ~ NA_integer_
    ),
    RetainedNextFall = case_when(
      is.na(RetainedNextFall) ~ NA_character_,
      RetainedNextFall == 1   ~ "Retained",
      TRUE                    ~ "Not Retained"
    )
  ) %>%
  select(
    StudentID, AcademicCollege, AcademicDepartment, AcademicMajor,
    EnrollmentTerm, TermSeason, TermYear,
    CohortYear, FirstTimeStatus,
    CreditsAttempted, FullTimeStatus, GPA,
    RetainedNextFall
  )


# Save output -------------------------------------------------------------

panel %>% 
  write_csv(
    file = "./Data/fictitious_student_enrollment_panel.csv",
    na = ""
  )




# Clear Environment -------------------------------------------------------

rm(list = ls())
p_unload("all")
