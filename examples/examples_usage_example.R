#' Example Usage of OMOPConceptMapper Package
#' 
#' This script demonstrates the typical workflow for mapping ICD codes
#' to SNOMED concepts and querying their relationships.

library(OMOPConceptMapper)

# ---- Step 1: Create Database Connection ----
# Make sure environment variables are set:
# - SERVER_SERVERLESS
# - USERNAME  
# - PASSWORD

connection <- create_db_connection()

# Specify your CDM schema
cdm_schema <- "healthverity_marketplace_omop_20250331"


# ---- Step 2: Map ICD Codes to SNOMED (LIKE pattern) ----
# Search for all ICD codes starting with C16

results_like <- map_icd_to_snomed(
  connection = connection,
  cdm_schema = cdm_schema,
  pattern_type = "like",
  pattern_values = "C16%",
  print_sql = TRUE
)

View(results_like)
cat("\nFound", nrow(results_like), "ICD to SNOMED mappings\n")


# ---- Step 3: Map Specific ICD Codes (IN clause) ----
# Search for exact ICD codes

results_in <- map_icd_to_snomed(
  connection = connection,
  cdm_schema = cdm_schema,
  pattern_type = "in",
  pattern_values = c("C16.0", "C16.1", "C16.2"),
  print_sql = FALSE
)

View(results_in)


# ---- Step 4: Extract SNOMED IDs ----

snomed_ids <- extract_snomed_ids(results_like)
cat("\nUnique SNOMED IDs:", length(snomed_ids), "\n")

# As comma-separated string
snomed_string <- extract_snomed_ids(results_like, as_string = TRUE)
cat(snomed_string, "\n")


# ---- Step 5: Query Concept Relationships ----
# Get all related concepts for the SNOMED IDs

concept_relationships <- query_concept_relationships(
  snomed_ids = snomed_ids,
  cdm_schema = cdm_schema,
  connection = connection,
  only_standard = TRUE,
  only_direct = FALSE
)

View(concept_relationships)


# ---- Alternative: Complete Workflow in One Call ----
# Use the convenience function for the entire workflow

all_relationships <- icd_to_concept_relationships(
  connection = connection,
  cdm_schema = cdm_schema,
  pattern_type = "like",
  pattern_values = "C16%",
  only_standard = TRUE,
  only_direct = FALSE,
  return_mappings = FALSE
)

View(all_relationships)


# ---- Get Both Mappings and Relationships ----

complete_results <- icd_to_concept_relationships(
  connection = connection,
  cdm_schema = cdm_schema,
  pattern_type = "in",
  pattern_values = c("C16.0", "C16.1", "C16.2"),
  return_mappings = TRUE
)

View(complete_results$mappings)
View(complete_results$relationships)


# ---- Analyze Results ----

# Summary by relationship category
table(concept_relationships$REL_CATEGORY)

# Summary by direction
table(concept_relationships$DIRECTION)

# Filter for specific relationship types
library(dplyr)

descendants_only <- concept_relationships %>%
  filter(REL_CATEGORY == "HIERARCHY_DESCENDANT")

direct_relationships <- concept_relationships %>%
  filter(REL_CATEGORY == "RELATIONSHIP")

# Don't forget to disconnect when done
DatabaseConnector::disconnect(connection)
