#' Complete ICD to Concept Relationship Workflow
#'
#' Performs a complete workflow from ICD codes to full concept relationships:
#' 1. Maps ICD codes to SNOMED concepts
#' 2. Extracts SNOMED concept IDs
#' 3. Queries all relationships for those SNOMED concepts
#'
#' @inheritParams map_icd_to_snomed
#' @param only_standard Logical. If TRUE, only return standard concepts in relationships. 
#'   Default is TRUE
#' @param only_direct Logical. If TRUE, only return direct relationships. Default is FALSE
#' @param return_mappings Logical. If TRUE, returns a list with both mappings and relationships.
#'   If FALSE, returns only relationships. Default is FALSE
#'
#' @return If return_mappings=FALSE: Data frame of concept relationships.
#'   If return_mappings=TRUE: List with elements 'mappings' and 'relationships'
#' @export
#'
#' @examples
#' \dontrun{
#' conn <- create_db_connection()
#' 
#' # Get all relationships for ICD codes starting with C16
#' relationships <- icd_to_concept_relationships(
#'   connection = conn,
#'   cdm_schema = "healthverity_marketplace_omop_20250331",
#'   pattern_type = "like",
#'   pattern_values = "C16%"
#' )
#' 
#' # Get both mappings and relationships
#' results <- icd_to_concept_relationships(
#'   connection = conn,
#'   cdm_schema = "healthverity_marketplace_omop_20250331",
#'   pattern_type = "in",
#'   pattern_values = c("C16.0", "C16.1", "C16.2"),
#'   return_mappings = TRUE
#' )
#' }
icd_to_concept_relationships <- function(
    connection,
    cdm_schema,
    pattern_type = c("like", "in"),
    pattern_values,
    source_vocabulary_id = c("ICD10CM", "ICD9CM"),
    target_vocabulary_id = "SNOMED",
    relationship_id = "Maps to",
    only_standard = TRUE,
    only_direct = FALSE,
    print_sql = FALSE,
    return_mappings = FALSE
) {
  
  # Step 1: Map ICD to SNOMED
  message("\n=== Step 1: Mapping ICD codes to SNOMED ===")
  mappings <- map_icd_to_snomed(
    connection = connection,
    cdm_schema = cdm_schema,
    pattern_type = pattern_type,
    pattern_values = pattern_values,
    source_vocabulary_id = source_vocabulary_id,
    target_vocabulary_id = target_vocabulary_id,
    relationship_id = relationship_id,
    print_sql = print_sql
  )
  
  if (nrow(mappings) == 0) {
    warning("No ICD to SNOMED mappings found. Returning empty result.")
    if (return_mappings) {
      return(list(mappings = mappings, relationships = data.frame()))
    } else {
      return(data.frame())
    }
  }
  
  # Step 2: Extract SNOMED IDs
  message("\n=== Step 2: Extracting SNOMED concept IDs ===")
  snomed_ids <- extract_snomed_ids(mappings)
  message("Found ", length(snomed_ids), " unique SNOMED concept IDs")
  
  # Step 3: Query concept relationships
  message("\n=== Step 3: Querying concept relationships ===")
  relationships <- query_concept_relationships(
    snomed_ids = snomed_ids,
    cdm_schema = cdm_schema,
    connection = connection,
    only_standard = only_standard,
    only_direct = only_direct,
    print_sql = print_sql
  )
  
  message("\n=== Workflow complete ===")
  
  # Return results
  if (return_mappings) {
    return(list(
      mappings = mappings,
      relationships = relationships
    ))
  } else {
    return(relationships)
  }
}