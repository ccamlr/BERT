## The subgroup agreed that the bootstrap would permit the following
##
## Permit the specification of the number of years at liberty
## Natural mortality, tag-induced mortality and tag-loss implemented as
## Bernoulli trials
## Allow for finer-scale temporal variability than 1 year? yes - this is monthly

## 1) We calculate the available tags based on some assumptions
## 2) We use the CCMALR data scripts


#' Specify parameters for tag-based biomass estimate
#'
#' Specify parameters for tag-based biomass estimate
#' @param seasons vector of CCAMLR fishing seasons
#' @param tag_mort initial tag-induced mortality rates
#' @param tag_shed annual chronic (ongoing) tag shedding
#' @param chronic_mort vector of monthly chronic (ongoing) tag-induced mortality
#' @param report tag reporting rates
#' @param nat_mort annual natural mortality (instantaneous)
#' @param move_rate annual movement rate
#' @export
specify_params <- function(seasons, tag_mort, tag_shed, chronic_mort, report,
                           nat_mort, move_rate){
  ## some checks then combine into matrices
  ## define the number of months
  n_months <- length(seasons) * 12
  ## make this into a function that can be applied
  if(length(tag_mort) == 1) tag_mort <- rep(tag_mort, n_months)
  if(length(report) == 1) report <- rep(report, n_months)
  if(length(nat_mort) == 1) nat_mort <- rep(nat_mort, n_months)
  if(length(tag_shed) == 1) tag_shed <- rep(tag_shed, n_months)
  if(length(chronic_mort) == 1) chronic_mort <- rep(chronic_mort, n_months)
  if(length(move_rate) == 1) move_rate <- rep(move_rate, n_months)
  ## create a dataframe
  obj <- data.frame(cbind("Season" = rep(seasons, each = 12),
                          "Month" = rep(1:12, length(seasons)),
                          "tag_mort" = tag_mort,
                          "tag_shed" = tag_shed,
                          "chronic_mort" = chronic_mort,
                          "report" = report,
                          "nat_mort" = nat_mort,
                          "move_rate" = move_rate))
  #* consider adding a class
  ## return the list of parameter matrices
  obj
}
