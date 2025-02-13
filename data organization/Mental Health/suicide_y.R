# library(dplyr)
# library(readr)
# source("configurations/path_config.R")

############################################################
# SUICIDE - Diagnoses [Youth]
############################################################

get_suicide_y <- function(ksad_tables){

  if(!"suicidality" %in% names(ksad_tables)) return()

  # check with mh_y_ksads_si
  ksads_y <- read_csv(file.path(abcd_core_mental_health_path, "mh_y_ksads_ss.csv"))
  sui_vars <- c(paste0("ksads_23_9", c(45:66), "_t"),
                paste0("ksads2_23_9", c(10:26, "05", "06", "07", "08", "09"), "_t"))
  suicide <- ksads_y %>% select(matches("src|event"), all_of(sui_vars))
  suicide[suicide == 888] = NA

  suicide <- suicide %>%
     mutate(
        # SA
        ksads_23_952_912_t = coalesce(ksads_23_952_t, ksads2_23_912_t), # Diagnosis - InterruptedAttemptPresent
        ksads_23_953_913_t = coalesce(ksads_23_953_t, ksads2_23_913_t), # Diagnosis - AbortedAttemptPresent
        ksads_23_954_914_t = coalesce(ksads_23_954_t, ksads2_23_914_t), # Diagnosis - SuicideAttemptPresent

        ksads_23_963_923_t = coalesce(ksads_23_963_t, ksads2_23_923_t), # Diagnosis - InterruptedAttemptPast
        ksads_23_964_924_t = coalesce(ksads_23_964_t, ksads2_23_924_t), # Diagnosis - AbortedAttemptPast
        ksads_23_965_925_t = coalesce(ksads_23_965_t, ksads2_23_925_t), # Diagnosis - SuicideAttemptPast

        # SI
        ksads_23_946_906_t = coalesce(ksads_23_946_t, ksads2_23_906_t), # Diagnosis - SuicidalideationPassivePresent
        ksads_23_947_907_t = coalesce(ksads_23_947_t, ksads2_23_907_t), # Diagnosis - SuicidalideationActivenonspecificPresent
        ksads_23_948_908_t = coalesce(ksads_23_948_t, ksads2_23_908_t), # Diagnosis - SuicidalideationActivemethodPresent
        ksads_23_949_909_t = coalesce(ksads_23_949_t, ksads2_23_909_t), # Diagnosis - SuicidalideationActiveintentPresent
        ksads_23_950_910_t = coalesce(ksads_23_950_t, ksads2_23_910_t), # Diagnosis - SuicidalideationActiveplanPresent
        ksads_23_951_911_t = coalesce(ksads_23_951_t, ksads2_23_911_t), # Diagnosis - PreparatoryActionstowardimminentSuicidalbehaviorPresent

        ksads_23_957_917_t = coalesce(ksads_23_957_t, ksads2_23_917_t), # Diagnosis - SuicidalideationPassivePast
        ksads_23_958_918_t = coalesce(ksads_23_958_t, ksads2_23_918_t), # Diagnosis - SuicidalideationActivenonspecificPast
        ksads_23_959_919_t = coalesce(ksads_23_959_t, ksads2_23_919_t), # Diagnosis - SuicidalideationActivemethodPast
        ksads_23_960_920_t = coalesce(ksads_23_960_t, ksads2_23_920_t), # Diagnosis - SuicidalideationActiveintentPast
        ksads_23_961_921_t = coalesce(ksads_23_961_t, ksads2_23_921_t), # Diagnosis - SuicidalideationActiveplanPast
        ksads_23_962_922_t = coalesce(ksads_23_962_t, ksads2_23_922_t), # Diagnosis - PreparatoryActionstowardimminentSuicidalbehaviorPast

        # NSSI
        ksads_23_945_905_t = coalesce(ksads_23_945_t, ksads2_23_905_t), # Diagnosis - SelfInjuriousBehaviorwithoutsuicidalintentPresent
        ksads_23_956_916_t = coalesce(ksads_23_956_t, ksads2_23_916_t), # Diagnosis - SelfInjuriousBehaviorwithoutsuicidalintentPast

        # No suicide
        ksads_23_955_915_t = coalesce(ksads_23_955_t, ksads2_23_915_t), # Diagnosis - NoSuicidalIdeationOrBehaviorPresent
        ksads_23_966_926_t = coalesce(ksads_23_966_t, ksads2_23_926_t)  # Diagnosis - NoSuicidalIdeationOrBehaviorPast
     )

  # Create ever (past or present) variable
  ## SA
  suicide$SA_current_y <- apply(suicide[,which(grepl("ksads_23_(952|953|954)_9", colnames(suicide)))], 1, \(x) {any(x == 1)*1})
  suicide$SA_past_y <- apply(suicide[,which(grepl("ksads_23_(963|964|965)_9", colnames(suicide)))], 1, \(x) {any(x == 1)*1})
  suicide$SA_y <-(suicide$SA_current_y == 1 | suicide$SA_past_y == 1)*1

  ## SI: current or past = 1 & SA_y != 1 --> 1
  suicide$SI_current_y <- apply(suicide[,which(grepl("ksads_23_(946|947|948|949|950|951)_9", colnames(suicide)))], 1, \(x) {any(x == 1)*1})
  suicide$SI_past_y <- apply(suicide[,which(grepl("ksads_23_(957|958|959|960|961|962)_9", colnames(suicide)))], 1, \(x) {any(x == 1)*1})
  suicide$SI_y <- (suicide$SI_current_y == 1 | suicide$SI_past_y == 1)*1
  ## Those with SA_y == 1 will get SI_y == 0/NA depends on the project
  # suicide$SI_y[suicide$SA_y == 1] <- 0/NA

  ## nssi
  suicide$nssi_current_y <- suicide$ksads_23_945_905_t
  suicide$nssi_past_y <- suicide$ksads_23_956_916_t
  suicide$nssi_y <- (suicide$nssi_current_y == 1 | suicide$nssi_past_y == 1)*1

  ## no suicide - no range, all 0
  # suicide$nosui_current_y <- suicide$ksads_23_955_915_t
  # suicide$nosui_past_y <- suicide$ksads_23_966_926_t
  # suicide$nosui_y <- (suicide$nosui_current_y == 1 | suicide$nosui_past_y == 1)*1

  suicide <- suicide %>% select(matches("src|event|_y$"))

  # write_csv(suicide, "data/suicide.csv", na = "")
  # return("data/suicide.csv")
  return(suicide)

}


