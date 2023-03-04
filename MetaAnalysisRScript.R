library(dplyr)
library(tidyr)
library(readxl)
library(readr)
library(psychmeta)

simplify_moderators <- function(.data, moderators) {
     if (any(.data$analysis_type == "Hierarchical Moderator")) {
          message("Results contain hierarchical moderator analyses. No reshaping performed.")
          return(.data)
     }
     if (! any(.data$analysis_type == "Simple Moderator")) {
          message("Results contain no moderator analyses. No reshaping performed.")
          return(.data)
     }
     overall <- as_tibble(.data) %>%
          dplyr::filter(analysis_type == "Overall") %>%
          mutate(moderator = "overall", level = "") %>%
          select(-any_of(moderators))
     mod <- as_tibble(.data) %>%
          dplyr::filter(analysis_type == "Simple Moderator") %>%
          na_if("All Levels") %>%
          pivot_longer(
               cols = any_of(moderators),
               names_to = "moderator",
               values_to = "level") %>%
          drop_na(level)
     out <- bind_rows(overall, mod) %>%
          arrange(analysis_id) %>%
          relocate(c(moderator, level), .after = analysis_type)
     if (! (inherits(.data, "ma_psychmeta") | inherits(.data, "ma_table"))) {
          out <- select(out, -analysis_type)
     }
     out <- psychmeta::reattribute(.data, out)
     return(out)
}

ma_dat <- read_excel("ForRPersonACE.xlsx")
moderators <- c("type")

ma_obj <- ma_r(rxyi = rxyi,
               n = n,
               sample_id = sample_id,
               rxx = rxxi,
               ryy = ryyi,
               construct_x = x_name,
               construct_y = y_name,
               moderators = moderators,
               ma_method = "ad",
               ad_method = "tsa",
               data = ma_dat) %>%
     simplify_moderators(moderators)
summary(ma_obj)


# Output summary table to Word
metabulate(ma_obj, "meta-analysis_results.docx")



# Extra stuff you might like

# Artifact distributions (with moderators)
AD_x <- get_ad(ma_obj)$ad$ad_x %>%
     filter(artifact %in% c("rxxi_drr", "qxi_drr"),
            analysis_type == "Overall") %>%
     select(artifact, construct_x, k_total, N_total, mean, var, var_res) %>%
     unique() %>%
     transmute(Construct = construct_x,
               artifact = recode(artifact, qxi_drr = "sqrt_rel", rxxi_drr = "rel"),
               k = k_total,
               N = N_total,
               mean = mean,
               sd_obs = sqrt(var),
               sd_res = sqrt(var_res)) %>%
     arrange(artifact) %>%
     pivot_wider(id_cols = c(Construct, k, N), names_from = artifact, values_from = c(mean, sd_obs, sd_res))
AD_y <- get_ad(ma_obj)$ad$ad_y %>%
     filter(artifact %in% c("rxxi_drr", "qxi_drr"),
            analysis_type == "Overall") %>%
     select(artifact, construct_y, k_total, N_total, mean, var, var_res) %>%
     unique() %>%
     transmute(Construct = construct_y,
               artifact = recode(artifact, qxi_drr = "sqrt_rel", rxxi_drr = "rel"),
               k = k_total,
               N = N_total,
               mean = mean,
               sd_obs = sqrt(var),
               sd_res = sqrt(var_res)) %>%
     arrange(artifact) %>%
     pivot_wider(id_cols = c(Construct, k, N), names_from = artifact, values_from = c(mean, sd_obs, sd_res))
AD <- bind_rows(AD_x, AD_y) %>%
     select(1:3, c(4, 6, 8), c(5, 7, 9)) %>%
     setNames(c("Construct", "k", "N", "Mean Rel.", "SD_obs Rel.", "SD_res Rel.", "Mean \u221A(Rel.)", "SD_obs \u221A(Rel.)", "SD_res \u221A(Rel.)"))
print(AD)
write_excel_csv(AD, "art_dists.csv")


# Additional heterogeneity stats (e.g., CI on SDrho)
ma_obj <- heterogeneity(ma_obj)

ma_het_res <- get_heterogeneity(ma_obj) %>%
     lapply(function(x) x$artifact_distribution$true_score)

# Table SDrho CIs
SD_rho_CI <- lapply(ma_het_res, function(x) as.list(x$HS_method$tau)) %>%
     data.table::rbindlist(idcol = "analysis_id") %>%
     as.data.frame() %>%
     separate(analysis_id, into = c(NA, "analysis_id"), sep = 13) %>%
     mutate(tau = ifelse(is.nan(tau), 0, tau),
            se = ifelse(is.nan(se), NA, se),
            analysis_id = as.integer(analysis_id)) %>%
     full_join(select(as.data.frame(ma_obj),
                      any_of(c("analysis_id", "pair_id", "construct_x", "construct_y", "analysis_type", "moderator", "level", moderators))),
               .,
               by = "analysis_id")
print(SD_rho_CI)
write_excel_csv(SD_rho_CI, "sd_rho_ci.csv")

