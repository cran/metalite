## ---- echo = FALSE, message = FALSE, include = FALSE--------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  out.width = "100%"
)

## ---- message = FALSE---------------------------------------------------------
library(metalite)
library(r2rtf)
library(dplyr)

## -----------------------------------------------------------------------------
metadata <- meta_adam(
  population = r2rtf_adsl,
  observation = r2rtf_adae
)

## -----------------------------------------------------------------------------
plan <- plan(
  analysis = "ae_summary", population = "apat",
  observation = c("wk12", "wk24"), parameter = "any;rel;ser"
) |>
  add_plan(
    analysis = "ae_specific", population = "apat",
    observation = c("wk12", "wk24"),
    parameter = c("any", "aeosi", "rel", "ser")
  )

## -----------------------------------------------------------------------------
metadata <- metadata |> define_plan(plan)

## -----------------------------------------------------------------------------
metadata <- metadata |>
  define_population(
    name = "apat",
    group = "TRT01A",
    subset = SAFFL == "Y"
  ) |>
  define_observation(
    name = "wk12",
    group = "TRTA",
    subset = SAFFL == "Y",
    label = "Weeks 0 to 12"
  ) |>
  define_observation(
    name = "wk24",
    group = "TRTA",
    subset = AOCC01FL == "Y", # just for demo, another flag shall be used.
    label = "Weeks 0 to 24"
  )

## -----------------------------------------------------------------------------
metadata <- metadata |>
  define_parameter(
    name = "rel",
    subset = AEREL %in% c("POSSIBLE", "PROBABLE")
  ) |>
  define_parameter(
    name = "aeosi",
    subset = AEOSI == "Y",
    label = "adverse events of special interest"
  ) |>
  define_analysis(
    name = "ae_summary",
    title = "Summary of Adverse Events"
  ) |>
  define_analysis(
    name = "ae_specific",
    title = "Summary of Specific Adverse Events"
  )

## -----------------------------------------------------------------------------
metadata <- metadata |> meta_build()

## -----------------------------------------------------------------------------
ar_grid <- data.frame(
  title = spec_title(metadata),
  filename = spec_filename(metadata),
  function_name = metadata$plan$analysis,
  population = spec_analysis_population(metadata)
)

## -----------------------------------------------------------------------------
ar_grid |>
  mutate(across(everything(), ~ gsub("\n", "<br>", .x))) |>
  gt::gt() |>
  gt::fmt_markdown(columns = gt::everything()) |>
  gt::tab_options(table.font.size = 15)

