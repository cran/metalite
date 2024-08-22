## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----message=FALSE, warning=FALSE---------------------------------------------
library(metalite)
library(r2rtf)
library(dplyr)

## -----------------------------------------------------------------------------
adae <- r2rtf::r2rtf_adae
adsl <- r2rtf::r2rtf_adsl |> rename(TRTA = TRT01A)
meta <- meta_adam(
  observation = adae,
  population = adsl
)

## -----------------------------------------------------------------------------
plan <- plan(
  analysis = "ae_summary", population = "apat",
  observation = "wk12", parameter = "any;rel;ser"
) |>
  add_plan(
    analysis = "ae_specific", population = "apat",
    observation = "wk12",
    parameter = c("any", "rel", "ser")
  )
plan

## -----------------------------------------------------------------------------
meta <- meta |> define_plan(plan)

## -----------------------------------------------------------------------------
meta <- meta |>
  define_population(
    name = "apat",
    group = "TRTA",
    group_order = c(
      "High Dose" = "Xanomeline High Dose",
      "Placebo" = "Placebo"
    ),
    subset = SAFFL == "Y"
  )

## -----------------------------------------------------------------------------
meta <- meta |>
  define_observation(
    name = "wk12",
    group = "TRTA",
    ae_var = "AEDECOD",
    var = c("AEREL", "AESER"),
    subset = SAFFL == "Y",
    label = "Weeks 0 to 12"
  )

## -----------------------------------------------------------------------------
meta <- meta |>
  define_parameter(
    name = "any",
    end_notes = c()
  ) |>
  define_parameter(
    name = "rel",
    subset = AEREL %in% c("POSSIBLE", "PROBABLE"),
    label = "drug-realted adverse events"
  ) |>
  define_parameter(
    name = "ser",
    subset = AESER == "Y",
    label = "serious adverse events",
    end_notes = c("Serious adverse events up to 90 days of last dose are included.")
  )

## -----------------------------------------------------------------------------
meta <- meta |>
  define_analysis(
    name = "ae_summary",
    title = "Summary of Adverse Events"
  )

## -----------------------------------------------------------------------------
meta <- meta |> meta_build()

## -----------------------------------------------------------------------------
meta$plan

## -----------------------------------------------------------------------------
meta

## -----------------------------------------------------------------------------
meta$plan <- meta$plan |>
  mutate(
    mockup = TRUE,
    output_report = paste0("./tlf/mock-", analysis, ".rtf")
  )
meta$plan

## -----------------------------------------------------------------------------
spec_call_program(meta,
  data_source = "[adam-adsl; adae]"
)

## -----------------------------------------------------------------------------
ae_summary <- function(meta,
                       population,
                       observation,
                       parameter,
                       mockup,
                       output_report,
                       data_source,
                       ...) {
  # Identify parameter keywords
  para_list <- unlist(strsplit(parameter, ";"))

  # Row label
  ae_label <- vapply(para_list,
    FUN = function(x) {
      collect_adam_mapping(meta, x)$label
    },
    FUN.VALUE = character(1)
  )

  # Get treatment grouping order
  trt_order <- eval(collect_adam_mapping(meta, population)$group_order)

  # Get the title/endnotes of TLF
  title_text <- collect_title(
    meta = meta,
    population = population,
    observation = observation,
    parameter = parameter, analysis = "ae_summary"
  )
  title_text[2] <- "{Week}"

  # Logic to create mockup table
  if (mockup) {
    # Generate RTF
    x <- tibble::tibble(
      ae_label = ae_label,
      n_1 = rep("x", length(ae_label)),
      n_2 = rep("x", length(ae_label)), # no display_total/CI
      pct_1 = rep("x.xx", length(ae_label)),
      pct_2 = rep("x.xx", length(ae_label))
    ) |>
      dplyr::select(ae_label, n_1, pct_1, n_2, pct_2, everything()) |>
      # r2rtf::rtf_title(rtf_mock_color(title_text)) |>
      r2rtf::rtf_title(title_text) |>
      r2rtf::rtf_colheader(paste0(" | ", paste(names(trt_order), collapse = " | "), " "),
        col_rel_width = c(3, rep(2, length(trt_order)))
      ) |>
      r2rtf::rtf_colheader(" | n | (%) | n | (%) ",
        border_top = c("", rep("single", 2 * length(trt_order))),
        border_bottom = "single",
        border_left = c("single", rep(c("single", ""), length(trt_order))),
        col_rel_width = c(3, rep(1, 2 * length(trt_order)))
      ) |>
      r2rtf::rtf_body(
        col_rel_width = c(3, rep(1, 2 * length(trt_order))),
        border_left = c("single", rep(c("single", ""), length(trt_order))),
        text_justification = c("l", rep("c", 2 * length(trt_order)))
      ) |>
      r2rtf::rtf_source(data_source)

    # Require r2rtf to use color
    attr(x, "page")$use_color <- TRUE

    # Save RTF to a path
    if (!is.null(output_report)) {
      x |>
        r2rtf::rtf_encode() |>
        r2rtf::write_rtf(output_report)
    }
  }

  if (!mockup) {
    # Logic to perform actual AE summary table.
    # Omit here
  }

  output_report
}

## -----------------------------------------------------------------------------
meta_run(meta,
  i = 1,
  data_source = "[adam-adsl; adae]"
)

## ----eval = FALSE, echo=FALSE-------------------------------------------------
#  # Convert RTF to PDF
#  r2rtf:::rtf_convert_format(
#    input = "tlf/mock-ae_summary.rtf",
#    output_dir = "tlf/",
#    format = "pdf"
#  )

## ----out.width = "100%", out.height = "400px", echo = FALSE, fig.align = "center"----
knitr::include_graphics("tlf/mock-ae_summary.pdf")

