## ----echo = FALSE, message = FALSE, include = FALSE---------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  out.width = "100%"
)

## ----message = FALSE----------------------------------------------------------
library(metalite)

## -----------------------------------------------------------------------------
x <- adam_mapping(
  name = "apat",
  id = "USUBJID",
  group = "TRT01A",
  subset = TRTFL == "Y",
  label = "All Participants as Treated"
)

x

## -----------------------------------------------------------------------------
x$subset

## -----------------------------------------------------------------------------
x$var <- "AGE"
x$subset <- quote(SAFFL == "Y") # using quote for an expression

## -----------------------------------------------------------------------------
x

## -----------------------------------------------------------------------------
df <- r2rtf::r2rtf_adsl

## -----------------------------------------------------------------------------
ana <- df[eval(x$subset, df), ]

split(ana, ana[[x$group]]) |>
  sapply(function(y) mean(y[[x$var]]))

## ----eval = FALSE-------------------------------------------------------------
#  library(dplyr)
#  df |>
#    dplyr::filter(!!x$subset) |>
#    dplyr::group_by(.data[[x$group]]) |>
#    dplyr::summarise(mean = mean(.data[[x$var]]))

## -----------------------------------------------------------------------------
ser_default <- adam_mapping(
  name = "ser",
  label = "serious adverse events",
  subset = quote(AESER == "Y")
)

ser_default

## -----------------------------------------------------------------------------
ser_user <- adam_mapping(
  name = "ser",
  id = "USUBJID",
  group = "TRT01A",
  label = "serious{^a} adverse events",
  footnote = "{^a} this is a footnote"
)

## -----------------------------------------------------------------------------
ser_user

## -----------------------------------------------------------------------------
merge(ser_user, ser_default)

