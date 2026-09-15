
# table_helpers.R
#
# This scripts helps generating the tables in APA style and export them in PNG
# 
# 
#   rq1_digital_content/02_engagement_model.R
#   rq2_electoral_grievances/02_grievance_model.R
#   rq3_congruence/table1_and_appendix.R
#
# Every script that uses this needs to start with:
#   source("R/table_helpers.R")


library(gt)
library(dplyr)

apa_table <- function(df, title, source = "Source: own elaboration.") {
  gt(df) |>
    tab_header(title = md(paste0("**", title, "**"))) |>
    tab_source_note(source_note = md(paste0("*", source, "*"))) |>
    opt_align_table_header(align = "left") |>
    tab_options(
      table.font.names = c("Times New Roman", "Times", "serif"),
      table.font.size = px(14),
      heading.title.font.size = px(15),
      heading.align = "left",
      source_notes.font.size = px(12),
      heading.border.bottom.style = "none",
      table.border.top.style = "none",
      table.border.bottom.style = "none",
      column_labels.border.top.style = "solid",
      column_labels.border.top.width = px(2),
      column_labels.border.top.color = "black",
      column_labels.border.bottom.style = "solid",
      column_labels.border.bottom.width = px(1),
      column_labels.border.bottom.color = "black",
      table_body.hlines.style = "none",
      table_body.border.bottom.style = "solid",
      table_body.border.bottom.width = px(2),
      table_body.border.bottom.color = "black",
      data_row.padding = px(4)
    )
}

save_png <- function(gt_tbl, file, output_dir = "appendix_tables") {
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  gt::gtsave(gt_tbl, filename = file.path(output_dir, file), zoom = 3, expand = 8)
}

formatear_con_estrellas <- function(estimate, p, decimales = 3) {
  estrellas <- dplyr::case_when(
    p < .001 ~ "***", p < .01 ~ "**", p < .05 ~ "*", p < .1 ~ "\u2020", TRUE ~ ""
  )
  paste0(sprintf(paste0("%.", decimales, "f"), estimate), estrellas)
}
