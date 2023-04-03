figure 1 - weight and length
================
Bradley Demarest
2023-03-03

``` r
library(data.table)
library(ggplot2)
library(tidyverse)
library(readxl)
library(gt)
library(here)
library(stringr)
library(patchwork)
```

``` r
# Load data from excel sheet.
# 

tab = readxl::read_excel(path=here("figure_1",
  "summary_weight_length_data_jag2b_6wpf_to_2yrold_20230303bld.xlsx"),
  na="NA")

tab = as.data.table(tab)

# Standardize all genotypes to uppercase.
tab[, genotype:=toupper(genotype)]

# Use simple genotype labels (+/+, +/-, etc.)
convert_genotype_labels = c(WT="+/+", HET="+/-", MUT="-/-")
tab[, genotype2:=convert_genotype_labels[genotype]]
# Set factor and level order of 'genotype2' column.
tab[, genotype2:=factor(genotype2, levels=convert_genotype_labels)]


# Change age labels to format "6 weeks", "6 months", etc..    
tab[, age2:=str_replace(age, "month", " months")]
tab[, age2:=str_replace(age2, "wpf", " weeks")]

# Set factor order of age2 column, for plotting.
age_order_vec = c(paste(c(6, 8, 10, 12), "weeks"),
                  paste(c(4, 6, 8, 10, 13, 24), "months"))
tab[, age2:=factor(age2, levels=age_order_vec)]

# Convert dob to character.
tab[, dob:=as.character(dob)]
```

``` r
# Table showing sample size for all time points, genotypes.

dcast(data=tab, genotype2 ~ age2, fun.aggregate=length) %>% 
  as_tibble() %>%
  gt() %>%
  tab_header(title="1. jag2b fish counts by timepoint and genotype.")
```

<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#hhbqqdsrhf .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  /* table.margin.left */
  margin-right: auto;
  /* table.margin.right */
  color: #333333;
  font-size: 16px;
  /* table.font.size */
  background-color: #FFFFFF;
  /* table.background.color */
  width: auto;
  /* table.width */
  border-top-style: solid;
  /* table.border.top.style */
  border-top-width: 2px;
  /* table.border.top.width */
  border-top-color: #A8A8A8;
  /* table.border.top.color */
  border-bottom-style: solid;
  /* table.border.bottom.style */
  border-bottom-width: 2px;
  /* table.border.bottom.width */
  border-bottom-color: #A8A8A8;
  /* table.border.bottom.color */
}

#hhbqqdsrhf .gt_heading {
  background-color: #FFFFFF;
  /* heading.background.color */
  border-bottom-color: #FFFFFF;
  /* table.background.color */
  border-left-style: hidden;
  /* heading.border.lr.style */
  border-left-width: 1px;
  /* heading.border.lr.width */
  border-left-color: #D3D3D3;
  /* heading.border.lr.color */
  border-right-style: hidden;
  /* heading.border.lr.style */
  border-right-width: 1px;
  /* heading.border.lr.width */
  border-right-color: #D3D3D3;
  /* heading.border.lr.color */
}

#hhbqqdsrhf .gt_title {
  color: #333333;
  font-size: 125%;
  /* heading.title.font.size */
  font-weight: initial;
  /* heading.title.font.weight */
  padding-top: 4px;
  /* heading.top.padding - not yet used */
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  /* table.background.color */
  border-bottom-width: 0;
}

#hhbqqdsrhf .gt_subtitle {
  color: #333333;
  font-size: 85%;
  /* heading.subtitle.font.size */
  font-weight: initial;
  /* heading.subtitle.font.weight */
  padding-top: 0;
  padding-bottom: 4px;
  /* heading.bottom.padding - not yet used */
  border-top-color: #FFFFFF;
  /* table.background.color */
  border-top-width: 0;
}

#hhbqqdsrhf .gt_bottom_border {
  border-bottom-style: solid;
  /* heading.border.bottom.style */
  border-bottom-width: 2px;
  /* heading.border.bottom.width */
  border-bottom-color: #D3D3D3;
  /* heading.border.bottom.color */
}

#hhbqqdsrhf .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  padding-top: 4px;
  padding-bottom: 4px;
}

#hhbqqdsrhf .gt_col_headings {
  border-top-style: solid;
  /* column_labels.border.top.style */
  border-top-width: 2px;
  /* column_labels.border.top.width */
  border-top-color: #D3D3D3;
  /* column_labels.border.top.color */
  border-bottom-style: solid;
  /* column_labels.border.bottom.style */
  border-bottom-width: 2px;
  /* column_labels.border.bottom.width */
  border-bottom-color: #D3D3D3;
  /* column_labels.border.bottom.color */
  border-left-style: none;
  /* column_labels.border.lr.style */
  border-left-width: 1px;
  /* column_labels.border.lr.width */
  border-left-color: #D3D3D3;
  /* column_labels.border.lr.color */
  border-right-style: none;
  /* column_labels.border.lr.style */
  border-right-width: 1px;
  /* column_labels.border.lr.width */
  border-right-color: #D3D3D3;
  /* column_labels.border.lr.color */
}

#hhbqqdsrhf .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  /* column_labels.background.color */
  font-size: 100%;
  /* column_labels.font.size */
  font-weight: normal;
  /* column_labels.font.weight */
  text-transform: inherit;
  /* column_labels.text_transform */
  vertical-align: middle;
  padding: 5px;
  margin: 10px;
  overflow-x: hidden;
}

#hhbqqdsrhf .gt_sep_right {
  border-right: 5px solid #FFFFFF;
}

#hhbqqdsrhf .gt_group_heading {
  padding: 8px;
  /* row_group.padding */
  color: #333333;
  background-color: #FFFFFF;
  /* row_group.background.color */
  font-size: 100%;
  /* row_group.font.size */
  font-weight: initial;
  /* row_group.font.weight */
  text-transform: inherit;
  /* row_group.text_transform */
  border-top-style: solid;
  /* row_group.border.top.style */
  border-top-width: 2px;
  /* row_group.border.top.width */
  border-top-color: #D3D3D3;
  /* row_group.border.top.color */
  border-bottom-style: solid;
  /* row_group.border.bottom.style */
  border-bottom-width: 2px;
  /* row_group.border.bottom.width */
  border-bottom-color: #D3D3D3;
  /* row_group.border.bottom.color */
  border-left-style: none;
  /* row_group.border.left.style */
  border-left-width: 1px;
  /* row_group.border.left.width */
  border-left-color: #D3D3D3;
  /* row_group.border.left.color */
  border-right-style: none;
  /* row_group.border.right.style */
  border-right-width: 1px;
  /* row_group.border.right.width */
  border-right-color: #D3D3D3;
  /* row_group.border.right.color */
  vertical-align: middle;
}

#hhbqqdsrhf .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  /* row_group.background.color */
  font-size: 100%;
  /* row_group.font.size */
  font-weight: initial;
  /* row_group.font.weight */
  border-top-style: solid;
  /* row_group.border.top.style */
  border-top-width: 2px;
  /* row_group.border.top.width */
  border-top-color: #D3D3D3;
  /* row_group.border.top.color */
  border-bottom-style: solid;
  /* row_group.border.bottom.style */
  border-bottom-width: 2px;
  /* row_group.border.bottom.width */
  border-bottom-color: #D3D3D3;
  /* row_group.border.bottom.color */
  vertical-align: middle;
}

#hhbqqdsrhf .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
  /* row.striping.background_color */
}

#hhbqqdsrhf .gt_from_md > :first-child {
  margin-top: 0;
}

#hhbqqdsrhf .gt_from_md > :last-child {
  margin-bottom: 0;
}

#hhbqqdsrhf .gt_row {
  padding-top: 8px;
  /* data_row.padding */
  padding-bottom: 8px;
  /* data_row.padding */
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  /* table_body.hlines.style */
  border-top-width: 1px;
  /* table_body.hlines.width */
  border-top-color: #D3D3D3;
  /* table_body.hlines.color */
  border-left-style: none;
  /* table_body.vlines.style */
  border-left-width: 1px;
  /* table_body.vlines.width */
  border-left-color: #D3D3D3;
  /* table_body.vlines.color */
  border-right-style: none;
  /* table_body.vlines.style */
  border-right-width: 1px;
  /* table_body.vlines.width */
  border-right-color: #D3D3D3;
  /* table_body.vlines.color */
  vertical-align: middle;
  overflow-x: hidden;
}

#hhbqqdsrhf .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  /* stub.background.color */
  font-weight: initial;
  /* stub.font.weight */
  text-transform: inherit;
  /* stub.text_transform */
  border-right-style: solid;
  /* stub.border.style */
  border-right-width: 2px;
  /* stub.border.width */
  border-right-color: #D3D3D3;
  /* stub.border.color */
  padding-left: 12px;
}

#hhbqqdsrhf .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  /* summary_row.background.color */
  text-transform: inherit;
  /* summary_row.text_transform */
  padding-top: 8px;
  /* summary_row.padding */
  padding-bottom: 8px;
  /* summary_row.padding */
  padding-left: 5px;
  padding-right: 5px;
}

#hhbqqdsrhf .gt_first_summary_row {
  padding-top: 8px;
  /* summary_row.padding */
  padding-bottom: 8px;
  /* summary_row.padding */
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  /* summary_row.border.style */
  border-top-width: 2px;
  /* summary_row.border.width */
  border-top-color: #D3D3D3;
  /* summary_row.border.color */
}

#hhbqqdsrhf .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  /* grand_summary_row.background.color */
  text-transform: inherit;
  /* grand_summary_row.text_transform */
  padding-top: 8px;
  /* grand_summary_row.padding */
  padding-bottom: 8px;
  /* grand_summary_row.padding */
  padding-left: 5px;
  padding-right: 5px;
}

#hhbqqdsrhf .gt_first_grand_summary_row {
  padding-top: 8px;
  /* grand_summary_row.padding */
  padding-bottom: 8px;
  /* grand_summary_row.padding */
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  /* grand_summary_row.border.style */
  border-top-width: 6px;
  /* grand_summary_row.border.width */
  border-top-color: #D3D3D3;
  /* grand_summary_row.border.color */
}

#hhbqqdsrhf .gt_table_body {
  border-top-style: solid;
  /* table_body.border.top.style */
  border-top-width: 2px;
  /* table_body.border.top.width */
  border-top-color: #D3D3D3;
  /* table_body.border.top.color */
  border-bottom-style: solid;
  /* table_body.border.bottom.style */
  border-bottom-width: 2px;
  /* table_body.border.bottom.width */
  border-bottom-color: #D3D3D3;
  /* table_body.border.bottom.color */
}

#hhbqqdsrhf .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  /* footnotes.background.color */
  border-bottom-style: none;
  /* footnotes.border.bottom.style */
  border-bottom-width: 2px;
  /* footnotes.border.bottom.width */
  border-bottom-color: #D3D3D3;
  /* footnotes.border.bottom.color */
  border-left-style: none;
  /* footnotes.border.lr.color */
  border-left-width: 2px;
  /* footnotes.border.lr.color */
  border-left-color: #D3D3D3;
  /* footnotes.border.lr.color */
  border-right-style: none;
  /* footnotes.border.lr.color */
  border-right-width: 2px;
  /* footnotes.border.lr.color */
  border-right-color: #D3D3D3;
  /* footnotes.border.lr.color */
}

#hhbqqdsrhf .gt_footnote {
  margin: 0px;
  font-size: 90%;
  /* footnotes.font.size */
  padding: 4px;
  /* footnotes.padding */
}

#hhbqqdsrhf .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  /* source_notes.background.color */
  border-bottom-style: none;
  /* source_notes.border.bottom.style */
  border-bottom-width: 2px;
  /* source_notes.border.bottom.width */
  border-bottom-color: #D3D3D3;
  /* source_notes.border.bottom.color */
  border-left-style: none;
  /* source_notes.border.lr.style */
  border-left-width: 2px;
  /* source_notes.border.lr.style */
  border-left-color: #D3D3D3;
  /* source_notes.border.lr.style */
  border-right-style: none;
  /* source_notes.border.lr.style */
  border-right-width: 2px;
  /* source_notes.border.lr.style */
  border-right-color: #D3D3D3;
  /* source_notes.border.lr.style */
}

#hhbqqdsrhf .gt_sourcenote {
  font-size: 90%;
  /* source_notes.font.size */
  padding: 4px;
  /* source_notes.padding */
}

#hhbqqdsrhf .gt_left {
  text-align: left;
}

#hhbqqdsrhf .gt_center {
  text-align: center;
}

#hhbqqdsrhf .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#hhbqqdsrhf .gt_font_normal {
  font-weight: normal;
}

#hhbqqdsrhf .gt_font_bold {
  font-weight: bold;
}

#hhbqqdsrhf .gt_font_italic {
  font-style: italic;
}

#hhbqqdsrhf .gt_super {
  font-size: 65%;
}

#hhbqqdsrhf .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>
<div id="hhbqqdsrhf" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;"><table class="gt_table">
  <thead class="gt_header">
    <tr>
      <th colspan="11" class="gt_heading gt_title gt_font_normal gt_center" style>1. jag2b fish counts by timepoint and genotype.</th>
    </tr>
    <tr>
      <th colspan="11" class="gt_heading gt_subtitle gt_font_normal gt_center gt_bottom_border" style></th>
    </tr>
  </thead>
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">genotype2</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">6 weeks</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">8 weeks</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">10 weeks</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">12 weeks</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">4 months</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">6 months</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">8 months</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">10 months</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">13 months</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">24 months</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr>
      <td class="gt_row gt_center">+/+</td>
      <td class="gt_row gt_center">10</td>
      <td class="gt_row gt_center">8</td>
      <td class="gt_row gt_center">8</td>
      <td class="gt_row gt_center">3</td>
      <td class="gt_row gt_center">16</td>
      <td class="gt_row gt_center">16</td>
      <td class="gt_row gt_center">8</td>
      <td class="gt_row gt_center">4</td>
      <td class="gt_row gt_center">10</td>
      <td class="gt_row gt_center">11</td>
    </tr>
    <tr>
      <td class="gt_row gt_center gt_striped">+/-</td>
      <td class="gt_row gt_center gt_striped">13</td>
      <td class="gt_row gt_center gt_striped">8</td>
      <td class="gt_row gt_center gt_striped">8</td>
      <td class="gt_row gt_center gt_striped">3</td>
      <td class="gt_row gt_center gt_striped">18</td>
      <td class="gt_row gt_center gt_striped">10</td>
      <td class="gt_row gt_center gt_striped">5</td>
      <td class="gt_row gt_center gt_striped">6</td>
      <td class="gt_row gt_center gt_striped">8</td>
      <td class="gt_row gt_center gt_striped">13</td>
    </tr>
    <tr>
      <td class="gt_row gt_center">-/-</td>
      <td class="gt_row gt_center">12</td>
      <td class="gt_row gt_center">8</td>
      <td class="gt_row gt_center">7</td>
      <td class="gt_row gt_center">3</td>
      <td class="gt_row gt_center">16</td>
      <td class="gt_row gt_center">9</td>
      <td class="gt_row gt_center">3</td>
      <td class="gt_row gt_center">7</td>
      <td class="gt_row gt_center">6</td>
      <td class="gt_row gt_center">0</td>
    </tr>
  </tbody>
  
  
</table></div>

``` r
# Table for all time points and sexes, grouped by genotype.

dcast(data=tab, sex + genotype2 ~ age2, fun.aggregate=length) %>% 
  as_tibble() %>%
  group_by(genotype2) %>%
  gt() %>%
  tab_header(title="2. jag2b fish counts by timepoint, genotype, and sex") %>%
  tab_source_note(
    source_note=md("Data modifications: For plots and stats analysis, we will
    
    1. Combine males and females for 6- to 12-week timepoints, changing sex to \"unknown\" for these fish.
    2. Discard females for timepoints 10- to 24-month.
    
    Rationale: Determining sex for 6- to 12-week old fish is difficult and error prone.
    Sex differences in weight and length are generally minimal at these ages,
    and jag2b mutants are impossible to sex.
    The number of females of ages 10- to 24-months is too small to allow
    adding sex to the ANOVA model."))
```

<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#rjvemecial .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  /* table.margin.left */
  margin-right: auto;
  /* table.margin.right */
  color: #333333;
  font-size: 16px;
  /* table.font.size */
  background-color: #FFFFFF;
  /* table.background.color */
  width: auto;
  /* table.width */
  border-top-style: solid;
  /* table.border.top.style */
  border-top-width: 2px;
  /* table.border.top.width */
  border-top-color: #A8A8A8;
  /* table.border.top.color */
  border-bottom-style: solid;
  /* table.border.bottom.style */
  border-bottom-width: 2px;
  /* table.border.bottom.width */
  border-bottom-color: #A8A8A8;
  /* table.border.bottom.color */
}

#rjvemecial .gt_heading {
  background-color: #FFFFFF;
  /* heading.background.color */
  border-bottom-color: #FFFFFF;
  /* table.background.color */
  border-left-style: hidden;
  /* heading.border.lr.style */
  border-left-width: 1px;
  /* heading.border.lr.width */
  border-left-color: #D3D3D3;
  /* heading.border.lr.color */
  border-right-style: hidden;
  /* heading.border.lr.style */
  border-right-width: 1px;
  /* heading.border.lr.width */
  border-right-color: #D3D3D3;
  /* heading.border.lr.color */
}

#rjvemecial .gt_title {
  color: #333333;
  font-size: 125%;
  /* heading.title.font.size */
  font-weight: initial;
  /* heading.title.font.weight */
  padding-top: 4px;
  /* heading.top.padding - not yet used */
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  /* table.background.color */
  border-bottom-width: 0;
}

#rjvemecial .gt_subtitle {
  color: #333333;
  font-size: 85%;
  /* heading.subtitle.font.size */
  font-weight: initial;
  /* heading.subtitle.font.weight */
  padding-top: 0;
  padding-bottom: 4px;
  /* heading.bottom.padding - not yet used */
  border-top-color: #FFFFFF;
  /* table.background.color */
  border-top-width: 0;
}

#rjvemecial .gt_bottom_border {
  border-bottom-style: solid;
  /* heading.border.bottom.style */
  border-bottom-width: 2px;
  /* heading.border.bottom.width */
  border-bottom-color: #D3D3D3;
  /* heading.border.bottom.color */
}

#rjvemecial .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  padding-top: 4px;
  padding-bottom: 4px;
}

#rjvemecial .gt_col_headings {
  border-top-style: solid;
  /* column_labels.border.top.style */
  border-top-width: 2px;
  /* column_labels.border.top.width */
  border-top-color: #D3D3D3;
  /* column_labels.border.top.color */
  border-bottom-style: solid;
  /* column_labels.border.bottom.style */
  border-bottom-width: 2px;
  /* column_labels.border.bottom.width */
  border-bottom-color: #D3D3D3;
  /* column_labels.border.bottom.color */
  border-left-style: none;
  /* column_labels.border.lr.style */
  border-left-width: 1px;
  /* column_labels.border.lr.width */
  border-left-color: #D3D3D3;
  /* column_labels.border.lr.color */
  border-right-style: none;
  /* column_labels.border.lr.style */
  border-right-width: 1px;
  /* column_labels.border.lr.width */
  border-right-color: #D3D3D3;
  /* column_labels.border.lr.color */
}

#rjvemecial .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  /* column_labels.background.color */
  font-size: 100%;
  /* column_labels.font.size */
  font-weight: normal;
  /* column_labels.font.weight */
  text-transform: inherit;
  /* column_labels.text_transform */
  vertical-align: middle;
  padding: 5px;
  margin: 10px;
  overflow-x: hidden;
}

#rjvemecial .gt_sep_right {
  border-right: 5px solid #FFFFFF;
}

#rjvemecial .gt_group_heading {
  padding: 8px;
  /* row_group.padding */
  color: #333333;
  background-color: #FFFFFF;
  /* row_group.background.color */
  font-size: 100%;
  /* row_group.font.size */
  font-weight: initial;
  /* row_group.font.weight */
  text-transform: inherit;
  /* row_group.text_transform */
  border-top-style: solid;
  /* row_group.border.top.style */
  border-top-width: 2px;
  /* row_group.border.top.width */
  border-top-color: #D3D3D3;
  /* row_group.border.top.color */
  border-bottom-style: solid;
  /* row_group.border.bottom.style */
  border-bottom-width: 2px;
  /* row_group.border.bottom.width */
  border-bottom-color: #D3D3D3;
  /* row_group.border.bottom.color */
  border-left-style: none;
  /* row_group.border.left.style */
  border-left-width: 1px;
  /* row_group.border.left.width */
  border-left-color: #D3D3D3;
  /* row_group.border.left.color */
  border-right-style: none;
  /* row_group.border.right.style */
  border-right-width: 1px;
  /* row_group.border.right.width */
  border-right-color: #D3D3D3;
  /* row_group.border.right.color */
  vertical-align: middle;
}

#rjvemecial .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  /* row_group.background.color */
  font-size: 100%;
  /* row_group.font.size */
  font-weight: initial;
  /* row_group.font.weight */
  border-top-style: solid;
  /* row_group.border.top.style */
  border-top-width: 2px;
  /* row_group.border.top.width */
  border-top-color: #D3D3D3;
  /* row_group.border.top.color */
  border-bottom-style: solid;
  /* row_group.border.bottom.style */
  border-bottom-width: 2px;
  /* row_group.border.bottom.width */
  border-bottom-color: #D3D3D3;
  /* row_group.border.bottom.color */
  vertical-align: middle;
}

#rjvemecial .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
  /* row.striping.background_color */
}

#rjvemecial .gt_from_md > :first-child {
  margin-top: 0;
}

#rjvemecial .gt_from_md > :last-child {
  margin-bottom: 0;
}

#rjvemecial .gt_row {
  padding-top: 8px;
  /* data_row.padding */
  padding-bottom: 8px;
  /* data_row.padding */
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  /* table_body.hlines.style */
  border-top-width: 1px;
  /* table_body.hlines.width */
  border-top-color: #D3D3D3;
  /* table_body.hlines.color */
  border-left-style: none;
  /* table_body.vlines.style */
  border-left-width: 1px;
  /* table_body.vlines.width */
  border-left-color: #D3D3D3;
  /* table_body.vlines.color */
  border-right-style: none;
  /* table_body.vlines.style */
  border-right-width: 1px;
  /* table_body.vlines.width */
  border-right-color: #D3D3D3;
  /* table_body.vlines.color */
  vertical-align: middle;
  overflow-x: hidden;
}

#rjvemecial .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  /* stub.background.color */
  font-weight: initial;
  /* stub.font.weight */
  text-transform: inherit;
  /* stub.text_transform */
  border-right-style: solid;
  /* stub.border.style */
  border-right-width: 2px;
  /* stub.border.width */
  border-right-color: #D3D3D3;
  /* stub.border.color */
  padding-left: 12px;
}

#rjvemecial .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  /* summary_row.background.color */
  text-transform: inherit;
  /* summary_row.text_transform */
  padding-top: 8px;
  /* summary_row.padding */
  padding-bottom: 8px;
  /* summary_row.padding */
  padding-left: 5px;
  padding-right: 5px;
}

#rjvemecial .gt_first_summary_row {
  padding-top: 8px;
  /* summary_row.padding */
  padding-bottom: 8px;
  /* summary_row.padding */
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  /* summary_row.border.style */
  border-top-width: 2px;
  /* summary_row.border.width */
  border-top-color: #D3D3D3;
  /* summary_row.border.color */
}

#rjvemecial .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  /* grand_summary_row.background.color */
  text-transform: inherit;
  /* grand_summary_row.text_transform */
  padding-top: 8px;
  /* grand_summary_row.padding */
  padding-bottom: 8px;
  /* grand_summary_row.padding */
  padding-left: 5px;
  padding-right: 5px;
}

#rjvemecial .gt_first_grand_summary_row {
  padding-top: 8px;
  /* grand_summary_row.padding */
  padding-bottom: 8px;
  /* grand_summary_row.padding */
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  /* grand_summary_row.border.style */
  border-top-width: 6px;
  /* grand_summary_row.border.width */
  border-top-color: #D3D3D3;
  /* grand_summary_row.border.color */
}

#rjvemecial .gt_table_body {
  border-top-style: solid;
  /* table_body.border.top.style */
  border-top-width: 2px;
  /* table_body.border.top.width */
  border-top-color: #D3D3D3;
  /* table_body.border.top.color */
  border-bottom-style: solid;
  /* table_body.border.bottom.style */
  border-bottom-width: 2px;
  /* table_body.border.bottom.width */
  border-bottom-color: #D3D3D3;
  /* table_body.border.bottom.color */
}

#rjvemecial .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  /* footnotes.background.color */
  border-bottom-style: none;
  /* footnotes.border.bottom.style */
  border-bottom-width: 2px;
  /* footnotes.border.bottom.width */
  border-bottom-color: #D3D3D3;
  /* footnotes.border.bottom.color */
  border-left-style: none;
  /* footnotes.border.lr.color */
  border-left-width: 2px;
  /* footnotes.border.lr.color */
  border-left-color: #D3D3D3;
  /* footnotes.border.lr.color */
  border-right-style: none;
  /* footnotes.border.lr.color */
  border-right-width: 2px;
  /* footnotes.border.lr.color */
  border-right-color: #D3D3D3;
  /* footnotes.border.lr.color */
}

#rjvemecial .gt_footnote {
  margin: 0px;
  font-size: 90%;
  /* footnotes.font.size */
  padding: 4px;
  /* footnotes.padding */
}

#rjvemecial .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  /* source_notes.background.color */
  border-bottom-style: none;
  /* source_notes.border.bottom.style */
  border-bottom-width: 2px;
  /* source_notes.border.bottom.width */
  border-bottom-color: #D3D3D3;
  /* source_notes.border.bottom.color */
  border-left-style: none;
  /* source_notes.border.lr.style */
  border-left-width: 2px;
  /* source_notes.border.lr.style */
  border-left-color: #D3D3D3;
  /* source_notes.border.lr.style */
  border-right-style: none;
  /* source_notes.border.lr.style */
  border-right-width: 2px;
  /* source_notes.border.lr.style */
  border-right-color: #D3D3D3;
  /* source_notes.border.lr.style */
}

#rjvemecial .gt_sourcenote {
  font-size: 90%;
  /* source_notes.font.size */
  padding: 4px;
  /* source_notes.padding */
}

#rjvemecial .gt_left {
  text-align: left;
}

#rjvemecial .gt_center {
  text-align: center;
}

#rjvemecial .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#rjvemecial .gt_font_normal {
  font-weight: normal;
}

#rjvemecial .gt_font_bold {
  font-weight: bold;
}

#rjvemecial .gt_font_italic {
  font-style: italic;
}

#rjvemecial .gt_super {
  font-size: 65%;
}

#rjvemecial .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>
<div id="rjvemecial" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;"><table class="gt_table">
  <thead class="gt_header">
    <tr>
      <th colspan="11" class="gt_heading gt_title gt_font_normal gt_center" style>2. jag2b fish counts by timepoint, genotype, and sex</th>
    </tr>
    <tr>
      <th colspan="11" class="gt_heading gt_subtitle gt_font_normal gt_center gt_bottom_border" style></th>
    </tr>
  </thead>
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">sex</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">6 weeks</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">8 weeks</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">10 weeks</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">12 weeks</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">4 months</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">6 months</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">8 months</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">10 months</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">13 months</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">24 months</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr class="gt_group_heading_row">
      <td colspan="11" class="gt_group_heading">+/+</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">F</td>
      <td class="gt_row gt_center">2</td>
      <td class="gt_row gt_center">7</td>
      <td class="gt_row gt_center">4</td>
      <td class="gt_row gt_center">2</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">1</td>
      <td class="gt_row gt_center">2</td>
      <td class="gt_row gt_center">2</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_striped">M</td>
      <td class="gt_row gt_center gt_striped">8</td>
      <td class="gt_row gt_center gt_striped">1</td>
      <td class="gt_row gt_center gt_striped">4</td>
      <td class="gt_row gt_center gt_striped">1</td>
      <td class="gt_row gt_center gt_striped">16</td>
      <td class="gt_row gt_center gt_striped">16</td>
      <td class="gt_row gt_center gt_striped">8</td>
      <td class="gt_row gt_center gt_striped">3</td>
      <td class="gt_row gt_center gt_striped">8</td>
      <td class="gt_row gt_center gt_striped">9</td>
    </tr>
    <tr class="gt_group_heading_row">
      <td colspan="11" class="gt_group_heading">+/-</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">F</td>
      <td class="gt_row gt_center">3</td>
      <td class="gt_row gt_center">4</td>
      <td class="gt_row gt_center">6</td>
      <td class="gt_row gt_center">1</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">2</td>
      <td class="gt_row gt_center">3</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_striped">M</td>
      <td class="gt_row gt_center gt_striped">10</td>
      <td class="gt_row gt_center gt_striped">4</td>
      <td class="gt_row gt_center gt_striped">2</td>
      <td class="gt_row gt_center gt_striped">2</td>
      <td class="gt_row gt_center gt_striped">18</td>
      <td class="gt_row gt_center gt_striped">10</td>
      <td class="gt_row gt_center gt_striped">5</td>
      <td class="gt_row gt_center gt_striped">6</td>
      <td class="gt_row gt_center gt_striped">6</td>
      <td class="gt_row gt_center gt_striped">10</td>
    </tr>
    <tr class="gt_group_heading_row">
      <td colspan="11" class="gt_group_heading">-/-</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">F</td>
      <td class="gt_row gt_center">3</td>
      <td class="gt_row gt_center">1</td>
      <td class="gt_row gt_center">1</td>
      <td class="gt_row gt_center">1</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">2</td>
      <td class="gt_row gt_center">0</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_striped">M</td>
      <td class="gt_row gt_center gt_striped">9</td>
      <td class="gt_row gt_center gt_striped">7</td>
      <td class="gt_row gt_center gt_striped">6</td>
      <td class="gt_row gt_center gt_striped">2</td>
      <td class="gt_row gt_center gt_striped">16</td>
      <td class="gt_row gt_center gt_striped">9</td>
      <td class="gt_row gt_center gt_striped">3</td>
      <td class="gt_row gt_center gt_striped">7</td>
      <td class="gt_row gt_center gt_striped">4</td>
      <td class="gt_row gt_center gt_striped">0</td>
    </tr>
  </tbody>
  <tfoot class="gt_sourcenotes">
    <tr>
      <td class="gt_sourcenote" colspan="11">Data modifications: For plots and stats analysis, we will</p>
<pre><code>1. Combine males and females for 6- to 12-week timepoints, changing sex to &quot;unknown&quot; for these fish.
2. Discard females for timepoints 10- to 24-month.

Rationale: Determining sex for 6- to 12-week old fish is difficult and error prone.
Sex differences in weight and length are generally minimal at these ages,
and jag2b mutants are impossible to sex.
The number of females of ages 10- to 24-months is too small to allow
adding sex to the ANOVA model.
</code></pre>
</td>
    </tr>
  </tfoot>
  
</table></div>

``` r
# Table dob batches, by time point.

dcast(data=tab, dob ~ age2, fun.aggregate=length) %>% 
  as_tibble() %>%
  gt() %>%
  tab_header(title="jag2b fish counts by timepoint and date-of-birth.")
```

<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#lippbrsxac .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  /* table.margin.left */
  margin-right: auto;
  /* table.margin.right */
  color: #333333;
  font-size: 16px;
  /* table.font.size */
  background-color: #FFFFFF;
  /* table.background.color */
  width: auto;
  /* table.width */
  border-top-style: solid;
  /* table.border.top.style */
  border-top-width: 2px;
  /* table.border.top.width */
  border-top-color: #A8A8A8;
  /* table.border.top.color */
  border-bottom-style: solid;
  /* table.border.bottom.style */
  border-bottom-width: 2px;
  /* table.border.bottom.width */
  border-bottom-color: #A8A8A8;
  /* table.border.bottom.color */
}

#lippbrsxac .gt_heading {
  background-color: #FFFFFF;
  /* heading.background.color */
  border-bottom-color: #FFFFFF;
  /* table.background.color */
  border-left-style: hidden;
  /* heading.border.lr.style */
  border-left-width: 1px;
  /* heading.border.lr.width */
  border-left-color: #D3D3D3;
  /* heading.border.lr.color */
  border-right-style: hidden;
  /* heading.border.lr.style */
  border-right-width: 1px;
  /* heading.border.lr.width */
  border-right-color: #D3D3D3;
  /* heading.border.lr.color */
}

#lippbrsxac .gt_title {
  color: #333333;
  font-size: 125%;
  /* heading.title.font.size */
  font-weight: initial;
  /* heading.title.font.weight */
  padding-top: 4px;
  /* heading.top.padding - not yet used */
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  /* table.background.color */
  border-bottom-width: 0;
}

#lippbrsxac .gt_subtitle {
  color: #333333;
  font-size: 85%;
  /* heading.subtitle.font.size */
  font-weight: initial;
  /* heading.subtitle.font.weight */
  padding-top: 0;
  padding-bottom: 4px;
  /* heading.bottom.padding - not yet used */
  border-top-color: #FFFFFF;
  /* table.background.color */
  border-top-width: 0;
}

#lippbrsxac .gt_bottom_border {
  border-bottom-style: solid;
  /* heading.border.bottom.style */
  border-bottom-width: 2px;
  /* heading.border.bottom.width */
  border-bottom-color: #D3D3D3;
  /* heading.border.bottom.color */
}

#lippbrsxac .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  padding-top: 4px;
  padding-bottom: 4px;
}

#lippbrsxac .gt_col_headings {
  border-top-style: solid;
  /* column_labels.border.top.style */
  border-top-width: 2px;
  /* column_labels.border.top.width */
  border-top-color: #D3D3D3;
  /* column_labels.border.top.color */
  border-bottom-style: solid;
  /* column_labels.border.bottom.style */
  border-bottom-width: 2px;
  /* column_labels.border.bottom.width */
  border-bottom-color: #D3D3D3;
  /* column_labels.border.bottom.color */
  border-left-style: none;
  /* column_labels.border.lr.style */
  border-left-width: 1px;
  /* column_labels.border.lr.width */
  border-left-color: #D3D3D3;
  /* column_labels.border.lr.color */
  border-right-style: none;
  /* column_labels.border.lr.style */
  border-right-width: 1px;
  /* column_labels.border.lr.width */
  border-right-color: #D3D3D3;
  /* column_labels.border.lr.color */
}

#lippbrsxac .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  /* column_labels.background.color */
  font-size: 100%;
  /* column_labels.font.size */
  font-weight: normal;
  /* column_labels.font.weight */
  text-transform: inherit;
  /* column_labels.text_transform */
  vertical-align: middle;
  padding: 5px;
  margin: 10px;
  overflow-x: hidden;
}

#lippbrsxac .gt_sep_right {
  border-right: 5px solid #FFFFFF;
}

#lippbrsxac .gt_group_heading {
  padding: 8px;
  /* row_group.padding */
  color: #333333;
  background-color: #FFFFFF;
  /* row_group.background.color */
  font-size: 100%;
  /* row_group.font.size */
  font-weight: initial;
  /* row_group.font.weight */
  text-transform: inherit;
  /* row_group.text_transform */
  border-top-style: solid;
  /* row_group.border.top.style */
  border-top-width: 2px;
  /* row_group.border.top.width */
  border-top-color: #D3D3D3;
  /* row_group.border.top.color */
  border-bottom-style: solid;
  /* row_group.border.bottom.style */
  border-bottom-width: 2px;
  /* row_group.border.bottom.width */
  border-bottom-color: #D3D3D3;
  /* row_group.border.bottom.color */
  border-left-style: none;
  /* row_group.border.left.style */
  border-left-width: 1px;
  /* row_group.border.left.width */
  border-left-color: #D3D3D3;
  /* row_group.border.left.color */
  border-right-style: none;
  /* row_group.border.right.style */
  border-right-width: 1px;
  /* row_group.border.right.width */
  border-right-color: #D3D3D3;
  /* row_group.border.right.color */
  vertical-align: middle;
}

#lippbrsxac .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  /* row_group.background.color */
  font-size: 100%;
  /* row_group.font.size */
  font-weight: initial;
  /* row_group.font.weight */
  border-top-style: solid;
  /* row_group.border.top.style */
  border-top-width: 2px;
  /* row_group.border.top.width */
  border-top-color: #D3D3D3;
  /* row_group.border.top.color */
  border-bottom-style: solid;
  /* row_group.border.bottom.style */
  border-bottom-width: 2px;
  /* row_group.border.bottom.width */
  border-bottom-color: #D3D3D3;
  /* row_group.border.bottom.color */
  vertical-align: middle;
}

#lippbrsxac .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
  /* row.striping.background_color */
}

#lippbrsxac .gt_from_md > :first-child {
  margin-top: 0;
}

#lippbrsxac .gt_from_md > :last-child {
  margin-bottom: 0;
}

#lippbrsxac .gt_row {
  padding-top: 8px;
  /* data_row.padding */
  padding-bottom: 8px;
  /* data_row.padding */
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  /* table_body.hlines.style */
  border-top-width: 1px;
  /* table_body.hlines.width */
  border-top-color: #D3D3D3;
  /* table_body.hlines.color */
  border-left-style: none;
  /* table_body.vlines.style */
  border-left-width: 1px;
  /* table_body.vlines.width */
  border-left-color: #D3D3D3;
  /* table_body.vlines.color */
  border-right-style: none;
  /* table_body.vlines.style */
  border-right-width: 1px;
  /* table_body.vlines.width */
  border-right-color: #D3D3D3;
  /* table_body.vlines.color */
  vertical-align: middle;
  overflow-x: hidden;
}

#lippbrsxac .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  /* stub.background.color */
  font-weight: initial;
  /* stub.font.weight */
  text-transform: inherit;
  /* stub.text_transform */
  border-right-style: solid;
  /* stub.border.style */
  border-right-width: 2px;
  /* stub.border.width */
  border-right-color: #D3D3D3;
  /* stub.border.color */
  padding-left: 12px;
}

#lippbrsxac .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  /* summary_row.background.color */
  text-transform: inherit;
  /* summary_row.text_transform */
  padding-top: 8px;
  /* summary_row.padding */
  padding-bottom: 8px;
  /* summary_row.padding */
  padding-left: 5px;
  padding-right: 5px;
}

#lippbrsxac .gt_first_summary_row {
  padding-top: 8px;
  /* summary_row.padding */
  padding-bottom: 8px;
  /* summary_row.padding */
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  /* summary_row.border.style */
  border-top-width: 2px;
  /* summary_row.border.width */
  border-top-color: #D3D3D3;
  /* summary_row.border.color */
}

#lippbrsxac .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  /* grand_summary_row.background.color */
  text-transform: inherit;
  /* grand_summary_row.text_transform */
  padding-top: 8px;
  /* grand_summary_row.padding */
  padding-bottom: 8px;
  /* grand_summary_row.padding */
  padding-left: 5px;
  padding-right: 5px;
}

#lippbrsxac .gt_first_grand_summary_row {
  padding-top: 8px;
  /* grand_summary_row.padding */
  padding-bottom: 8px;
  /* grand_summary_row.padding */
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  /* grand_summary_row.border.style */
  border-top-width: 6px;
  /* grand_summary_row.border.width */
  border-top-color: #D3D3D3;
  /* grand_summary_row.border.color */
}

#lippbrsxac .gt_table_body {
  border-top-style: solid;
  /* table_body.border.top.style */
  border-top-width: 2px;
  /* table_body.border.top.width */
  border-top-color: #D3D3D3;
  /* table_body.border.top.color */
  border-bottom-style: solid;
  /* table_body.border.bottom.style */
  border-bottom-width: 2px;
  /* table_body.border.bottom.width */
  border-bottom-color: #D3D3D3;
  /* table_body.border.bottom.color */
}

#lippbrsxac .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  /* footnotes.background.color */
  border-bottom-style: none;
  /* footnotes.border.bottom.style */
  border-bottom-width: 2px;
  /* footnotes.border.bottom.width */
  border-bottom-color: #D3D3D3;
  /* footnotes.border.bottom.color */
  border-left-style: none;
  /* footnotes.border.lr.color */
  border-left-width: 2px;
  /* footnotes.border.lr.color */
  border-left-color: #D3D3D3;
  /* footnotes.border.lr.color */
  border-right-style: none;
  /* footnotes.border.lr.color */
  border-right-width: 2px;
  /* footnotes.border.lr.color */
  border-right-color: #D3D3D3;
  /* footnotes.border.lr.color */
}

#lippbrsxac .gt_footnote {
  margin: 0px;
  font-size: 90%;
  /* footnotes.font.size */
  padding: 4px;
  /* footnotes.padding */
}

#lippbrsxac .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  /* source_notes.background.color */
  border-bottom-style: none;
  /* source_notes.border.bottom.style */
  border-bottom-width: 2px;
  /* source_notes.border.bottom.width */
  border-bottom-color: #D3D3D3;
  /* source_notes.border.bottom.color */
  border-left-style: none;
  /* source_notes.border.lr.style */
  border-left-width: 2px;
  /* source_notes.border.lr.style */
  border-left-color: #D3D3D3;
  /* source_notes.border.lr.style */
  border-right-style: none;
  /* source_notes.border.lr.style */
  border-right-width: 2px;
  /* source_notes.border.lr.style */
  border-right-color: #D3D3D3;
  /* source_notes.border.lr.style */
}

#lippbrsxac .gt_sourcenote {
  font-size: 90%;
  /* source_notes.font.size */
  padding: 4px;
  /* source_notes.padding */
}

#lippbrsxac .gt_left {
  text-align: left;
}

#lippbrsxac .gt_center {
  text-align: center;
}

#lippbrsxac .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#lippbrsxac .gt_font_normal {
  font-weight: normal;
}

#lippbrsxac .gt_font_bold {
  font-weight: bold;
}

#lippbrsxac .gt_font_italic {
  font-style: italic;
}

#lippbrsxac .gt_super {
  font-size: 65%;
}

#lippbrsxac .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>
<div id="lippbrsxac" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;"><table class="gt_table">
  <thead class="gt_header">
    <tr>
      <th colspan="11" class="gt_heading gt_title gt_font_normal gt_center" style>jag2b fish counts by timepoint and date-of-birth.</th>
    </tr>
    <tr>
      <th colspan="11" class="gt_heading gt_subtitle gt_font_normal gt_center gt_bottom_border" style></th>
    </tr>
  </thead>
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">dob</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">6 weeks</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">8 weeks</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">10 weeks</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">12 weeks</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">4 months</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">6 months</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">8 months</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">10 months</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">13 months</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">24 months</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr>
      <td class="gt_row gt_left">20190813</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">24</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_striped">20200922</td>
      <td class="gt_row gt_center gt_striped">0</td>
      <td class="gt_row gt_center gt_striped">0</td>
      <td class="gt_row gt_center gt_striped">0</td>
      <td class="gt_row gt_center gt_striped">0</td>
      <td class="gt_row gt_center gt_striped">0</td>
      <td class="gt_row gt_center gt_striped">0</td>
      <td class="gt_row gt_center gt_striped">0</td>
      <td class="gt_row gt_center gt_striped">17</td>
      <td class="gt_row gt_center gt_striped">0</td>
      <td class="gt_row gt_center gt_striped">0</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">20201014</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">24</td>
      <td class="gt_row gt_center">0</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_striped">20210715</td>
      <td class="gt_row gt_center gt_striped">10</td>
      <td class="gt_row gt_center gt_striped">12</td>
      <td class="gt_row gt_center gt_striped">12</td>
      <td class="gt_row gt_center gt_striped">9</td>
      <td class="gt_row gt_center gt_striped">0</td>
      <td class="gt_row gt_center gt_striped">0</td>
      <td class="gt_row gt_center gt_striped">0</td>
      <td class="gt_row gt_center gt_striped">0</td>
      <td class="gt_row gt_center gt_striped">0</td>
      <td class="gt_row gt_center gt_striped">0</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">20210728</td>
      <td class="gt_row gt_center">13</td>
      <td class="gt_row gt_center">12</td>
      <td class="gt_row gt_center">11</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_striped">20210819</td>
      <td class="gt_row gt_center gt_striped">12</td>
      <td class="gt_row gt_center gt_striped">0</td>
      <td class="gt_row gt_center gt_striped">0</td>
      <td class="gt_row gt_center gt_striped">0</td>
      <td class="gt_row gt_center gt_striped">50</td>
      <td class="gt_row gt_center gt_striped">0</td>
      <td class="gt_row gt_center gt_striped">0</td>
      <td class="gt_row gt_center gt_striped">0</td>
      <td class="gt_row gt_center gt_striped">0</td>
      <td class="gt_row gt_center gt_striped">0</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">20211003</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">16</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_striped">20211210</td>
      <td class="gt_row gt_center gt_striped">0</td>
      <td class="gt_row gt_center gt_striped">0</td>
      <td class="gt_row gt_center gt_striped">0</td>
      <td class="gt_row gt_center gt_striped">0</td>
      <td class="gt_row gt_center gt_striped">0</td>
      <td class="gt_row gt_center gt_striped">35</td>
      <td class="gt_row gt_center gt_striped">0</td>
      <td class="gt_row gt_center gt_striped">0</td>
      <td class="gt_row gt_center gt_striped">0</td>
      <td class="gt_row gt_center gt_striped">0</td>
    </tr>
  </tbody>
  
  
</table></div>

``` r
# Table genotype by time point, grouped by dob.

dcast(data=tab, dob + genotype2 ~ age2, fun.aggregate=length) %>% 
  as_tibble() %>%
  group_by(dob) %>%
  gt() %>%
  tab_header(title="jag2b fish counts by timepoint, data-of-birth, and genotype.")
```

<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#wzkbxugzci .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  /* table.margin.left */
  margin-right: auto;
  /* table.margin.right */
  color: #333333;
  font-size: 16px;
  /* table.font.size */
  background-color: #FFFFFF;
  /* table.background.color */
  width: auto;
  /* table.width */
  border-top-style: solid;
  /* table.border.top.style */
  border-top-width: 2px;
  /* table.border.top.width */
  border-top-color: #A8A8A8;
  /* table.border.top.color */
  border-bottom-style: solid;
  /* table.border.bottom.style */
  border-bottom-width: 2px;
  /* table.border.bottom.width */
  border-bottom-color: #A8A8A8;
  /* table.border.bottom.color */
}

#wzkbxugzci .gt_heading {
  background-color: #FFFFFF;
  /* heading.background.color */
  border-bottom-color: #FFFFFF;
  /* table.background.color */
  border-left-style: hidden;
  /* heading.border.lr.style */
  border-left-width: 1px;
  /* heading.border.lr.width */
  border-left-color: #D3D3D3;
  /* heading.border.lr.color */
  border-right-style: hidden;
  /* heading.border.lr.style */
  border-right-width: 1px;
  /* heading.border.lr.width */
  border-right-color: #D3D3D3;
  /* heading.border.lr.color */
}

#wzkbxugzci .gt_title {
  color: #333333;
  font-size: 125%;
  /* heading.title.font.size */
  font-weight: initial;
  /* heading.title.font.weight */
  padding-top: 4px;
  /* heading.top.padding - not yet used */
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  /* table.background.color */
  border-bottom-width: 0;
}

#wzkbxugzci .gt_subtitle {
  color: #333333;
  font-size: 85%;
  /* heading.subtitle.font.size */
  font-weight: initial;
  /* heading.subtitle.font.weight */
  padding-top: 0;
  padding-bottom: 4px;
  /* heading.bottom.padding - not yet used */
  border-top-color: #FFFFFF;
  /* table.background.color */
  border-top-width: 0;
}

#wzkbxugzci .gt_bottom_border {
  border-bottom-style: solid;
  /* heading.border.bottom.style */
  border-bottom-width: 2px;
  /* heading.border.bottom.width */
  border-bottom-color: #D3D3D3;
  /* heading.border.bottom.color */
}

#wzkbxugzci .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  padding-top: 4px;
  padding-bottom: 4px;
}

#wzkbxugzci .gt_col_headings {
  border-top-style: solid;
  /* column_labels.border.top.style */
  border-top-width: 2px;
  /* column_labels.border.top.width */
  border-top-color: #D3D3D3;
  /* column_labels.border.top.color */
  border-bottom-style: solid;
  /* column_labels.border.bottom.style */
  border-bottom-width: 2px;
  /* column_labels.border.bottom.width */
  border-bottom-color: #D3D3D3;
  /* column_labels.border.bottom.color */
  border-left-style: none;
  /* column_labels.border.lr.style */
  border-left-width: 1px;
  /* column_labels.border.lr.width */
  border-left-color: #D3D3D3;
  /* column_labels.border.lr.color */
  border-right-style: none;
  /* column_labels.border.lr.style */
  border-right-width: 1px;
  /* column_labels.border.lr.width */
  border-right-color: #D3D3D3;
  /* column_labels.border.lr.color */
}

#wzkbxugzci .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  /* column_labels.background.color */
  font-size: 100%;
  /* column_labels.font.size */
  font-weight: normal;
  /* column_labels.font.weight */
  text-transform: inherit;
  /* column_labels.text_transform */
  vertical-align: middle;
  padding: 5px;
  margin: 10px;
  overflow-x: hidden;
}

#wzkbxugzci .gt_sep_right {
  border-right: 5px solid #FFFFFF;
}

#wzkbxugzci .gt_group_heading {
  padding: 8px;
  /* row_group.padding */
  color: #333333;
  background-color: #FFFFFF;
  /* row_group.background.color */
  font-size: 100%;
  /* row_group.font.size */
  font-weight: initial;
  /* row_group.font.weight */
  text-transform: inherit;
  /* row_group.text_transform */
  border-top-style: solid;
  /* row_group.border.top.style */
  border-top-width: 2px;
  /* row_group.border.top.width */
  border-top-color: #D3D3D3;
  /* row_group.border.top.color */
  border-bottom-style: solid;
  /* row_group.border.bottom.style */
  border-bottom-width: 2px;
  /* row_group.border.bottom.width */
  border-bottom-color: #D3D3D3;
  /* row_group.border.bottom.color */
  border-left-style: none;
  /* row_group.border.left.style */
  border-left-width: 1px;
  /* row_group.border.left.width */
  border-left-color: #D3D3D3;
  /* row_group.border.left.color */
  border-right-style: none;
  /* row_group.border.right.style */
  border-right-width: 1px;
  /* row_group.border.right.width */
  border-right-color: #D3D3D3;
  /* row_group.border.right.color */
  vertical-align: middle;
}

#wzkbxugzci .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  /* row_group.background.color */
  font-size: 100%;
  /* row_group.font.size */
  font-weight: initial;
  /* row_group.font.weight */
  border-top-style: solid;
  /* row_group.border.top.style */
  border-top-width: 2px;
  /* row_group.border.top.width */
  border-top-color: #D3D3D3;
  /* row_group.border.top.color */
  border-bottom-style: solid;
  /* row_group.border.bottom.style */
  border-bottom-width: 2px;
  /* row_group.border.bottom.width */
  border-bottom-color: #D3D3D3;
  /* row_group.border.bottom.color */
  vertical-align: middle;
}

#wzkbxugzci .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
  /* row.striping.background_color */
}

#wzkbxugzci .gt_from_md > :first-child {
  margin-top: 0;
}

#wzkbxugzci .gt_from_md > :last-child {
  margin-bottom: 0;
}

#wzkbxugzci .gt_row {
  padding-top: 8px;
  /* data_row.padding */
  padding-bottom: 8px;
  /* data_row.padding */
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  /* table_body.hlines.style */
  border-top-width: 1px;
  /* table_body.hlines.width */
  border-top-color: #D3D3D3;
  /* table_body.hlines.color */
  border-left-style: none;
  /* table_body.vlines.style */
  border-left-width: 1px;
  /* table_body.vlines.width */
  border-left-color: #D3D3D3;
  /* table_body.vlines.color */
  border-right-style: none;
  /* table_body.vlines.style */
  border-right-width: 1px;
  /* table_body.vlines.width */
  border-right-color: #D3D3D3;
  /* table_body.vlines.color */
  vertical-align: middle;
  overflow-x: hidden;
}

#wzkbxugzci .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  /* stub.background.color */
  font-weight: initial;
  /* stub.font.weight */
  text-transform: inherit;
  /* stub.text_transform */
  border-right-style: solid;
  /* stub.border.style */
  border-right-width: 2px;
  /* stub.border.width */
  border-right-color: #D3D3D3;
  /* stub.border.color */
  padding-left: 12px;
}

#wzkbxugzci .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  /* summary_row.background.color */
  text-transform: inherit;
  /* summary_row.text_transform */
  padding-top: 8px;
  /* summary_row.padding */
  padding-bottom: 8px;
  /* summary_row.padding */
  padding-left: 5px;
  padding-right: 5px;
}

#wzkbxugzci .gt_first_summary_row {
  padding-top: 8px;
  /* summary_row.padding */
  padding-bottom: 8px;
  /* summary_row.padding */
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  /* summary_row.border.style */
  border-top-width: 2px;
  /* summary_row.border.width */
  border-top-color: #D3D3D3;
  /* summary_row.border.color */
}

#wzkbxugzci .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  /* grand_summary_row.background.color */
  text-transform: inherit;
  /* grand_summary_row.text_transform */
  padding-top: 8px;
  /* grand_summary_row.padding */
  padding-bottom: 8px;
  /* grand_summary_row.padding */
  padding-left: 5px;
  padding-right: 5px;
}

#wzkbxugzci .gt_first_grand_summary_row {
  padding-top: 8px;
  /* grand_summary_row.padding */
  padding-bottom: 8px;
  /* grand_summary_row.padding */
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  /* grand_summary_row.border.style */
  border-top-width: 6px;
  /* grand_summary_row.border.width */
  border-top-color: #D3D3D3;
  /* grand_summary_row.border.color */
}

#wzkbxugzci .gt_table_body {
  border-top-style: solid;
  /* table_body.border.top.style */
  border-top-width: 2px;
  /* table_body.border.top.width */
  border-top-color: #D3D3D3;
  /* table_body.border.top.color */
  border-bottom-style: solid;
  /* table_body.border.bottom.style */
  border-bottom-width: 2px;
  /* table_body.border.bottom.width */
  border-bottom-color: #D3D3D3;
  /* table_body.border.bottom.color */
}

#wzkbxugzci .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  /* footnotes.background.color */
  border-bottom-style: none;
  /* footnotes.border.bottom.style */
  border-bottom-width: 2px;
  /* footnotes.border.bottom.width */
  border-bottom-color: #D3D3D3;
  /* footnotes.border.bottom.color */
  border-left-style: none;
  /* footnotes.border.lr.color */
  border-left-width: 2px;
  /* footnotes.border.lr.color */
  border-left-color: #D3D3D3;
  /* footnotes.border.lr.color */
  border-right-style: none;
  /* footnotes.border.lr.color */
  border-right-width: 2px;
  /* footnotes.border.lr.color */
  border-right-color: #D3D3D3;
  /* footnotes.border.lr.color */
}

#wzkbxugzci .gt_footnote {
  margin: 0px;
  font-size: 90%;
  /* footnotes.font.size */
  padding: 4px;
  /* footnotes.padding */
}

#wzkbxugzci .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  /* source_notes.background.color */
  border-bottom-style: none;
  /* source_notes.border.bottom.style */
  border-bottom-width: 2px;
  /* source_notes.border.bottom.width */
  border-bottom-color: #D3D3D3;
  /* source_notes.border.bottom.color */
  border-left-style: none;
  /* source_notes.border.lr.style */
  border-left-width: 2px;
  /* source_notes.border.lr.style */
  border-left-color: #D3D3D3;
  /* source_notes.border.lr.style */
  border-right-style: none;
  /* source_notes.border.lr.style */
  border-right-width: 2px;
  /* source_notes.border.lr.style */
  border-right-color: #D3D3D3;
  /* source_notes.border.lr.style */
}

#wzkbxugzci .gt_sourcenote {
  font-size: 90%;
  /* source_notes.font.size */
  padding: 4px;
  /* source_notes.padding */
}

#wzkbxugzci .gt_left {
  text-align: left;
}

#wzkbxugzci .gt_center {
  text-align: center;
}

#wzkbxugzci .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#wzkbxugzci .gt_font_normal {
  font-weight: normal;
}

#wzkbxugzci .gt_font_bold {
  font-weight: bold;
}

#wzkbxugzci .gt_font_italic {
  font-style: italic;
}

#wzkbxugzci .gt_super {
  font-size: 65%;
}

#wzkbxugzci .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>
<div id="wzkbxugzci" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;"><table class="gt_table">
  <thead class="gt_header">
    <tr>
      <th colspan="11" class="gt_heading gt_title gt_font_normal gt_center" style>jag2b fish counts by timepoint, data-of-birth, and genotype.</th>
    </tr>
    <tr>
      <th colspan="11" class="gt_heading gt_subtitle gt_font_normal gt_center gt_bottom_border" style></th>
    </tr>
  </thead>
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">genotype2</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">6 weeks</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">8 weeks</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">10 weeks</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">12 weeks</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">4 months</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">6 months</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">8 months</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">10 months</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">13 months</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">24 months</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr class="gt_group_heading_row">
      <td colspan="11" class="gt_group_heading">20190813</td>
    </tr>
    <tr>
      <td class="gt_row gt_center">+/+</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">11</td>
    </tr>
    <tr>
      <td class="gt_row gt_center gt_striped">+/-</td>
      <td class="gt_row gt_center gt_striped">0</td>
      <td class="gt_row gt_center gt_striped">0</td>
      <td class="gt_row gt_center gt_striped">0</td>
      <td class="gt_row gt_center gt_striped">0</td>
      <td class="gt_row gt_center gt_striped">0</td>
      <td class="gt_row gt_center gt_striped">0</td>
      <td class="gt_row gt_center gt_striped">0</td>
      <td class="gt_row gt_center gt_striped">0</td>
      <td class="gt_row gt_center gt_striped">0</td>
      <td class="gt_row gt_center gt_striped">13</td>
    </tr>
    <tr class="gt_group_heading_row">
      <td colspan="11" class="gt_group_heading">20200922</td>
    </tr>
    <tr>
      <td class="gt_row gt_center">+/+</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">4</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
    </tr>
    <tr>
      <td class="gt_row gt_center gt_striped">+/-</td>
      <td class="gt_row gt_center gt_striped">0</td>
      <td class="gt_row gt_center gt_striped">0</td>
      <td class="gt_row gt_center gt_striped">0</td>
      <td class="gt_row gt_center gt_striped">0</td>
      <td class="gt_row gt_center gt_striped">0</td>
      <td class="gt_row gt_center gt_striped">0</td>
      <td class="gt_row gt_center gt_striped">0</td>
      <td class="gt_row gt_center gt_striped">6</td>
      <td class="gt_row gt_center gt_striped">0</td>
      <td class="gt_row gt_center gt_striped">0</td>
    </tr>
    <tr>
      <td class="gt_row gt_center">-/-</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">7</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
    </tr>
    <tr class="gt_group_heading_row">
      <td colspan="11" class="gt_group_heading">20201014</td>
    </tr>
    <tr>
      <td class="gt_row gt_center gt_striped">+/+</td>
      <td class="gt_row gt_center gt_striped">0</td>
      <td class="gt_row gt_center gt_striped">0</td>
      <td class="gt_row gt_center gt_striped">0</td>
      <td class="gt_row gt_center gt_striped">0</td>
      <td class="gt_row gt_center gt_striped">0</td>
      <td class="gt_row gt_center gt_striped">0</td>
      <td class="gt_row gt_center gt_striped">0</td>
      <td class="gt_row gt_center gt_striped">0</td>
      <td class="gt_row gt_center gt_striped">10</td>
      <td class="gt_row gt_center gt_striped">0</td>
    </tr>
    <tr>
      <td class="gt_row gt_center">+/-</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">8</td>
      <td class="gt_row gt_center">0</td>
    </tr>
    <tr>
      <td class="gt_row gt_center gt_striped">-/-</td>
      <td class="gt_row gt_center gt_striped">0</td>
      <td class="gt_row gt_center gt_striped">0</td>
      <td class="gt_row gt_center gt_striped">0</td>
      <td class="gt_row gt_center gt_striped">0</td>
      <td class="gt_row gt_center gt_striped">0</td>
      <td class="gt_row gt_center gt_striped">0</td>
      <td class="gt_row gt_center gt_striped">0</td>
      <td class="gt_row gt_center gt_striped">0</td>
      <td class="gt_row gt_center gt_striped">6</td>
      <td class="gt_row gt_center gt_striped">0</td>
    </tr>
    <tr class="gt_group_heading_row">
      <td colspan="11" class="gt_group_heading">20210715</td>
    </tr>
    <tr>
      <td class="gt_row gt_center">+/+</td>
      <td class="gt_row gt_center">3</td>
      <td class="gt_row gt_center">4</td>
      <td class="gt_row gt_center">4</td>
      <td class="gt_row gt_center">3</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
    </tr>
    <tr>
      <td class="gt_row gt_center gt_striped">+/-</td>
      <td class="gt_row gt_center gt_striped">4</td>
      <td class="gt_row gt_center gt_striped">4</td>
      <td class="gt_row gt_center gt_striped">4</td>
      <td class="gt_row gt_center gt_striped">3</td>
      <td class="gt_row gt_center gt_striped">0</td>
      <td class="gt_row gt_center gt_striped">0</td>
      <td class="gt_row gt_center gt_striped">0</td>
      <td class="gt_row gt_center gt_striped">0</td>
      <td class="gt_row gt_center gt_striped">0</td>
      <td class="gt_row gt_center gt_striped">0</td>
    </tr>
    <tr>
      <td class="gt_row gt_center">-/-</td>
      <td class="gt_row gt_center">3</td>
      <td class="gt_row gt_center">4</td>
      <td class="gt_row gt_center">4</td>
      <td class="gt_row gt_center">3</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
    </tr>
    <tr class="gt_group_heading_row">
      <td colspan="11" class="gt_group_heading">20210728</td>
    </tr>
    <tr>
      <td class="gt_row gt_center gt_striped">+/+</td>
      <td class="gt_row gt_center gt_striped">3</td>
      <td class="gt_row gt_center gt_striped">4</td>
      <td class="gt_row gt_center gt_striped">4</td>
      <td class="gt_row gt_center gt_striped">0</td>
      <td class="gt_row gt_center gt_striped">0</td>
      <td class="gt_row gt_center gt_striped">0</td>
      <td class="gt_row gt_center gt_striped">0</td>
      <td class="gt_row gt_center gt_striped">0</td>
      <td class="gt_row gt_center gt_striped">0</td>
      <td class="gt_row gt_center gt_striped">0</td>
    </tr>
    <tr>
      <td class="gt_row gt_center">+/-</td>
      <td class="gt_row gt_center">5</td>
      <td class="gt_row gt_center">4</td>
      <td class="gt_row gt_center">4</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
    </tr>
    <tr>
      <td class="gt_row gt_center gt_striped">-/-</td>
      <td class="gt_row gt_center gt_striped">5</td>
      <td class="gt_row gt_center gt_striped">4</td>
      <td class="gt_row gt_center gt_striped">3</td>
      <td class="gt_row gt_center gt_striped">0</td>
      <td class="gt_row gt_center gt_striped">0</td>
      <td class="gt_row gt_center gt_striped">0</td>
      <td class="gt_row gt_center gt_striped">0</td>
      <td class="gt_row gt_center gt_striped">0</td>
      <td class="gt_row gt_center gt_striped">0</td>
      <td class="gt_row gt_center gt_striped">0</td>
    </tr>
    <tr class="gt_group_heading_row">
      <td colspan="11" class="gt_group_heading">20210819</td>
    </tr>
    <tr>
      <td class="gt_row gt_center">+/+</td>
      <td class="gt_row gt_center">4</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">16</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
    </tr>
    <tr>
      <td class="gt_row gt_center gt_striped">+/-</td>
      <td class="gt_row gt_center gt_striped">4</td>
      <td class="gt_row gt_center gt_striped">0</td>
      <td class="gt_row gt_center gt_striped">0</td>
      <td class="gt_row gt_center gt_striped">0</td>
      <td class="gt_row gt_center gt_striped">18</td>
      <td class="gt_row gt_center gt_striped">0</td>
      <td class="gt_row gt_center gt_striped">0</td>
      <td class="gt_row gt_center gt_striped">0</td>
      <td class="gt_row gt_center gt_striped">0</td>
      <td class="gt_row gt_center gt_striped">0</td>
    </tr>
    <tr>
      <td class="gt_row gt_center">-/-</td>
      <td class="gt_row gt_center">4</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">16</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
    </tr>
    <tr class="gt_group_heading_row">
      <td colspan="11" class="gt_group_heading">20211003</td>
    </tr>
    <tr>
      <td class="gt_row gt_center gt_striped">+/+</td>
      <td class="gt_row gt_center gt_striped">0</td>
      <td class="gt_row gt_center gt_striped">0</td>
      <td class="gt_row gt_center gt_striped">0</td>
      <td class="gt_row gt_center gt_striped">0</td>
      <td class="gt_row gt_center gt_striped">0</td>
      <td class="gt_row gt_center gt_striped">0</td>
      <td class="gt_row gt_center gt_striped">8</td>
      <td class="gt_row gt_center gt_striped">0</td>
      <td class="gt_row gt_center gt_striped">0</td>
      <td class="gt_row gt_center gt_striped">0</td>
    </tr>
    <tr>
      <td class="gt_row gt_center">+/-</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">5</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
    </tr>
    <tr>
      <td class="gt_row gt_center gt_striped">-/-</td>
      <td class="gt_row gt_center gt_striped">0</td>
      <td class="gt_row gt_center gt_striped">0</td>
      <td class="gt_row gt_center gt_striped">0</td>
      <td class="gt_row gt_center gt_striped">0</td>
      <td class="gt_row gt_center gt_striped">0</td>
      <td class="gt_row gt_center gt_striped">0</td>
      <td class="gt_row gt_center gt_striped">3</td>
      <td class="gt_row gt_center gt_striped">0</td>
      <td class="gt_row gt_center gt_striped">0</td>
      <td class="gt_row gt_center gt_striped">0</td>
    </tr>
    <tr class="gt_group_heading_row">
      <td colspan="11" class="gt_group_heading">20211210</td>
    </tr>
    <tr>
      <td class="gt_row gt_center">+/+</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">16</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
    </tr>
    <tr>
      <td class="gt_row gt_center gt_striped">+/-</td>
      <td class="gt_row gt_center gt_striped">0</td>
      <td class="gt_row gt_center gt_striped">0</td>
      <td class="gt_row gt_center gt_striped">0</td>
      <td class="gt_row gt_center gt_striped">0</td>
      <td class="gt_row gt_center gt_striped">0</td>
      <td class="gt_row gt_center gt_striped">10</td>
      <td class="gt_row gt_center gt_striped">0</td>
      <td class="gt_row gt_center gt_striped">0</td>
      <td class="gt_row gt_center gt_striped">0</td>
      <td class="gt_row gt_center gt_striped">0</td>
    </tr>
    <tr>
      <td class="gt_row gt_center">-/-</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">9</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
    </tr>
  </tbody>
  
  
</table></div>

``` r
# Edit the data set.
# 1. Remove females 10-, 13-, and 24-months.
# 2. For 6- through 12-week fish, relabel males and females as 'unknown'.

tab2 = tab[!(age %in% c("10month", "13month", "24month") & sex %in% "F")]

# Show that females of correct ages were removed
dcast(data=tab2, sex ~ age2, fun.aggregate=length) %>% 
  as_tibble() %>%
  gt() %>%
  tab_header(title="Updated jag2b fish counts by timepoint and sex.",
             subtitle="Females removed for timepoints 10-, 13-, 24-months")
```

<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#klavjfwtik .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  /* table.margin.left */
  margin-right: auto;
  /* table.margin.right */
  color: #333333;
  font-size: 16px;
  /* table.font.size */
  background-color: #FFFFFF;
  /* table.background.color */
  width: auto;
  /* table.width */
  border-top-style: solid;
  /* table.border.top.style */
  border-top-width: 2px;
  /* table.border.top.width */
  border-top-color: #A8A8A8;
  /* table.border.top.color */
  border-bottom-style: solid;
  /* table.border.bottom.style */
  border-bottom-width: 2px;
  /* table.border.bottom.width */
  border-bottom-color: #A8A8A8;
  /* table.border.bottom.color */
}

#klavjfwtik .gt_heading {
  background-color: #FFFFFF;
  /* heading.background.color */
  border-bottom-color: #FFFFFF;
  /* table.background.color */
  border-left-style: hidden;
  /* heading.border.lr.style */
  border-left-width: 1px;
  /* heading.border.lr.width */
  border-left-color: #D3D3D3;
  /* heading.border.lr.color */
  border-right-style: hidden;
  /* heading.border.lr.style */
  border-right-width: 1px;
  /* heading.border.lr.width */
  border-right-color: #D3D3D3;
  /* heading.border.lr.color */
}

#klavjfwtik .gt_title {
  color: #333333;
  font-size: 125%;
  /* heading.title.font.size */
  font-weight: initial;
  /* heading.title.font.weight */
  padding-top: 4px;
  /* heading.top.padding - not yet used */
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  /* table.background.color */
  border-bottom-width: 0;
}

#klavjfwtik .gt_subtitle {
  color: #333333;
  font-size: 85%;
  /* heading.subtitle.font.size */
  font-weight: initial;
  /* heading.subtitle.font.weight */
  padding-top: 0;
  padding-bottom: 4px;
  /* heading.bottom.padding - not yet used */
  border-top-color: #FFFFFF;
  /* table.background.color */
  border-top-width: 0;
}

#klavjfwtik .gt_bottom_border {
  border-bottom-style: solid;
  /* heading.border.bottom.style */
  border-bottom-width: 2px;
  /* heading.border.bottom.width */
  border-bottom-color: #D3D3D3;
  /* heading.border.bottom.color */
}

#klavjfwtik .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  padding-top: 4px;
  padding-bottom: 4px;
}

#klavjfwtik .gt_col_headings {
  border-top-style: solid;
  /* column_labels.border.top.style */
  border-top-width: 2px;
  /* column_labels.border.top.width */
  border-top-color: #D3D3D3;
  /* column_labels.border.top.color */
  border-bottom-style: solid;
  /* column_labels.border.bottom.style */
  border-bottom-width: 2px;
  /* column_labels.border.bottom.width */
  border-bottom-color: #D3D3D3;
  /* column_labels.border.bottom.color */
  border-left-style: none;
  /* column_labels.border.lr.style */
  border-left-width: 1px;
  /* column_labels.border.lr.width */
  border-left-color: #D3D3D3;
  /* column_labels.border.lr.color */
  border-right-style: none;
  /* column_labels.border.lr.style */
  border-right-width: 1px;
  /* column_labels.border.lr.width */
  border-right-color: #D3D3D3;
  /* column_labels.border.lr.color */
}

#klavjfwtik .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  /* column_labels.background.color */
  font-size: 100%;
  /* column_labels.font.size */
  font-weight: normal;
  /* column_labels.font.weight */
  text-transform: inherit;
  /* column_labels.text_transform */
  vertical-align: middle;
  padding: 5px;
  margin: 10px;
  overflow-x: hidden;
}

#klavjfwtik .gt_sep_right {
  border-right: 5px solid #FFFFFF;
}

#klavjfwtik .gt_group_heading {
  padding: 8px;
  /* row_group.padding */
  color: #333333;
  background-color: #FFFFFF;
  /* row_group.background.color */
  font-size: 100%;
  /* row_group.font.size */
  font-weight: initial;
  /* row_group.font.weight */
  text-transform: inherit;
  /* row_group.text_transform */
  border-top-style: solid;
  /* row_group.border.top.style */
  border-top-width: 2px;
  /* row_group.border.top.width */
  border-top-color: #D3D3D3;
  /* row_group.border.top.color */
  border-bottom-style: solid;
  /* row_group.border.bottom.style */
  border-bottom-width: 2px;
  /* row_group.border.bottom.width */
  border-bottom-color: #D3D3D3;
  /* row_group.border.bottom.color */
  border-left-style: none;
  /* row_group.border.left.style */
  border-left-width: 1px;
  /* row_group.border.left.width */
  border-left-color: #D3D3D3;
  /* row_group.border.left.color */
  border-right-style: none;
  /* row_group.border.right.style */
  border-right-width: 1px;
  /* row_group.border.right.width */
  border-right-color: #D3D3D3;
  /* row_group.border.right.color */
  vertical-align: middle;
}

#klavjfwtik .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  /* row_group.background.color */
  font-size: 100%;
  /* row_group.font.size */
  font-weight: initial;
  /* row_group.font.weight */
  border-top-style: solid;
  /* row_group.border.top.style */
  border-top-width: 2px;
  /* row_group.border.top.width */
  border-top-color: #D3D3D3;
  /* row_group.border.top.color */
  border-bottom-style: solid;
  /* row_group.border.bottom.style */
  border-bottom-width: 2px;
  /* row_group.border.bottom.width */
  border-bottom-color: #D3D3D3;
  /* row_group.border.bottom.color */
  vertical-align: middle;
}

#klavjfwtik .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
  /* row.striping.background_color */
}

#klavjfwtik .gt_from_md > :first-child {
  margin-top: 0;
}

#klavjfwtik .gt_from_md > :last-child {
  margin-bottom: 0;
}

#klavjfwtik .gt_row {
  padding-top: 8px;
  /* data_row.padding */
  padding-bottom: 8px;
  /* data_row.padding */
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  /* table_body.hlines.style */
  border-top-width: 1px;
  /* table_body.hlines.width */
  border-top-color: #D3D3D3;
  /* table_body.hlines.color */
  border-left-style: none;
  /* table_body.vlines.style */
  border-left-width: 1px;
  /* table_body.vlines.width */
  border-left-color: #D3D3D3;
  /* table_body.vlines.color */
  border-right-style: none;
  /* table_body.vlines.style */
  border-right-width: 1px;
  /* table_body.vlines.width */
  border-right-color: #D3D3D3;
  /* table_body.vlines.color */
  vertical-align: middle;
  overflow-x: hidden;
}

#klavjfwtik .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  /* stub.background.color */
  font-weight: initial;
  /* stub.font.weight */
  text-transform: inherit;
  /* stub.text_transform */
  border-right-style: solid;
  /* stub.border.style */
  border-right-width: 2px;
  /* stub.border.width */
  border-right-color: #D3D3D3;
  /* stub.border.color */
  padding-left: 12px;
}

#klavjfwtik .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  /* summary_row.background.color */
  text-transform: inherit;
  /* summary_row.text_transform */
  padding-top: 8px;
  /* summary_row.padding */
  padding-bottom: 8px;
  /* summary_row.padding */
  padding-left: 5px;
  padding-right: 5px;
}

#klavjfwtik .gt_first_summary_row {
  padding-top: 8px;
  /* summary_row.padding */
  padding-bottom: 8px;
  /* summary_row.padding */
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  /* summary_row.border.style */
  border-top-width: 2px;
  /* summary_row.border.width */
  border-top-color: #D3D3D3;
  /* summary_row.border.color */
}

#klavjfwtik .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  /* grand_summary_row.background.color */
  text-transform: inherit;
  /* grand_summary_row.text_transform */
  padding-top: 8px;
  /* grand_summary_row.padding */
  padding-bottom: 8px;
  /* grand_summary_row.padding */
  padding-left: 5px;
  padding-right: 5px;
}

#klavjfwtik .gt_first_grand_summary_row {
  padding-top: 8px;
  /* grand_summary_row.padding */
  padding-bottom: 8px;
  /* grand_summary_row.padding */
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  /* grand_summary_row.border.style */
  border-top-width: 6px;
  /* grand_summary_row.border.width */
  border-top-color: #D3D3D3;
  /* grand_summary_row.border.color */
}

#klavjfwtik .gt_table_body {
  border-top-style: solid;
  /* table_body.border.top.style */
  border-top-width: 2px;
  /* table_body.border.top.width */
  border-top-color: #D3D3D3;
  /* table_body.border.top.color */
  border-bottom-style: solid;
  /* table_body.border.bottom.style */
  border-bottom-width: 2px;
  /* table_body.border.bottom.width */
  border-bottom-color: #D3D3D3;
  /* table_body.border.bottom.color */
}

#klavjfwtik .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  /* footnotes.background.color */
  border-bottom-style: none;
  /* footnotes.border.bottom.style */
  border-bottom-width: 2px;
  /* footnotes.border.bottom.width */
  border-bottom-color: #D3D3D3;
  /* footnotes.border.bottom.color */
  border-left-style: none;
  /* footnotes.border.lr.color */
  border-left-width: 2px;
  /* footnotes.border.lr.color */
  border-left-color: #D3D3D3;
  /* footnotes.border.lr.color */
  border-right-style: none;
  /* footnotes.border.lr.color */
  border-right-width: 2px;
  /* footnotes.border.lr.color */
  border-right-color: #D3D3D3;
  /* footnotes.border.lr.color */
}

#klavjfwtik .gt_footnote {
  margin: 0px;
  font-size: 90%;
  /* footnotes.font.size */
  padding: 4px;
  /* footnotes.padding */
}

#klavjfwtik .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  /* source_notes.background.color */
  border-bottom-style: none;
  /* source_notes.border.bottom.style */
  border-bottom-width: 2px;
  /* source_notes.border.bottom.width */
  border-bottom-color: #D3D3D3;
  /* source_notes.border.bottom.color */
  border-left-style: none;
  /* source_notes.border.lr.style */
  border-left-width: 2px;
  /* source_notes.border.lr.style */
  border-left-color: #D3D3D3;
  /* source_notes.border.lr.style */
  border-right-style: none;
  /* source_notes.border.lr.style */
  border-right-width: 2px;
  /* source_notes.border.lr.style */
  border-right-color: #D3D3D3;
  /* source_notes.border.lr.style */
}

#klavjfwtik .gt_sourcenote {
  font-size: 90%;
  /* source_notes.font.size */
  padding: 4px;
  /* source_notes.padding */
}

#klavjfwtik .gt_left {
  text-align: left;
}

#klavjfwtik .gt_center {
  text-align: center;
}

#klavjfwtik .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#klavjfwtik .gt_font_normal {
  font-weight: normal;
}

#klavjfwtik .gt_font_bold {
  font-weight: bold;
}

#klavjfwtik .gt_font_italic {
  font-style: italic;
}

#klavjfwtik .gt_super {
  font-size: 65%;
}

#klavjfwtik .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>
<div id="klavjfwtik" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;"><table class="gt_table">
  <thead class="gt_header">
    <tr>
      <th colspan="11" class="gt_heading gt_title gt_font_normal gt_center" style>Updated jag2b fish counts by timepoint and sex.</th>
    </tr>
    <tr>
      <th colspan="11" class="gt_heading gt_subtitle gt_font_normal gt_center gt_bottom_border" style>Females removed for timepoints 10-, 13-, 24-months</th>
    </tr>
  </thead>
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">sex</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">6 weeks</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">8 weeks</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">10 weeks</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">12 weeks</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">4 months</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">6 months</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">8 months</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">10 months</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">13 months</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">24 months</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr>
      <td class="gt_row gt_left">F</td>
      <td class="gt_row gt_center">8</td>
      <td class="gt_row gt_center">12</td>
      <td class="gt_row gt_center">11</td>
      <td class="gt_row gt_center">4</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_striped">M</td>
      <td class="gt_row gt_center gt_striped">27</td>
      <td class="gt_row gt_center gt_striped">12</td>
      <td class="gt_row gt_center gt_striped">12</td>
      <td class="gt_row gt_center gt_striped">5</td>
      <td class="gt_row gt_center gt_striped">50</td>
      <td class="gt_row gt_center gt_striped">35</td>
      <td class="gt_row gt_center gt_striped">16</td>
      <td class="gt_row gt_center gt_striped">16</td>
      <td class="gt_row gt_center gt_striped">18</td>
      <td class="gt_row gt_center gt_striped">19</td>
    </tr>
  </tbody>
  
  
</table></div>

``` r
tab2[age %in% c("6wpf", "8wpf", "10wpf", "12wpf"), sex:="unknown"]

dcast(data=tab2, sex ~ age2, fun.aggregate=length) %>% 
  as_tibble() %>%
  gt() %>%
  tab_header(title="Updated jag2b fish sex labels.",
             subtitle="Males and females re-labeled as \'unknown\' for ages 6- through 12-weeks.")
```

<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#crdiqhjnjh .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  /* table.margin.left */
  margin-right: auto;
  /* table.margin.right */
  color: #333333;
  font-size: 16px;
  /* table.font.size */
  background-color: #FFFFFF;
  /* table.background.color */
  width: auto;
  /* table.width */
  border-top-style: solid;
  /* table.border.top.style */
  border-top-width: 2px;
  /* table.border.top.width */
  border-top-color: #A8A8A8;
  /* table.border.top.color */
  border-bottom-style: solid;
  /* table.border.bottom.style */
  border-bottom-width: 2px;
  /* table.border.bottom.width */
  border-bottom-color: #A8A8A8;
  /* table.border.bottom.color */
}

#crdiqhjnjh .gt_heading {
  background-color: #FFFFFF;
  /* heading.background.color */
  border-bottom-color: #FFFFFF;
  /* table.background.color */
  border-left-style: hidden;
  /* heading.border.lr.style */
  border-left-width: 1px;
  /* heading.border.lr.width */
  border-left-color: #D3D3D3;
  /* heading.border.lr.color */
  border-right-style: hidden;
  /* heading.border.lr.style */
  border-right-width: 1px;
  /* heading.border.lr.width */
  border-right-color: #D3D3D3;
  /* heading.border.lr.color */
}

#crdiqhjnjh .gt_title {
  color: #333333;
  font-size: 125%;
  /* heading.title.font.size */
  font-weight: initial;
  /* heading.title.font.weight */
  padding-top: 4px;
  /* heading.top.padding - not yet used */
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  /* table.background.color */
  border-bottom-width: 0;
}

#crdiqhjnjh .gt_subtitle {
  color: #333333;
  font-size: 85%;
  /* heading.subtitle.font.size */
  font-weight: initial;
  /* heading.subtitle.font.weight */
  padding-top: 0;
  padding-bottom: 4px;
  /* heading.bottom.padding - not yet used */
  border-top-color: #FFFFFF;
  /* table.background.color */
  border-top-width: 0;
}

#crdiqhjnjh .gt_bottom_border {
  border-bottom-style: solid;
  /* heading.border.bottom.style */
  border-bottom-width: 2px;
  /* heading.border.bottom.width */
  border-bottom-color: #D3D3D3;
  /* heading.border.bottom.color */
}

#crdiqhjnjh .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  padding-top: 4px;
  padding-bottom: 4px;
}

#crdiqhjnjh .gt_col_headings {
  border-top-style: solid;
  /* column_labels.border.top.style */
  border-top-width: 2px;
  /* column_labels.border.top.width */
  border-top-color: #D3D3D3;
  /* column_labels.border.top.color */
  border-bottom-style: solid;
  /* column_labels.border.bottom.style */
  border-bottom-width: 2px;
  /* column_labels.border.bottom.width */
  border-bottom-color: #D3D3D3;
  /* column_labels.border.bottom.color */
  border-left-style: none;
  /* column_labels.border.lr.style */
  border-left-width: 1px;
  /* column_labels.border.lr.width */
  border-left-color: #D3D3D3;
  /* column_labels.border.lr.color */
  border-right-style: none;
  /* column_labels.border.lr.style */
  border-right-width: 1px;
  /* column_labels.border.lr.width */
  border-right-color: #D3D3D3;
  /* column_labels.border.lr.color */
}

#crdiqhjnjh .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  /* column_labels.background.color */
  font-size: 100%;
  /* column_labels.font.size */
  font-weight: normal;
  /* column_labels.font.weight */
  text-transform: inherit;
  /* column_labels.text_transform */
  vertical-align: middle;
  padding: 5px;
  margin: 10px;
  overflow-x: hidden;
}

#crdiqhjnjh .gt_sep_right {
  border-right: 5px solid #FFFFFF;
}

#crdiqhjnjh .gt_group_heading {
  padding: 8px;
  /* row_group.padding */
  color: #333333;
  background-color: #FFFFFF;
  /* row_group.background.color */
  font-size: 100%;
  /* row_group.font.size */
  font-weight: initial;
  /* row_group.font.weight */
  text-transform: inherit;
  /* row_group.text_transform */
  border-top-style: solid;
  /* row_group.border.top.style */
  border-top-width: 2px;
  /* row_group.border.top.width */
  border-top-color: #D3D3D3;
  /* row_group.border.top.color */
  border-bottom-style: solid;
  /* row_group.border.bottom.style */
  border-bottom-width: 2px;
  /* row_group.border.bottom.width */
  border-bottom-color: #D3D3D3;
  /* row_group.border.bottom.color */
  border-left-style: none;
  /* row_group.border.left.style */
  border-left-width: 1px;
  /* row_group.border.left.width */
  border-left-color: #D3D3D3;
  /* row_group.border.left.color */
  border-right-style: none;
  /* row_group.border.right.style */
  border-right-width: 1px;
  /* row_group.border.right.width */
  border-right-color: #D3D3D3;
  /* row_group.border.right.color */
  vertical-align: middle;
}

#crdiqhjnjh .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  /* row_group.background.color */
  font-size: 100%;
  /* row_group.font.size */
  font-weight: initial;
  /* row_group.font.weight */
  border-top-style: solid;
  /* row_group.border.top.style */
  border-top-width: 2px;
  /* row_group.border.top.width */
  border-top-color: #D3D3D3;
  /* row_group.border.top.color */
  border-bottom-style: solid;
  /* row_group.border.bottom.style */
  border-bottom-width: 2px;
  /* row_group.border.bottom.width */
  border-bottom-color: #D3D3D3;
  /* row_group.border.bottom.color */
  vertical-align: middle;
}

#crdiqhjnjh .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
  /* row.striping.background_color */
}

#crdiqhjnjh .gt_from_md > :first-child {
  margin-top: 0;
}

#crdiqhjnjh .gt_from_md > :last-child {
  margin-bottom: 0;
}

#crdiqhjnjh .gt_row {
  padding-top: 8px;
  /* data_row.padding */
  padding-bottom: 8px;
  /* data_row.padding */
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  /* table_body.hlines.style */
  border-top-width: 1px;
  /* table_body.hlines.width */
  border-top-color: #D3D3D3;
  /* table_body.hlines.color */
  border-left-style: none;
  /* table_body.vlines.style */
  border-left-width: 1px;
  /* table_body.vlines.width */
  border-left-color: #D3D3D3;
  /* table_body.vlines.color */
  border-right-style: none;
  /* table_body.vlines.style */
  border-right-width: 1px;
  /* table_body.vlines.width */
  border-right-color: #D3D3D3;
  /* table_body.vlines.color */
  vertical-align: middle;
  overflow-x: hidden;
}

#crdiqhjnjh .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  /* stub.background.color */
  font-weight: initial;
  /* stub.font.weight */
  text-transform: inherit;
  /* stub.text_transform */
  border-right-style: solid;
  /* stub.border.style */
  border-right-width: 2px;
  /* stub.border.width */
  border-right-color: #D3D3D3;
  /* stub.border.color */
  padding-left: 12px;
}

#crdiqhjnjh .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  /* summary_row.background.color */
  text-transform: inherit;
  /* summary_row.text_transform */
  padding-top: 8px;
  /* summary_row.padding */
  padding-bottom: 8px;
  /* summary_row.padding */
  padding-left: 5px;
  padding-right: 5px;
}

#crdiqhjnjh .gt_first_summary_row {
  padding-top: 8px;
  /* summary_row.padding */
  padding-bottom: 8px;
  /* summary_row.padding */
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  /* summary_row.border.style */
  border-top-width: 2px;
  /* summary_row.border.width */
  border-top-color: #D3D3D3;
  /* summary_row.border.color */
}

#crdiqhjnjh .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  /* grand_summary_row.background.color */
  text-transform: inherit;
  /* grand_summary_row.text_transform */
  padding-top: 8px;
  /* grand_summary_row.padding */
  padding-bottom: 8px;
  /* grand_summary_row.padding */
  padding-left: 5px;
  padding-right: 5px;
}

#crdiqhjnjh .gt_first_grand_summary_row {
  padding-top: 8px;
  /* grand_summary_row.padding */
  padding-bottom: 8px;
  /* grand_summary_row.padding */
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  /* grand_summary_row.border.style */
  border-top-width: 6px;
  /* grand_summary_row.border.width */
  border-top-color: #D3D3D3;
  /* grand_summary_row.border.color */
}

#crdiqhjnjh .gt_table_body {
  border-top-style: solid;
  /* table_body.border.top.style */
  border-top-width: 2px;
  /* table_body.border.top.width */
  border-top-color: #D3D3D3;
  /* table_body.border.top.color */
  border-bottom-style: solid;
  /* table_body.border.bottom.style */
  border-bottom-width: 2px;
  /* table_body.border.bottom.width */
  border-bottom-color: #D3D3D3;
  /* table_body.border.bottom.color */
}

#crdiqhjnjh .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  /* footnotes.background.color */
  border-bottom-style: none;
  /* footnotes.border.bottom.style */
  border-bottom-width: 2px;
  /* footnotes.border.bottom.width */
  border-bottom-color: #D3D3D3;
  /* footnotes.border.bottom.color */
  border-left-style: none;
  /* footnotes.border.lr.color */
  border-left-width: 2px;
  /* footnotes.border.lr.color */
  border-left-color: #D3D3D3;
  /* footnotes.border.lr.color */
  border-right-style: none;
  /* footnotes.border.lr.color */
  border-right-width: 2px;
  /* footnotes.border.lr.color */
  border-right-color: #D3D3D3;
  /* footnotes.border.lr.color */
}

#crdiqhjnjh .gt_footnote {
  margin: 0px;
  font-size: 90%;
  /* footnotes.font.size */
  padding: 4px;
  /* footnotes.padding */
}

#crdiqhjnjh .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  /* source_notes.background.color */
  border-bottom-style: none;
  /* source_notes.border.bottom.style */
  border-bottom-width: 2px;
  /* source_notes.border.bottom.width */
  border-bottom-color: #D3D3D3;
  /* source_notes.border.bottom.color */
  border-left-style: none;
  /* source_notes.border.lr.style */
  border-left-width: 2px;
  /* source_notes.border.lr.style */
  border-left-color: #D3D3D3;
  /* source_notes.border.lr.style */
  border-right-style: none;
  /* source_notes.border.lr.style */
  border-right-width: 2px;
  /* source_notes.border.lr.style */
  border-right-color: #D3D3D3;
  /* source_notes.border.lr.style */
}

#crdiqhjnjh .gt_sourcenote {
  font-size: 90%;
  /* source_notes.font.size */
  padding: 4px;
  /* source_notes.padding */
}

#crdiqhjnjh .gt_left {
  text-align: left;
}

#crdiqhjnjh .gt_center {
  text-align: center;
}

#crdiqhjnjh .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#crdiqhjnjh .gt_font_normal {
  font-weight: normal;
}

#crdiqhjnjh .gt_font_bold {
  font-weight: bold;
}

#crdiqhjnjh .gt_font_italic {
  font-style: italic;
}

#crdiqhjnjh .gt_super {
  font-size: 65%;
}

#crdiqhjnjh .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>
<div id="crdiqhjnjh" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;"><table class="gt_table">
  <thead class="gt_header">
    <tr>
      <th colspan="11" class="gt_heading gt_title gt_font_normal gt_center" style>Updated jag2b fish sex labels.</th>
    </tr>
    <tr>
      <th colspan="11" class="gt_heading gt_subtitle gt_font_normal gt_center gt_bottom_border" style>Males and females re-labeled as 'unknown' for ages 6- through 12-weeks.</th>
    </tr>
  </thead>
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">sex</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">6 weeks</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">8 weeks</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">10 weeks</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">12 weeks</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">4 months</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">6 months</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">8 months</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">10 months</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">13 months</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">24 months</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr>
      <td class="gt_row gt_left">M</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_center">50</td>
      <td class="gt_row gt_center">35</td>
      <td class="gt_row gt_center">16</td>
      <td class="gt_row gt_center">16</td>
      <td class="gt_row gt_center">18</td>
      <td class="gt_row gt_center">19</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_striped">unknown</td>
      <td class="gt_row gt_center gt_striped">35</td>
      <td class="gt_row gt_center gt_striped">24</td>
      <td class="gt_row gt_center gt_striped">23</td>
      <td class="gt_row gt_center gt_striped">9</td>
      <td class="gt_row gt_center gt_striped">0</td>
      <td class="gt_row gt_center gt_striped">0</td>
      <td class="gt_row gt_center gt_striped">0</td>
      <td class="gt_row gt_center gt_striped">0</td>
      <td class="gt_row gt_center gt_striped">0</td>
      <td class="gt_row gt_center gt_striped">0</td>
    </tr>
  </tbody>
  
  
</table></div>

``` r
# Plot weights and lengths.
# Use 'tab2' data (updated to combine males and females, remove some females).

genotype_colors  = c( "WT"="#80b1d3",
                     "HET"="#b3de69",
                     "MUT"="#fb8072")

genotype2_colors  = c("+/+"="#80b1d3",
                      "+/-"="#b3de69",
                      "-/-"="#fb8072")

# Compute summary statistics, grouped by age2 and genotype2.
by_genotype = tab2[, list(mean_weight=mean(weight_gr, na.rm=TRUE),
                           sd_weight=sd(weight_gr, na.rm=TRUE),
                         mean_length=mean(length_cm, na.rm=TRUE),
                         sd_length=sd(length_cm, na.rm=TRUE)),
                      by=list(age2, genotype2)]

# Expand y-axis 5% on bottom end and 25% on upper end, to make room for p-values.
yaxis_expand = expansion(mult=c(0.05, 0.25))

p2 = ggplot() +
     theme_bw() +
     geom_errorbar(data=by_genotype,
                   aes(x=genotype2,
                       ymin=mean_weight - sd_weight,
                       ymax=mean_weight + sd_weight,
                       color=genotype2),
                   linewidth=0.625,
                   width=0.6) +
     geom_segment(data=by_genotype,
                  aes(y=mean_weight,
                      yend=mean_weight,
                      x=as.integer(genotype2) - 0.45,
                      xend=as.integer(genotype2) + 0.45,
                      color=genotype2),
                  linewidth=1.0) +
     geom_point(data=tab2,
                aes(x=genotype2, 
                    y=weight_gr,
                    fill=genotype2,
                    shape=sex),
                #shape=21,
                color="grey30",
                size=1.4) +
     scale_shape_manual(values=c(M=21, unknown=24)) +
     scale_color_manual(values=genotype2_colors) +
     scale_fill_manual(values=genotype2_colors) +
     scale_y_continuous(expand=yaxis_expand) +
     guides(fill="none") +
     # theme(panel.grid.minor.y=element_blank()) +
     # theme(panel.grid.major.x=element_blank()) +
     # theme(axis.text.x=element_text(size=rel(0.8))) +
#     labs(title="Jag2b weight data, 10 developmental time points.") +
#     labs(subtitle="Horizontal bar = mean, whiskers = +/- standard deviation") +
     labs(x=NULL, y="Weight (grams)", color="Genotype", shape="Sex") +
     facet_grid(cols=vars(age2))


# Lengths.
p3 = ggplot() +
     theme_bw() +
     geom_errorbar(data=by_genotype,
                   aes(x=genotype2,
                       ymin=mean_length - sd_length,
                       ymax=mean_length + sd_length,
                       color=genotype2),
                   linewidth=0.625,
                   width=0.6) +
     geom_segment(data=by_genotype,
                  aes(y=mean_length,
                      yend=mean_length,
                      x=as.integer(genotype2) - 0.45,
                      xend=as.integer(genotype2) + 0.45,
                      color=genotype2),
                  linewidth=1.0) +
     geom_point(data=tab2,
                aes(x=genotype2, 
                    y=length_cm,
                    fill=genotype2,
                    shape=sex),
                #shape=21,
                color="grey30",
                size=1.4) +
     scale_shape_manual(values=c(M=21, unknown=24)) +
     scale_color_manual(values=genotype2_colors) +
     scale_fill_manual(values=genotype2_colors) +
     scale_y_continuous(expand=yaxis_expand) +
     guides(fill="none") +
     # theme(panel.grid.minor.y=element_blank()) +
     # theme(panel.grid.major.x=element_blank()) +
     theme(axis.text.x=element_text(size=rel(0.8))) +
#      labs(title="Jag2b length data, 10 developmental time points.") +
#      labs(subtitle="Horizontal bar = mean, whiskers = +/- standard deviation") +
     labs(x=NULL, y="Length (cm)", color="Genotype", shape="Sex") +
     facet_grid(cols=vars(age2))


p2_p3 = p2 / p3 + plot_layout(guides='collect')
      #  + plot_annotation(title="Jag2b weight and length data, 10 developmental time points.",
      #    subtitle="Horizontal bar = mean, whiskers = +/- standard deviation")


# add 'useDingbats=FALSE' to allow file to open correctly in Illustrator.
ggsave(here("figure_1", "weights_lengths_10ages_20230329.pdf"),
       plot=p2_p3, 
       height=6, width=10, 
       useDingbats=FALSE)
```

    ## Warning: Removed 2 rows containing missing values (`geom_point()`).

    ## Warning: Removed 4 rows containing missing values (`geom_point()`).

``` r
print(p2_p3)
```

    ## Warning: Removed 2 rows containing missing values (`geom_point()`).

    ## Warning: Removed 4 rows containing missing values (`geom_point()`).

![](figure_1_weight_and_length_files/figure-gfm/print-figure-in-markdown-1.png)<!-- -->

``` r
# Example code for pairwise t-test (wt-mut, het-mut, wt-het) for
# each timepoint.

# (1) Using Anova + TukeyHSD does not allow us to choose only the comparisons
#     of interest. For example we want 3 pairwise genotype comparison _within_
#     each timepoint.
# (2) Instead we will stick with previous method of running selected pairwise
#     t-tests, combined with FDR-corrected p-values.
# (3) Problem remaining -- How to correct 95% conf intervals for multiple
#     comparisons? `multcomp` package may have Tukey-stype 'family-wise' 
#     correction for confidence intervals.



comp_tab = data.table(group1=rep(c("WT", "WT", "HET"), times=2),
                      group2=rep(c("HET", "MUT", "MUT"), times=2),
                      variable=rep(c("weight_gr", "length_cm"), each=3),
                      age=rep(levels(tab2$age2), each=6))

table(comp_tab$age, comp_tab$variable)
```

    ##            
    ##             length_cm weight_gr
    ##   10 months         3         3
    ##   10 weeks          3         3
    ##   12 weeks          3         3
    ##   13 months         3         3
    ##   24 months         3         3
    ##   4 months          3         3
    ##   6 months          3         3
    ##   6 weeks           3         3
    ##   8 months          3         3
    ##   8 weeks           3         3

``` r
tres_list = list()

for (i in seq(nrow(comp_tab))) {
  row_i = comp_tab[i]
  tmp_group1 = row_i$group1
  tmp_group2 = row_i$group2
  tmp_age = row_i$age
  tmp_var = row_i$variable
  # what is `..` syntax?
  group1_data = tab2[genotype == tmp_group1 & age2 == tmp_age, ..tmp_var]
  
  group2_data = tab2[genotype == tmp_group2 & age2 == tmp_age, ..tmp_var]
  
  if (nrow(group1_data) > 0L & nrow(group2_data) > 0L) {
      
    tmp_res = t.test(x=group1_data,
                     y=group2_data,
                     alternative = "two.sided",
                     conf.level=0.95)
  
    new_row = data.table(group1=tmp_group1,
                         group2=tmp_group2,
                         age=tmp_age,
                         comparison_variable=tmp_var,
                         effect_size=tmp_res$estimate[1] - tmp_res$estimate[2], # (mutant or het mean) - wt mean
                         pvalue=tmp_res$p.value,
                         conf_lower=tmp_res$conf.int[1],
                         conf_upper=tmp_res$conf.int[2],
                         sample_size_group1=nrow(group1_data),
                         sample_size_group2=nrow(group2_data),
                         t_statistic=tmp_res$statistic,
                         df=tmp_res$parameter)
  } else {
    # When one group has no fish, fill in t-test results with NA values.
    new_row = data.table(group1=tmp_group1,
                         group2=tmp_group2,
                         age=tmp_age,
                         comparison_variable=tmp_var,
                         effect_size=NA_real_,
                         pvalue=NA_real_,
                         conf_lower=NA_real_,
                         conf_upper=NA_real_,
                         sample_size_group1=nrow(group1_data),
                         sample_size_group2=nrow(group2_data),
                         t_statistic=NA_real_,
                         df=NA_real_)
  }
  
  tres_list[[i]] = new_row
                       
}


# Summary table of all pair-wise t.test results and related values.
stats_tab = rbindlist(tres_list)

n_comparisons = sum(!is.na(stats_tab$pvalue))

stats_tab[, padj:=p.adjust(pvalue, method="fdr", n=n_comparisons)]

stats_tab[, genotype_pair:=paste(group1, group2, sep="-")]

stats_tab[, genotype_pair:=factor(genotype_pair,
                                  levels=c("WT-HET", "HET-MUT", "WT-MUT"))]

# Format padj values for adding to figures.
# Format string: 0 = leading zeros, .2 = precision , 
# "g" = precision is interpreted as n of significant digits.
stats_tab[, padj_format:=sprintf("%0.2g", padj)]

# Output stats to text file.
fwrite(stats_tab, 
       file=here("figure_1",
                 "pairwise_ttest_results_weight_length_10ages_20230403.txt"), 
       sep="\t")


# Write out a filtered, sorted version for Leo to use adding pvalues in AI.

stats2 = stats_tab[padj < 0.05, 
                   list(age, comparison_variable, genotype_pair, padj_format)]

stats2[, age:=factor(age, levels=age_order_vec)]


setorder(stats2, comparison_variable, age, -genotype_pair)

library(writexl)
write_xlsx(stats2, 
           path=here("figure_1", 
                     "pairwise_ttest_padj05_table_for_figure_1_20230403.xlsx"))
```

``` r
  stats2 %>% as_tibble() %>%
  gt() %>%
  tab_header(title="Pairwise t-test results within each timepoint. FDR-adj p-value < 0.05")
```

<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#mknzyrseex .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  /* table.margin.left */
  margin-right: auto;
  /* table.margin.right */
  color: #333333;
  font-size: 16px;
  /* table.font.size */
  background-color: #FFFFFF;
  /* table.background.color */
  width: auto;
  /* table.width */
  border-top-style: solid;
  /* table.border.top.style */
  border-top-width: 2px;
  /* table.border.top.width */
  border-top-color: #A8A8A8;
  /* table.border.top.color */
  border-bottom-style: solid;
  /* table.border.bottom.style */
  border-bottom-width: 2px;
  /* table.border.bottom.width */
  border-bottom-color: #A8A8A8;
  /* table.border.bottom.color */
}

#mknzyrseex .gt_heading {
  background-color: #FFFFFF;
  /* heading.background.color */
  border-bottom-color: #FFFFFF;
  /* table.background.color */
  border-left-style: hidden;
  /* heading.border.lr.style */
  border-left-width: 1px;
  /* heading.border.lr.width */
  border-left-color: #D3D3D3;
  /* heading.border.lr.color */
  border-right-style: hidden;
  /* heading.border.lr.style */
  border-right-width: 1px;
  /* heading.border.lr.width */
  border-right-color: #D3D3D3;
  /* heading.border.lr.color */
}

#mknzyrseex .gt_title {
  color: #333333;
  font-size: 125%;
  /* heading.title.font.size */
  font-weight: initial;
  /* heading.title.font.weight */
  padding-top: 4px;
  /* heading.top.padding - not yet used */
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  /* table.background.color */
  border-bottom-width: 0;
}

#mknzyrseex .gt_subtitle {
  color: #333333;
  font-size: 85%;
  /* heading.subtitle.font.size */
  font-weight: initial;
  /* heading.subtitle.font.weight */
  padding-top: 0;
  padding-bottom: 4px;
  /* heading.bottom.padding - not yet used */
  border-top-color: #FFFFFF;
  /* table.background.color */
  border-top-width: 0;
}

#mknzyrseex .gt_bottom_border {
  border-bottom-style: solid;
  /* heading.border.bottom.style */
  border-bottom-width: 2px;
  /* heading.border.bottom.width */
  border-bottom-color: #D3D3D3;
  /* heading.border.bottom.color */
}

#mknzyrseex .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  padding-top: 4px;
  padding-bottom: 4px;
}

#mknzyrseex .gt_col_headings {
  border-top-style: solid;
  /* column_labels.border.top.style */
  border-top-width: 2px;
  /* column_labels.border.top.width */
  border-top-color: #D3D3D3;
  /* column_labels.border.top.color */
  border-bottom-style: solid;
  /* column_labels.border.bottom.style */
  border-bottom-width: 2px;
  /* column_labels.border.bottom.width */
  border-bottom-color: #D3D3D3;
  /* column_labels.border.bottom.color */
  border-left-style: none;
  /* column_labels.border.lr.style */
  border-left-width: 1px;
  /* column_labels.border.lr.width */
  border-left-color: #D3D3D3;
  /* column_labels.border.lr.color */
  border-right-style: none;
  /* column_labels.border.lr.style */
  border-right-width: 1px;
  /* column_labels.border.lr.width */
  border-right-color: #D3D3D3;
  /* column_labels.border.lr.color */
}

#mknzyrseex .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  /* column_labels.background.color */
  font-size: 100%;
  /* column_labels.font.size */
  font-weight: normal;
  /* column_labels.font.weight */
  text-transform: inherit;
  /* column_labels.text_transform */
  vertical-align: middle;
  padding: 5px;
  margin: 10px;
  overflow-x: hidden;
}

#mknzyrseex .gt_sep_right {
  border-right: 5px solid #FFFFFF;
}

#mknzyrseex .gt_group_heading {
  padding: 8px;
  /* row_group.padding */
  color: #333333;
  background-color: #FFFFFF;
  /* row_group.background.color */
  font-size: 100%;
  /* row_group.font.size */
  font-weight: initial;
  /* row_group.font.weight */
  text-transform: inherit;
  /* row_group.text_transform */
  border-top-style: solid;
  /* row_group.border.top.style */
  border-top-width: 2px;
  /* row_group.border.top.width */
  border-top-color: #D3D3D3;
  /* row_group.border.top.color */
  border-bottom-style: solid;
  /* row_group.border.bottom.style */
  border-bottom-width: 2px;
  /* row_group.border.bottom.width */
  border-bottom-color: #D3D3D3;
  /* row_group.border.bottom.color */
  border-left-style: none;
  /* row_group.border.left.style */
  border-left-width: 1px;
  /* row_group.border.left.width */
  border-left-color: #D3D3D3;
  /* row_group.border.left.color */
  border-right-style: none;
  /* row_group.border.right.style */
  border-right-width: 1px;
  /* row_group.border.right.width */
  border-right-color: #D3D3D3;
  /* row_group.border.right.color */
  vertical-align: middle;
}

#mknzyrseex .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  /* row_group.background.color */
  font-size: 100%;
  /* row_group.font.size */
  font-weight: initial;
  /* row_group.font.weight */
  border-top-style: solid;
  /* row_group.border.top.style */
  border-top-width: 2px;
  /* row_group.border.top.width */
  border-top-color: #D3D3D3;
  /* row_group.border.top.color */
  border-bottom-style: solid;
  /* row_group.border.bottom.style */
  border-bottom-width: 2px;
  /* row_group.border.bottom.width */
  border-bottom-color: #D3D3D3;
  /* row_group.border.bottom.color */
  vertical-align: middle;
}

#mknzyrseex .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
  /* row.striping.background_color */
}

#mknzyrseex .gt_from_md > :first-child {
  margin-top: 0;
}

#mknzyrseex .gt_from_md > :last-child {
  margin-bottom: 0;
}

#mknzyrseex .gt_row {
  padding-top: 8px;
  /* data_row.padding */
  padding-bottom: 8px;
  /* data_row.padding */
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  /* table_body.hlines.style */
  border-top-width: 1px;
  /* table_body.hlines.width */
  border-top-color: #D3D3D3;
  /* table_body.hlines.color */
  border-left-style: none;
  /* table_body.vlines.style */
  border-left-width: 1px;
  /* table_body.vlines.width */
  border-left-color: #D3D3D3;
  /* table_body.vlines.color */
  border-right-style: none;
  /* table_body.vlines.style */
  border-right-width: 1px;
  /* table_body.vlines.width */
  border-right-color: #D3D3D3;
  /* table_body.vlines.color */
  vertical-align: middle;
  overflow-x: hidden;
}

#mknzyrseex .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  /* stub.background.color */
  font-weight: initial;
  /* stub.font.weight */
  text-transform: inherit;
  /* stub.text_transform */
  border-right-style: solid;
  /* stub.border.style */
  border-right-width: 2px;
  /* stub.border.width */
  border-right-color: #D3D3D3;
  /* stub.border.color */
  padding-left: 12px;
}

#mknzyrseex .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  /* summary_row.background.color */
  text-transform: inherit;
  /* summary_row.text_transform */
  padding-top: 8px;
  /* summary_row.padding */
  padding-bottom: 8px;
  /* summary_row.padding */
  padding-left: 5px;
  padding-right: 5px;
}

#mknzyrseex .gt_first_summary_row {
  padding-top: 8px;
  /* summary_row.padding */
  padding-bottom: 8px;
  /* summary_row.padding */
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  /* summary_row.border.style */
  border-top-width: 2px;
  /* summary_row.border.width */
  border-top-color: #D3D3D3;
  /* summary_row.border.color */
}

#mknzyrseex .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  /* grand_summary_row.background.color */
  text-transform: inherit;
  /* grand_summary_row.text_transform */
  padding-top: 8px;
  /* grand_summary_row.padding */
  padding-bottom: 8px;
  /* grand_summary_row.padding */
  padding-left: 5px;
  padding-right: 5px;
}

#mknzyrseex .gt_first_grand_summary_row {
  padding-top: 8px;
  /* grand_summary_row.padding */
  padding-bottom: 8px;
  /* grand_summary_row.padding */
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  /* grand_summary_row.border.style */
  border-top-width: 6px;
  /* grand_summary_row.border.width */
  border-top-color: #D3D3D3;
  /* grand_summary_row.border.color */
}

#mknzyrseex .gt_table_body {
  border-top-style: solid;
  /* table_body.border.top.style */
  border-top-width: 2px;
  /* table_body.border.top.width */
  border-top-color: #D3D3D3;
  /* table_body.border.top.color */
  border-bottom-style: solid;
  /* table_body.border.bottom.style */
  border-bottom-width: 2px;
  /* table_body.border.bottom.width */
  border-bottom-color: #D3D3D3;
  /* table_body.border.bottom.color */
}

#mknzyrseex .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  /* footnotes.background.color */
  border-bottom-style: none;
  /* footnotes.border.bottom.style */
  border-bottom-width: 2px;
  /* footnotes.border.bottom.width */
  border-bottom-color: #D3D3D3;
  /* footnotes.border.bottom.color */
  border-left-style: none;
  /* footnotes.border.lr.color */
  border-left-width: 2px;
  /* footnotes.border.lr.color */
  border-left-color: #D3D3D3;
  /* footnotes.border.lr.color */
  border-right-style: none;
  /* footnotes.border.lr.color */
  border-right-width: 2px;
  /* footnotes.border.lr.color */
  border-right-color: #D3D3D3;
  /* footnotes.border.lr.color */
}

#mknzyrseex .gt_footnote {
  margin: 0px;
  font-size: 90%;
  /* footnotes.font.size */
  padding: 4px;
  /* footnotes.padding */
}

#mknzyrseex .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  /* source_notes.background.color */
  border-bottom-style: none;
  /* source_notes.border.bottom.style */
  border-bottom-width: 2px;
  /* source_notes.border.bottom.width */
  border-bottom-color: #D3D3D3;
  /* source_notes.border.bottom.color */
  border-left-style: none;
  /* source_notes.border.lr.style */
  border-left-width: 2px;
  /* source_notes.border.lr.style */
  border-left-color: #D3D3D3;
  /* source_notes.border.lr.style */
  border-right-style: none;
  /* source_notes.border.lr.style */
  border-right-width: 2px;
  /* source_notes.border.lr.style */
  border-right-color: #D3D3D3;
  /* source_notes.border.lr.style */
}

#mknzyrseex .gt_sourcenote {
  font-size: 90%;
  /* source_notes.font.size */
  padding: 4px;
  /* source_notes.padding */
}

#mknzyrseex .gt_left {
  text-align: left;
}

#mknzyrseex .gt_center {
  text-align: center;
}

#mknzyrseex .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#mknzyrseex .gt_font_normal {
  font-weight: normal;
}

#mknzyrseex .gt_font_bold {
  font-weight: bold;
}

#mknzyrseex .gt_font_italic {
  font-style: italic;
}

#mknzyrseex .gt_super {
  font-size: 65%;
}

#mknzyrseex .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>
<div id="mknzyrseex" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;"><table class="gt_table">
  <thead class="gt_header">
    <tr>
      <th colspan="4" class="gt_heading gt_title gt_font_normal gt_center" style>Pairwise t-test results within each timepoint. FDR-adj p-value &lt; 0.05</th>
    </tr>
    <tr>
      <th colspan="4" class="gt_heading gt_subtitle gt_font_normal gt_center gt_bottom_border" style></th>
    </tr>
  </thead>
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">age</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">comparison_variable</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">genotype_pair</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">padj_format</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr>
      <td class="gt_row gt_center">6 weeks</td>
      <td class="gt_row gt_left">length_cm</td>
      <td class="gt_row gt_center">WT-MUT</td>
      <td class="gt_row gt_left">6.6e-07</td>
    </tr>
    <tr>
      <td class="gt_row gt_center gt_striped">6 weeks</td>
      <td class="gt_row gt_left gt_striped">length_cm</td>
      <td class="gt_row gt_center gt_striped">HET-MUT</td>
      <td class="gt_row gt_left gt_striped">7e-07</td>
    </tr>
    <tr>
      <td class="gt_row gt_center">8 weeks</td>
      <td class="gt_row gt_left">length_cm</td>
      <td class="gt_row gt_center">WT-MUT</td>
      <td class="gt_row gt_left">0.00098</td>
    </tr>
    <tr>
      <td class="gt_row gt_center gt_striped">8 weeks</td>
      <td class="gt_row gt_left gt_striped">length_cm</td>
      <td class="gt_row gt_center gt_striped">HET-MUT</td>
      <td class="gt_row gt_left gt_striped">0.00023</td>
    </tr>
    <tr>
      <td class="gt_row gt_center">10 weeks</td>
      <td class="gt_row gt_left">length_cm</td>
      <td class="gt_row gt_center">WT-MUT</td>
      <td class="gt_row gt_left">8.1e-05</td>
    </tr>
    <tr>
      <td class="gt_row gt_center gt_striped">10 weeks</td>
      <td class="gt_row gt_left gt_striped">length_cm</td>
      <td class="gt_row gt_center gt_striped">HET-MUT</td>
      <td class="gt_row gt_left gt_striped">0.00019</td>
    </tr>
    <tr>
      <td class="gt_row gt_center">4 months</td>
      <td class="gt_row gt_left">length_cm</td>
      <td class="gt_row gt_center">WT-MUT</td>
      <td class="gt_row gt_left">0.031</td>
    </tr>
    <tr>
      <td class="gt_row gt_center gt_striped">4 months</td>
      <td class="gt_row gt_left gt_striped">length_cm</td>
      <td class="gt_row gt_center gt_striped">HET-MUT</td>
      <td class="gt_row gt_left gt_striped">0.014</td>
    </tr>
    <tr>
      <td class="gt_row gt_center">6 months</td>
      <td class="gt_row gt_left">length_cm</td>
      <td class="gt_row gt_center">WT-MUT</td>
      <td class="gt_row gt_left">0.0072</td>
    </tr>
    <tr>
      <td class="gt_row gt_center gt_striped">10 months</td>
      <td class="gt_row gt_left gt_striped">length_cm</td>
      <td class="gt_row gt_center gt_striped">HET-MUT</td>
      <td class="gt_row gt_left gt_striped">0.0031</td>
    </tr>
    <tr>
      <td class="gt_row gt_center">13 months</td>
      <td class="gt_row gt_left">length_cm</td>
      <td class="gt_row gt_center">WT-MUT</td>
      <td class="gt_row gt_left">0.017</td>
    </tr>
    <tr>
      <td class="gt_row gt_center gt_striped">6 weeks</td>
      <td class="gt_row gt_left gt_striped">weight_gr</td>
      <td class="gt_row gt_center gt_striped">WT-MUT</td>
      <td class="gt_row gt_left gt_striped">7e-07</td>
    </tr>
    <tr>
      <td class="gt_row gt_center">6 weeks</td>
      <td class="gt_row gt_left">weight_gr</td>
      <td class="gt_row gt_center">HET-MUT</td>
      <td class="gt_row gt_left">2.2e-08</td>
    </tr>
    <tr>
      <td class="gt_row gt_center gt_striped">8 weeks</td>
      <td class="gt_row gt_left gt_striped">weight_gr</td>
      <td class="gt_row gt_center gt_striped">WT-MUT</td>
      <td class="gt_row gt_left gt_striped">0.00019</td>
    </tr>
    <tr>
      <td class="gt_row gt_center">8 weeks</td>
      <td class="gt_row gt_left">weight_gr</td>
      <td class="gt_row gt_center">HET-MUT</td>
      <td class="gt_row gt_left">6.5e-06</td>
    </tr>
    <tr>
      <td class="gt_row gt_center gt_striped">10 weeks</td>
      <td class="gt_row gt_left gt_striped">weight_gr</td>
      <td class="gt_row gt_center gt_striped">WT-MUT</td>
      <td class="gt_row gt_left gt_striped">0.00011</td>
    </tr>
    <tr>
      <td class="gt_row gt_center">10 weeks</td>
      <td class="gt_row gt_left">weight_gr</td>
      <td class="gt_row gt_center">HET-MUT</td>
      <td class="gt_row gt_left">0.00011</td>
    </tr>
    <tr>
      <td class="gt_row gt_center gt_striped">4 months</td>
      <td class="gt_row gt_left gt_striped">weight_gr</td>
      <td class="gt_row gt_center gt_striped">HET-MUT</td>
      <td class="gt_row gt_left gt_striped">0.025</td>
    </tr>
    <tr>
      <td class="gt_row gt_center">10 months</td>
      <td class="gt_row gt_left">weight_gr</td>
      <td class="gt_row gt_center">HET-MUT</td>
      <td class="gt_row gt_left">0.00041</td>
    </tr>
    <tr>
      <td class="gt_row gt_center gt_striped">13 months</td>
      <td class="gt_row gt_left gt_striped">weight_gr</td>
      <td class="gt_row gt_center gt_striped">WT-MUT</td>
      <td class="gt_row gt_left gt_striped">0.0016</td>
    </tr>
    <tr>
      <td class="gt_row gt_center">13 months</td>
      <td class="gt_row gt_left">weight_gr</td>
      <td class="gt_row gt_center">HET-MUT</td>
      <td class="gt_row gt_left">0.017</td>
    </tr>
  </tbody>
  
  
</table></div>

``` r
  stats_tab %>% as_tibble() %>%
  gt() %>%
  tab_header(title="Pairwise t-test results with each timepoint. All results.")
```

<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#yiufsyedoe .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  /* table.margin.left */
  margin-right: auto;
  /* table.margin.right */
  color: #333333;
  font-size: 16px;
  /* table.font.size */
  background-color: #FFFFFF;
  /* table.background.color */
  width: auto;
  /* table.width */
  border-top-style: solid;
  /* table.border.top.style */
  border-top-width: 2px;
  /* table.border.top.width */
  border-top-color: #A8A8A8;
  /* table.border.top.color */
  border-bottom-style: solid;
  /* table.border.bottom.style */
  border-bottom-width: 2px;
  /* table.border.bottom.width */
  border-bottom-color: #A8A8A8;
  /* table.border.bottom.color */
}

#yiufsyedoe .gt_heading {
  background-color: #FFFFFF;
  /* heading.background.color */
  border-bottom-color: #FFFFFF;
  /* table.background.color */
  border-left-style: hidden;
  /* heading.border.lr.style */
  border-left-width: 1px;
  /* heading.border.lr.width */
  border-left-color: #D3D3D3;
  /* heading.border.lr.color */
  border-right-style: hidden;
  /* heading.border.lr.style */
  border-right-width: 1px;
  /* heading.border.lr.width */
  border-right-color: #D3D3D3;
  /* heading.border.lr.color */
}

#yiufsyedoe .gt_title {
  color: #333333;
  font-size: 125%;
  /* heading.title.font.size */
  font-weight: initial;
  /* heading.title.font.weight */
  padding-top: 4px;
  /* heading.top.padding - not yet used */
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  /* table.background.color */
  border-bottom-width: 0;
}

#yiufsyedoe .gt_subtitle {
  color: #333333;
  font-size: 85%;
  /* heading.subtitle.font.size */
  font-weight: initial;
  /* heading.subtitle.font.weight */
  padding-top: 0;
  padding-bottom: 4px;
  /* heading.bottom.padding - not yet used */
  border-top-color: #FFFFFF;
  /* table.background.color */
  border-top-width: 0;
}

#yiufsyedoe .gt_bottom_border {
  border-bottom-style: solid;
  /* heading.border.bottom.style */
  border-bottom-width: 2px;
  /* heading.border.bottom.width */
  border-bottom-color: #D3D3D3;
  /* heading.border.bottom.color */
}

#yiufsyedoe .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  padding-top: 4px;
  padding-bottom: 4px;
}

#yiufsyedoe .gt_col_headings {
  border-top-style: solid;
  /* column_labels.border.top.style */
  border-top-width: 2px;
  /* column_labels.border.top.width */
  border-top-color: #D3D3D3;
  /* column_labels.border.top.color */
  border-bottom-style: solid;
  /* column_labels.border.bottom.style */
  border-bottom-width: 2px;
  /* column_labels.border.bottom.width */
  border-bottom-color: #D3D3D3;
  /* column_labels.border.bottom.color */
  border-left-style: none;
  /* column_labels.border.lr.style */
  border-left-width: 1px;
  /* column_labels.border.lr.width */
  border-left-color: #D3D3D3;
  /* column_labels.border.lr.color */
  border-right-style: none;
  /* column_labels.border.lr.style */
  border-right-width: 1px;
  /* column_labels.border.lr.width */
  border-right-color: #D3D3D3;
  /* column_labels.border.lr.color */
}

#yiufsyedoe .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  /* column_labels.background.color */
  font-size: 100%;
  /* column_labels.font.size */
  font-weight: normal;
  /* column_labels.font.weight */
  text-transform: inherit;
  /* column_labels.text_transform */
  vertical-align: middle;
  padding: 5px;
  margin: 10px;
  overflow-x: hidden;
}

#yiufsyedoe .gt_sep_right {
  border-right: 5px solid #FFFFFF;
}

#yiufsyedoe .gt_group_heading {
  padding: 8px;
  /* row_group.padding */
  color: #333333;
  background-color: #FFFFFF;
  /* row_group.background.color */
  font-size: 100%;
  /* row_group.font.size */
  font-weight: initial;
  /* row_group.font.weight */
  text-transform: inherit;
  /* row_group.text_transform */
  border-top-style: solid;
  /* row_group.border.top.style */
  border-top-width: 2px;
  /* row_group.border.top.width */
  border-top-color: #D3D3D3;
  /* row_group.border.top.color */
  border-bottom-style: solid;
  /* row_group.border.bottom.style */
  border-bottom-width: 2px;
  /* row_group.border.bottom.width */
  border-bottom-color: #D3D3D3;
  /* row_group.border.bottom.color */
  border-left-style: none;
  /* row_group.border.left.style */
  border-left-width: 1px;
  /* row_group.border.left.width */
  border-left-color: #D3D3D3;
  /* row_group.border.left.color */
  border-right-style: none;
  /* row_group.border.right.style */
  border-right-width: 1px;
  /* row_group.border.right.width */
  border-right-color: #D3D3D3;
  /* row_group.border.right.color */
  vertical-align: middle;
}

#yiufsyedoe .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  /* row_group.background.color */
  font-size: 100%;
  /* row_group.font.size */
  font-weight: initial;
  /* row_group.font.weight */
  border-top-style: solid;
  /* row_group.border.top.style */
  border-top-width: 2px;
  /* row_group.border.top.width */
  border-top-color: #D3D3D3;
  /* row_group.border.top.color */
  border-bottom-style: solid;
  /* row_group.border.bottom.style */
  border-bottom-width: 2px;
  /* row_group.border.bottom.width */
  border-bottom-color: #D3D3D3;
  /* row_group.border.bottom.color */
  vertical-align: middle;
}

#yiufsyedoe .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
  /* row.striping.background_color */
}

#yiufsyedoe .gt_from_md > :first-child {
  margin-top: 0;
}

#yiufsyedoe .gt_from_md > :last-child {
  margin-bottom: 0;
}

#yiufsyedoe .gt_row {
  padding-top: 8px;
  /* data_row.padding */
  padding-bottom: 8px;
  /* data_row.padding */
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  /* table_body.hlines.style */
  border-top-width: 1px;
  /* table_body.hlines.width */
  border-top-color: #D3D3D3;
  /* table_body.hlines.color */
  border-left-style: none;
  /* table_body.vlines.style */
  border-left-width: 1px;
  /* table_body.vlines.width */
  border-left-color: #D3D3D3;
  /* table_body.vlines.color */
  border-right-style: none;
  /* table_body.vlines.style */
  border-right-width: 1px;
  /* table_body.vlines.width */
  border-right-color: #D3D3D3;
  /* table_body.vlines.color */
  vertical-align: middle;
  overflow-x: hidden;
}

#yiufsyedoe .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  /* stub.background.color */
  font-weight: initial;
  /* stub.font.weight */
  text-transform: inherit;
  /* stub.text_transform */
  border-right-style: solid;
  /* stub.border.style */
  border-right-width: 2px;
  /* stub.border.width */
  border-right-color: #D3D3D3;
  /* stub.border.color */
  padding-left: 12px;
}

#yiufsyedoe .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  /* summary_row.background.color */
  text-transform: inherit;
  /* summary_row.text_transform */
  padding-top: 8px;
  /* summary_row.padding */
  padding-bottom: 8px;
  /* summary_row.padding */
  padding-left: 5px;
  padding-right: 5px;
}

#yiufsyedoe .gt_first_summary_row {
  padding-top: 8px;
  /* summary_row.padding */
  padding-bottom: 8px;
  /* summary_row.padding */
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  /* summary_row.border.style */
  border-top-width: 2px;
  /* summary_row.border.width */
  border-top-color: #D3D3D3;
  /* summary_row.border.color */
}

#yiufsyedoe .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  /* grand_summary_row.background.color */
  text-transform: inherit;
  /* grand_summary_row.text_transform */
  padding-top: 8px;
  /* grand_summary_row.padding */
  padding-bottom: 8px;
  /* grand_summary_row.padding */
  padding-left: 5px;
  padding-right: 5px;
}

#yiufsyedoe .gt_first_grand_summary_row {
  padding-top: 8px;
  /* grand_summary_row.padding */
  padding-bottom: 8px;
  /* grand_summary_row.padding */
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  /* grand_summary_row.border.style */
  border-top-width: 6px;
  /* grand_summary_row.border.width */
  border-top-color: #D3D3D3;
  /* grand_summary_row.border.color */
}

#yiufsyedoe .gt_table_body {
  border-top-style: solid;
  /* table_body.border.top.style */
  border-top-width: 2px;
  /* table_body.border.top.width */
  border-top-color: #D3D3D3;
  /* table_body.border.top.color */
  border-bottom-style: solid;
  /* table_body.border.bottom.style */
  border-bottom-width: 2px;
  /* table_body.border.bottom.width */
  border-bottom-color: #D3D3D3;
  /* table_body.border.bottom.color */
}

#yiufsyedoe .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  /* footnotes.background.color */
  border-bottom-style: none;
  /* footnotes.border.bottom.style */
  border-bottom-width: 2px;
  /* footnotes.border.bottom.width */
  border-bottom-color: #D3D3D3;
  /* footnotes.border.bottom.color */
  border-left-style: none;
  /* footnotes.border.lr.color */
  border-left-width: 2px;
  /* footnotes.border.lr.color */
  border-left-color: #D3D3D3;
  /* footnotes.border.lr.color */
  border-right-style: none;
  /* footnotes.border.lr.color */
  border-right-width: 2px;
  /* footnotes.border.lr.color */
  border-right-color: #D3D3D3;
  /* footnotes.border.lr.color */
}

#yiufsyedoe .gt_footnote {
  margin: 0px;
  font-size: 90%;
  /* footnotes.font.size */
  padding: 4px;
  /* footnotes.padding */
}

#yiufsyedoe .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  /* source_notes.background.color */
  border-bottom-style: none;
  /* source_notes.border.bottom.style */
  border-bottom-width: 2px;
  /* source_notes.border.bottom.width */
  border-bottom-color: #D3D3D3;
  /* source_notes.border.bottom.color */
  border-left-style: none;
  /* source_notes.border.lr.style */
  border-left-width: 2px;
  /* source_notes.border.lr.style */
  border-left-color: #D3D3D3;
  /* source_notes.border.lr.style */
  border-right-style: none;
  /* source_notes.border.lr.style */
  border-right-width: 2px;
  /* source_notes.border.lr.style */
  border-right-color: #D3D3D3;
  /* source_notes.border.lr.style */
}

#yiufsyedoe .gt_sourcenote {
  font-size: 90%;
  /* source_notes.font.size */
  padding: 4px;
  /* source_notes.padding */
}

#yiufsyedoe .gt_left {
  text-align: left;
}

#yiufsyedoe .gt_center {
  text-align: center;
}

#yiufsyedoe .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#yiufsyedoe .gt_font_normal {
  font-weight: normal;
}

#yiufsyedoe .gt_font_bold {
  font-weight: bold;
}

#yiufsyedoe .gt_font_italic {
  font-style: italic;
}

#yiufsyedoe .gt_super {
  font-size: 65%;
}

#yiufsyedoe .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>
<div id="yiufsyedoe" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;"><table class="gt_table">
  <thead class="gt_header">
    <tr>
      <th colspan="15" class="gt_heading gt_title gt_font_normal gt_center" style>Pairwise t-test results with each timepoint. All results.</th>
    </tr>
    <tr>
      <th colspan="15" class="gt_heading gt_subtitle gt_font_normal gt_center gt_bottom_border" style></th>
    </tr>
  </thead>
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">group1</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">group2</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">age</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">comparison_variable</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">effect_size</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">pvalue</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">conf_lower</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">conf_upper</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">sample_size_group1</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">sample_size_group2</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">t_statistic</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">df</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">padj</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">genotype_pair</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">padj_format</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr>
      <td class="gt_row gt_left">WT</td>
      <td class="gt_row gt_left">HET</td>
      <td class="gt_row gt_left">6 weeks</td>
      <td class="gt_row gt_left">weight_gr</td>
      <td class="gt_row gt_right">0.01077692</td>
      <td class="gt_row gt_right">4.323546e-01</td>
      <td class="gt_row gt_right">-0.0174340506</td>
      <td class="gt_row gt_right">0.03898790</td>
      <td class="gt_row gt_center">10</td>
      <td class="gt_row gt_center">13</td>
      <td class="gt_row gt_right">0.80344905</td>
      <td class="gt_row gt_right">17.731454</td>
      <td class="gt_row gt_right">5.098759e-01</td>
      <td class="gt_row gt_center">WT-HET</td>
      <td class="gt_row gt_left">0.51</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_striped">WT</td>
      <td class="gt_row gt_left gt_striped">MUT</td>
      <td class="gt_row gt_left gt_striped">6 weeks</td>
      <td class="gt_row gt_left gt_striped">weight_gr</td>
      <td class="gt_row gt_right gt_striped">0.12278333</td>
      <td class="gt_row gt_right gt_striped">4.965021e-08</td>
      <td class="gt_row gt_right gt_striped">0.0954099997</td>
      <td class="gt_row gt_right gt_striped">0.15015667</td>
      <td class="gt_row gt_center gt_striped">10</td>
      <td class="gt_row gt_center gt_striped">12</td>
      <td class="gt_row gt_right gt_striped">9.49844560</td>
      <td class="gt_row gt_right gt_striped">16.218665</td>
      <td class="gt_row gt_right gt_striped">6.951029e-07</td>
      <td class="gt_row gt_center gt_striped">WT-MUT</td>
      <td class="gt_row gt_left gt_striped">7e-07</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">HET</td>
      <td class="gt_row gt_left">MUT</td>
      <td class="gt_row gt_left">6 weeks</td>
      <td class="gt_row gt_left">weight_gr</td>
      <td class="gt_row gt_right">0.11200641</td>
      <td class="gt_row gt_right">3.922903e-10</td>
      <td class="gt_row gt_right">0.0896874483</td>
      <td class="gt_row gt_right">0.13432537</td>
      <td class="gt_row gt_center">13</td>
      <td class="gt_row gt_center">12</td>
      <td class="gt_row gt_right">10.38399837</td>
      <td class="gt_row gt_right">22.897953</td>
      <td class="gt_row gt_right">2.196825e-08</td>
      <td class="gt_row gt_center">HET-MUT</td>
      <td class="gt_row gt_left">2.2e-08</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_striped">WT</td>
      <td class="gt_row gt_left gt_striped">HET</td>
      <td class="gt_row gt_left gt_striped">6 weeks</td>
      <td class="gt_row gt_left gt_striped">length_cm</td>
      <td class="gt_row gt_right gt_striped">0.05346154</td>
      <td class="gt_row gt_right gt_striped">4.557691e-01</td>
      <td class="gt_row gt_right gt_striped">-0.0945445594</td>
      <td class="gt_row gt_right gt_striped">0.20146764</td>
      <td class="gt_row gt_center gt_striped">10</td>
      <td class="gt_row gt_center gt_striped">13</td>
      <td class="gt_row gt_right gt_striped">0.76377988</td>
      <td class="gt_row gt_right gt_striped">16.520144</td>
      <td class="gt_row gt_right gt_striped">5.208789e-01</td>
      <td class="gt_row gt_center gt_striped">WT-HET</td>
      <td class="gt_row gt_left gt_striped">0.52</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">WT</td>
      <td class="gt_row gt_left">MUT</td>
      <td class="gt_row gt_left">6 weeks</td>
      <td class="gt_row gt_left">length_cm</td>
      <td class="gt_row gt_right">0.84958333</td>
      <td class="gt_row gt_right">2.342617e-08</td>
      <td class="gt_row gt_right">0.6515672544</td>
      <td class="gt_row gt_right">1.04759941</td>
      <td class="gt_row gt_center">10</td>
      <td class="gt_row gt_center">12</td>
      <td class="gt_row gt_right">8.96310167</td>
      <td class="gt_row gt_right">19.546689</td>
      <td class="gt_row gt_right">6.559328e-07</td>
      <td class="gt_row gt_center">WT-MUT</td>
      <td class="gt_row gt_left">6.6e-07</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_striped">HET</td>
      <td class="gt_row gt_left gt_striped">MUT</td>
      <td class="gt_row gt_left gt_striped">6 weeks</td>
      <td class="gt_row gt_left gt_striped">length_cm</td>
      <td class="gt_row gt_right gt_striped">0.79612179</td>
      <td class="gt_row gt_right gt_striped">4.402406e-08</td>
      <td class="gt_row gt_right gt_striped">0.6173549529</td>
      <td class="gt_row gt_right gt_striped">0.97488864</td>
      <td class="gt_row gt_center gt_striped">13</td>
      <td class="gt_row gt_center gt_striped">12</td>
      <td class="gt_row gt_right gt_striped">9.40904388</td>
      <td class="gt_row gt_right gt_striped">16.693170</td>
      <td class="gt_row gt_right gt_striped">6.951029e-07</td>
      <td class="gt_row gt_center gt_striped">HET-MUT</td>
      <td class="gt_row gt_left gt_striped">7e-07</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">WT</td>
      <td class="gt_row gt_left">HET</td>
      <td class="gt_row gt_left">8 weeks</td>
      <td class="gt_row gt_left">weight_gr</td>
      <td class="gt_row gt_right">0.01012500</td>
      <td class="gt_row gt_right">7.593169e-01</td>
      <td class="gt_row gt_right">-0.0603703185</td>
      <td class="gt_row gt_right">0.08062032</td>
      <td class="gt_row gt_center">8</td>
      <td class="gt_row gt_center">8</td>
      <td class="gt_row gt_right">0.31358926</td>
      <td class="gt_row gt_right">11.778521</td>
      <td class="gt_row gt_right">7.874397e-01</td>
      <td class="gt_row gt_center">WT-HET</td>
      <td class="gt_row gt_left">0.79</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_striped">WT</td>
      <td class="gt_row gt_left gt_striped">MUT</td>
      <td class="gt_row gt_left gt_striped">8 weeks</td>
      <td class="gt_row gt_left gt_striped">weight_gr</td>
      <td class="gt_row gt_right gt_striped">0.20987500</td>
      <td class="gt_row gt_right gt_striped">3.385485e-05</td>
      <td class="gt_row gt_right gt_striped">0.1408659109</td>
      <td class="gt_row gt_right gt_striped">0.27888409</td>
      <td class="gt_row gt_center gt_striped">8</td>
      <td class="gt_row gt_center gt_striped">8</td>
      <td class="gt_row gt_right gt_striped">6.69258251</td>
      <td class="gt_row gt_right gt_striped">11.016175</td>
      <td class="gt_row gt_right gt_striped">1.895872e-04</td>
      <td class="gt_row gt_center gt_striped">WT-MUT</td>
      <td class="gt_row gt_left gt_striped">0.00019</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">HET</td>
      <td class="gt_row gt_left">MUT</td>
      <td class="gt_row gt_left">8 weeks</td>
      <td class="gt_row gt_left">weight_gr</td>
      <td class="gt_row gt_right">0.19975000</td>
      <td class="gt_row gt_right">5.806347e-07</td>
      <td class="gt_row gt_right">0.1502844146</td>
      <td class="gt_row gt_right">0.24921559</td>
      <td class="gt_row gt_center">8</td>
      <td class="gt_row gt_center">8</td>
      <td class="gt_row gt_right">8.67107293</td>
      <td class="gt_row gt_right">13.828652</td>
      <td class="gt_row gt_right">6.503109e-06</td>
      <td class="gt_row gt_center">HET-MUT</td>
      <td class="gt_row gt_left">6.5e-06</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_striped">WT</td>
      <td class="gt_row gt_left gt_striped">HET</td>
      <td class="gt_row gt_left gt_striped">8 weeks</td>
      <td class="gt_row gt_left gt_striped">length_cm</td>
      <td class="gt_row gt_right gt_striped">-0.11950000</td>
      <td class="gt_row gt_right gt_striped">5.061317e-01</td>
      <td class="gt_row gt_right gt_striped">-0.4962426554</td>
      <td class="gt_row gt_right gt_striped">0.25724266</td>
      <td class="gt_row gt_center gt_striped">8</td>
      <td class="gt_row gt_center gt_striped">8</td>
      <td class="gt_row gt_right gt_striped">-0.68318612</td>
      <td class="gt_row gt_right gt_striped">13.398681</td>
      <td class="gt_row gt_right gt_striped">5.557524e-01</td>
      <td class="gt_row gt_center gt_striped">WT-HET</td>
      <td class="gt_row gt_left gt_striped">0.56</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">WT</td>
      <td class="gt_row gt_left">MUT</td>
      <td class="gt_row gt_left">8 weeks</td>
      <td class="gt_row gt_left">length_cm</td>
      <td class="gt_row gt_right">0.99600000</td>
      <td class="gt_row gt_right">2.263872e-04</td>
      <td class="gt_row gt_right">0.5624697422</td>
      <td class="gt_row gt_right">1.42953026</td>
      <td class="gt_row gt_center">8</td>
      <td class="gt_row gt_center">8</td>
      <td class="gt_row gt_right">4.93131059</td>
      <td class="gt_row gt_right">13.884743</td>
      <td class="gt_row gt_right">9.752062e-04</td>
      <td class="gt_row gt_center">WT-MUT</td>
      <td class="gt_row gt_left">0.00098</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_striped">HET</td>
      <td class="gt_row gt_left gt_striped">MUT</td>
      <td class="gt_row gt_left gt_striped">8 weeks</td>
      <td class="gt_row gt_left gt_striped">length_cm</td>
      <td class="gt_row gt_right gt_striped">1.11550000</td>
      <td class="gt_row gt_right gt_striped">4.484663e-05</td>
      <td class="gt_row gt_right gt_striped">0.7148911472</td>
      <td class="gt_row gt_right gt_striped">1.51610885</td>
      <td class="gt_row gt_center gt_striped">8</td>
      <td class="gt_row gt_center gt_striped">8</td>
      <td class="gt_row gt_right gt_striped">6.02206054</td>
      <td class="gt_row gt_right gt_striped">12.863633</td>
      <td class="gt_row gt_right gt_striped">2.283101e-04</td>
      <td class="gt_row gt_center gt_striped">HET-MUT</td>
      <td class="gt_row gt_left gt_striped">0.00023</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">WT</td>
      <td class="gt_row gt_left">HET</td>
      <td class="gt_row gt_left">10 weeks</td>
      <td class="gt_row gt_left">weight_gr</td>
      <td class="gt_row gt_right">-0.00150000</td>
      <td class="gt_row gt_right">9.583739e-01</td>
      <td class="gt_row gt_right">-0.0620734358</td>
      <td class="gt_row gt_right">0.05907344</td>
      <td class="gt_row gt_center">8</td>
      <td class="gt_row gt_center">8</td>
      <td class="gt_row gt_right">-0.05314225</td>
      <td class="gt_row gt_right">13.915728</td>
      <td class="gt_row gt_right">9.583739e-01</td>
      <td class="gt_row gt_center">WT-HET</td>
      <td class="gt_row gt_left">0.96</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_striped">WT</td>
      <td class="gt_row gt_left gt_striped">MUT</td>
      <td class="gt_row gt_left gt_striped">10 weeks</td>
      <td class="gt_row gt_left gt_striped">weight_gr</td>
      <td class="gt_row gt_right gt_striped">0.19621429</td>
      <td class="gt_row gt_right gt_striped">1.333559e-05</td>
      <td class="gt_row gt_right gt_striped">0.1343156943</td>
      <td class="gt_row gt_right gt_striped">0.25811288</td>
      <td class="gt_row gt_center gt_striped">8</td>
      <td class="gt_row gt_center gt_striped">7</td>
      <td class="gt_row gt_right gt_striped">6.87003379</td>
      <td class="gt_row gt_right gt_striped">12.606143</td>
      <td class="gt_row gt_right gt_striped">1.066847e-04</td>
      <td class="gt_row gt_center gt_striped">WT-MUT</td>
      <td class="gt_row gt_left gt_striped">0.00011</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">HET</td>
      <td class="gt_row gt_left">MUT</td>
      <td class="gt_row gt_left">10 weeks</td>
      <td class="gt_row gt_left">weight_gr</td>
      <td class="gt_row gt_right">0.19771429</td>
      <td class="gt_row gt_right">1.607865e-05</td>
      <td class="gt_row gt_right">0.1336466425</td>
      <td class="gt_row gt_right">0.26178193</td>
      <td class="gt_row gt_center">8</td>
      <td class="gt_row gt_center">7</td>
      <td class="gt_row gt_right">6.67358109</td>
      <td class="gt_row gt_right">12.874140</td>
      <td class="gt_row gt_right">1.125505e-04</td>
      <td class="gt_row gt_center">HET-MUT</td>
      <td class="gt_row gt_left">0.00011</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_striped">WT</td>
      <td class="gt_row gt_left gt_striped">HET</td>
      <td class="gt_row gt_left gt_striped">10 weeks</td>
      <td class="gt_row gt_left gt_striped">length_cm</td>
      <td class="gt_row gt_right gt_striped">0.08350000</td>
      <td class="gt_row gt_right gt_striped">3.510375e-01</td>
      <td class="gt_row gt_right gt_striped">-0.1058374696</td>
      <td class="gt_row gt_right gt_striped">0.27283747</td>
      <td class="gt_row gt_center gt_striped">8</td>
      <td class="gt_row gt_center gt_striped">8</td>
      <td class="gt_row gt_right gt_striped">0.97573771</td>
      <td class="gt_row gt_right gt_striped">10.549779</td>
      <td class="gt_row gt_right gt_striped">4.273500e-01</td>
      <td class="gt_row gt_center gt_striped">WT-HET</td>
      <td class="gt_row gt_left gt_striped">0.43</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">WT</td>
      <td class="gt_row gt_left">MUT</td>
      <td class="gt_row gt_left">10 weeks</td>
      <td class="gt_row gt_left">length_cm</td>
      <td class="gt_row gt_right">0.75191071</td>
      <td class="gt_row gt_right">8.723514e-06</td>
      <td class="gt_row gt_right">0.5599991990</td>
      <td class="gt_row gt_right">0.94382223</td>
      <td class="gt_row gt_center">8</td>
      <td class="gt_row gt_center">7</td>
      <td class="gt_row gt_right">8.83750358</td>
      <td class="gt_row gt_right">9.174596</td>
      <td class="gt_row gt_right">8.141946e-05</td>
      <td class="gt_row gt_center">WT-MUT</td>
      <td class="gt_row gt_left">8.1e-05</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_striped">HET</td>
      <td class="gt_row gt_left gt_striped">MUT</td>
      <td class="gt_row gt_left gt_striped">10 weeks</td>
      <td class="gt_row gt_left gt_striped">length_cm</td>
      <td class="gt_row gt_right gt_striped">0.66841071</td>
      <td class="gt_row gt_right gt_striped">3.023609e-05</td>
      <td class="gt_row gt_right gt_striped">0.4373599257</td>
      <td class="gt_row gt_right gt_striped">0.89946150</td>
      <td class="gt_row gt_center gt_striped">8</td>
      <td class="gt_row gt_center gt_striped">7</td>
      <td class="gt_row gt_right gt_striped">6.25285860</td>
      <td class="gt_row gt_right gt_striped">12.937070</td>
      <td class="gt_row gt_right gt_striped">1.881357e-04</td>
      <td class="gt_row gt_center gt_striped">HET-MUT</td>
      <td class="gt_row gt_left gt_striped">0.00019</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">WT</td>
      <td class="gt_row gt_left">HET</td>
      <td class="gt_row gt_left">12 weeks</td>
      <td class="gt_row gt_left">weight_gr</td>
      <td class="gt_row gt_right">0.05400000</td>
      <td class="gt_row gt_right">4.370365e-01</td>
      <td class="gt_row gt_right">-0.1339873597</td>
      <td class="gt_row gt_right">0.24198736</td>
      <td class="gt_row gt_center">3</td>
      <td class="gt_row gt_center">3</td>
      <td class="gt_row gt_right">0.88748806</td>
      <td class="gt_row gt_right">3.167037</td>
      <td class="gt_row gt_right">5.098759e-01</td>
      <td class="gt_row gt_center">WT-HET</td>
      <td class="gt_row gt_left">0.51</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_striped">WT</td>
      <td class="gt_row gt_left gt_striped">MUT</td>
      <td class="gt_row gt_left gt_striped">12 weeks</td>
      <td class="gt_row gt_left gt_striped">weight_gr</td>
      <td class="gt_row gt_right gt_striped">0.25833333</td>
      <td class="gt_row gt_right gt_striped">2.213631e-02</td>
      <td class="gt_row gt_right gt_striped">0.0612446532</td>
      <td class="gt_row gt_right gt_striped">0.45542201</td>
      <td class="gt_row gt_center gt_striped">3</td>
      <td class="gt_row gt_center gt_striped">3</td>
      <td class="gt_row gt_right gt_striped">3.66189868</td>
      <td class="gt_row gt_right gt_striped">3.938069</td>
      <td class="gt_row gt_right gt_striped">5.634696e-02</td>
      <td class="gt_row gt_center gt_striped">WT-MUT</td>
      <td class="gt_row gt_left gt_striped">0.056</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">HET</td>
      <td class="gt_row gt_left">MUT</td>
      <td class="gt_row gt_left">12 weeks</td>
      <td class="gt_row gt_left">weight_gr</td>
      <td class="gt_row gt_right">0.20433333</td>
      <td class="gt_row gt_right">2.783766e-02</td>
      <td class="gt_row gt_right">0.0392977280</td>
      <td class="gt_row gt_right">0.36936894</td>
      <td class="gt_row gt_center">3</td>
      <td class="gt_row gt_center">3</td>
      <td class="gt_row gt_right">3.68296061</td>
      <td class="gt_row gt_right">3.414558</td>
      <td class="gt_row gt_right">6.449014e-02</td>
      <td class="gt_row gt_center">HET-MUT</td>
      <td class="gt_row gt_left">0.064</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_striped">WT</td>
      <td class="gt_row gt_left gt_striped">HET</td>
      <td class="gt_row gt_left gt_striped">12 weeks</td>
      <td class="gt_row gt_left gt_striped">length_cm</td>
      <td class="gt_row gt_right gt_striped">0.19633333</td>
      <td class="gt_row gt_right gt_striped">2.981030e-01</td>
      <td class="gt_row gt_right gt_striped">-0.3798161233</td>
      <td class="gt_row gt_right gt_striped">0.77248279</td>
      <td class="gt_row gt_center gt_striped">3</td>
      <td class="gt_row gt_center gt_striped">3</td>
      <td class="gt_row gt_right gt_striped">1.35791808</td>
      <td class="gt_row gt_right gt_striped">2.176574</td>
      <td class="gt_row gt_right gt_striped">3.794038e-01</td>
      <td class="gt_row gt_center gt_striped">WT-HET</td>
      <td class="gt_row gt_left gt_striped">0.38</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">WT</td>
      <td class="gt_row gt_left">MUT</td>
      <td class="gt_row gt_left">12 weeks</td>
      <td class="gt_row gt_left">length_cm</td>
      <td class="gt_row gt_right">0.79600000</td>
      <td class="gt_row gt_right">3.092026e-02</td>
      <td class="gt_row gt_right">0.1233967954</td>
      <td class="gt_row gt_right">1.46860320</td>
      <td class="gt_row gt_center">3</td>
      <td class="gt_row gt_center">3</td>
      <td class="gt_row gt_right">3.38643016</td>
      <td class="gt_row gt_right">3.718117</td>
      <td class="gt_row gt_right">6.659749e-02</td>
      <td class="gt_row gt_center">WT-MUT</td>
      <td class="gt_row gt_left">0.067</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_striped">HET</td>
      <td class="gt_row gt_left gt_striped">MUT</td>
      <td class="gt_row gt_left gt_striped">12 weeks</td>
      <td class="gt_row gt_left gt_striped">length_cm</td>
      <td class="gt_row gt_right gt_striped">0.59966667</td>
      <td class="gt_row gt_right gt_striped">8.211919e-02</td>
      <td class="gt_row gt_right gt_striped">-0.1816773439</td>
      <td class="gt_row gt_right gt_striped">1.38101068</td>
      <td class="gt_row gt_center gt_striped">3</td>
      <td class="gt_row gt_center gt_striped">3</td>
      <td class="gt_row gt_right gt_striped">3.15537321</td>
      <td class="gt_row gt_right gt_striped">2.100464</td>
      <td class="gt_row gt_right gt_striped">1.437086e-01</td>
      <td class="gt_row gt_center gt_striped">HET-MUT</td>
      <td class="gt_row gt_left gt_striped">0.14</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">WT</td>
      <td class="gt_row gt_left">HET</td>
      <td class="gt_row gt_left">4 months</td>
      <td class="gt_row gt_left">weight_gr</td>
      <td class="gt_row gt_right">-0.02156250</td>
      <td class="gt_row gt_right">5.358769e-01</td>
      <td class="gt_row gt_right">-0.0919068025</td>
      <td class="gt_row gt_right">0.04878180</td>
      <td class="gt_row gt_center">16</td>
      <td class="gt_row gt_center">18</td>
      <td class="gt_row gt_right">-0.62636294</td>
      <td class="gt_row gt_right">29.606380</td>
      <td class="gt_row gt_right">5.662095e-01</td>
      <td class="gt_row gt_center">WT-HET</td>
      <td class="gt_row gt_left">0.57</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_striped">WT</td>
      <td class="gt_row gt_left gt_striped">MUT</td>
      <td class="gt_row gt_left gt_striped">4 months</td>
      <td class="gt_row gt_left gt_striped">weight_gr</td>
      <td class="gt_row gt_right gt_striped">0.07100000</td>
      <td class="gt_row gt_right gt_striped">5.191514e-02</td>
      <td class="gt_row gt_right gt_striped">-0.0006281862</td>
      <td class="gt_row gt_right gt_striped">0.14262819</td>
      <td class="gt_row gt_center gt_striped">16</td>
      <td class="gt_row gt_center gt_striped">16</td>
      <td class="gt_row gt_right gt_striped">2.02483899</td>
      <td class="gt_row gt_right gt_striped">29.832245</td>
      <td class="gt_row gt_right gt_striped">1.038303e-01</td>
      <td class="gt_row gt_center gt_striped">WT-MUT</td>
      <td class="gt_row gt_left gt_striped">0.1</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">HET</td>
      <td class="gt_row gt_left">MUT</td>
      <td class="gt_row gt_left">4 months</td>
      <td class="gt_row gt_left">weight_gr</td>
      <td class="gt_row gt_right">0.09256250</td>
      <td class="gt_row gt_right">8.861220e-03</td>
      <td class="gt_row gt_right">0.0250430026</td>
      <td class="gt_row gt_right">0.16008200</td>
      <td class="gt_row gt_center">18</td>
      <td class="gt_row gt_center">16</td>
      <td class="gt_row gt_right">2.79994612</td>
      <td class="gt_row gt_right">29.950467</td>
      <td class="gt_row gt_right">2.481142e-02</td>
      <td class="gt_row gt_center">HET-MUT</td>
      <td class="gt_row gt_left">0.025</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_striped">WT</td>
      <td class="gt_row gt_left gt_striped">HET</td>
      <td class="gt_row gt_left gt_striped">4 months</td>
      <td class="gt_row gt_left gt_striped">length_cm</td>
      <td class="gt_row gt_right gt_striped">-0.01475000</td>
      <td class="gt_row gt_right gt_striped">8.355968e-01</td>
      <td class="gt_row gt_right gt_striped">-0.1587867998</td>
      <td class="gt_row gt_right gt_striped">0.12928680</td>
      <td class="gt_row gt_center gt_striped">16</td>
      <td class="gt_row gt_center gt_striped">18</td>
      <td class="gt_row gt_right gt_striped">-0.20939474</td>
      <td class="gt_row gt_right gt_striped">29.146602</td>
      <td class="gt_row gt_right gt_striped">8.507895e-01</td>
      <td class="gt_row gt_center gt_striped">WT-HET</td>
      <td class="gt_row gt_left gt_striped">0.85</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">WT</td>
      <td class="gt_row gt_left">MUT</td>
      <td class="gt_row gt_left">4 months</td>
      <td class="gt_row gt_left">length_cm</td>
      <td class="gt_row gt_right">0.20956250</td>
      <td class="gt_row gt_right">1.166651e-02</td>
      <td class="gt_row gt_right">0.0502402906</td>
      <td class="gt_row gt_right">0.36888471</td>
      <td class="gt_row gt_center">16</td>
      <td class="gt_row gt_center">16</td>
      <td class="gt_row gt_right">2.68650606</td>
      <td class="gt_row gt_right">29.939351</td>
      <td class="gt_row gt_right">3.111069e-02</td>
      <td class="gt_row gt_center">WT-MUT</td>
      <td class="gt_row gt_left">0.031</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_striped">HET</td>
      <td class="gt_row gt_left gt_striped">MUT</td>
      <td class="gt_row gt_left gt_striped">4 months</td>
      <td class="gt_row gt_left gt_striped">length_cm</td>
      <td class="gt_row gt_right gt_striped">0.22431250</td>
      <td class="gt_row gt_right gt_striped">4.309750e-03</td>
      <td class="gt_row gt_right gt_striped">0.0762502074</td>
      <td class="gt_row gt_right gt_striped">0.37237479</td>
      <td class="gt_row gt_center gt_striped">18</td>
      <td class="gt_row gt_center gt_striped">16</td>
      <td class="gt_row gt_right gt_striped">3.09999575</td>
      <td class="gt_row gt_right gt_striped">28.680772</td>
      <td class="gt_row gt_right gt_striped">1.419682e-02</td>
      <td class="gt_row gt_center gt_striped">HET-MUT</td>
      <td class="gt_row gt_left gt_striped">0.014</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">WT</td>
      <td class="gt_row gt_left">HET</td>
      <td class="gt_row gt_left">6 months</td>
      <td class="gt_row gt_left">weight_gr</td>
      <td class="gt_row gt_right">0.03793750</td>
      <td class="gt_row gt_right">1.583234e-01</td>
      <td class="gt_row gt_right">-0.0160179132</td>
      <td class="gt_row gt_right">0.09189291</td>
      <td class="gt_row gt_center">16</td>
      <td class="gt_row gt_center">10</td>
      <td class="gt_row gt_right">1.46372375</td>
      <td class="gt_row gt_right">20.654357</td>
      <td class="gt_row gt_right">2.316792e-01</td>
      <td class="gt_row gt_center">WT-HET</td>
      <td class="gt_row gt_left">0.23</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_striped">WT</td>
      <td class="gt_row gt_left gt_striped">MUT</td>
      <td class="gt_row gt_left gt_striped">6 months</td>
      <td class="gt_row gt_left gt_striped">weight_gr</td>
      <td class="gt_row gt_right gt_striped">0.06638194</td>
      <td class="gt_row gt_right gt_striped">2.347196e-02</td>
      <td class="gt_row gt_right gt_striped">0.0098278344</td>
      <td class="gt_row gt_right gt_striped">0.12293605</td>
      <td class="gt_row gt_center gt_striped">16</td>
      <td class="gt_row gt_center gt_striped">9</td>
      <td class="gt_row gt_right gt_striped">2.43268347</td>
      <td class="gt_row gt_right gt_striped">22.249575</td>
      <td class="gt_row gt_right gt_striped">5.714911e-02</td>
      <td class="gt_row gt_center gt_striped">WT-MUT</td>
      <td class="gt_row gt_left gt_striped">0.057</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">HET</td>
      <td class="gt_row gt_left">MUT</td>
      <td class="gt_row gt_left">6 months</td>
      <td class="gt_row gt_left">weight_gr</td>
      <td class="gt_row gt_right">0.02844444</td>
      <td class="gt_row gt_right">1.289009e-01</td>
      <td class="gt_row gt_right">-0.0092367045</td>
      <td class="gt_row gt_right">0.06612559</td>
      <td class="gt_row gt_center">10</td>
      <td class="gt_row gt_center">9</td>
      <td class="gt_row gt_right">1.60295190</td>
      <td class="gt_row gt_right">15.675714</td>
      <td class="gt_row gt_right">2.005126e-01</td>
      <td class="gt_row gt_center">HET-MUT</td>
      <td class="gt_row gt_left">0.2</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_striped">WT</td>
      <td class="gt_row gt_left gt_striped">HET</td>
      <td class="gt_row gt_left gt_striped">6 months</td>
      <td class="gt_row gt_left gt_striped">length_cm</td>
      <td class="gt_row gt_right gt_striped">0.09984722</td>
      <td class="gt_row gt_right gt_striped">1.229148e-01</td>
      <td class="gt_row gt_right gt_striped">-0.0292046357</td>
      <td class="gt_row gt_right gt_striped">0.22889908</td>
      <td class="gt_row gt_center gt_striped">16</td>
      <td class="gt_row gt_center gt_striped">10</td>
      <td class="gt_row gt_right gt_striped">1.60365310</td>
      <td class="gt_row gt_right gt_striped">22.214851</td>
      <td class="gt_row gt_right gt_striped">1.966637e-01</td>
      <td class="gt_row gt_center gt_striped">WT-HET</td>
      <td class="gt_row gt_left gt_striped">0.2</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">WT</td>
      <td class="gt_row gt_left">MUT</td>
      <td class="gt_row gt_left">6 months</td>
      <td class="gt_row gt_left">length_cm</td>
      <td class="gt_row gt_right">0.27929167</td>
      <td class="gt_row gt_right">2.061703e-03</td>
      <td class="gt_row gt_right">0.1170518134</td>
      <td class="gt_row gt_right">0.44153152</td>
      <td class="gt_row gt_center">16</td>
      <td class="gt_row gt_center">9</td>
      <td class="gt_row gt_right">3.63216358</td>
      <td class="gt_row gt_right">16.989487</td>
      <td class="gt_row gt_right">7.215960e-03</td>
      <td class="gt_row gt_center">WT-MUT</td>
      <td class="gt_row gt_left">0.0072</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_striped">HET</td>
      <td class="gt_row gt_left gt_striped">MUT</td>
      <td class="gt_row gt_left gt_striped">6 months</td>
      <td class="gt_row gt_left gt_striped">length_cm</td>
      <td class="gt_row gt_right gt_striped">0.17944444</td>
      <td class="gt_row gt_right gt_striped">2.879024e-02</td>
      <td class="gt_row gt_right gt_striped">0.0214680704</td>
      <td class="gt_row gt_right gt_striped">0.33742082</td>
      <td class="gt_row gt_center gt_striped">10</td>
      <td class="gt_row gt_center gt_striped">9</td>
      <td class="gt_row gt_right gt_striped">2.43583025</td>
      <td class="gt_row gt_right gt_striped">14.025807</td>
      <td class="gt_row gt_right gt_striped">6.449014e-02</td>
      <td class="gt_row gt_center gt_striped">HET-MUT</td>
      <td class="gt_row gt_left gt_striped">0.064</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">WT</td>
      <td class="gt_row gt_left">HET</td>
      <td class="gt_row gt_left">8 months</td>
      <td class="gt_row gt_left">weight_gr</td>
      <td class="gt_row gt_right">0.07537500</td>
      <td class="gt_row gt_right">1.654852e-01</td>
      <td class="gt_row gt_right">-0.0367696512</td>
      <td class="gt_row gt_right">0.18751965</td>
      <td class="gt_row gt_center">8</td>
      <td class="gt_row gt_center">5</td>
      <td class="gt_row gt_right">1.49422475</td>
      <td class="gt_row gt_right">10.168584</td>
      <td class="gt_row gt_right">2.316792e-01</td>
      <td class="gt_row gt_center">WT-HET</td>
      <td class="gt_row gt_left">0.23</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_striped">WT</td>
      <td class="gt_row gt_left gt_striped">MUT</td>
      <td class="gt_row gt_left gt_striped">8 months</td>
      <td class="gt_row gt_left gt_striped">weight_gr</td>
      <td class="gt_row gt_right gt_striped">0.15870833</td>
      <td class="gt_row gt_right gt_striped">6.287145e-02</td>
      <td class="gt_row gt_right gt_striped">-0.0121737068</td>
      <td class="gt_row gt_right gt_striped">0.32959037</td>
      <td class="gt_row gt_center gt_striped">8</td>
      <td class="gt_row gt_center gt_striped">3</td>
      <td class="gt_row gt_right gt_striped">2.34698522</td>
      <td class="gt_row gt_right gt_striped">5.303121</td>
      <td class="gt_row gt_right gt_striped">1.173600e-01</td>
      <td class="gt_row gt_center gt_striped">WT-MUT</td>
      <td class="gt_row gt_left gt_striped">0.12</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">HET</td>
      <td class="gt_row gt_left">MUT</td>
      <td class="gt_row gt_left">8 months</td>
      <td class="gt_row gt_left">weight_gr</td>
      <td class="gt_row gt_right">0.08333333</td>
      <td class="gt_row gt_right">2.386006e-01</td>
      <td class="gt_row gt_right">-0.0992560170</td>
      <td class="gt_row gt_right">0.26592268</td>
      <td class="gt_row gt_center">5</td>
      <td class="gt_row gt_center">3</td>
      <td class="gt_row gt_right">1.47805620</td>
      <td class="gt_row gt_right">2.910506</td>
      <td class="gt_row gt_right">3.181342e-01</td>
      <td class="gt_row gt_center">HET-MUT</td>
      <td class="gt_row gt_left">0.32</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_striped">WT</td>
      <td class="gt_row gt_left gt_striped">HET</td>
      <td class="gt_row gt_left gt_striped">8 months</td>
      <td class="gt_row gt_left gt_striped">length_cm</td>
      <td class="gt_row gt_right gt_striped">0.05340000</td>
      <td class="gt_row gt_right gt_striped">5.306760e-01</td>
      <td class="gt_row gt_right gt_striped">-0.1289292816</td>
      <td class="gt_row gt_right gt_striped">0.23572928</td>
      <td class="gt_row gt_center gt_striped">8</td>
      <td class="gt_row gt_center gt_striped">5</td>
      <td class="gt_row gt_right gt_striped">0.64835024</td>
      <td class="gt_row gt_right gt_striped">10.504039</td>
      <td class="gt_row gt_right gt_striped">5.662095e-01</td>
      <td class="gt_row gt_center gt_striped">WT-HET</td>
      <td class="gt_row gt_left gt_striped">0.57</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">WT</td>
      <td class="gt_row gt_left">MUT</td>
      <td class="gt_row gt_left">8 months</td>
      <td class="gt_row gt_left">length_cm</td>
      <td class="gt_row gt_right">0.41000000</td>
      <td class="gt_row gt_right">3.698248e-02</td>
      <td class="gt_row gt_right">0.0437728089</td>
      <td class="gt_row gt_right">0.77622719</td>
      <td class="gt_row gt_center">8</td>
      <td class="gt_row gt_center">3</td>
      <td class="gt_row gt_right">3.34985666</td>
      <td class="gt_row gt_right">3.373257</td>
      <td class="gt_row gt_right">7.670441e-02</td>
      <td class="gt_row gt_center">WT-MUT</td>
      <td class="gt_row gt_left">0.077</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_striped">HET</td>
      <td class="gt_row gt_left gt_striped">MUT</td>
      <td class="gt_row gt_left gt_striped">8 months</td>
      <td class="gt_row gt_left gt_striped">length_cm</td>
      <td class="gt_row gt_right gt_striped">0.35660000</td>
      <td class="gt_row gt_right gt_striped">5.618708e-02</td>
      <td class="gt_row gt_right gt_striped">-0.0171056053</td>
      <td class="gt_row gt_right gt_striped">0.73030561</td>
      <td class="gt_row gt_center gt_striped">5</td>
      <td class="gt_row gt_center gt_striped">3</td>
      <td class="gt_row gt_right gt_striped">2.95649267</td>
      <td class="gt_row gt_right gt_striped">3.150185</td>
      <td class="gt_row gt_right gt_striped">1.084992e-01</td>
      <td class="gt_row gt_center gt_striped">HET-MUT</td>
      <td class="gt_row gt_left gt_striped">0.11</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">WT</td>
      <td class="gt_row gt_left">HET</td>
      <td class="gt_row gt_left">10 months</td>
      <td class="gt_row gt_left">weight_gr</td>
      <td class="gt_row gt_right">-0.20850000</td>
      <td class="gt_row gt_right">1.424807e-01</td>
      <td class="gt_row gt_right">-0.5681511396</td>
      <td class="gt_row gt_right">0.15115114</td>
      <td class="gt_row gt_center">3</td>
      <td class="gt_row gt_center">6</td>
      <td class="gt_row gt_right">-2.18044005</td>
      <td class="gt_row gt_right">2.335699</td>
      <td class="gt_row gt_right">2.156465e-01</td>
      <td class="gt_row gt_center">WT-HET</td>
      <td class="gt_row gt_left">0.22</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_striped">WT</td>
      <td class="gt_row gt_left gt_striped">MUT</td>
      <td class="gt_row gt_left gt_striped">10 months</td>
      <td class="gt_row gt_left gt_striped">weight_gr</td>
      <td class="gt_row gt_right gt_striped">0.25819048</td>
      <td class="gt_row gt_right gt_striped">8.115582e-02</td>
      <td class="gt_row gt_right gt_striped">-0.0519209124</td>
      <td class="gt_row gt_right gt_striped">0.56830186</td>
      <td class="gt_row gt_center gt_striped">3</td>
      <td class="gt_row gt_center gt_striped">7</td>
      <td class="gt_row gt_right gt_striped">2.35427185</td>
      <td class="gt_row gt_right gt_striped">3.823578</td>
      <td class="gt_row gt_right gt_striped">1.437086e-01</td>
      <td class="gt_row gt_center gt_striped">WT-MUT</td>
      <td class="gt_row gt_left gt_striped">0.14</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">HET</td>
      <td class="gt_row gt_left">MUT</td>
      <td class="gt_row gt_left">10 months</td>
      <td class="gt_row gt_left">weight_gr</td>
      <td class="gt_row gt_right">0.46669048</td>
      <td class="gt_row gt_right">8.754234e-05</td>
      <td class="gt_row gt_right">0.3165660983</td>
      <td class="gt_row gt_right">0.61681485</td>
      <td class="gt_row gt_center">6</td>
      <td class="gt_row gt_center">7</td>
      <td class="gt_row gt_right">7.14070157</td>
      <td class="gt_row gt_right">8.183986</td>
      <td class="gt_row gt_right">4.085309e-04</td>
      <td class="gt_row gt_center">HET-MUT</td>
      <td class="gt_row gt_left">0.00041</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_striped">WT</td>
      <td class="gt_row gt_left gt_striped">HET</td>
      <td class="gt_row gt_left gt_striped">10 months</td>
      <td class="gt_row gt_left gt_striped">length_cm</td>
      <td class="gt_row gt_right gt_striped">-0.20783333</td>
      <td class="gt_row gt_right gt_striped">4.794465e-01</td>
      <td class="gt_row gt_right gt_striped">-1.0467343138</td>
      <td class="gt_row gt_right gt_striped">0.63106765</td>
      <td class="gt_row gt_center gt_striped">3</td>
      <td class="gt_row gt_center gt_striped">6</td>
      <td class="gt_row gt_right gt_striped">-0.81088645</td>
      <td class="gt_row gt_right gt_striped">2.859050</td>
      <td class="gt_row gt_right gt_striped">5.369801e-01</td>
      <td class="gt_row gt_center gt_striped">WT-HET</td>
      <td class="gt_row gt_left gt_striped">0.54</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">WT</td>
      <td class="gt_row gt_left">MUT</td>
      <td class="gt_row gt_left">10 months</td>
      <td class="gt_row gt_left">length_cm</td>
      <td class="gt_row gt_right">0.47647619</td>
      <td class="gt_row gt_right">1.641646e-01</td>
      <td class="gt_row gt_right">-0.3597144025</td>
      <td class="gt_row gt_right">1.31266678</td>
      <td class="gt_row gt_center">3</td>
      <td class="gt_row gt_center">7</td>
      <td class="gt_row gt_right">1.85658902</td>
      <td class="gt_row gt_right">2.880856</td>
      <td class="gt_row gt_right">2.316792e-01</td>
      <td class="gt_row gt_center">WT-MUT</td>
      <td class="gt_row gt_left">0.23</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_striped">HET</td>
      <td class="gt_row gt_left gt_striped">MUT</td>
      <td class="gt_row gt_left gt_striped">10 months</td>
      <td class="gt_row gt_left gt_striped">length_cm</td>
      <td class="gt_row gt_right gt_striped">0.68430952</td>
      <td class="gt_row gt_right gt_striped">8.405781e-04</td>
      <td class="gt_row gt_right gt_striped">0.3532353946</td>
      <td class="gt_row gt_right gt_striped">1.01538365</td>
      <td class="gt_row gt_center gt_striped">6</td>
      <td class="gt_row gt_center gt_striped">7</td>
      <td class="gt_row gt_right gt_striped">4.55318159</td>
      <td class="gt_row gt_right gt_striped">10.923641</td>
      <td class="gt_row gt_right gt_striped">3.138158e-03</td>
      <td class="gt_row gt_center gt_striped">HET-MUT</td>
      <td class="gt_row gt_left gt_striped">0.0031</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">WT</td>
      <td class="gt_row gt_left">HET</td>
      <td class="gt_row gt_left">13 months</td>
      <td class="gt_row gt_left">weight_gr</td>
      <td class="gt_row gt_right">0.10850000</td>
      <td class="gt_row gt_right">9.086120e-02</td>
      <td class="gt_row gt_right">-0.0200935794</td>
      <td class="gt_row gt_right">0.23709358</td>
      <td class="gt_row gt_center">8</td>
      <td class="gt_row gt_center">6</td>
      <td class="gt_row gt_right">1.83883213</td>
      <td class="gt_row gt_right">11.972238</td>
      <td class="gt_row gt_right">1.541887e-01</td>
      <td class="gt_row gt_center">WT-HET</td>
      <td class="gt_row gt_left">0.15</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_striped">WT</td>
      <td class="gt_row gt_left gt_striped">MUT</td>
      <td class="gt_row gt_left gt_striped">13 months</td>
      <td class="gt_row gt_left gt_striped">weight_gr</td>
      <td class="gt_row gt_right gt_striped">0.27425000</td>
      <td class="gt_row gt_right gt_striped">4.012757e-04</td>
      <td class="gt_row gt_right gt_striped">0.1578059773</td>
      <td class="gt_row gt_right gt_striped">0.39069402</td>
      <td class="gt_row gt_center gt_striped">8</td>
      <td class="gt_row gt_center gt_striped">4</td>
      <td class="gt_row gt_right gt_striped">5.26968725</td>
      <td class="gt_row gt_right gt_striped">9.701685</td>
      <td class="gt_row gt_right gt_striped">1.605103e-03</td>
      <td class="gt_row gt_center gt_striped">WT-MUT</td>
      <td class="gt_row gt_left gt_striped">0.0016</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">HET</td>
      <td class="gt_row gt_left">MUT</td>
      <td class="gt_row gt_left">13 months</td>
      <td class="gt_row gt_left">weight_gr</td>
      <td class="gt_row gt_right">0.16575000</td>
      <td class="gt_row gt_right">5.924125e-03</td>
      <td class="gt_row gt_right">0.0631895484</td>
      <td class="gt_row gt_right">0.26831045</td>
      <td class="gt_row gt_center">6</td>
      <td class="gt_row gt_center">4</td>
      <td class="gt_row gt_right">3.74114818</td>
      <td class="gt_row gt_right">7.827213</td>
      <td class="gt_row gt_right">1.746058e-02</td>
      <td class="gt_row gt_center">HET-MUT</td>
      <td class="gt_row gt_left">0.017</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_striped">WT</td>
      <td class="gt_row gt_left gt_striped">HET</td>
      <td class="gt_row gt_left gt_striped">13 months</td>
      <td class="gt_row gt_left gt_striped">length_cm</td>
      <td class="gt_row gt_right gt_striped">0.22470833</td>
      <td class="gt_row gt_right gt_striped">1.008492e-01</td>
      <td class="gt_row gt_right gt_striped">-0.0543979310</td>
      <td class="gt_row gt_right gt_striped">0.50381460</td>
      <td class="gt_row gt_center gt_striped">8</td>
      <td class="gt_row gt_center gt_striped">6</td>
      <td class="gt_row gt_right gt_striped">1.84680359</td>
      <td class="gt_row gt_right gt_striped">8.250321</td>
      <td class="gt_row gt_right gt_striped">1.661045e-01</td>
      <td class="gt_row gt_center gt_striped">WT-HET</td>
      <td class="gt_row gt_left gt_striped">0.17</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">WT</td>
      <td class="gt_row gt_left">MUT</td>
      <td class="gt_row gt_left">13 months</td>
      <td class="gt_row gt_left">length_cm</td>
      <td class="gt_row gt_right">0.34670833</td>
      <td class="gt_row gt_right">5.701559e-03</td>
      <td class="gt_row gt_right">0.1416316407</td>
      <td class="gt_row gt_right">0.55178503</td>
      <td class="gt_row gt_center">8</td>
      <td class="gt_row gt_center">4</td>
      <td class="gt_row gt_right">4.07433508</td>
      <td class="gt_row gt_right">6.404260</td>
      <td class="gt_row gt_right">1.746058e-02</td>
      <td class="gt_row gt_center">WT-MUT</td>
      <td class="gt_row gt_left">0.017</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_striped">HET</td>
      <td class="gt_row gt_left gt_striped">MUT</td>
      <td class="gt_row gt_left gt_striped">13 months</td>
      <td class="gt_row gt_left gt_striped">length_cm</td>
      <td class="gt_row gt_right gt_striped">0.12200000</td>
      <td class="gt_row gt_right gt_striped">3.468120e-01</td>
      <td class="gt_row gt_right gt_striped">-0.1644267529</td>
      <td class="gt_row gt_right gt_striped">0.40842675</td>
      <td class="gt_row gt_center gt_striped">6</td>
      <td class="gt_row gt_center gt_striped">4</td>
      <td class="gt_row gt_right gt_striped">1.00916475</td>
      <td class="gt_row gt_right gt_striped">6.932824</td>
      <td class="gt_row gt_right gt_striped">4.273500e-01</td>
      <td class="gt_row gt_center gt_striped">HET-MUT</td>
      <td class="gt_row gt_left gt_striped">0.43</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">WT</td>
      <td class="gt_row gt_left">HET</td>
      <td class="gt_row gt_left">24 months</td>
      <td class="gt_row gt_left">weight_gr</td>
      <td class="gt_row gt_right">0.08396667</td>
      <td class="gt_row gt_right">2.453113e-01</td>
      <td class="gt_row gt_right">-0.0632879921</td>
      <td class="gt_row gt_right">0.23122133</td>
      <td class="gt_row gt_center">9</td>
      <td class="gt_row gt_center">10</td>
      <td class="gt_row gt_right">1.20372012</td>
      <td class="gt_row gt_right">16.875993</td>
      <td class="gt_row gt_right">3.194751e-01</td>
      <td class="gt_row gt_center">WT-HET</td>
      <td class="gt_row gt_left">0.32</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_striped">WT</td>
      <td class="gt_row gt_left gt_striped">MUT</td>
      <td class="gt_row gt_left gt_striped">24 months</td>
      <td class="gt_row gt_left gt_striped">weight_gr</td>
      <td class="gt_row gt_right gt_striped">NA</td>
      <td class="gt_row gt_right gt_striped">NA</td>
      <td class="gt_row gt_right gt_striped">NA</td>
      <td class="gt_row gt_right gt_striped">NA</td>
      <td class="gt_row gt_center gt_striped">9</td>
      <td class="gt_row gt_center gt_striped">0</td>
      <td class="gt_row gt_right gt_striped">NA</td>
      <td class="gt_row gt_right gt_striped">NA</td>
      <td class="gt_row gt_right gt_striped">NA</td>
      <td class="gt_row gt_center gt_striped">WT-MUT</td>
      <td class="gt_row gt_left gt_striped">NA</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">HET</td>
      <td class="gt_row gt_left">MUT</td>
      <td class="gt_row gt_left">24 months</td>
      <td class="gt_row gt_left">weight_gr</td>
      <td class="gt_row gt_right">NA</td>
      <td class="gt_row gt_right">NA</td>
      <td class="gt_row gt_right">NA</td>
      <td class="gt_row gt_right">NA</td>
      <td class="gt_row gt_center">10</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_right">NA</td>
      <td class="gt_row gt_right">NA</td>
      <td class="gt_row gt_right">NA</td>
      <td class="gt_row gt_center">HET-MUT</td>
      <td class="gt_row gt_left">NA</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_striped">WT</td>
      <td class="gt_row gt_left gt_striped">HET</td>
      <td class="gt_row gt_left gt_striped">24 months</td>
      <td class="gt_row gt_left gt_striped">length_cm</td>
      <td class="gt_row gt_right gt_striped">0.19035556</td>
      <td class="gt_row gt_right gt_striped">2.000748e-01</td>
      <td class="gt_row gt_right gt_striped">-0.1112864647</td>
      <td class="gt_row gt_right gt_striped">0.49199758</td>
      <td class="gt_row gt_center gt_striped">9</td>
      <td class="gt_row gt_center gt_striped">10</td>
      <td class="gt_row gt_right gt_striped">1.33508832</td>
      <td class="gt_row gt_right gt_striped">16.409563</td>
      <td class="gt_row gt_right gt_striped">2.732729e-01</td>
      <td class="gt_row gt_center gt_striped">WT-HET</td>
      <td class="gt_row gt_left gt_striped">0.27</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">WT</td>
      <td class="gt_row gt_left">MUT</td>
      <td class="gt_row gt_left">24 months</td>
      <td class="gt_row gt_left">length_cm</td>
      <td class="gt_row gt_right">NA</td>
      <td class="gt_row gt_right">NA</td>
      <td class="gt_row gt_right">NA</td>
      <td class="gt_row gt_right">NA</td>
      <td class="gt_row gt_center">9</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_right">NA</td>
      <td class="gt_row gt_right">NA</td>
      <td class="gt_row gt_right">NA</td>
      <td class="gt_row gt_center">WT-MUT</td>
      <td class="gt_row gt_left">NA</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_striped">HET</td>
      <td class="gt_row gt_left gt_striped">MUT</td>
      <td class="gt_row gt_left gt_striped">24 months</td>
      <td class="gt_row gt_left gt_striped">length_cm</td>
      <td class="gt_row gt_right gt_striped">NA</td>
      <td class="gt_row gt_right gt_striped">NA</td>
      <td class="gt_row gt_right gt_striped">NA</td>
      <td class="gt_row gt_right gt_striped">NA</td>
      <td class="gt_row gt_center gt_striped">10</td>
      <td class="gt_row gt_center gt_striped">0</td>
      <td class="gt_row gt_right gt_striped">NA</td>
      <td class="gt_row gt_right gt_striped">NA</td>
      <td class="gt_row gt_right gt_striped">NA</td>
      <td class="gt_row gt_center gt_striped">HET-MUT</td>
      <td class="gt_row gt_left gt_striped">NA</td>
    </tr>
  </tbody>
  
  
</table></div>
