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

<div id="fsrwhgxgmx" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#fsrwhgxgmx table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#fsrwhgxgmx thead, #fsrwhgxgmx tbody, #fsrwhgxgmx tfoot, #fsrwhgxgmx tr, #fsrwhgxgmx td, #fsrwhgxgmx th {
  border-style: none;
}

#fsrwhgxgmx p {
  margin: 0;
  padding: 0;
}

#fsrwhgxgmx .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#fsrwhgxgmx .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#fsrwhgxgmx .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#fsrwhgxgmx .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#fsrwhgxgmx .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#fsrwhgxgmx .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#fsrwhgxgmx .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#fsrwhgxgmx .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#fsrwhgxgmx .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#fsrwhgxgmx .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#fsrwhgxgmx .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#fsrwhgxgmx .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#fsrwhgxgmx .gt_spanner_row {
  border-bottom-style: hidden;
}

#fsrwhgxgmx .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#fsrwhgxgmx .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#fsrwhgxgmx .gt_from_md > :first-child {
  margin-top: 0;
}

#fsrwhgxgmx .gt_from_md > :last-child {
  margin-bottom: 0;
}

#fsrwhgxgmx .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#fsrwhgxgmx .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#fsrwhgxgmx .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#fsrwhgxgmx .gt_row_group_first td {
  border-top-width: 2px;
}

#fsrwhgxgmx .gt_row_group_first th {
  border-top-width: 2px;
}

#fsrwhgxgmx .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#fsrwhgxgmx .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#fsrwhgxgmx .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#fsrwhgxgmx .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#fsrwhgxgmx .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#fsrwhgxgmx .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#fsrwhgxgmx .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#fsrwhgxgmx .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#fsrwhgxgmx .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#fsrwhgxgmx .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#fsrwhgxgmx .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#fsrwhgxgmx .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#fsrwhgxgmx .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#fsrwhgxgmx .gt_left {
  text-align: left;
}

#fsrwhgxgmx .gt_center {
  text-align: center;
}

#fsrwhgxgmx .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#fsrwhgxgmx .gt_font_normal {
  font-weight: normal;
}

#fsrwhgxgmx .gt_font_bold {
  font-weight: bold;
}

#fsrwhgxgmx .gt_font_italic {
  font-style: italic;
}

#fsrwhgxgmx .gt_super {
  font-size: 65%;
}

#fsrwhgxgmx .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#fsrwhgxgmx .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#fsrwhgxgmx .gt_indent_1 {
  text-indent: 5px;
}

#fsrwhgxgmx .gt_indent_2 {
  text-indent: 10px;
}

#fsrwhgxgmx .gt_indent_3 {
  text-indent: 15px;
}

#fsrwhgxgmx .gt_indent_4 {
  text-indent: 20px;
}

#fsrwhgxgmx .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_heading">
      <td colspan="11" class="gt_heading gt_title gt_font_normal gt_bottom_border" style>1. jag2b fish counts by timepoint and genotype.</td>
    </tr>
    
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="genotype2">genotype2</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="6 weeks">6 weeks</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="8 weeks">8 weeks</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="10 weeks">10 weeks</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="12 weeks">12 weeks</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="4 months">4 months</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="6 months">6 months</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="8 months">8 months</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="10 months">10 months</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="13 months">13 months</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="24 months">24 months</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="genotype2" class="gt_row gt_center">+/+</td>
<td headers="6 weeks" class="gt_row gt_right">10</td>
<td headers="8 weeks" class="gt_row gt_right">8</td>
<td headers="10 weeks" class="gt_row gt_right">8</td>
<td headers="12 weeks" class="gt_row gt_right">3</td>
<td headers="4 months" class="gt_row gt_right">16</td>
<td headers="6 months" class="gt_row gt_right">16</td>
<td headers="8 months" class="gt_row gt_right">8</td>
<td headers="10 months" class="gt_row gt_right">4</td>
<td headers="13 months" class="gt_row gt_right">10</td>
<td headers="24 months" class="gt_row gt_right">11</td></tr>
    <tr><td headers="genotype2" class="gt_row gt_center">+/-</td>
<td headers="6 weeks" class="gt_row gt_right">13</td>
<td headers="8 weeks" class="gt_row gt_right">8</td>
<td headers="10 weeks" class="gt_row gt_right">8</td>
<td headers="12 weeks" class="gt_row gt_right">3</td>
<td headers="4 months" class="gt_row gt_right">18</td>
<td headers="6 months" class="gt_row gt_right">10</td>
<td headers="8 months" class="gt_row gt_right">5</td>
<td headers="10 months" class="gt_row gt_right">6</td>
<td headers="13 months" class="gt_row gt_right">8</td>
<td headers="24 months" class="gt_row gt_right">13</td></tr>
    <tr><td headers="genotype2" class="gt_row gt_center">-/-</td>
<td headers="6 weeks" class="gt_row gt_right">12</td>
<td headers="8 weeks" class="gt_row gt_right">8</td>
<td headers="10 weeks" class="gt_row gt_right">7</td>
<td headers="12 weeks" class="gt_row gt_right">3</td>
<td headers="4 months" class="gt_row gt_right">16</td>
<td headers="6 months" class="gt_row gt_right">9</td>
<td headers="8 months" class="gt_row gt_right">3</td>
<td headers="10 months" class="gt_row gt_right">7</td>
<td headers="13 months" class="gt_row gt_right">6</td>
<td headers="24 months" class="gt_row gt_right">0</td></tr>
  </tbody>
  
  
</table>
</div>

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
    adding sex to the ANOVA model.")) %>% as_raw_html()
```

<div id="wpqhlvqend" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
  
  <table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false" style="-webkit-font-smoothing: antialiased; -moz-osx-font-smoothing: grayscale; font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji'; display: table; border-collapse: collapse; line-height: normal; margin-left: auto; margin-right: auto; color: #333333; font-size: 16px; font-weight: normal; font-style: normal; background-color: #FFFFFF; width: auto; border-top-style: solid; border-top-width: 2px; border-top-color: #A8A8A8; border-right-style: none; border-right-width: 2px; border-right-color: #D3D3D3; border-bottom-style: solid; border-bottom-width: 2px; border-bottom-color: #A8A8A8; border-left-style: none; border-left-width: 2px; border-left-color: #D3D3D3;" bgcolor="#FFFFFF">
  <thead style="border-style: none;">
    <tr class="gt_heading" style="border-style: none; background-color: #FFFFFF; text-align: center; border-bottom-color: #FFFFFF; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3;" bgcolor="#FFFFFF" align="center">
      <td colspan="11" class="gt_heading gt_title gt_font_normal gt_bottom_border" style="border-style: none; color: #333333; font-size: 125%; padding-top: 4px; padding-bottom: 4px; padding-left: 5px; padding-right: 5px; background-color: #FFFFFF; text-align: center; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; border-bottom-style: solid; border-bottom-width: 2px; border-bottom-color: #D3D3D3; font-weight: normal;" bgcolor="#FFFFFF" align="center">2. jag2b fish counts by timepoint, genotype, and sex</td>
    </tr>
    
    <tr class="gt_col_headings" style="border-style: none; border-top-style: solid; border-top-width: 2px; border-top-color: #D3D3D3; border-bottom-style: solid; border-bottom-width: 2px; border-bottom-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3;">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="sex" style="border-style: none; color: #333333; background-color: #FFFFFF; font-size: 100%; font-weight: normal; text-transform: inherit; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: bottom; padding-top: 5px; padding-bottom: 6px; padding-left: 5px; padding-right: 5px; overflow-x: hidden; text-align: left;" bgcolor="#FFFFFF" valign="bottom" align="left">sex</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="6 weeks" style="border-style: none; color: #333333; background-color: #FFFFFF; font-size: 100%; font-weight: normal; text-transform: inherit; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: bottom; padding-top: 5px; padding-bottom: 6px; padding-left: 5px; padding-right: 5px; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums;" bgcolor="#FFFFFF" valign="bottom" align="right">6 weeks</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="8 weeks" style="border-style: none; color: #333333; background-color: #FFFFFF; font-size: 100%; font-weight: normal; text-transform: inherit; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: bottom; padding-top: 5px; padding-bottom: 6px; padding-left: 5px; padding-right: 5px; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums;" bgcolor="#FFFFFF" valign="bottom" align="right">8 weeks</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="10 weeks" style="border-style: none; color: #333333; background-color: #FFFFFF; font-size: 100%; font-weight: normal; text-transform: inherit; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: bottom; padding-top: 5px; padding-bottom: 6px; padding-left: 5px; padding-right: 5px; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums;" bgcolor="#FFFFFF" valign="bottom" align="right">10 weeks</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="12 weeks" style="border-style: none; color: #333333; background-color: #FFFFFF; font-size: 100%; font-weight: normal; text-transform: inherit; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: bottom; padding-top: 5px; padding-bottom: 6px; padding-left: 5px; padding-right: 5px; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums;" bgcolor="#FFFFFF" valign="bottom" align="right">12 weeks</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="4 months" style="border-style: none; color: #333333; background-color: #FFFFFF; font-size: 100%; font-weight: normal; text-transform: inherit; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: bottom; padding-top: 5px; padding-bottom: 6px; padding-left: 5px; padding-right: 5px; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums;" bgcolor="#FFFFFF" valign="bottom" align="right">4 months</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="6 months" style="border-style: none; color: #333333; background-color: #FFFFFF; font-size: 100%; font-weight: normal; text-transform: inherit; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: bottom; padding-top: 5px; padding-bottom: 6px; padding-left: 5px; padding-right: 5px; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums;" bgcolor="#FFFFFF" valign="bottom" align="right">6 months</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="8 months" style="border-style: none; color: #333333; background-color: #FFFFFF; font-size: 100%; font-weight: normal; text-transform: inherit; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: bottom; padding-top: 5px; padding-bottom: 6px; padding-left: 5px; padding-right: 5px; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums;" bgcolor="#FFFFFF" valign="bottom" align="right">8 months</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="10 months" style="border-style: none; color: #333333; background-color: #FFFFFF; font-size: 100%; font-weight: normal; text-transform: inherit; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: bottom; padding-top: 5px; padding-bottom: 6px; padding-left: 5px; padding-right: 5px; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums;" bgcolor="#FFFFFF" valign="bottom" align="right">10 months</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="13 months" style="border-style: none; color: #333333; background-color: #FFFFFF; font-size: 100%; font-weight: normal; text-transform: inherit; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: bottom; padding-top: 5px; padding-bottom: 6px; padding-left: 5px; padding-right: 5px; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums;" bgcolor="#FFFFFF" valign="bottom" align="right">13 months</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="24 months" style="border-style: none; color: #333333; background-color: #FFFFFF; font-size: 100%; font-weight: normal; text-transform: inherit; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: bottom; padding-top: 5px; padding-bottom: 6px; padding-left: 5px; padding-right: 5px; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums;" bgcolor="#FFFFFF" valign="bottom" align="right">24 months</th>
    </tr>
  </thead>
  <tbody class="gt_table_body" style="border-style: none; border-top-style: solid; border-top-width: 2px; border-top-color: #D3D3D3; border-bottom-style: solid; border-bottom-width: 2px; border-bottom-color: #D3D3D3;">
    <tr class="gt_group_heading_row" style="border-style: none;">
      <th colspan="11" class="gt_group_heading" scope="colgroup" id="+/+" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; color: #333333; background-color: #FFFFFF; font-size: 100%; font-weight: initial; text-transform: inherit; border-top-style: solid; border-top-width: 2px; border-top-color: #D3D3D3; border-bottom-style: solid; border-bottom-width: 2px; border-bottom-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; text-align: left;" bgcolor="#FFFFFF" valign="middle" align="left">+/+</th>
    </tr>
    <tr class="gt_row_group_first" style="border-style: none;"><td headers="+/+  sex" class="gt_row gt_left" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: left; border-top-width: 2px;" valign="middle" align="left">F</td>
<td headers="+/+  6 weeks" class="gt_row gt_right" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums; border-top-width: 2px;" valign="middle" align="right">2</td>
<td headers="+/+  8 weeks" class="gt_row gt_right" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums; border-top-width: 2px;" valign="middle" align="right">7</td>
<td headers="+/+  10 weeks" class="gt_row gt_right" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums; border-top-width: 2px;" valign="middle" align="right">4</td>
<td headers="+/+  12 weeks" class="gt_row gt_right" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums; border-top-width: 2px;" valign="middle" align="right">2</td>
<td headers="+/+  4 months" class="gt_row gt_right" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums; border-top-width: 2px;" valign="middle" align="right">0</td>
<td headers="+/+  6 months" class="gt_row gt_right" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums; border-top-width: 2px;" valign="middle" align="right">0</td>
<td headers="+/+  8 months" class="gt_row gt_right" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums; border-top-width: 2px;" valign="middle" align="right">0</td>
<td headers="+/+  10 months" class="gt_row gt_right" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums; border-top-width: 2px;" valign="middle" align="right">1</td>
<td headers="+/+  13 months" class="gt_row gt_right" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums; border-top-width: 2px;" valign="middle" align="right">2</td>
<td headers="+/+  24 months" class="gt_row gt_right" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums; border-top-width: 2px;" valign="middle" align="right">2</td></tr>
    <tr style="border-style: none;"><td headers="+/+  sex" class="gt_row gt_left" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: left;" valign="middle" align="left">M</td>
<td headers="+/+  6 weeks" class="gt_row gt_right" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums;" valign="middle" align="right">8</td>
<td headers="+/+  8 weeks" class="gt_row gt_right" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums;" valign="middle" align="right">1</td>
<td headers="+/+  10 weeks" class="gt_row gt_right" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums;" valign="middle" align="right">4</td>
<td headers="+/+  12 weeks" class="gt_row gt_right" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums;" valign="middle" align="right">1</td>
<td headers="+/+  4 months" class="gt_row gt_right" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums;" valign="middle" align="right">16</td>
<td headers="+/+  6 months" class="gt_row gt_right" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums;" valign="middle" align="right">16</td>
<td headers="+/+  8 months" class="gt_row gt_right" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums;" valign="middle" align="right">8</td>
<td headers="+/+  10 months" class="gt_row gt_right" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums;" valign="middle" align="right">3</td>
<td headers="+/+  13 months" class="gt_row gt_right" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums;" valign="middle" align="right">8</td>
<td headers="+/+  24 months" class="gt_row gt_right" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums;" valign="middle" align="right">9</td></tr>
    <tr class="gt_group_heading_row" style="border-style: none;">
      <th colspan="11" class="gt_group_heading" scope="colgroup" id="+/-" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; color: #333333; background-color: #FFFFFF; font-size: 100%; font-weight: initial; text-transform: inherit; border-top-style: solid; border-top-width: 2px; border-top-color: #D3D3D3; border-bottom-style: solid; border-bottom-width: 2px; border-bottom-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; text-align: left;" bgcolor="#FFFFFF" valign="middle" align="left">+/-</th>
    </tr>
    <tr class="gt_row_group_first" style="border-style: none;"><td headers="+/-  sex" class="gt_row gt_left" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: left; border-top-width: 2px;" valign="middle" align="left">F</td>
<td headers="+/-  6 weeks" class="gt_row gt_right" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums; border-top-width: 2px;" valign="middle" align="right">3</td>
<td headers="+/-  8 weeks" class="gt_row gt_right" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums; border-top-width: 2px;" valign="middle" align="right">4</td>
<td headers="+/-  10 weeks" class="gt_row gt_right" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums; border-top-width: 2px;" valign="middle" align="right">6</td>
<td headers="+/-  12 weeks" class="gt_row gt_right" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums; border-top-width: 2px;" valign="middle" align="right">1</td>
<td headers="+/-  4 months" class="gt_row gt_right" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums; border-top-width: 2px;" valign="middle" align="right">0</td>
<td headers="+/-  6 months" class="gt_row gt_right" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums; border-top-width: 2px;" valign="middle" align="right">0</td>
<td headers="+/-  8 months" class="gt_row gt_right" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums; border-top-width: 2px;" valign="middle" align="right">0</td>
<td headers="+/-  10 months" class="gt_row gt_right" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums; border-top-width: 2px;" valign="middle" align="right">0</td>
<td headers="+/-  13 months" class="gt_row gt_right" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums; border-top-width: 2px;" valign="middle" align="right">2</td>
<td headers="+/-  24 months" class="gt_row gt_right" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums; border-top-width: 2px;" valign="middle" align="right">3</td></tr>
    <tr style="border-style: none;"><td headers="+/-  sex" class="gt_row gt_left" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: left;" valign="middle" align="left">M</td>
<td headers="+/-  6 weeks" class="gt_row gt_right" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums;" valign="middle" align="right">10</td>
<td headers="+/-  8 weeks" class="gt_row gt_right" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums;" valign="middle" align="right">4</td>
<td headers="+/-  10 weeks" class="gt_row gt_right" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums;" valign="middle" align="right">2</td>
<td headers="+/-  12 weeks" class="gt_row gt_right" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums;" valign="middle" align="right">2</td>
<td headers="+/-  4 months" class="gt_row gt_right" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums;" valign="middle" align="right">18</td>
<td headers="+/-  6 months" class="gt_row gt_right" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums;" valign="middle" align="right">10</td>
<td headers="+/-  8 months" class="gt_row gt_right" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums;" valign="middle" align="right">5</td>
<td headers="+/-  10 months" class="gt_row gt_right" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums;" valign="middle" align="right">6</td>
<td headers="+/-  13 months" class="gt_row gt_right" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums;" valign="middle" align="right">6</td>
<td headers="+/-  24 months" class="gt_row gt_right" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums;" valign="middle" align="right">10</td></tr>
    <tr class="gt_group_heading_row" style="border-style: none;">
      <th colspan="11" class="gt_group_heading" scope="colgroup" id="-/-" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; color: #333333; background-color: #FFFFFF; font-size: 100%; font-weight: initial; text-transform: inherit; border-top-style: solid; border-top-width: 2px; border-top-color: #D3D3D3; border-bottom-style: solid; border-bottom-width: 2px; border-bottom-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; text-align: left;" bgcolor="#FFFFFF" valign="middle" align="left">-/-</th>
    </tr>
    <tr class="gt_row_group_first" style="border-style: none;"><td headers="-/-  sex" class="gt_row gt_left" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: left; border-top-width: 2px;" valign="middle" align="left">F</td>
<td headers="-/-  6 weeks" class="gt_row gt_right" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums; border-top-width: 2px;" valign="middle" align="right">3</td>
<td headers="-/-  8 weeks" class="gt_row gt_right" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums; border-top-width: 2px;" valign="middle" align="right">1</td>
<td headers="-/-  10 weeks" class="gt_row gt_right" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums; border-top-width: 2px;" valign="middle" align="right">1</td>
<td headers="-/-  12 weeks" class="gt_row gt_right" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums; border-top-width: 2px;" valign="middle" align="right">1</td>
<td headers="-/-  4 months" class="gt_row gt_right" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums; border-top-width: 2px;" valign="middle" align="right">0</td>
<td headers="-/-  6 months" class="gt_row gt_right" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums; border-top-width: 2px;" valign="middle" align="right">0</td>
<td headers="-/-  8 months" class="gt_row gt_right" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums; border-top-width: 2px;" valign="middle" align="right">0</td>
<td headers="-/-  10 months" class="gt_row gt_right" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums; border-top-width: 2px;" valign="middle" align="right">0</td>
<td headers="-/-  13 months" class="gt_row gt_right" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums; border-top-width: 2px;" valign="middle" align="right">2</td>
<td headers="-/-  24 months" class="gt_row gt_right" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums; border-top-width: 2px;" valign="middle" align="right">0</td></tr>
    <tr style="border-style: none;"><td headers="-/-  sex" class="gt_row gt_left" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: left;" valign="middle" align="left">M</td>
<td headers="-/-  6 weeks" class="gt_row gt_right" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums;" valign="middle" align="right">9</td>
<td headers="-/-  8 weeks" class="gt_row gt_right" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums;" valign="middle" align="right">7</td>
<td headers="-/-  10 weeks" class="gt_row gt_right" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums;" valign="middle" align="right">6</td>
<td headers="-/-  12 weeks" class="gt_row gt_right" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums;" valign="middle" align="right">2</td>
<td headers="-/-  4 months" class="gt_row gt_right" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums;" valign="middle" align="right">16</td>
<td headers="-/-  6 months" class="gt_row gt_right" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums;" valign="middle" align="right">9</td>
<td headers="-/-  8 months" class="gt_row gt_right" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums;" valign="middle" align="right">3</td>
<td headers="-/-  10 months" class="gt_row gt_right" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums;" valign="middle" align="right">7</td>
<td headers="-/-  13 months" class="gt_row gt_right" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums;" valign="middle" align="right">4</td>
<td headers="-/-  24 months" class="gt_row gt_right" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums;" valign="middle" align="right">0</td></tr>
  </tbody>
  <tfoot class="gt_sourcenotes" style="border-style: none; color: #333333; background-color: #FFFFFF; border-bottom-style: none; border-bottom-width: 2px; border-bottom-color: #D3D3D3; border-left-style: none; border-left-width: 2px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 2px; border-right-color: #D3D3D3;" bgcolor="#FFFFFF">
    <tr style="border-style: none;">
      <td class="gt_sourcenote" colspan="11" style="border-style: none; font-size: 90%; padding-top: 4px; padding-bottom: 4px; padding-left: 5px; padding-right: 5px;">Data modifications: For plots and stats analysis, we will<p style="margin: 0; padding: 0;"></p>
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
  
</table>
</div>

``` r
# Table dob batches, by time point.

dcast(data=tab, dob ~ age2, fun.aggregate=length) %>% 
  as_tibble() %>%
  gt() %>%
  tab_header(title="jag2b fish counts by timepoint and date-of-birth.")
```

<div id="vrfleqrjoi" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#vrfleqrjoi table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#vrfleqrjoi thead, #vrfleqrjoi tbody, #vrfleqrjoi tfoot, #vrfleqrjoi tr, #vrfleqrjoi td, #vrfleqrjoi th {
  border-style: none;
}

#vrfleqrjoi p {
  margin: 0;
  padding: 0;
}

#vrfleqrjoi .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#vrfleqrjoi .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#vrfleqrjoi .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#vrfleqrjoi .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#vrfleqrjoi .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#vrfleqrjoi .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#vrfleqrjoi .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#vrfleqrjoi .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#vrfleqrjoi .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#vrfleqrjoi .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#vrfleqrjoi .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#vrfleqrjoi .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#vrfleqrjoi .gt_spanner_row {
  border-bottom-style: hidden;
}

#vrfleqrjoi .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#vrfleqrjoi .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#vrfleqrjoi .gt_from_md > :first-child {
  margin-top: 0;
}

#vrfleqrjoi .gt_from_md > :last-child {
  margin-bottom: 0;
}

#vrfleqrjoi .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#vrfleqrjoi .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#vrfleqrjoi .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#vrfleqrjoi .gt_row_group_first td {
  border-top-width: 2px;
}

#vrfleqrjoi .gt_row_group_first th {
  border-top-width: 2px;
}

#vrfleqrjoi .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#vrfleqrjoi .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#vrfleqrjoi .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#vrfleqrjoi .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#vrfleqrjoi .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#vrfleqrjoi .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#vrfleqrjoi .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#vrfleqrjoi .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#vrfleqrjoi .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#vrfleqrjoi .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#vrfleqrjoi .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#vrfleqrjoi .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#vrfleqrjoi .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#vrfleqrjoi .gt_left {
  text-align: left;
}

#vrfleqrjoi .gt_center {
  text-align: center;
}

#vrfleqrjoi .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#vrfleqrjoi .gt_font_normal {
  font-weight: normal;
}

#vrfleqrjoi .gt_font_bold {
  font-weight: bold;
}

#vrfleqrjoi .gt_font_italic {
  font-style: italic;
}

#vrfleqrjoi .gt_super {
  font-size: 65%;
}

#vrfleqrjoi .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#vrfleqrjoi .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#vrfleqrjoi .gt_indent_1 {
  text-indent: 5px;
}

#vrfleqrjoi .gt_indent_2 {
  text-indent: 10px;
}

#vrfleqrjoi .gt_indent_3 {
  text-indent: 15px;
}

#vrfleqrjoi .gt_indent_4 {
  text-indent: 20px;
}

#vrfleqrjoi .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_heading">
      <td colspan="11" class="gt_heading gt_title gt_font_normal gt_bottom_border" style>jag2b fish counts by timepoint and date-of-birth.</td>
    </tr>
    
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="dob">dob</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="6 weeks">6 weeks</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="8 weeks">8 weeks</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="10 weeks">10 weeks</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="12 weeks">12 weeks</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="4 months">4 months</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="6 months">6 months</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="8 months">8 months</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="10 months">10 months</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="13 months">13 months</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="24 months">24 months</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="dob" class="gt_row gt_right">20190813</td>
<td headers="6 weeks" class="gt_row gt_right">0</td>
<td headers="8 weeks" class="gt_row gt_right">0</td>
<td headers="10 weeks" class="gt_row gt_right">0</td>
<td headers="12 weeks" class="gt_row gt_right">0</td>
<td headers="4 months" class="gt_row gt_right">0</td>
<td headers="6 months" class="gt_row gt_right">0</td>
<td headers="8 months" class="gt_row gt_right">0</td>
<td headers="10 months" class="gt_row gt_right">0</td>
<td headers="13 months" class="gt_row gt_right">0</td>
<td headers="24 months" class="gt_row gt_right">24</td></tr>
    <tr><td headers="dob" class="gt_row gt_right">20200922</td>
<td headers="6 weeks" class="gt_row gt_right">0</td>
<td headers="8 weeks" class="gt_row gt_right">0</td>
<td headers="10 weeks" class="gt_row gt_right">0</td>
<td headers="12 weeks" class="gt_row gt_right">0</td>
<td headers="4 months" class="gt_row gt_right">0</td>
<td headers="6 months" class="gt_row gt_right">0</td>
<td headers="8 months" class="gt_row gt_right">0</td>
<td headers="10 months" class="gt_row gt_right">17</td>
<td headers="13 months" class="gt_row gt_right">0</td>
<td headers="24 months" class="gt_row gt_right">0</td></tr>
    <tr><td headers="dob" class="gt_row gt_right">20201014</td>
<td headers="6 weeks" class="gt_row gt_right">0</td>
<td headers="8 weeks" class="gt_row gt_right">0</td>
<td headers="10 weeks" class="gt_row gt_right">0</td>
<td headers="12 weeks" class="gt_row gt_right">0</td>
<td headers="4 months" class="gt_row gt_right">0</td>
<td headers="6 months" class="gt_row gt_right">0</td>
<td headers="8 months" class="gt_row gt_right">0</td>
<td headers="10 months" class="gt_row gt_right">0</td>
<td headers="13 months" class="gt_row gt_right">24</td>
<td headers="24 months" class="gt_row gt_right">0</td></tr>
    <tr><td headers="dob" class="gt_row gt_right">20210715</td>
<td headers="6 weeks" class="gt_row gt_right">10</td>
<td headers="8 weeks" class="gt_row gt_right">12</td>
<td headers="10 weeks" class="gt_row gt_right">12</td>
<td headers="12 weeks" class="gt_row gt_right">9</td>
<td headers="4 months" class="gt_row gt_right">0</td>
<td headers="6 months" class="gt_row gt_right">0</td>
<td headers="8 months" class="gt_row gt_right">0</td>
<td headers="10 months" class="gt_row gt_right">0</td>
<td headers="13 months" class="gt_row gt_right">0</td>
<td headers="24 months" class="gt_row gt_right">0</td></tr>
    <tr><td headers="dob" class="gt_row gt_right">20210728</td>
<td headers="6 weeks" class="gt_row gt_right">13</td>
<td headers="8 weeks" class="gt_row gt_right">12</td>
<td headers="10 weeks" class="gt_row gt_right">11</td>
<td headers="12 weeks" class="gt_row gt_right">0</td>
<td headers="4 months" class="gt_row gt_right">0</td>
<td headers="6 months" class="gt_row gt_right">0</td>
<td headers="8 months" class="gt_row gt_right">0</td>
<td headers="10 months" class="gt_row gt_right">0</td>
<td headers="13 months" class="gt_row gt_right">0</td>
<td headers="24 months" class="gt_row gt_right">0</td></tr>
    <tr><td headers="dob" class="gt_row gt_right">20210819</td>
<td headers="6 weeks" class="gt_row gt_right">12</td>
<td headers="8 weeks" class="gt_row gt_right">0</td>
<td headers="10 weeks" class="gt_row gt_right">0</td>
<td headers="12 weeks" class="gt_row gt_right">0</td>
<td headers="4 months" class="gt_row gt_right">50</td>
<td headers="6 months" class="gt_row gt_right">0</td>
<td headers="8 months" class="gt_row gt_right">0</td>
<td headers="10 months" class="gt_row gt_right">0</td>
<td headers="13 months" class="gt_row gt_right">0</td>
<td headers="24 months" class="gt_row gt_right">0</td></tr>
    <tr><td headers="dob" class="gt_row gt_right">20211003</td>
<td headers="6 weeks" class="gt_row gt_right">0</td>
<td headers="8 weeks" class="gt_row gt_right">0</td>
<td headers="10 weeks" class="gt_row gt_right">0</td>
<td headers="12 weeks" class="gt_row gt_right">0</td>
<td headers="4 months" class="gt_row gt_right">0</td>
<td headers="6 months" class="gt_row gt_right">0</td>
<td headers="8 months" class="gt_row gt_right">16</td>
<td headers="10 months" class="gt_row gt_right">0</td>
<td headers="13 months" class="gt_row gt_right">0</td>
<td headers="24 months" class="gt_row gt_right">0</td></tr>
    <tr><td headers="dob" class="gt_row gt_right">20211210</td>
<td headers="6 weeks" class="gt_row gt_right">0</td>
<td headers="8 weeks" class="gt_row gt_right">0</td>
<td headers="10 weeks" class="gt_row gt_right">0</td>
<td headers="12 weeks" class="gt_row gt_right">0</td>
<td headers="4 months" class="gt_row gt_right">0</td>
<td headers="6 months" class="gt_row gt_right">35</td>
<td headers="8 months" class="gt_row gt_right">0</td>
<td headers="10 months" class="gt_row gt_right">0</td>
<td headers="13 months" class="gt_row gt_right">0</td>
<td headers="24 months" class="gt_row gt_right">0</td></tr>
  </tbody>
  
  
</table>
</div>

``` r
# Table genotype by time point, grouped by dob.

dcast(data=tab, dob + genotype2 ~ age2, fun.aggregate=length) %>% 
  as_tibble() %>%
  group_by(dob) %>%
  gt() %>%
  tab_header(title="jag2b fish counts by timepoint, data-of-birth, and genotype.")
```

<div id="covozoyxll" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#covozoyxll table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#covozoyxll thead, #covozoyxll tbody, #covozoyxll tfoot, #covozoyxll tr, #covozoyxll td, #covozoyxll th {
  border-style: none;
}

#covozoyxll p {
  margin: 0;
  padding: 0;
}

#covozoyxll .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#covozoyxll .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#covozoyxll .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#covozoyxll .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#covozoyxll .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#covozoyxll .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#covozoyxll .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#covozoyxll .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#covozoyxll .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#covozoyxll .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#covozoyxll .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#covozoyxll .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#covozoyxll .gt_spanner_row {
  border-bottom-style: hidden;
}

#covozoyxll .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#covozoyxll .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#covozoyxll .gt_from_md > :first-child {
  margin-top: 0;
}

#covozoyxll .gt_from_md > :last-child {
  margin-bottom: 0;
}

#covozoyxll .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#covozoyxll .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#covozoyxll .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#covozoyxll .gt_row_group_first td {
  border-top-width: 2px;
}

#covozoyxll .gt_row_group_first th {
  border-top-width: 2px;
}

#covozoyxll .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#covozoyxll .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#covozoyxll .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#covozoyxll .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#covozoyxll .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#covozoyxll .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#covozoyxll .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#covozoyxll .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#covozoyxll .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#covozoyxll .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#covozoyxll .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#covozoyxll .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#covozoyxll .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#covozoyxll .gt_left {
  text-align: left;
}

#covozoyxll .gt_center {
  text-align: center;
}

#covozoyxll .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#covozoyxll .gt_font_normal {
  font-weight: normal;
}

#covozoyxll .gt_font_bold {
  font-weight: bold;
}

#covozoyxll .gt_font_italic {
  font-style: italic;
}

#covozoyxll .gt_super {
  font-size: 65%;
}

#covozoyxll .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#covozoyxll .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#covozoyxll .gt_indent_1 {
  text-indent: 5px;
}

#covozoyxll .gt_indent_2 {
  text-indent: 10px;
}

#covozoyxll .gt_indent_3 {
  text-indent: 15px;
}

#covozoyxll .gt_indent_4 {
  text-indent: 20px;
}

#covozoyxll .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_heading">
      <td colspan="11" class="gt_heading gt_title gt_font_normal gt_bottom_border" style>jag2b fish counts by timepoint, data-of-birth, and genotype.</td>
    </tr>
    
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="genotype2">genotype2</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="6 weeks">6 weeks</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="8 weeks">8 weeks</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="10 weeks">10 weeks</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="12 weeks">12 weeks</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="4 months">4 months</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="6 months">6 months</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="8 months">8 months</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="10 months">10 months</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="13 months">13 months</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="24 months">24 months</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr class="gt_group_heading_row">
      <th colspan="11" class="gt_group_heading" scope="colgroup" id="20190813">20190813</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="20190813  genotype2" class="gt_row gt_center">+/+</td>
<td headers="20190813  6 weeks" class="gt_row gt_right">0</td>
<td headers="20190813  8 weeks" class="gt_row gt_right">0</td>
<td headers="20190813  10 weeks" class="gt_row gt_right">0</td>
<td headers="20190813  12 weeks" class="gt_row gt_right">0</td>
<td headers="20190813  4 months" class="gt_row gt_right">0</td>
<td headers="20190813  6 months" class="gt_row gt_right">0</td>
<td headers="20190813  8 months" class="gt_row gt_right">0</td>
<td headers="20190813  10 months" class="gt_row gt_right">0</td>
<td headers="20190813  13 months" class="gt_row gt_right">0</td>
<td headers="20190813  24 months" class="gt_row gt_right">11</td></tr>
    <tr><td headers="20190813  genotype2" class="gt_row gt_center">+/-</td>
<td headers="20190813  6 weeks" class="gt_row gt_right">0</td>
<td headers="20190813  8 weeks" class="gt_row gt_right">0</td>
<td headers="20190813  10 weeks" class="gt_row gt_right">0</td>
<td headers="20190813  12 weeks" class="gt_row gt_right">0</td>
<td headers="20190813  4 months" class="gt_row gt_right">0</td>
<td headers="20190813  6 months" class="gt_row gt_right">0</td>
<td headers="20190813  8 months" class="gt_row gt_right">0</td>
<td headers="20190813  10 months" class="gt_row gt_right">0</td>
<td headers="20190813  13 months" class="gt_row gt_right">0</td>
<td headers="20190813  24 months" class="gt_row gt_right">13</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="11" class="gt_group_heading" scope="colgroup" id="20200922">20200922</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="20200922  genotype2" class="gt_row gt_center">+/+</td>
<td headers="20200922  6 weeks" class="gt_row gt_right">0</td>
<td headers="20200922  8 weeks" class="gt_row gt_right">0</td>
<td headers="20200922  10 weeks" class="gt_row gt_right">0</td>
<td headers="20200922  12 weeks" class="gt_row gt_right">0</td>
<td headers="20200922  4 months" class="gt_row gt_right">0</td>
<td headers="20200922  6 months" class="gt_row gt_right">0</td>
<td headers="20200922  8 months" class="gt_row gt_right">0</td>
<td headers="20200922  10 months" class="gt_row gt_right">4</td>
<td headers="20200922  13 months" class="gt_row gt_right">0</td>
<td headers="20200922  24 months" class="gt_row gt_right">0</td></tr>
    <tr><td headers="20200922  genotype2" class="gt_row gt_center">+/-</td>
<td headers="20200922  6 weeks" class="gt_row gt_right">0</td>
<td headers="20200922  8 weeks" class="gt_row gt_right">0</td>
<td headers="20200922  10 weeks" class="gt_row gt_right">0</td>
<td headers="20200922  12 weeks" class="gt_row gt_right">0</td>
<td headers="20200922  4 months" class="gt_row gt_right">0</td>
<td headers="20200922  6 months" class="gt_row gt_right">0</td>
<td headers="20200922  8 months" class="gt_row gt_right">0</td>
<td headers="20200922  10 months" class="gt_row gt_right">6</td>
<td headers="20200922  13 months" class="gt_row gt_right">0</td>
<td headers="20200922  24 months" class="gt_row gt_right">0</td></tr>
    <tr><td headers="20200922  genotype2" class="gt_row gt_center">-/-</td>
<td headers="20200922  6 weeks" class="gt_row gt_right">0</td>
<td headers="20200922  8 weeks" class="gt_row gt_right">0</td>
<td headers="20200922  10 weeks" class="gt_row gt_right">0</td>
<td headers="20200922  12 weeks" class="gt_row gt_right">0</td>
<td headers="20200922  4 months" class="gt_row gt_right">0</td>
<td headers="20200922  6 months" class="gt_row gt_right">0</td>
<td headers="20200922  8 months" class="gt_row gt_right">0</td>
<td headers="20200922  10 months" class="gt_row gt_right">7</td>
<td headers="20200922  13 months" class="gt_row gt_right">0</td>
<td headers="20200922  24 months" class="gt_row gt_right">0</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="11" class="gt_group_heading" scope="colgroup" id="20201014">20201014</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="20201014  genotype2" class="gt_row gt_center">+/+</td>
<td headers="20201014  6 weeks" class="gt_row gt_right">0</td>
<td headers="20201014  8 weeks" class="gt_row gt_right">0</td>
<td headers="20201014  10 weeks" class="gt_row gt_right">0</td>
<td headers="20201014  12 weeks" class="gt_row gt_right">0</td>
<td headers="20201014  4 months" class="gt_row gt_right">0</td>
<td headers="20201014  6 months" class="gt_row gt_right">0</td>
<td headers="20201014  8 months" class="gt_row gt_right">0</td>
<td headers="20201014  10 months" class="gt_row gt_right">0</td>
<td headers="20201014  13 months" class="gt_row gt_right">10</td>
<td headers="20201014  24 months" class="gt_row gt_right">0</td></tr>
    <tr><td headers="20201014  genotype2" class="gt_row gt_center">+/-</td>
<td headers="20201014  6 weeks" class="gt_row gt_right">0</td>
<td headers="20201014  8 weeks" class="gt_row gt_right">0</td>
<td headers="20201014  10 weeks" class="gt_row gt_right">0</td>
<td headers="20201014  12 weeks" class="gt_row gt_right">0</td>
<td headers="20201014  4 months" class="gt_row gt_right">0</td>
<td headers="20201014  6 months" class="gt_row gt_right">0</td>
<td headers="20201014  8 months" class="gt_row gt_right">0</td>
<td headers="20201014  10 months" class="gt_row gt_right">0</td>
<td headers="20201014  13 months" class="gt_row gt_right">8</td>
<td headers="20201014  24 months" class="gt_row gt_right">0</td></tr>
    <tr><td headers="20201014  genotype2" class="gt_row gt_center">-/-</td>
<td headers="20201014  6 weeks" class="gt_row gt_right">0</td>
<td headers="20201014  8 weeks" class="gt_row gt_right">0</td>
<td headers="20201014  10 weeks" class="gt_row gt_right">0</td>
<td headers="20201014  12 weeks" class="gt_row gt_right">0</td>
<td headers="20201014  4 months" class="gt_row gt_right">0</td>
<td headers="20201014  6 months" class="gt_row gt_right">0</td>
<td headers="20201014  8 months" class="gt_row gt_right">0</td>
<td headers="20201014  10 months" class="gt_row gt_right">0</td>
<td headers="20201014  13 months" class="gt_row gt_right">6</td>
<td headers="20201014  24 months" class="gt_row gt_right">0</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="11" class="gt_group_heading" scope="colgroup" id="20210715">20210715</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="20210715  genotype2" class="gt_row gt_center">+/+</td>
<td headers="20210715  6 weeks" class="gt_row gt_right">3</td>
<td headers="20210715  8 weeks" class="gt_row gt_right">4</td>
<td headers="20210715  10 weeks" class="gt_row gt_right">4</td>
<td headers="20210715  12 weeks" class="gt_row gt_right">3</td>
<td headers="20210715  4 months" class="gt_row gt_right">0</td>
<td headers="20210715  6 months" class="gt_row gt_right">0</td>
<td headers="20210715  8 months" class="gt_row gt_right">0</td>
<td headers="20210715  10 months" class="gt_row gt_right">0</td>
<td headers="20210715  13 months" class="gt_row gt_right">0</td>
<td headers="20210715  24 months" class="gt_row gt_right">0</td></tr>
    <tr><td headers="20210715  genotype2" class="gt_row gt_center">+/-</td>
<td headers="20210715  6 weeks" class="gt_row gt_right">4</td>
<td headers="20210715  8 weeks" class="gt_row gt_right">4</td>
<td headers="20210715  10 weeks" class="gt_row gt_right">4</td>
<td headers="20210715  12 weeks" class="gt_row gt_right">3</td>
<td headers="20210715  4 months" class="gt_row gt_right">0</td>
<td headers="20210715  6 months" class="gt_row gt_right">0</td>
<td headers="20210715  8 months" class="gt_row gt_right">0</td>
<td headers="20210715  10 months" class="gt_row gt_right">0</td>
<td headers="20210715  13 months" class="gt_row gt_right">0</td>
<td headers="20210715  24 months" class="gt_row gt_right">0</td></tr>
    <tr><td headers="20210715  genotype2" class="gt_row gt_center">-/-</td>
<td headers="20210715  6 weeks" class="gt_row gt_right">3</td>
<td headers="20210715  8 weeks" class="gt_row gt_right">4</td>
<td headers="20210715  10 weeks" class="gt_row gt_right">4</td>
<td headers="20210715  12 weeks" class="gt_row gt_right">3</td>
<td headers="20210715  4 months" class="gt_row gt_right">0</td>
<td headers="20210715  6 months" class="gt_row gt_right">0</td>
<td headers="20210715  8 months" class="gt_row gt_right">0</td>
<td headers="20210715  10 months" class="gt_row gt_right">0</td>
<td headers="20210715  13 months" class="gt_row gt_right">0</td>
<td headers="20210715  24 months" class="gt_row gt_right">0</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="11" class="gt_group_heading" scope="colgroup" id="20210728">20210728</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="20210728  genotype2" class="gt_row gt_center">+/+</td>
<td headers="20210728  6 weeks" class="gt_row gt_right">3</td>
<td headers="20210728  8 weeks" class="gt_row gt_right">4</td>
<td headers="20210728  10 weeks" class="gt_row gt_right">4</td>
<td headers="20210728  12 weeks" class="gt_row gt_right">0</td>
<td headers="20210728  4 months" class="gt_row gt_right">0</td>
<td headers="20210728  6 months" class="gt_row gt_right">0</td>
<td headers="20210728  8 months" class="gt_row gt_right">0</td>
<td headers="20210728  10 months" class="gt_row gt_right">0</td>
<td headers="20210728  13 months" class="gt_row gt_right">0</td>
<td headers="20210728  24 months" class="gt_row gt_right">0</td></tr>
    <tr><td headers="20210728  genotype2" class="gt_row gt_center">+/-</td>
<td headers="20210728  6 weeks" class="gt_row gt_right">5</td>
<td headers="20210728  8 weeks" class="gt_row gt_right">4</td>
<td headers="20210728  10 weeks" class="gt_row gt_right">4</td>
<td headers="20210728  12 weeks" class="gt_row gt_right">0</td>
<td headers="20210728  4 months" class="gt_row gt_right">0</td>
<td headers="20210728  6 months" class="gt_row gt_right">0</td>
<td headers="20210728  8 months" class="gt_row gt_right">0</td>
<td headers="20210728  10 months" class="gt_row gt_right">0</td>
<td headers="20210728  13 months" class="gt_row gt_right">0</td>
<td headers="20210728  24 months" class="gt_row gt_right">0</td></tr>
    <tr><td headers="20210728  genotype2" class="gt_row gt_center">-/-</td>
<td headers="20210728  6 weeks" class="gt_row gt_right">5</td>
<td headers="20210728  8 weeks" class="gt_row gt_right">4</td>
<td headers="20210728  10 weeks" class="gt_row gt_right">3</td>
<td headers="20210728  12 weeks" class="gt_row gt_right">0</td>
<td headers="20210728  4 months" class="gt_row gt_right">0</td>
<td headers="20210728  6 months" class="gt_row gt_right">0</td>
<td headers="20210728  8 months" class="gt_row gt_right">0</td>
<td headers="20210728  10 months" class="gt_row gt_right">0</td>
<td headers="20210728  13 months" class="gt_row gt_right">0</td>
<td headers="20210728  24 months" class="gt_row gt_right">0</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="11" class="gt_group_heading" scope="colgroup" id="20210819">20210819</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="20210819  genotype2" class="gt_row gt_center">+/+</td>
<td headers="20210819  6 weeks" class="gt_row gt_right">4</td>
<td headers="20210819  8 weeks" class="gt_row gt_right">0</td>
<td headers="20210819  10 weeks" class="gt_row gt_right">0</td>
<td headers="20210819  12 weeks" class="gt_row gt_right">0</td>
<td headers="20210819  4 months" class="gt_row gt_right">16</td>
<td headers="20210819  6 months" class="gt_row gt_right">0</td>
<td headers="20210819  8 months" class="gt_row gt_right">0</td>
<td headers="20210819  10 months" class="gt_row gt_right">0</td>
<td headers="20210819  13 months" class="gt_row gt_right">0</td>
<td headers="20210819  24 months" class="gt_row gt_right">0</td></tr>
    <tr><td headers="20210819  genotype2" class="gt_row gt_center">+/-</td>
<td headers="20210819  6 weeks" class="gt_row gt_right">4</td>
<td headers="20210819  8 weeks" class="gt_row gt_right">0</td>
<td headers="20210819  10 weeks" class="gt_row gt_right">0</td>
<td headers="20210819  12 weeks" class="gt_row gt_right">0</td>
<td headers="20210819  4 months" class="gt_row gt_right">18</td>
<td headers="20210819  6 months" class="gt_row gt_right">0</td>
<td headers="20210819  8 months" class="gt_row gt_right">0</td>
<td headers="20210819  10 months" class="gt_row gt_right">0</td>
<td headers="20210819  13 months" class="gt_row gt_right">0</td>
<td headers="20210819  24 months" class="gt_row gt_right">0</td></tr>
    <tr><td headers="20210819  genotype2" class="gt_row gt_center">-/-</td>
<td headers="20210819  6 weeks" class="gt_row gt_right">4</td>
<td headers="20210819  8 weeks" class="gt_row gt_right">0</td>
<td headers="20210819  10 weeks" class="gt_row gt_right">0</td>
<td headers="20210819  12 weeks" class="gt_row gt_right">0</td>
<td headers="20210819  4 months" class="gt_row gt_right">16</td>
<td headers="20210819  6 months" class="gt_row gt_right">0</td>
<td headers="20210819  8 months" class="gt_row gt_right">0</td>
<td headers="20210819  10 months" class="gt_row gt_right">0</td>
<td headers="20210819  13 months" class="gt_row gt_right">0</td>
<td headers="20210819  24 months" class="gt_row gt_right">0</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="11" class="gt_group_heading" scope="colgroup" id="20211003">20211003</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="20211003  genotype2" class="gt_row gt_center">+/+</td>
<td headers="20211003  6 weeks" class="gt_row gt_right">0</td>
<td headers="20211003  8 weeks" class="gt_row gt_right">0</td>
<td headers="20211003  10 weeks" class="gt_row gt_right">0</td>
<td headers="20211003  12 weeks" class="gt_row gt_right">0</td>
<td headers="20211003  4 months" class="gt_row gt_right">0</td>
<td headers="20211003  6 months" class="gt_row gt_right">0</td>
<td headers="20211003  8 months" class="gt_row gt_right">8</td>
<td headers="20211003  10 months" class="gt_row gt_right">0</td>
<td headers="20211003  13 months" class="gt_row gt_right">0</td>
<td headers="20211003  24 months" class="gt_row gt_right">0</td></tr>
    <tr><td headers="20211003  genotype2" class="gt_row gt_center">+/-</td>
<td headers="20211003  6 weeks" class="gt_row gt_right">0</td>
<td headers="20211003  8 weeks" class="gt_row gt_right">0</td>
<td headers="20211003  10 weeks" class="gt_row gt_right">0</td>
<td headers="20211003  12 weeks" class="gt_row gt_right">0</td>
<td headers="20211003  4 months" class="gt_row gt_right">0</td>
<td headers="20211003  6 months" class="gt_row gt_right">0</td>
<td headers="20211003  8 months" class="gt_row gt_right">5</td>
<td headers="20211003  10 months" class="gt_row gt_right">0</td>
<td headers="20211003  13 months" class="gt_row gt_right">0</td>
<td headers="20211003  24 months" class="gt_row gt_right">0</td></tr>
    <tr><td headers="20211003  genotype2" class="gt_row gt_center">-/-</td>
<td headers="20211003  6 weeks" class="gt_row gt_right">0</td>
<td headers="20211003  8 weeks" class="gt_row gt_right">0</td>
<td headers="20211003  10 weeks" class="gt_row gt_right">0</td>
<td headers="20211003  12 weeks" class="gt_row gt_right">0</td>
<td headers="20211003  4 months" class="gt_row gt_right">0</td>
<td headers="20211003  6 months" class="gt_row gt_right">0</td>
<td headers="20211003  8 months" class="gt_row gt_right">3</td>
<td headers="20211003  10 months" class="gt_row gt_right">0</td>
<td headers="20211003  13 months" class="gt_row gt_right">0</td>
<td headers="20211003  24 months" class="gt_row gt_right">0</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="11" class="gt_group_heading" scope="colgroup" id="20211210">20211210</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="20211210  genotype2" class="gt_row gt_center">+/+</td>
<td headers="20211210  6 weeks" class="gt_row gt_right">0</td>
<td headers="20211210  8 weeks" class="gt_row gt_right">0</td>
<td headers="20211210  10 weeks" class="gt_row gt_right">0</td>
<td headers="20211210  12 weeks" class="gt_row gt_right">0</td>
<td headers="20211210  4 months" class="gt_row gt_right">0</td>
<td headers="20211210  6 months" class="gt_row gt_right">16</td>
<td headers="20211210  8 months" class="gt_row gt_right">0</td>
<td headers="20211210  10 months" class="gt_row gt_right">0</td>
<td headers="20211210  13 months" class="gt_row gt_right">0</td>
<td headers="20211210  24 months" class="gt_row gt_right">0</td></tr>
    <tr><td headers="20211210  genotype2" class="gt_row gt_center">+/-</td>
<td headers="20211210  6 weeks" class="gt_row gt_right">0</td>
<td headers="20211210  8 weeks" class="gt_row gt_right">0</td>
<td headers="20211210  10 weeks" class="gt_row gt_right">0</td>
<td headers="20211210  12 weeks" class="gt_row gt_right">0</td>
<td headers="20211210  4 months" class="gt_row gt_right">0</td>
<td headers="20211210  6 months" class="gt_row gt_right">10</td>
<td headers="20211210  8 months" class="gt_row gt_right">0</td>
<td headers="20211210  10 months" class="gt_row gt_right">0</td>
<td headers="20211210  13 months" class="gt_row gt_right">0</td>
<td headers="20211210  24 months" class="gt_row gt_right">0</td></tr>
    <tr><td headers="20211210  genotype2" class="gt_row gt_center">-/-</td>
<td headers="20211210  6 weeks" class="gt_row gt_right">0</td>
<td headers="20211210  8 weeks" class="gt_row gt_right">0</td>
<td headers="20211210  10 weeks" class="gt_row gt_right">0</td>
<td headers="20211210  12 weeks" class="gt_row gt_right">0</td>
<td headers="20211210  4 months" class="gt_row gt_right">0</td>
<td headers="20211210  6 months" class="gt_row gt_right">9</td>
<td headers="20211210  8 months" class="gt_row gt_right">0</td>
<td headers="20211210  10 months" class="gt_row gt_right">0</td>
<td headers="20211210  13 months" class="gt_row gt_right">0</td>
<td headers="20211210  24 months" class="gt_row gt_right">0</td></tr>
  </tbody>
  
  
</table>
</div>

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

<div id="tppoljttfq" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#tppoljttfq table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#tppoljttfq thead, #tppoljttfq tbody, #tppoljttfq tfoot, #tppoljttfq tr, #tppoljttfq td, #tppoljttfq th {
  border-style: none;
}

#tppoljttfq p {
  margin: 0;
  padding: 0;
}

#tppoljttfq .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#tppoljttfq .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#tppoljttfq .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#tppoljttfq .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#tppoljttfq .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#tppoljttfq .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#tppoljttfq .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#tppoljttfq .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#tppoljttfq .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#tppoljttfq .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#tppoljttfq .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#tppoljttfq .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#tppoljttfq .gt_spanner_row {
  border-bottom-style: hidden;
}

#tppoljttfq .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#tppoljttfq .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#tppoljttfq .gt_from_md > :first-child {
  margin-top: 0;
}

#tppoljttfq .gt_from_md > :last-child {
  margin-bottom: 0;
}

#tppoljttfq .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#tppoljttfq .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#tppoljttfq .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#tppoljttfq .gt_row_group_first td {
  border-top-width: 2px;
}

#tppoljttfq .gt_row_group_first th {
  border-top-width: 2px;
}

#tppoljttfq .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#tppoljttfq .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#tppoljttfq .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#tppoljttfq .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#tppoljttfq .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#tppoljttfq .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#tppoljttfq .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#tppoljttfq .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#tppoljttfq .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#tppoljttfq .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#tppoljttfq .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#tppoljttfq .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#tppoljttfq .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#tppoljttfq .gt_left {
  text-align: left;
}

#tppoljttfq .gt_center {
  text-align: center;
}

#tppoljttfq .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#tppoljttfq .gt_font_normal {
  font-weight: normal;
}

#tppoljttfq .gt_font_bold {
  font-weight: bold;
}

#tppoljttfq .gt_font_italic {
  font-style: italic;
}

#tppoljttfq .gt_super {
  font-size: 65%;
}

#tppoljttfq .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#tppoljttfq .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#tppoljttfq .gt_indent_1 {
  text-indent: 5px;
}

#tppoljttfq .gt_indent_2 {
  text-indent: 10px;
}

#tppoljttfq .gt_indent_3 {
  text-indent: 15px;
}

#tppoljttfq .gt_indent_4 {
  text-indent: 20px;
}

#tppoljttfq .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_heading">
      <td colspan="11" class="gt_heading gt_title gt_font_normal" style>Updated jag2b fish counts by timepoint and sex.</td>
    </tr>
    <tr class="gt_heading">
      <td colspan="11" class="gt_heading gt_subtitle gt_font_normal gt_bottom_border" style>Females removed for timepoints 10-, 13-, 24-months</td>
    </tr>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="sex">sex</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="6 weeks">6 weeks</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="8 weeks">8 weeks</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="10 weeks">10 weeks</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="12 weeks">12 weeks</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="4 months">4 months</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="6 months">6 months</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="8 months">8 months</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="10 months">10 months</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="13 months">13 months</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="24 months">24 months</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="sex" class="gt_row gt_left">F</td>
<td headers="6 weeks" class="gt_row gt_right">8</td>
<td headers="8 weeks" class="gt_row gt_right">12</td>
<td headers="10 weeks" class="gt_row gt_right">11</td>
<td headers="12 weeks" class="gt_row gt_right">4</td>
<td headers="4 months" class="gt_row gt_right">0</td>
<td headers="6 months" class="gt_row gt_right">0</td>
<td headers="8 months" class="gt_row gt_right">0</td>
<td headers="10 months" class="gt_row gt_right">0</td>
<td headers="13 months" class="gt_row gt_right">0</td>
<td headers="24 months" class="gt_row gt_right">0</td></tr>
    <tr><td headers="sex" class="gt_row gt_left">M</td>
<td headers="6 weeks" class="gt_row gt_right">27</td>
<td headers="8 weeks" class="gt_row gt_right">12</td>
<td headers="10 weeks" class="gt_row gt_right">12</td>
<td headers="12 weeks" class="gt_row gt_right">5</td>
<td headers="4 months" class="gt_row gt_right">50</td>
<td headers="6 months" class="gt_row gt_right">35</td>
<td headers="8 months" class="gt_row gt_right">16</td>
<td headers="10 months" class="gt_row gt_right">16</td>
<td headers="13 months" class="gt_row gt_right">18</td>
<td headers="24 months" class="gt_row gt_right">19</td></tr>
  </tbody>
  
  
</table>
</div>

``` r
tab2[age %in% c("6wpf", "8wpf", "10wpf", "12wpf"), sex:="unknown"]

dcast(data=tab2, sex ~ age2, fun.aggregate=length) %>% 
  as_tibble() %>%
  gt() %>%
  tab_header(title="Updated jag2b fish sex labels.",
             subtitle="Males and females re-labeled as \'unknown\' for ages 6- through 12-weeks.")
```

<div id="cyzrjrcsgc" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#cyzrjrcsgc table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#cyzrjrcsgc thead, #cyzrjrcsgc tbody, #cyzrjrcsgc tfoot, #cyzrjrcsgc tr, #cyzrjrcsgc td, #cyzrjrcsgc th {
  border-style: none;
}

#cyzrjrcsgc p {
  margin: 0;
  padding: 0;
}

#cyzrjrcsgc .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#cyzrjrcsgc .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#cyzrjrcsgc .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#cyzrjrcsgc .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#cyzrjrcsgc .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#cyzrjrcsgc .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#cyzrjrcsgc .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#cyzrjrcsgc .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#cyzrjrcsgc .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#cyzrjrcsgc .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#cyzrjrcsgc .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#cyzrjrcsgc .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#cyzrjrcsgc .gt_spanner_row {
  border-bottom-style: hidden;
}

#cyzrjrcsgc .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#cyzrjrcsgc .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#cyzrjrcsgc .gt_from_md > :first-child {
  margin-top: 0;
}

#cyzrjrcsgc .gt_from_md > :last-child {
  margin-bottom: 0;
}

#cyzrjrcsgc .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#cyzrjrcsgc .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#cyzrjrcsgc .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#cyzrjrcsgc .gt_row_group_first td {
  border-top-width: 2px;
}

#cyzrjrcsgc .gt_row_group_first th {
  border-top-width: 2px;
}

#cyzrjrcsgc .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#cyzrjrcsgc .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#cyzrjrcsgc .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#cyzrjrcsgc .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#cyzrjrcsgc .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#cyzrjrcsgc .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#cyzrjrcsgc .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#cyzrjrcsgc .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#cyzrjrcsgc .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#cyzrjrcsgc .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#cyzrjrcsgc .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#cyzrjrcsgc .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#cyzrjrcsgc .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#cyzrjrcsgc .gt_left {
  text-align: left;
}

#cyzrjrcsgc .gt_center {
  text-align: center;
}

#cyzrjrcsgc .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#cyzrjrcsgc .gt_font_normal {
  font-weight: normal;
}

#cyzrjrcsgc .gt_font_bold {
  font-weight: bold;
}

#cyzrjrcsgc .gt_font_italic {
  font-style: italic;
}

#cyzrjrcsgc .gt_super {
  font-size: 65%;
}

#cyzrjrcsgc .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#cyzrjrcsgc .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#cyzrjrcsgc .gt_indent_1 {
  text-indent: 5px;
}

#cyzrjrcsgc .gt_indent_2 {
  text-indent: 10px;
}

#cyzrjrcsgc .gt_indent_3 {
  text-indent: 15px;
}

#cyzrjrcsgc .gt_indent_4 {
  text-indent: 20px;
}

#cyzrjrcsgc .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_heading">
      <td colspan="11" class="gt_heading gt_title gt_font_normal" style>Updated jag2b fish sex labels.</td>
    </tr>
    <tr class="gt_heading">
      <td colspan="11" class="gt_heading gt_subtitle gt_font_normal gt_bottom_border" style>Males and females re-labeled as 'unknown' for ages 6- through 12-weeks.</td>
    </tr>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="sex">sex</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="6 weeks">6 weeks</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="8 weeks">8 weeks</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="10 weeks">10 weeks</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="12 weeks">12 weeks</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="4 months">4 months</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="6 months">6 months</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="8 months">8 months</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="10 months">10 months</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="13 months">13 months</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="24 months">24 months</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="sex" class="gt_row gt_left">M</td>
<td headers="6 weeks" class="gt_row gt_right">0</td>
<td headers="8 weeks" class="gt_row gt_right">0</td>
<td headers="10 weeks" class="gt_row gt_right">0</td>
<td headers="12 weeks" class="gt_row gt_right">0</td>
<td headers="4 months" class="gt_row gt_right">50</td>
<td headers="6 months" class="gt_row gt_right">35</td>
<td headers="8 months" class="gt_row gt_right">16</td>
<td headers="10 months" class="gt_row gt_right">16</td>
<td headers="13 months" class="gt_row gt_right">18</td>
<td headers="24 months" class="gt_row gt_right">19</td></tr>
    <tr><td headers="sex" class="gt_row gt_left">unknown</td>
<td headers="6 weeks" class="gt_row gt_right">35</td>
<td headers="8 weeks" class="gt_row gt_right">24</td>
<td headers="10 weeks" class="gt_row gt_right">23</td>
<td headers="12 weeks" class="gt_row gt_right">9</td>
<td headers="4 months" class="gt_row gt_right">0</td>
<td headers="6 months" class="gt_row gt_right">0</td>
<td headers="8 months" class="gt_row gt_right">0</td>
<td headers="10 months" class="gt_row gt_right">0</td>
<td headers="13 months" class="gt_row gt_right">0</td>
<td headers="24 months" class="gt_row gt_right">0</td></tr>
  </tbody>
  
  
</table>
</div>

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

<div id="whftmfafex" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#whftmfafex table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#whftmfafex thead, #whftmfafex tbody, #whftmfafex tfoot, #whftmfafex tr, #whftmfafex td, #whftmfafex th {
  border-style: none;
}

#whftmfafex p {
  margin: 0;
  padding: 0;
}

#whftmfafex .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#whftmfafex .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#whftmfafex .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#whftmfafex .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#whftmfafex .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#whftmfafex .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#whftmfafex .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#whftmfafex .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#whftmfafex .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#whftmfafex .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#whftmfafex .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#whftmfafex .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#whftmfafex .gt_spanner_row {
  border-bottom-style: hidden;
}

#whftmfafex .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#whftmfafex .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#whftmfafex .gt_from_md > :first-child {
  margin-top: 0;
}

#whftmfafex .gt_from_md > :last-child {
  margin-bottom: 0;
}

#whftmfafex .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#whftmfafex .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#whftmfafex .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#whftmfafex .gt_row_group_first td {
  border-top-width: 2px;
}

#whftmfafex .gt_row_group_first th {
  border-top-width: 2px;
}

#whftmfafex .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#whftmfafex .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#whftmfafex .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#whftmfafex .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#whftmfafex .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#whftmfafex .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#whftmfafex .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#whftmfafex .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#whftmfafex .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#whftmfafex .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#whftmfafex .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#whftmfafex .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#whftmfafex .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#whftmfafex .gt_left {
  text-align: left;
}

#whftmfafex .gt_center {
  text-align: center;
}

#whftmfafex .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#whftmfafex .gt_font_normal {
  font-weight: normal;
}

#whftmfafex .gt_font_bold {
  font-weight: bold;
}

#whftmfafex .gt_font_italic {
  font-style: italic;
}

#whftmfafex .gt_super {
  font-size: 65%;
}

#whftmfafex .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#whftmfafex .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#whftmfafex .gt_indent_1 {
  text-indent: 5px;
}

#whftmfafex .gt_indent_2 {
  text-indent: 10px;
}

#whftmfafex .gt_indent_3 {
  text-indent: 15px;
}

#whftmfafex .gt_indent_4 {
  text-indent: 20px;
}

#whftmfafex .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_heading">
      <td colspan="4" class="gt_heading gt_title gt_font_normal gt_bottom_border" style>Pairwise t-test results within each timepoint. FDR-adj p-value &lt; 0.05</td>
    </tr>
    
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="age">age</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="comparison_variable">comparison_variable</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="genotype_pair">genotype_pair</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="padj_format">padj_format</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="age" class="gt_row gt_center">6 weeks</td>
<td headers="comparison_variable" class="gt_row gt_left">length_cm</td>
<td headers="genotype_pair" class="gt_row gt_center">WT-MUT</td>
<td headers="padj_format" class="gt_row gt_left">6.6e-07</td></tr>
    <tr><td headers="age" class="gt_row gt_center">6 weeks</td>
<td headers="comparison_variable" class="gt_row gt_left">length_cm</td>
<td headers="genotype_pair" class="gt_row gt_center">HET-MUT</td>
<td headers="padj_format" class="gt_row gt_left">7e-07</td></tr>
    <tr><td headers="age" class="gt_row gt_center">8 weeks</td>
<td headers="comparison_variable" class="gt_row gt_left">length_cm</td>
<td headers="genotype_pair" class="gt_row gt_center">WT-MUT</td>
<td headers="padj_format" class="gt_row gt_left">0.00098</td></tr>
    <tr><td headers="age" class="gt_row gt_center">8 weeks</td>
<td headers="comparison_variable" class="gt_row gt_left">length_cm</td>
<td headers="genotype_pair" class="gt_row gt_center">HET-MUT</td>
<td headers="padj_format" class="gt_row gt_left">0.00023</td></tr>
    <tr><td headers="age" class="gt_row gt_center">10 weeks</td>
<td headers="comparison_variable" class="gt_row gt_left">length_cm</td>
<td headers="genotype_pair" class="gt_row gt_center">WT-MUT</td>
<td headers="padj_format" class="gt_row gt_left">8.1e-05</td></tr>
    <tr><td headers="age" class="gt_row gt_center">10 weeks</td>
<td headers="comparison_variable" class="gt_row gt_left">length_cm</td>
<td headers="genotype_pair" class="gt_row gt_center">HET-MUT</td>
<td headers="padj_format" class="gt_row gt_left">0.00019</td></tr>
    <tr><td headers="age" class="gt_row gt_center">4 months</td>
<td headers="comparison_variable" class="gt_row gt_left">length_cm</td>
<td headers="genotype_pair" class="gt_row gt_center">WT-MUT</td>
<td headers="padj_format" class="gt_row gt_left">0.031</td></tr>
    <tr><td headers="age" class="gt_row gt_center">4 months</td>
<td headers="comparison_variable" class="gt_row gt_left">length_cm</td>
<td headers="genotype_pair" class="gt_row gt_center">HET-MUT</td>
<td headers="padj_format" class="gt_row gt_left">0.014</td></tr>
    <tr><td headers="age" class="gt_row gt_center">6 months</td>
<td headers="comparison_variable" class="gt_row gt_left">length_cm</td>
<td headers="genotype_pair" class="gt_row gt_center">WT-MUT</td>
<td headers="padj_format" class="gt_row gt_left">0.0072</td></tr>
    <tr><td headers="age" class="gt_row gt_center">10 months</td>
<td headers="comparison_variable" class="gt_row gt_left">length_cm</td>
<td headers="genotype_pair" class="gt_row gt_center">HET-MUT</td>
<td headers="padj_format" class="gt_row gt_left">0.0031</td></tr>
    <tr><td headers="age" class="gt_row gt_center">13 months</td>
<td headers="comparison_variable" class="gt_row gt_left">length_cm</td>
<td headers="genotype_pair" class="gt_row gt_center">WT-MUT</td>
<td headers="padj_format" class="gt_row gt_left">0.017</td></tr>
    <tr><td headers="age" class="gt_row gt_center">6 weeks</td>
<td headers="comparison_variable" class="gt_row gt_left">weight_gr</td>
<td headers="genotype_pair" class="gt_row gt_center">WT-MUT</td>
<td headers="padj_format" class="gt_row gt_left">7e-07</td></tr>
    <tr><td headers="age" class="gt_row gt_center">6 weeks</td>
<td headers="comparison_variable" class="gt_row gt_left">weight_gr</td>
<td headers="genotype_pair" class="gt_row gt_center">HET-MUT</td>
<td headers="padj_format" class="gt_row gt_left">2.2e-08</td></tr>
    <tr><td headers="age" class="gt_row gt_center">8 weeks</td>
<td headers="comparison_variable" class="gt_row gt_left">weight_gr</td>
<td headers="genotype_pair" class="gt_row gt_center">WT-MUT</td>
<td headers="padj_format" class="gt_row gt_left">0.00019</td></tr>
    <tr><td headers="age" class="gt_row gt_center">8 weeks</td>
<td headers="comparison_variable" class="gt_row gt_left">weight_gr</td>
<td headers="genotype_pair" class="gt_row gt_center">HET-MUT</td>
<td headers="padj_format" class="gt_row gt_left">6.5e-06</td></tr>
    <tr><td headers="age" class="gt_row gt_center">10 weeks</td>
<td headers="comparison_variable" class="gt_row gt_left">weight_gr</td>
<td headers="genotype_pair" class="gt_row gt_center">WT-MUT</td>
<td headers="padj_format" class="gt_row gt_left">0.00011</td></tr>
    <tr><td headers="age" class="gt_row gt_center">10 weeks</td>
<td headers="comparison_variable" class="gt_row gt_left">weight_gr</td>
<td headers="genotype_pair" class="gt_row gt_center">HET-MUT</td>
<td headers="padj_format" class="gt_row gt_left">0.00011</td></tr>
    <tr><td headers="age" class="gt_row gt_center">4 months</td>
<td headers="comparison_variable" class="gt_row gt_left">weight_gr</td>
<td headers="genotype_pair" class="gt_row gt_center">HET-MUT</td>
<td headers="padj_format" class="gt_row gt_left">0.025</td></tr>
    <tr><td headers="age" class="gt_row gt_center">10 months</td>
<td headers="comparison_variable" class="gt_row gt_left">weight_gr</td>
<td headers="genotype_pair" class="gt_row gt_center">HET-MUT</td>
<td headers="padj_format" class="gt_row gt_left">0.00041</td></tr>
    <tr><td headers="age" class="gt_row gt_center">13 months</td>
<td headers="comparison_variable" class="gt_row gt_left">weight_gr</td>
<td headers="genotype_pair" class="gt_row gt_center">WT-MUT</td>
<td headers="padj_format" class="gt_row gt_left">0.0016</td></tr>
    <tr><td headers="age" class="gt_row gt_center">13 months</td>
<td headers="comparison_variable" class="gt_row gt_left">weight_gr</td>
<td headers="genotype_pair" class="gt_row gt_center">HET-MUT</td>
<td headers="padj_format" class="gt_row gt_left">0.017</td></tr>
  </tbody>
  
  
</table>
</div>

``` r
  stats_tab %>% as_tibble() %>%
  gt() %>%
  tab_header(title="Pairwise t-test results with each timepoint. All results.")
```

<div id="voxzbdysdb" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#voxzbdysdb table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#voxzbdysdb thead, #voxzbdysdb tbody, #voxzbdysdb tfoot, #voxzbdysdb tr, #voxzbdysdb td, #voxzbdysdb th {
  border-style: none;
}

#voxzbdysdb p {
  margin: 0;
  padding: 0;
}

#voxzbdysdb .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#voxzbdysdb .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#voxzbdysdb .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#voxzbdysdb .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#voxzbdysdb .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#voxzbdysdb .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#voxzbdysdb .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#voxzbdysdb .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#voxzbdysdb .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#voxzbdysdb .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#voxzbdysdb .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#voxzbdysdb .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#voxzbdysdb .gt_spanner_row {
  border-bottom-style: hidden;
}

#voxzbdysdb .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#voxzbdysdb .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#voxzbdysdb .gt_from_md > :first-child {
  margin-top: 0;
}

#voxzbdysdb .gt_from_md > :last-child {
  margin-bottom: 0;
}

#voxzbdysdb .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#voxzbdysdb .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#voxzbdysdb .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#voxzbdysdb .gt_row_group_first td {
  border-top-width: 2px;
}

#voxzbdysdb .gt_row_group_first th {
  border-top-width: 2px;
}

#voxzbdysdb .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#voxzbdysdb .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#voxzbdysdb .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#voxzbdysdb .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#voxzbdysdb .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#voxzbdysdb .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#voxzbdysdb .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#voxzbdysdb .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#voxzbdysdb .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#voxzbdysdb .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#voxzbdysdb .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#voxzbdysdb .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#voxzbdysdb .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#voxzbdysdb .gt_left {
  text-align: left;
}

#voxzbdysdb .gt_center {
  text-align: center;
}

#voxzbdysdb .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#voxzbdysdb .gt_font_normal {
  font-weight: normal;
}

#voxzbdysdb .gt_font_bold {
  font-weight: bold;
}

#voxzbdysdb .gt_font_italic {
  font-style: italic;
}

#voxzbdysdb .gt_super {
  font-size: 65%;
}

#voxzbdysdb .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#voxzbdysdb .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#voxzbdysdb .gt_indent_1 {
  text-indent: 5px;
}

#voxzbdysdb .gt_indent_2 {
  text-indent: 10px;
}

#voxzbdysdb .gt_indent_3 {
  text-indent: 15px;
}

#voxzbdysdb .gt_indent_4 {
  text-indent: 20px;
}

#voxzbdysdb .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_heading">
      <td colspan="15" class="gt_heading gt_title gt_font_normal gt_bottom_border" style>Pairwise t-test results with each timepoint. All results.</td>
    </tr>
    
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="group1">group1</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="group2">group2</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="age">age</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="comparison_variable">comparison_variable</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="effect_size">effect_size</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="pvalue">pvalue</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="conf_lower">conf_lower</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="conf_upper">conf_upper</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="sample_size_group1">sample_size_group1</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="sample_size_group2">sample_size_group2</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="t_statistic">t_statistic</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="df">df</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="padj">padj</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="genotype_pair">genotype_pair</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="padj_format">padj_format</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="group1" class="gt_row gt_left">WT</td>
<td headers="group2" class="gt_row gt_left">HET</td>
<td headers="age" class="gt_row gt_left">6 weeks</td>
<td headers="comparison_variable" class="gt_row gt_left">weight_gr</td>
<td headers="effect_size" class="gt_row gt_right">0.01077692</td>
<td headers="pvalue" class="gt_row gt_right">4.323546e-01</td>
<td headers="conf_lower" class="gt_row gt_right">-0.0174340506</td>
<td headers="conf_upper" class="gt_row gt_right">0.03898790</td>
<td headers="sample_size_group1" class="gt_row gt_right">10</td>
<td headers="sample_size_group2" class="gt_row gt_right">13</td>
<td headers="t_statistic" class="gt_row gt_right">0.80344905</td>
<td headers="df" class="gt_row gt_right">17.731454</td>
<td headers="padj" class="gt_row gt_right">5.098759e-01</td>
<td headers="genotype_pair" class="gt_row gt_center">WT-HET</td>
<td headers="padj_format" class="gt_row gt_left">0.51</td></tr>
    <tr><td headers="group1" class="gt_row gt_left">WT</td>
<td headers="group2" class="gt_row gt_left">MUT</td>
<td headers="age" class="gt_row gt_left">6 weeks</td>
<td headers="comparison_variable" class="gt_row gt_left">weight_gr</td>
<td headers="effect_size" class="gt_row gt_right">0.12278333</td>
<td headers="pvalue" class="gt_row gt_right">4.965021e-08</td>
<td headers="conf_lower" class="gt_row gt_right">0.0954099997</td>
<td headers="conf_upper" class="gt_row gt_right">0.15015667</td>
<td headers="sample_size_group1" class="gt_row gt_right">10</td>
<td headers="sample_size_group2" class="gt_row gt_right">12</td>
<td headers="t_statistic" class="gt_row gt_right">9.49844560</td>
<td headers="df" class="gt_row gt_right">16.218665</td>
<td headers="padj" class="gt_row gt_right">6.951029e-07</td>
<td headers="genotype_pair" class="gt_row gt_center">WT-MUT</td>
<td headers="padj_format" class="gt_row gt_left">7e-07</td></tr>
    <tr><td headers="group1" class="gt_row gt_left">HET</td>
<td headers="group2" class="gt_row gt_left">MUT</td>
<td headers="age" class="gt_row gt_left">6 weeks</td>
<td headers="comparison_variable" class="gt_row gt_left">weight_gr</td>
<td headers="effect_size" class="gt_row gt_right">0.11200641</td>
<td headers="pvalue" class="gt_row gt_right">3.922903e-10</td>
<td headers="conf_lower" class="gt_row gt_right">0.0896874483</td>
<td headers="conf_upper" class="gt_row gt_right">0.13432537</td>
<td headers="sample_size_group1" class="gt_row gt_right">13</td>
<td headers="sample_size_group2" class="gt_row gt_right">12</td>
<td headers="t_statistic" class="gt_row gt_right">10.38399837</td>
<td headers="df" class="gt_row gt_right">22.897953</td>
<td headers="padj" class="gt_row gt_right">2.196825e-08</td>
<td headers="genotype_pair" class="gt_row gt_center">HET-MUT</td>
<td headers="padj_format" class="gt_row gt_left">2.2e-08</td></tr>
    <tr><td headers="group1" class="gt_row gt_left">WT</td>
<td headers="group2" class="gt_row gt_left">HET</td>
<td headers="age" class="gt_row gt_left">6 weeks</td>
<td headers="comparison_variable" class="gt_row gt_left">length_cm</td>
<td headers="effect_size" class="gt_row gt_right">0.05346154</td>
<td headers="pvalue" class="gt_row gt_right">4.557691e-01</td>
<td headers="conf_lower" class="gt_row gt_right">-0.0945445594</td>
<td headers="conf_upper" class="gt_row gt_right">0.20146764</td>
<td headers="sample_size_group1" class="gt_row gt_right">10</td>
<td headers="sample_size_group2" class="gt_row gt_right">13</td>
<td headers="t_statistic" class="gt_row gt_right">0.76377988</td>
<td headers="df" class="gt_row gt_right">16.520144</td>
<td headers="padj" class="gt_row gt_right">5.208789e-01</td>
<td headers="genotype_pair" class="gt_row gt_center">WT-HET</td>
<td headers="padj_format" class="gt_row gt_left">0.52</td></tr>
    <tr><td headers="group1" class="gt_row gt_left">WT</td>
<td headers="group2" class="gt_row gt_left">MUT</td>
<td headers="age" class="gt_row gt_left">6 weeks</td>
<td headers="comparison_variable" class="gt_row gt_left">length_cm</td>
<td headers="effect_size" class="gt_row gt_right">0.84958333</td>
<td headers="pvalue" class="gt_row gt_right">2.342617e-08</td>
<td headers="conf_lower" class="gt_row gt_right">0.6515672544</td>
<td headers="conf_upper" class="gt_row gt_right">1.04759941</td>
<td headers="sample_size_group1" class="gt_row gt_right">10</td>
<td headers="sample_size_group2" class="gt_row gt_right">12</td>
<td headers="t_statistic" class="gt_row gt_right">8.96310167</td>
<td headers="df" class="gt_row gt_right">19.546689</td>
<td headers="padj" class="gt_row gt_right">6.559328e-07</td>
<td headers="genotype_pair" class="gt_row gt_center">WT-MUT</td>
<td headers="padj_format" class="gt_row gt_left">6.6e-07</td></tr>
    <tr><td headers="group1" class="gt_row gt_left">HET</td>
<td headers="group2" class="gt_row gt_left">MUT</td>
<td headers="age" class="gt_row gt_left">6 weeks</td>
<td headers="comparison_variable" class="gt_row gt_left">length_cm</td>
<td headers="effect_size" class="gt_row gt_right">0.79612179</td>
<td headers="pvalue" class="gt_row gt_right">4.402406e-08</td>
<td headers="conf_lower" class="gt_row gt_right">0.6173549529</td>
<td headers="conf_upper" class="gt_row gt_right">0.97488864</td>
<td headers="sample_size_group1" class="gt_row gt_right">13</td>
<td headers="sample_size_group2" class="gt_row gt_right">12</td>
<td headers="t_statistic" class="gt_row gt_right">9.40904388</td>
<td headers="df" class="gt_row gt_right">16.693170</td>
<td headers="padj" class="gt_row gt_right">6.951029e-07</td>
<td headers="genotype_pair" class="gt_row gt_center">HET-MUT</td>
<td headers="padj_format" class="gt_row gt_left">7e-07</td></tr>
    <tr><td headers="group1" class="gt_row gt_left">WT</td>
<td headers="group2" class="gt_row gt_left">HET</td>
<td headers="age" class="gt_row gt_left">8 weeks</td>
<td headers="comparison_variable" class="gt_row gt_left">weight_gr</td>
<td headers="effect_size" class="gt_row gt_right">0.01012500</td>
<td headers="pvalue" class="gt_row gt_right">7.593169e-01</td>
<td headers="conf_lower" class="gt_row gt_right">-0.0603703185</td>
<td headers="conf_upper" class="gt_row gt_right">0.08062032</td>
<td headers="sample_size_group1" class="gt_row gt_right">8</td>
<td headers="sample_size_group2" class="gt_row gt_right">8</td>
<td headers="t_statistic" class="gt_row gt_right">0.31358926</td>
<td headers="df" class="gt_row gt_right">11.778521</td>
<td headers="padj" class="gt_row gt_right">7.874397e-01</td>
<td headers="genotype_pair" class="gt_row gt_center">WT-HET</td>
<td headers="padj_format" class="gt_row gt_left">0.79</td></tr>
    <tr><td headers="group1" class="gt_row gt_left">WT</td>
<td headers="group2" class="gt_row gt_left">MUT</td>
<td headers="age" class="gt_row gt_left">8 weeks</td>
<td headers="comparison_variable" class="gt_row gt_left">weight_gr</td>
<td headers="effect_size" class="gt_row gt_right">0.20987500</td>
<td headers="pvalue" class="gt_row gt_right">3.385485e-05</td>
<td headers="conf_lower" class="gt_row gt_right">0.1408659109</td>
<td headers="conf_upper" class="gt_row gt_right">0.27888409</td>
<td headers="sample_size_group1" class="gt_row gt_right">8</td>
<td headers="sample_size_group2" class="gt_row gt_right">8</td>
<td headers="t_statistic" class="gt_row gt_right">6.69258251</td>
<td headers="df" class="gt_row gt_right">11.016175</td>
<td headers="padj" class="gt_row gt_right">1.895872e-04</td>
<td headers="genotype_pair" class="gt_row gt_center">WT-MUT</td>
<td headers="padj_format" class="gt_row gt_left">0.00019</td></tr>
    <tr><td headers="group1" class="gt_row gt_left">HET</td>
<td headers="group2" class="gt_row gt_left">MUT</td>
<td headers="age" class="gt_row gt_left">8 weeks</td>
<td headers="comparison_variable" class="gt_row gt_left">weight_gr</td>
<td headers="effect_size" class="gt_row gt_right">0.19975000</td>
<td headers="pvalue" class="gt_row gt_right">5.806347e-07</td>
<td headers="conf_lower" class="gt_row gt_right">0.1502844146</td>
<td headers="conf_upper" class="gt_row gt_right">0.24921559</td>
<td headers="sample_size_group1" class="gt_row gt_right">8</td>
<td headers="sample_size_group2" class="gt_row gt_right">8</td>
<td headers="t_statistic" class="gt_row gt_right">8.67107293</td>
<td headers="df" class="gt_row gt_right">13.828652</td>
<td headers="padj" class="gt_row gt_right">6.503109e-06</td>
<td headers="genotype_pair" class="gt_row gt_center">HET-MUT</td>
<td headers="padj_format" class="gt_row gt_left">6.5e-06</td></tr>
    <tr><td headers="group1" class="gt_row gt_left">WT</td>
<td headers="group2" class="gt_row gt_left">HET</td>
<td headers="age" class="gt_row gt_left">8 weeks</td>
<td headers="comparison_variable" class="gt_row gt_left">length_cm</td>
<td headers="effect_size" class="gt_row gt_right">-0.11950000</td>
<td headers="pvalue" class="gt_row gt_right">5.061317e-01</td>
<td headers="conf_lower" class="gt_row gt_right">-0.4962426554</td>
<td headers="conf_upper" class="gt_row gt_right">0.25724266</td>
<td headers="sample_size_group1" class="gt_row gt_right">8</td>
<td headers="sample_size_group2" class="gt_row gt_right">8</td>
<td headers="t_statistic" class="gt_row gt_right">-0.68318612</td>
<td headers="df" class="gt_row gt_right">13.398681</td>
<td headers="padj" class="gt_row gt_right">5.557524e-01</td>
<td headers="genotype_pair" class="gt_row gt_center">WT-HET</td>
<td headers="padj_format" class="gt_row gt_left">0.56</td></tr>
    <tr><td headers="group1" class="gt_row gt_left">WT</td>
<td headers="group2" class="gt_row gt_left">MUT</td>
<td headers="age" class="gt_row gt_left">8 weeks</td>
<td headers="comparison_variable" class="gt_row gt_left">length_cm</td>
<td headers="effect_size" class="gt_row gt_right">0.99600000</td>
<td headers="pvalue" class="gt_row gt_right">2.263872e-04</td>
<td headers="conf_lower" class="gt_row gt_right">0.5624697422</td>
<td headers="conf_upper" class="gt_row gt_right">1.42953026</td>
<td headers="sample_size_group1" class="gt_row gt_right">8</td>
<td headers="sample_size_group2" class="gt_row gt_right">8</td>
<td headers="t_statistic" class="gt_row gt_right">4.93131059</td>
<td headers="df" class="gt_row gt_right">13.884743</td>
<td headers="padj" class="gt_row gt_right">9.752062e-04</td>
<td headers="genotype_pair" class="gt_row gt_center">WT-MUT</td>
<td headers="padj_format" class="gt_row gt_left">0.00098</td></tr>
    <tr><td headers="group1" class="gt_row gt_left">HET</td>
<td headers="group2" class="gt_row gt_left">MUT</td>
<td headers="age" class="gt_row gt_left">8 weeks</td>
<td headers="comparison_variable" class="gt_row gt_left">length_cm</td>
<td headers="effect_size" class="gt_row gt_right">1.11550000</td>
<td headers="pvalue" class="gt_row gt_right">4.484663e-05</td>
<td headers="conf_lower" class="gt_row gt_right">0.7148911472</td>
<td headers="conf_upper" class="gt_row gt_right">1.51610885</td>
<td headers="sample_size_group1" class="gt_row gt_right">8</td>
<td headers="sample_size_group2" class="gt_row gt_right">8</td>
<td headers="t_statistic" class="gt_row gt_right">6.02206054</td>
<td headers="df" class="gt_row gt_right">12.863633</td>
<td headers="padj" class="gt_row gt_right">2.283101e-04</td>
<td headers="genotype_pair" class="gt_row gt_center">HET-MUT</td>
<td headers="padj_format" class="gt_row gt_left">0.00023</td></tr>
    <tr><td headers="group1" class="gt_row gt_left">WT</td>
<td headers="group2" class="gt_row gt_left">HET</td>
<td headers="age" class="gt_row gt_left">10 weeks</td>
<td headers="comparison_variable" class="gt_row gt_left">weight_gr</td>
<td headers="effect_size" class="gt_row gt_right">-0.00150000</td>
<td headers="pvalue" class="gt_row gt_right">9.583739e-01</td>
<td headers="conf_lower" class="gt_row gt_right">-0.0620734358</td>
<td headers="conf_upper" class="gt_row gt_right">0.05907344</td>
<td headers="sample_size_group1" class="gt_row gt_right">8</td>
<td headers="sample_size_group2" class="gt_row gt_right">8</td>
<td headers="t_statistic" class="gt_row gt_right">-0.05314225</td>
<td headers="df" class="gt_row gt_right">13.915728</td>
<td headers="padj" class="gt_row gt_right">9.583739e-01</td>
<td headers="genotype_pair" class="gt_row gt_center">WT-HET</td>
<td headers="padj_format" class="gt_row gt_left">0.96</td></tr>
    <tr><td headers="group1" class="gt_row gt_left">WT</td>
<td headers="group2" class="gt_row gt_left">MUT</td>
<td headers="age" class="gt_row gt_left">10 weeks</td>
<td headers="comparison_variable" class="gt_row gt_left">weight_gr</td>
<td headers="effect_size" class="gt_row gt_right">0.19621429</td>
<td headers="pvalue" class="gt_row gt_right">1.333559e-05</td>
<td headers="conf_lower" class="gt_row gt_right">0.1343156943</td>
<td headers="conf_upper" class="gt_row gt_right">0.25811288</td>
<td headers="sample_size_group1" class="gt_row gt_right">8</td>
<td headers="sample_size_group2" class="gt_row gt_right">7</td>
<td headers="t_statistic" class="gt_row gt_right">6.87003379</td>
<td headers="df" class="gt_row gt_right">12.606143</td>
<td headers="padj" class="gt_row gt_right">1.066847e-04</td>
<td headers="genotype_pair" class="gt_row gt_center">WT-MUT</td>
<td headers="padj_format" class="gt_row gt_left">0.00011</td></tr>
    <tr><td headers="group1" class="gt_row gt_left">HET</td>
<td headers="group2" class="gt_row gt_left">MUT</td>
<td headers="age" class="gt_row gt_left">10 weeks</td>
<td headers="comparison_variable" class="gt_row gt_left">weight_gr</td>
<td headers="effect_size" class="gt_row gt_right">0.19771429</td>
<td headers="pvalue" class="gt_row gt_right">1.607865e-05</td>
<td headers="conf_lower" class="gt_row gt_right">0.1336466425</td>
<td headers="conf_upper" class="gt_row gt_right">0.26178193</td>
<td headers="sample_size_group1" class="gt_row gt_right">8</td>
<td headers="sample_size_group2" class="gt_row gt_right">7</td>
<td headers="t_statistic" class="gt_row gt_right">6.67358109</td>
<td headers="df" class="gt_row gt_right">12.874140</td>
<td headers="padj" class="gt_row gt_right">1.125505e-04</td>
<td headers="genotype_pair" class="gt_row gt_center">HET-MUT</td>
<td headers="padj_format" class="gt_row gt_left">0.00011</td></tr>
    <tr><td headers="group1" class="gt_row gt_left">WT</td>
<td headers="group2" class="gt_row gt_left">HET</td>
<td headers="age" class="gt_row gt_left">10 weeks</td>
<td headers="comparison_variable" class="gt_row gt_left">length_cm</td>
<td headers="effect_size" class="gt_row gt_right">0.08350000</td>
<td headers="pvalue" class="gt_row gt_right">3.510375e-01</td>
<td headers="conf_lower" class="gt_row gt_right">-0.1058374696</td>
<td headers="conf_upper" class="gt_row gt_right">0.27283747</td>
<td headers="sample_size_group1" class="gt_row gt_right">8</td>
<td headers="sample_size_group2" class="gt_row gt_right">8</td>
<td headers="t_statistic" class="gt_row gt_right">0.97573771</td>
<td headers="df" class="gt_row gt_right">10.549779</td>
<td headers="padj" class="gt_row gt_right">4.273500e-01</td>
<td headers="genotype_pair" class="gt_row gt_center">WT-HET</td>
<td headers="padj_format" class="gt_row gt_left">0.43</td></tr>
    <tr><td headers="group1" class="gt_row gt_left">WT</td>
<td headers="group2" class="gt_row gt_left">MUT</td>
<td headers="age" class="gt_row gt_left">10 weeks</td>
<td headers="comparison_variable" class="gt_row gt_left">length_cm</td>
<td headers="effect_size" class="gt_row gt_right">0.75191071</td>
<td headers="pvalue" class="gt_row gt_right">8.723514e-06</td>
<td headers="conf_lower" class="gt_row gt_right">0.5599991990</td>
<td headers="conf_upper" class="gt_row gt_right">0.94382223</td>
<td headers="sample_size_group1" class="gt_row gt_right">8</td>
<td headers="sample_size_group2" class="gt_row gt_right">7</td>
<td headers="t_statistic" class="gt_row gt_right">8.83750358</td>
<td headers="df" class="gt_row gt_right">9.174596</td>
<td headers="padj" class="gt_row gt_right">8.141946e-05</td>
<td headers="genotype_pair" class="gt_row gt_center">WT-MUT</td>
<td headers="padj_format" class="gt_row gt_left">8.1e-05</td></tr>
    <tr><td headers="group1" class="gt_row gt_left">HET</td>
<td headers="group2" class="gt_row gt_left">MUT</td>
<td headers="age" class="gt_row gt_left">10 weeks</td>
<td headers="comparison_variable" class="gt_row gt_left">length_cm</td>
<td headers="effect_size" class="gt_row gt_right">0.66841071</td>
<td headers="pvalue" class="gt_row gt_right">3.023609e-05</td>
<td headers="conf_lower" class="gt_row gt_right">0.4373599257</td>
<td headers="conf_upper" class="gt_row gt_right">0.89946150</td>
<td headers="sample_size_group1" class="gt_row gt_right">8</td>
<td headers="sample_size_group2" class="gt_row gt_right">7</td>
<td headers="t_statistic" class="gt_row gt_right">6.25285860</td>
<td headers="df" class="gt_row gt_right">12.937070</td>
<td headers="padj" class="gt_row gt_right">1.881357e-04</td>
<td headers="genotype_pair" class="gt_row gt_center">HET-MUT</td>
<td headers="padj_format" class="gt_row gt_left">0.00019</td></tr>
    <tr><td headers="group1" class="gt_row gt_left">WT</td>
<td headers="group2" class="gt_row gt_left">HET</td>
<td headers="age" class="gt_row gt_left">12 weeks</td>
<td headers="comparison_variable" class="gt_row gt_left">weight_gr</td>
<td headers="effect_size" class="gt_row gt_right">0.05400000</td>
<td headers="pvalue" class="gt_row gt_right">4.370365e-01</td>
<td headers="conf_lower" class="gt_row gt_right">-0.1339873597</td>
<td headers="conf_upper" class="gt_row gt_right">0.24198736</td>
<td headers="sample_size_group1" class="gt_row gt_right">3</td>
<td headers="sample_size_group2" class="gt_row gt_right">3</td>
<td headers="t_statistic" class="gt_row gt_right">0.88748806</td>
<td headers="df" class="gt_row gt_right">3.167037</td>
<td headers="padj" class="gt_row gt_right">5.098759e-01</td>
<td headers="genotype_pair" class="gt_row gt_center">WT-HET</td>
<td headers="padj_format" class="gt_row gt_left">0.51</td></tr>
    <tr><td headers="group1" class="gt_row gt_left">WT</td>
<td headers="group2" class="gt_row gt_left">MUT</td>
<td headers="age" class="gt_row gt_left">12 weeks</td>
<td headers="comparison_variable" class="gt_row gt_left">weight_gr</td>
<td headers="effect_size" class="gt_row gt_right">0.25833333</td>
<td headers="pvalue" class="gt_row gt_right">2.213631e-02</td>
<td headers="conf_lower" class="gt_row gt_right">0.0612446532</td>
<td headers="conf_upper" class="gt_row gt_right">0.45542201</td>
<td headers="sample_size_group1" class="gt_row gt_right">3</td>
<td headers="sample_size_group2" class="gt_row gt_right">3</td>
<td headers="t_statistic" class="gt_row gt_right">3.66189868</td>
<td headers="df" class="gt_row gt_right">3.938069</td>
<td headers="padj" class="gt_row gt_right">5.634696e-02</td>
<td headers="genotype_pair" class="gt_row gt_center">WT-MUT</td>
<td headers="padj_format" class="gt_row gt_left">0.056</td></tr>
    <tr><td headers="group1" class="gt_row gt_left">HET</td>
<td headers="group2" class="gt_row gt_left">MUT</td>
<td headers="age" class="gt_row gt_left">12 weeks</td>
<td headers="comparison_variable" class="gt_row gt_left">weight_gr</td>
<td headers="effect_size" class="gt_row gt_right">0.20433333</td>
<td headers="pvalue" class="gt_row gt_right">2.783766e-02</td>
<td headers="conf_lower" class="gt_row gt_right">0.0392977280</td>
<td headers="conf_upper" class="gt_row gt_right">0.36936894</td>
<td headers="sample_size_group1" class="gt_row gt_right">3</td>
<td headers="sample_size_group2" class="gt_row gt_right">3</td>
<td headers="t_statistic" class="gt_row gt_right">3.68296061</td>
<td headers="df" class="gt_row gt_right">3.414558</td>
<td headers="padj" class="gt_row gt_right">6.449014e-02</td>
<td headers="genotype_pair" class="gt_row gt_center">HET-MUT</td>
<td headers="padj_format" class="gt_row gt_left">0.064</td></tr>
    <tr><td headers="group1" class="gt_row gt_left">WT</td>
<td headers="group2" class="gt_row gt_left">HET</td>
<td headers="age" class="gt_row gt_left">12 weeks</td>
<td headers="comparison_variable" class="gt_row gt_left">length_cm</td>
<td headers="effect_size" class="gt_row gt_right">0.19633333</td>
<td headers="pvalue" class="gt_row gt_right">2.981030e-01</td>
<td headers="conf_lower" class="gt_row gt_right">-0.3798161233</td>
<td headers="conf_upper" class="gt_row gt_right">0.77248279</td>
<td headers="sample_size_group1" class="gt_row gt_right">3</td>
<td headers="sample_size_group2" class="gt_row gt_right">3</td>
<td headers="t_statistic" class="gt_row gt_right">1.35791808</td>
<td headers="df" class="gt_row gt_right">2.176574</td>
<td headers="padj" class="gt_row gt_right">3.794038e-01</td>
<td headers="genotype_pair" class="gt_row gt_center">WT-HET</td>
<td headers="padj_format" class="gt_row gt_left">0.38</td></tr>
    <tr><td headers="group1" class="gt_row gt_left">WT</td>
<td headers="group2" class="gt_row gt_left">MUT</td>
<td headers="age" class="gt_row gt_left">12 weeks</td>
<td headers="comparison_variable" class="gt_row gt_left">length_cm</td>
<td headers="effect_size" class="gt_row gt_right">0.79600000</td>
<td headers="pvalue" class="gt_row gt_right">3.092026e-02</td>
<td headers="conf_lower" class="gt_row gt_right">0.1233967954</td>
<td headers="conf_upper" class="gt_row gt_right">1.46860320</td>
<td headers="sample_size_group1" class="gt_row gt_right">3</td>
<td headers="sample_size_group2" class="gt_row gt_right">3</td>
<td headers="t_statistic" class="gt_row gt_right">3.38643016</td>
<td headers="df" class="gt_row gt_right">3.718117</td>
<td headers="padj" class="gt_row gt_right">6.659749e-02</td>
<td headers="genotype_pair" class="gt_row gt_center">WT-MUT</td>
<td headers="padj_format" class="gt_row gt_left">0.067</td></tr>
    <tr><td headers="group1" class="gt_row gt_left">HET</td>
<td headers="group2" class="gt_row gt_left">MUT</td>
<td headers="age" class="gt_row gt_left">12 weeks</td>
<td headers="comparison_variable" class="gt_row gt_left">length_cm</td>
<td headers="effect_size" class="gt_row gt_right">0.59966667</td>
<td headers="pvalue" class="gt_row gt_right">8.211919e-02</td>
<td headers="conf_lower" class="gt_row gt_right">-0.1816773439</td>
<td headers="conf_upper" class="gt_row gt_right">1.38101068</td>
<td headers="sample_size_group1" class="gt_row gt_right">3</td>
<td headers="sample_size_group2" class="gt_row gt_right">3</td>
<td headers="t_statistic" class="gt_row gt_right">3.15537321</td>
<td headers="df" class="gt_row gt_right">2.100464</td>
<td headers="padj" class="gt_row gt_right">1.437086e-01</td>
<td headers="genotype_pair" class="gt_row gt_center">HET-MUT</td>
<td headers="padj_format" class="gt_row gt_left">0.14</td></tr>
    <tr><td headers="group1" class="gt_row gt_left">WT</td>
<td headers="group2" class="gt_row gt_left">HET</td>
<td headers="age" class="gt_row gt_left">4 months</td>
<td headers="comparison_variable" class="gt_row gt_left">weight_gr</td>
<td headers="effect_size" class="gt_row gt_right">-0.02156250</td>
<td headers="pvalue" class="gt_row gt_right">5.358769e-01</td>
<td headers="conf_lower" class="gt_row gt_right">-0.0919068025</td>
<td headers="conf_upper" class="gt_row gt_right">0.04878180</td>
<td headers="sample_size_group1" class="gt_row gt_right">16</td>
<td headers="sample_size_group2" class="gt_row gt_right">18</td>
<td headers="t_statistic" class="gt_row gt_right">-0.62636294</td>
<td headers="df" class="gt_row gt_right">29.606380</td>
<td headers="padj" class="gt_row gt_right">5.662095e-01</td>
<td headers="genotype_pair" class="gt_row gt_center">WT-HET</td>
<td headers="padj_format" class="gt_row gt_left">0.57</td></tr>
    <tr><td headers="group1" class="gt_row gt_left">WT</td>
<td headers="group2" class="gt_row gt_left">MUT</td>
<td headers="age" class="gt_row gt_left">4 months</td>
<td headers="comparison_variable" class="gt_row gt_left">weight_gr</td>
<td headers="effect_size" class="gt_row gt_right">0.07100000</td>
<td headers="pvalue" class="gt_row gt_right">5.191514e-02</td>
<td headers="conf_lower" class="gt_row gt_right">-0.0006281862</td>
<td headers="conf_upper" class="gt_row gt_right">0.14262819</td>
<td headers="sample_size_group1" class="gt_row gt_right">16</td>
<td headers="sample_size_group2" class="gt_row gt_right">16</td>
<td headers="t_statistic" class="gt_row gt_right">2.02483899</td>
<td headers="df" class="gt_row gt_right">29.832245</td>
<td headers="padj" class="gt_row gt_right">1.038303e-01</td>
<td headers="genotype_pair" class="gt_row gt_center">WT-MUT</td>
<td headers="padj_format" class="gt_row gt_left">0.1</td></tr>
    <tr><td headers="group1" class="gt_row gt_left">HET</td>
<td headers="group2" class="gt_row gt_left">MUT</td>
<td headers="age" class="gt_row gt_left">4 months</td>
<td headers="comparison_variable" class="gt_row gt_left">weight_gr</td>
<td headers="effect_size" class="gt_row gt_right">0.09256250</td>
<td headers="pvalue" class="gt_row gt_right">8.861220e-03</td>
<td headers="conf_lower" class="gt_row gt_right">0.0250430026</td>
<td headers="conf_upper" class="gt_row gt_right">0.16008200</td>
<td headers="sample_size_group1" class="gt_row gt_right">18</td>
<td headers="sample_size_group2" class="gt_row gt_right">16</td>
<td headers="t_statistic" class="gt_row gt_right">2.79994612</td>
<td headers="df" class="gt_row gt_right">29.950467</td>
<td headers="padj" class="gt_row gt_right">2.481142e-02</td>
<td headers="genotype_pair" class="gt_row gt_center">HET-MUT</td>
<td headers="padj_format" class="gt_row gt_left">0.025</td></tr>
    <tr><td headers="group1" class="gt_row gt_left">WT</td>
<td headers="group2" class="gt_row gt_left">HET</td>
<td headers="age" class="gt_row gt_left">4 months</td>
<td headers="comparison_variable" class="gt_row gt_left">length_cm</td>
<td headers="effect_size" class="gt_row gt_right">-0.01475000</td>
<td headers="pvalue" class="gt_row gt_right">8.355968e-01</td>
<td headers="conf_lower" class="gt_row gt_right">-0.1587867998</td>
<td headers="conf_upper" class="gt_row gt_right">0.12928680</td>
<td headers="sample_size_group1" class="gt_row gt_right">16</td>
<td headers="sample_size_group2" class="gt_row gt_right">18</td>
<td headers="t_statistic" class="gt_row gt_right">-0.20939474</td>
<td headers="df" class="gt_row gt_right">29.146602</td>
<td headers="padj" class="gt_row gt_right">8.507895e-01</td>
<td headers="genotype_pair" class="gt_row gt_center">WT-HET</td>
<td headers="padj_format" class="gt_row gt_left">0.85</td></tr>
    <tr><td headers="group1" class="gt_row gt_left">WT</td>
<td headers="group2" class="gt_row gt_left">MUT</td>
<td headers="age" class="gt_row gt_left">4 months</td>
<td headers="comparison_variable" class="gt_row gt_left">length_cm</td>
<td headers="effect_size" class="gt_row gt_right">0.20956250</td>
<td headers="pvalue" class="gt_row gt_right">1.166651e-02</td>
<td headers="conf_lower" class="gt_row gt_right">0.0502402906</td>
<td headers="conf_upper" class="gt_row gt_right">0.36888471</td>
<td headers="sample_size_group1" class="gt_row gt_right">16</td>
<td headers="sample_size_group2" class="gt_row gt_right">16</td>
<td headers="t_statistic" class="gt_row gt_right">2.68650606</td>
<td headers="df" class="gt_row gt_right">29.939351</td>
<td headers="padj" class="gt_row gt_right">3.111069e-02</td>
<td headers="genotype_pair" class="gt_row gt_center">WT-MUT</td>
<td headers="padj_format" class="gt_row gt_left">0.031</td></tr>
    <tr><td headers="group1" class="gt_row gt_left">HET</td>
<td headers="group2" class="gt_row gt_left">MUT</td>
<td headers="age" class="gt_row gt_left">4 months</td>
<td headers="comparison_variable" class="gt_row gt_left">length_cm</td>
<td headers="effect_size" class="gt_row gt_right">0.22431250</td>
<td headers="pvalue" class="gt_row gt_right">4.309750e-03</td>
<td headers="conf_lower" class="gt_row gt_right">0.0762502074</td>
<td headers="conf_upper" class="gt_row gt_right">0.37237479</td>
<td headers="sample_size_group1" class="gt_row gt_right">18</td>
<td headers="sample_size_group2" class="gt_row gt_right">16</td>
<td headers="t_statistic" class="gt_row gt_right">3.09999575</td>
<td headers="df" class="gt_row gt_right">28.680772</td>
<td headers="padj" class="gt_row gt_right">1.419682e-02</td>
<td headers="genotype_pair" class="gt_row gt_center">HET-MUT</td>
<td headers="padj_format" class="gt_row gt_left">0.014</td></tr>
    <tr><td headers="group1" class="gt_row gt_left">WT</td>
<td headers="group2" class="gt_row gt_left">HET</td>
<td headers="age" class="gt_row gt_left">6 months</td>
<td headers="comparison_variable" class="gt_row gt_left">weight_gr</td>
<td headers="effect_size" class="gt_row gt_right">0.03793750</td>
<td headers="pvalue" class="gt_row gt_right">1.583234e-01</td>
<td headers="conf_lower" class="gt_row gt_right">-0.0160179132</td>
<td headers="conf_upper" class="gt_row gt_right">0.09189291</td>
<td headers="sample_size_group1" class="gt_row gt_right">16</td>
<td headers="sample_size_group2" class="gt_row gt_right">10</td>
<td headers="t_statistic" class="gt_row gt_right">1.46372375</td>
<td headers="df" class="gt_row gt_right">20.654357</td>
<td headers="padj" class="gt_row gt_right">2.316792e-01</td>
<td headers="genotype_pair" class="gt_row gt_center">WT-HET</td>
<td headers="padj_format" class="gt_row gt_left">0.23</td></tr>
    <tr><td headers="group1" class="gt_row gt_left">WT</td>
<td headers="group2" class="gt_row gt_left">MUT</td>
<td headers="age" class="gt_row gt_left">6 months</td>
<td headers="comparison_variable" class="gt_row gt_left">weight_gr</td>
<td headers="effect_size" class="gt_row gt_right">0.06638194</td>
<td headers="pvalue" class="gt_row gt_right">2.347196e-02</td>
<td headers="conf_lower" class="gt_row gt_right">0.0098278344</td>
<td headers="conf_upper" class="gt_row gt_right">0.12293605</td>
<td headers="sample_size_group1" class="gt_row gt_right">16</td>
<td headers="sample_size_group2" class="gt_row gt_right">9</td>
<td headers="t_statistic" class="gt_row gt_right">2.43268347</td>
<td headers="df" class="gt_row gt_right">22.249575</td>
<td headers="padj" class="gt_row gt_right">5.714911e-02</td>
<td headers="genotype_pair" class="gt_row gt_center">WT-MUT</td>
<td headers="padj_format" class="gt_row gt_left">0.057</td></tr>
    <tr><td headers="group1" class="gt_row gt_left">HET</td>
<td headers="group2" class="gt_row gt_left">MUT</td>
<td headers="age" class="gt_row gt_left">6 months</td>
<td headers="comparison_variable" class="gt_row gt_left">weight_gr</td>
<td headers="effect_size" class="gt_row gt_right">0.02844444</td>
<td headers="pvalue" class="gt_row gt_right">1.289009e-01</td>
<td headers="conf_lower" class="gt_row gt_right">-0.0092367045</td>
<td headers="conf_upper" class="gt_row gt_right">0.06612559</td>
<td headers="sample_size_group1" class="gt_row gt_right">10</td>
<td headers="sample_size_group2" class="gt_row gt_right">9</td>
<td headers="t_statistic" class="gt_row gt_right">1.60295190</td>
<td headers="df" class="gt_row gt_right">15.675714</td>
<td headers="padj" class="gt_row gt_right">2.005126e-01</td>
<td headers="genotype_pair" class="gt_row gt_center">HET-MUT</td>
<td headers="padj_format" class="gt_row gt_left">0.2</td></tr>
    <tr><td headers="group1" class="gt_row gt_left">WT</td>
<td headers="group2" class="gt_row gt_left">HET</td>
<td headers="age" class="gt_row gt_left">6 months</td>
<td headers="comparison_variable" class="gt_row gt_left">length_cm</td>
<td headers="effect_size" class="gt_row gt_right">0.09984722</td>
<td headers="pvalue" class="gt_row gt_right">1.229148e-01</td>
<td headers="conf_lower" class="gt_row gt_right">-0.0292046357</td>
<td headers="conf_upper" class="gt_row gt_right">0.22889908</td>
<td headers="sample_size_group1" class="gt_row gt_right">16</td>
<td headers="sample_size_group2" class="gt_row gt_right">10</td>
<td headers="t_statistic" class="gt_row gt_right">1.60365310</td>
<td headers="df" class="gt_row gt_right">22.214851</td>
<td headers="padj" class="gt_row gt_right">1.966637e-01</td>
<td headers="genotype_pair" class="gt_row gt_center">WT-HET</td>
<td headers="padj_format" class="gt_row gt_left">0.2</td></tr>
    <tr><td headers="group1" class="gt_row gt_left">WT</td>
<td headers="group2" class="gt_row gt_left">MUT</td>
<td headers="age" class="gt_row gt_left">6 months</td>
<td headers="comparison_variable" class="gt_row gt_left">length_cm</td>
<td headers="effect_size" class="gt_row gt_right">0.27929167</td>
<td headers="pvalue" class="gt_row gt_right">2.061703e-03</td>
<td headers="conf_lower" class="gt_row gt_right">0.1170518134</td>
<td headers="conf_upper" class="gt_row gt_right">0.44153152</td>
<td headers="sample_size_group1" class="gt_row gt_right">16</td>
<td headers="sample_size_group2" class="gt_row gt_right">9</td>
<td headers="t_statistic" class="gt_row gt_right">3.63216358</td>
<td headers="df" class="gt_row gt_right">16.989487</td>
<td headers="padj" class="gt_row gt_right">7.215960e-03</td>
<td headers="genotype_pair" class="gt_row gt_center">WT-MUT</td>
<td headers="padj_format" class="gt_row gt_left">0.0072</td></tr>
    <tr><td headers="group1" class="gt_row gt_left">HET</td>
<td headers="group2" class="gt_row gt_left">MUT</td>
<td headers="age" class="gt_row gt_left">6 months</td>
<td headers="comparison_variable" class="gt_row gt_left">length_cm</td>
<td headers="effect_size" class="gt_row gt_right">0.17944444</td>
<td headers="pvalue" class="gt_row gt_right">2.879024e-02</td>
<td headers="conf_lower" class="gt_row gt_right">0.0214680704</td>
<td headers="conf_upper" class="gt_row gt_right">0.33742082</td>
<td headers="sample_size_group1" class="gt_row gt_right">10</td>
<td headers="sample_size_group2" class="gt_row gt_right">9</td>
<td headers="t_statistic" class="gt_row gt_right">2.43583025</td>
<td headers="df" class="gt_row gt_right">14.025807</td>
<td headers="padj" class="gt_row gt_right">6.449014e-02</td>
<td headers="genotype_pair" class="gt_row gt_center">HET-MUT</td>
<td headers="padj_format" class="gt_row gt_left">0.064</td></tr>
    <tr><td headers="group1" class="gt_row gt_left">WT</td>
<td headers="group2" class="gt_row gt_left">HET</td>
<td headers="age" class="gt_row gt_left">8 months</td>
<td headers="comparison_variable" class="gt_row gt_left">weight_gr</td>
<td headers="effect_size" class="gt_row gt_right">0.07537500</td>
<td headers="pvalue" class="gt_row gt_right">1.654852e-01</td>
<td headers="conf_lower" class="gt_row gt_right">-0.0367696512</td>
<td headers="conf_upper" class="gt_row gt_right">0.18751965</td>
<td headers="sample_size_group1" class="gt_row gt_right">8</td>
<td headers="sample_size_group2" class="gt_row gt_right">5</td>
<td headers="t_statistic" class="gt_row gt_right">1.49422475</td>
<td headers="df" class="gt_row gt_right">10.168584</td>
<td headers="padj" class="gt_row gt_right">2.316792e-01</td>
<td headers="genotype_pair" class="gt_row gt_center">WT-HET</td>
<td headers="padj_format" class="gt_row gt_left">0.23</td></tr>
    <tr><td headers="group1" class="gt_row gt_left">WT</td>
<td headers="group2" class="gt_row gt_left">MUT</td>
<td headers="age" class="gt_row gt_left">8 months</td>
<td headers="comparison_variable" class="gt_row gt_left">weight_gr</td>
<td headers="effect_size" class="gt_row gt_right">0.15870833</td>
<td headers="pvalue" class="gt_row gt_right">6.287145e-02</td>
<td headers="conf_lower" class="gt_row gt_right">-0.0121737068</td>
<td headers="conf_upper" class="gt_row gt_right">0.32959037</td>
<td headers="sample_size_group1" class="gt_row gt_right">8</td>
<td headers="sample_size_group2" class="gt_row gt_right">3</td>
<td headers="t_statistic" class="gt_row gt_right">2.34698522</td>
<td headers="df" class="gt_row gt_right">5.303121</td>
<td headers="padj" class="gt_row gt_right">1.173600e-01</td>
<td headers="genotype_pair" class="gt_row gt_center">WT-MUT</td>
<td headers="padj_format" class="gt_row gt_left">0.12</td></tr>
    <tr><td headers="group1" class="gt_row gt_left">HET</td>
<td headers="group2" class="gt_row gt_left">MUT</td>
<td headers="age" class="gt_row gt_left">8 months</td>
<td headers="comparison_variable" class="gt_row gt_left">weight_gr</td>
<td headers="effect_size" class="gt_row gt_right">0.08333333</td>
<td headers="pvalue" class="gt_row gt_right">2.386006e-01</td>
<td headers="conf_lower" class="gt_row gt_right">-0.0992560170</td>
<td headers="conf_upper" class="gt_row gt_right">0.26592268</td>
<td headers="sample_size_group1" class="gt_row gt_right">5</td>
<td headers="sample_size_group2" class="gt_row gt_right">3</td>
<td headers="t_statistic" class="gt_row gt_right">1.47805620</td>
<td headers="df" class="gt_row gt_right">2.910506</td>
<td headers="padj" class="gt_row gt_right">3.181342e-01</td>
<td headers="genotype_pair" class="gt_row gt_center">HET-MUT</td>
<td headers="padj_format" class="gt_row gt_left">0.32</td></tr>
    <tr><td headers="group1" class="gt_row gt_left">WT</td>
<td headers="group2" class="gt_row gt_left">HET</td>
<td headers="age" class="gt_row gt_left">8 months</td>
<td headers="comparison_variable" class="gt_row gt_left">length_cm</td>
<td headers="effect_size" class="gt_row gt_right">0.05340000</td>
<td headers="pvalue" class="gt_row gt_right">5.306760e-01</td>
<td headers="conf_lower" class="gt_row gt_right">-0.1289292816</td>
<td headers="conf_upper" class="gt_row gt_right">0.23572928</td>
<td headers="sample_size_group1" class="gt_row gt_right">8</td>
<td headers="sample_size_group2" class="gt_row gt_right">5</td>
<td headers="t_statistic" class="gt_row gt_right">0.64835024</td>
<td headers="df" class="gt_row gt_right">10.504039</td>
<td headers="padj" class="gt_row gt_right">5.662095e-01</td>
<td headers="genotype_pair" class="gt_row gt_center">WT-HET</td>
<td headers="padj_format" class="gt_row gt_left">0.57</td></tr>
    <tr><td headers="group1" class="gt_row gt_left">WT</td>
<td headers="group2" class="gt_row gt_left">MUT</td>
<td headers="age" class="gt_row gt_left">8 months</td>
<td headers="comparison_variable" class="gt_row gt_left">length_cm</td>
<td headers="effect_size" class="gt_row gt_right">0.41000000</td>
<td headers="pvalue" class="gt_row gt_right">3.698248e-02</td>
<td headers="conf_lower" class="gt_row gt_right">0.0437728089</td>
<td headers="conf_upper" class="gt_row gt_right">0.77622719</td>
<td headers="sample_size_group1" class="gt_row gt_right">8</td>
<td headers="sample_size_group2" class="gt_row gt_right">3</td>
<td headers="t_statistic" class="gt_row gt_right">3.34985666</td>
<td headers="df" class="gt_row gt_right">3.373257</td>
<td headers="padj" class="gt_row gt_right">7.670441e-02</td>
<td headers="genotype_pair" class="gt_row gt_center">WT-MUT</td>
<td headers="padj_format" class="gt_row gt_left">0.077</td></tr>
    <tr><td headers="group1" class="gt_row gt_left">HET</td>
<td headers="group2" class="gt_row gt_left">MUT</td>
<td headers="age" class="gt_row gt_left">8 months</td>
<td headers="comparison_variable" class="gt_row gt_left">length_cm</td>
<td headers="effect_size" class="gt_row gt_right">0.35660000</td>
<td headers="pvalue" class="gt_row gt_right">5.618708e-02</td>
<td headers="conf_lower" class="gt_row gt_right">-0.0171056053</td>
<td headers="conf_upper" class="gt_row gt_right">0.73030561</td>
<td headers="sample_size_group1" class="gt_row gt_right">5</td>
<td headers="sample_size_group2" class="gt_row gt_right">3</td>
<td headers="t_statistic" class="gt_row gt_right">2.95649267</td>
<td headers="df" class="gt_row gt_right">3.150185</td>
<td headers="padj" class="gt_row gt_right">1.084992e-01</td>
<td headers="genotype_pair" class="gt_row gt_center">HET-MUT</td>
<td headers="padj_format" class="gt_row gt_left">0.11</td></tr>
    <tr><td headers="group1" class="gt_row gt_left">WT</td>
<td headers="group2" class="gt_row gt_left">HET</td>
<td headers="age" class="gt_row gt_left">10 months</td>
<td headers="comparison_variable" class="gt_row gt_left">weight_gr</td>
<td headers="effect_size" class="gt_row gt_right">-0.20850000</td>
<td headers="pvalue" class="gt_row gt_right">1.424807e-01</td>
<td headers="conf_lower" class="gt_row gt_right">-0.5681511396</td>
<td headers="conf_upper" class="gt_row gt_right">0.15115114</td>
<td headers="sample_size_group1" class="gt_row gt_right">3</td>
<td headers="sample_size_group2" class="gt_row gt_right">6</td>
<td headers="t_statistic" class="gt_row gt_right">-2.18044005</td>
<td headers="df" class="gt_row gt_right">2.335699</td>
<td headers="padj" class="gt_row gt_right">2.156465e-01</td>
<td headers="genotype_pair" class="gt_row gt_center">WT-HET</td>
<td headers="padj_format" class="gt_row gt_left">0.22</td></tr>
    <tr><td headers="group1" class="gt_row gt_left">WT</td>
<td headers="group2" class="gt_row gt_left">MUT</td>
<td headers="age" class="gt_row gt_left">10 months</td>
<td headers="comparison_variable" class="gt_row gt_left">weight_gr</td>
<td headers="effect_size" class="gt_row gt_right">0.25819048</td>
<td headers="pvalue" class="gt_row gt_right">8.115582e-02</td>
<td headers="conf_lower" class="gt_row gt_right">-0.0519209124</td>
<td headers="conf_upper" class="gt_row gt_right">0.56830186</td>
<td headers="sample_size_group1" class="gt_row gt_right">3</td>
<td headers="sample_size_group2" class="gt_row gt_right">7</td>
<td headers="t_statistic" class="gt_row gt_right">2.35427185</td>
<td headers="df" class="gt_row gt_right">3.823578</td>
<td headers="padj" class="gt_row gt_right">1.437086e-01</td>
<td headers="genotype_pair" class="gt_row gt_center">WT-MUT</td>
<td headers="padj_format" class="gt_row gt_left">0.14</td></tr>
    <tr><td headers="group1" class="gt_row gt_left">HET</td>
<td headers="group2" class="gt_row gt_left">MUT</td>
<td headers="age" class="gt_row gt_left">10 months</td>
<td headers="comparison_variable" class="gt_row gt_left">weight_gr</td>
<td headers="effect_size" class="gt_row gt_right">0.46669048</td>
<td headers="pvalue" class="gt_row gt_right">8.754234e-05</td>
<td headers="conf_lower" class="gt_row gt_right">0.3165660983</td>
<td headers="conf_upper" class="gt_row gt_right">0.61681485</td>
<td headers="sample_size_group1" class="gt_row gt_right">6</td>
<td headers="sample_size_group2" class="gt_row gt_right">7</td>
<td headers="t_statistic" class="gt_row gt_right">7.14070157</td>
<td headers="df" class="gt_row gt_right">8.183986</td>
<td headers="padj" class="gt_row gt_right">4.085309e-04</td>
<td headers="genotype_pair" class="gt_row gt_center">HET-MUT</td>
<td headers="padj_format" class="gt_row gt_left">0.00041</td></tr>
    <tr><td headers="group1" class="gt_row gt_left">WT</td>
<td headers="group2" class="gt_row gt_left">HET</td>
<td headers="age" class="gt_row gt_left">10 months</td>
<td headers="comparison_variable" class="gt_row gt_left">length_cm</td>
<td headers="effect_size" class="gt_row gt_right">-0.20783333</td>
<td headers="pvalue" class="gt_row gt_right">4.794465e-01</td>
<td headers="conf_lower" class="gt_row gt_right">-1.0467343138</td>
<td headers="conf_upper" class="gt_row gt_right">0.63106765</td>
<td headers="sample_size_group1" class="gt_row gt_right">3</td>
<td headers="sample_size_group2" class="gt_row gt_right">6</td>
<td headers="t_statistic" class="gt_row gt_right">-0.81088645</td>
<td headers="df" class="gt_row gt_right">2.859050</td>
<td headers="padj" class="gt_row gt_right">5.369801e-01</td>
<td headers="genotype_pair" class="gt_row gt_center">WT-HET</td>
<td headers="padj_format" class="gt_row gt_left">0.54</td></tr>
    <tr><td headers="group1" class="gt_row gt_left">WT</td>
<td headers="group2" class="gt_row gt_left">MUT</td>
<td headers="age" class="gt_row gt_left">10 months</td>
<td headers="comparison_variable" class="gt_row gt_left">length_cm</td>
<td headers="effect_size" class="gt_row gt_right">0.47647619</td>
<td headers="pvalue" class="gt_row gt_right">1.641646e-01</td>
<td headers="conf_lower" class="gt_row gt_right">-0.3597144025</td>
<td headers="conf_upper" class="gt_row gt_right">1.31266678</td>
<td headers="sample_size_group1" class="gt_row gt_right">3</td>
<td headers="sample_size_group2" class="gt_row gt_right">7</td>
<td headers="t_statistic" class="gt_row gt_right">1.85658902</td>
<td headers="df" class="gt_row gt_right">2.880856</td>
<td headers="padj" class="gt_row gt_right">2.316792e-01</td>
<td headers="genotype_pair" class="gt_row gt_center">WT-MUT</td>
<td headers="padj_format" class="gt_row gt_left">0.23</td></tr>
    <tr><td headers="group1" class="gt_row gt_left">HET</td>
<td headers="group2" class="gt_row gt_left">MUT</td>
<td headers="age" class="gt_row gt_left">10 months</td>
<td headers="comparison_variable" class="gt_row gt_left">length_cm</td>
<td headers="effect_size" class="gt_row gt_right">0.68430952</td>
<td headers="pvalue" class="gt_row gt_right">8.405781e-04</td>
<td headers="conf_lower" class="gt_row gt_right">0.3532353946</td>
<td headers="conf_upper" class="gt_row gt_right">1.01538365</td>
<td headers="sample_size_group1" class="gt_row gt_right">6</td>
<td headers="sample_size_group2" class="gt_row gt_right">7</td>
<td headers="t_statistic" class="gt_row gt_right">4.55318159</td>
<td headers="df" class="gt_row gt_right">10.923641</td>
<td headers="padj" class="gt_row gt_right">3.138158e-03</td>
<td headers="genotype_pair" class="gt_row gt_center">HET-MUT</td>
<td headers="padj_format" class="gt_row gt_left">0.0031</td></tr>
    <tr><td headers="group1" class="gt_row gt_left">WT</td>
<td headers="group2" class="gt_row gt_left">HET</td>
<td headers="age" class="gt_row gt_left">13 months</td>
<td headers="comparison_variable" class="gt_row gt_left">weight_gr</td>
<td headers="effect_size" class="gt_row gt_right">0.10850000</td>
<td headers="pvalue" class="gt_row gt_right">9.086120e-02</td>
<td headers="conf_lower" class="gt_row gt_right">-0.0200935794</td>
<td headers="conf_upper" class="gt_row gt_right">0.23709358</td>
<td headers="sample_size_group1" class="gt_row gt_right">8</td>
<td headers="sample_size_group2" class="gt_row gt_right">6</td>
<td headers="t_statistic" class="gt_row gt_right">1.83883213</td>
<td headers="df" class="gt_row gt_right">11.972238</td>
<td headers="padj" class="gt_row gt_right">1.541887e-01</td>
<td headers="genotype_pair" class="gt_row gt_center">WT-HET</td>
<td headers="padj_format" class="gt_row gt_left">0.15</td></tr>
    <tr><td headers="group1" class="gt_row gt_left">WT</td>
<td headers="group2" class="gt_row gt_left">MUT</td>
<td headers="age" class="gt_row gt_left">13 months</td>
<td headers="comparison_variable" class="gt_row gt_left">weight_gr</td>
<td headers="effect_size" class="gt_row gt_right">0.27425000</td>
<td headers="pvalue" class="gt_row gt_right">4.012757e-04</td>
<td headers="conf_lower" class="gt_row gt_right">0.1578059773</td>
<td headers="conf_upper" class="gt_row gt_right">0.39069402</td>
<td headers="sample_size_group1" class="gt_row gt_right">8</td>
<td headers="sample_size_group2" class="gt_row gt_right">4</td>
<td headers="t_statistic" class="gt_row gt_right">5.26968725</td>
<td headers="df" class="gt_row gt_right">9.701685</td>
<td headers="padj" class="gt_row gt_right">1.605103e-03</td>
<td headers="genotype_pair" class="gt_row gt_center">WT-MUT</td>
<td headers="padj_format" class="gt_row gt_left">0.0016</td></tr>
    <tr><td headers="group1" class="gt_row gt_left">HET</td>
<td headers="group2" class="gt_row gt_left">MUT</td>
<td headers="age" class="gt_row gt_left">13 months</td>
<td headers="comparison_variable" class="gt_row gt_left">weight_gr</td>
<td headers="effect_size" class="gt_row gt_right">0.16575000</td>
<td headers="pvalue" class="gt_row gt_right">5.924125e-03</td>
<td headers="conf_lower" class="gt_row gt_right">0.0631895484</td>
<td headers="conf_upper" class="gt_row gt_right">0.26831045</td>
<td headers="sample_size_group1" class="gt_row gt_right">6</td>
<td headers="sample_size_group2" class="gt_row gt_right">4</td>
<td headers="t_statistic" class="gt_row gt_right">3.74114818</td>
<td headers="df" class="gt_row gt_right">7.827213</td>
<td headers="padj" class="gt_row gt_right">1.746058e-02</td>
<td headers="genotype_pair" class="gt_row gt_center">HET-MUT</td>
<td headers="padj_format" class="gt_row gt_left">0.017</td></tr>
    <tr><td headers="group1" class="gt_row gt_left">WT</td>
<td headers="group2" class="gt_row gt_left">HET</td>
<td headers="age" class="gt_row gt_left">13 months</td>
<td headers="comparison_variable" class="gt_row gt_left">length_cm</td>
<td headers="effect_size" class="gt_row gt_right">0.22470833</td>
<td headers="pvalue" class="gt_row gt_right">1.008492e-01</td>
<td headers="conf_lower" class="gt_row gt_right">-0.0543979310</td>
<td headers="conf_upper" class="gt_row gt_right">0.50381460</td>
<td headers="sample_size_group1" class="gt_row gt_right">8</td>
<td headers="sample_size_group2" class="gt_row gt_right">6</td>
<td headers="t_statistic" class="gt_row gt_right">1.84680359</td>
<td headers="df" class="gt_row gt_right">8.250321</td>
<td headers="padj" class="gt_row gt_right">1.661045e-01</td>
<td headers="genotype_pair" class="gt_row gt_center">WT-HET</td>
<td headers="padj_format" class="gt_row gt_left">0.17</td></tr>
    <tr><td headers="group1" class="gt_row gt_left">WT</td>
<td headers="group2" class="gt_row gt_left">MUT</td>
<td headers="age" class="gt_row gt_left">13 months</td>
<td headers="comparison_variable" class="gt_row gt_left">length_cm</td>
<td headers="effect_size" class="gt_row gt_right">0.34670833</td>
<td headers="pvalue" class="gt_row gt_right">5.701559e-03</td>
<td headers="conf_lower" class="gt_row gt_right">0.1416316407</td>
<td headers="conf_upper" class="gt_row gt_right">0.55178503</td>
<td headers="sample_size_group1" class="gt_row gt_right">8</td>
<td headers="sample_size_group2" class="gt_row gt_right">4</td>
<td headers="t_statistic" class="gt_row gt_right">4.07433508</td>
<td headers="df" class="gt_row gt_right">6.404260</td>
<td headers="padj" class="gt_row gt_right">1.746058e-02</td>
<td headers="genotype_pair" class="gt_row gt_center">WT-MUT</td>
<td headers="padj_format" class="gt_row gt_left">0.017</td></tr>
    <tr><td headers="group1" class="gt_row gt_left">HET</td>
<td headers="group2" class="gt_row gt_left">MUT</td>
<td headers="age" class="gt_row gt_left">13 months</td>
<td headers="comparison_variable" class="gt_row gt_left">length_cm</td>
<td headers="effect_size" class="gt_row gt_right">0.12200000</td>
<td headers="pvalue" class="gt_row gt_right">3.468120e-01</td>
<td headers="conf_lower" class="gt_row gt_right">-0.1644267529</td>
<td headers="conf_upper" class="gt_row gt_right">0.40842675</td>
<td headers="sample_size_group1" class="gt_row gt_right">6</td>
<td headers="sample_size_group2" class="gt_row gt_right">4</td>
<td headers="t_statistic" class="gt_row gt_right">1.00916475</td>
<td headers="df" class="gt_row gt_right">6.932824</td>
<td headers="padj" class="gt_row gt_right">4.273500e-01</td>
<td headers="genotype_pair" class="gt_row gt_center">HET-MUT</td>
<td headers="padj_format" class="gt_row gt_left">0.43</td></tr>
    <tr><td headers="group1" class="gt_row gt_left">WT</td>
<td headers="group2" class="gt_row gt_left">HET</td>
<td headers="age" class="gt_row gt_left">24 months</td>
<td headers="comparison_variable" class="gt_row gt_left">weight_gr</td>
<td headers="effect_size" class="gt_row gt_right">0.08396667</td>
<td headers="pvalue" class="gt_row gt_right">2.453113e-01</td>
<td headers="conf_lower" class="gt_row gt_right">-0.0632879921</td>
<td headers="conf_upper" class="gt_row gt_right">0.23122133</td>
<td headers="sample_size_group1" class="gt_row gt_right">9</td>
<td headers="sample_size_group2" class="gt_row gt_right">10</td>
<td headers="t_statistic" class="gt_row gt_right">1.20372012</td>
<td headers="df" class="gt_row gt_right">16.875993</td>
<td headers="padj" class="gt_row gt_right">3.194751e-01</td>
<td headers="genotype_pair" class="gt_row gt_center">WT-HET</td>
<td headers="padj_format" class="gt_row gt_left">0.32</td></tr>
    <tr><td headers="group1" class="gt_row gt_left">WT</td>
<td headers="group2" class="gt_row gt_left">MUT</td>
<td headers="age" class="gt_row gt_left">24 months</td>
<td headers="comparison_variable" class="gt_row gt_left">weight_gr</td>
<td headers="effect_size" class="gt_row gt_right">NA</td>
<td headers="pvalue" class="gt_row gt_right">NA</td>
<td headers="conf_lower" class="gt_row gt_right">NA</td>
<td headers="conf_upper" class="gt_row gt_right">NA</td>
<td headers="sample_size_group1" class="gt_row gt_right">9</td>
<td headers="sample_size_group2" class="gt_row gt_right">0</td>
<td headers="t_statistic" class="gt_row gt_right">NA</td>
<td headers="df" class="gt_row gt_right">NA</td>
<td headers="padj" class="gt_row gt_right">NA</td>
<td headers="genotype_pair" class="gt_row gt_center">WT-MUT</td>
<td headers="padj_format" class="gt_row gt_left">NA</td></tr>
    <tr><td headers="group1" class="gt_row gt_left">HET</td>
<td headers="group2" class="gt_row gt_left">MUT</td>
<td headers="age" class="gt_row gt_left">24 months</td>
<td headers="comparison_variable" class="gt_row gt_left">weight_gr</td>
<td headers="effect_size" class="gt_row gt_right">NA</td>
<td headers="pvalue" class="gt_row gt_right">NA</td>
<td headers="conf_lower" class="gt_row gt_right">NA</td>
<td headers="conf_upper" class="gt_row gt_right">NA</td>
<td headers="sample_size_group1" class="gt_row gt_right">10</td>
<td headers="sample_size_group2" class="gt_row gt_right">0</td>
<td headers="t_statistic" class="gt_row gt_right">NA</td>
<td headers="df" class="gt_row gt_right">NA</td>
<td headers="padj" class="gt_row gt_right">NA</td>
<td headers="genotype_pair" class="gt_row gt_center">HET-MUT</td>
<td headers="padj_format" class="gt_row gt_left">NA</td></tr>
    <tr><td headers="group1" class="gt_row gt_left">WT</td>
<td headers="group2" class="gt_row gt_left">HET</td>
<td headers="age" class="gt_row gt_left">24 months</td>
<td headers="comparison_variable" class="gt_row gt_left">length_cm</td>
<td headers="effect_size" class="gt_row gt_right">0.19035556</td>
<td headers="pvalue" class="gt_row gt_right">2.000748e-01</td>
<td headers="conf_lower" class="gt_row gt_right">-0.1112864647</td>
<td headers="conf_upper" class="gt_row gt_right">0.49199758</td>
<td headers="sample_size_group1" class="gt_row gt_right">9</td>
<td headers="sample_size_group2" class="gt_row gt_right">10</td>
<td headers="t_statistic" class="gt_row gt_right">1.33508832</td>
<td headers="df" class="gt_row gt_right">16.409563</td>
<td headers="padj" class="gt_row gt_right">2.732729e-01</td>
<td headers="genotype_pair" class="gt_row gt_center">WT-HET</td>
<td headers="padj_format" class="gt_row gt_left">0.27</td></tr>
    <tr><td headers="group1" class="gt_row gt_left">WT</td>
<td headers="group2" class="gt_row gt_left">MUT</td>
<td headers="age" class="gt_row gt_left">24 months</td>
<td headers="comparison_variable" class="gt_row gt_left">length_cm</td>
<td headers="effect_size" class="gt_row gt_right">NA</td>
<td headers="pvalue" class="gt_row gt_right">NA</td>
<td headers="conf_lower" class="gt_row gt_right">NA</td>
<td headers="conf_upper" class="gt_row gt_right">NA</td>
<td headers="sample_size_group1" class="gt_row gt_right">9</td>
<td headers="sample_size_group2" class="gt_row gt_right">0</td>
<td headers="t_statistic" class="gt_row gt_right">NA</td>
<td headers="df" class="gt_row gt_right">NA</td>
<td headers="padj" class="gt_row gt_right">NA</td>
<td headers="genotype_pair" class="gt_row gt_center">WT-MUT</td>
<td headers="padj_format" class="gt_row gt_left">NA</td></tr>
    <tr><td headers="group1" class="gt_row gt_left">HET</td>
<td headers="group2" class="gt_row gt_left">MUT</td>
<td headers="age" class="gt_row gt_left">24 months</td>
<td headers="comparison_variable" class="gt_row gt_left">length_cm</td>
<td headers="effect_size" class="gt_row gt_right">NA</td>
<td headers="pvalue" class="gt_row gt_right">NA</td>
<td headers="conf_lower" class="gt_row gt_right">NA</td>
<td headers="conf_upper" class="gt_row gt_right">NA</td>
<td headers="sample_size_group1" class="gt_row gt_right">10</td>
<td headers="sample_size_group2" class="gt_row gt_right">0</td>
<td headers="t_statistic" class="gt_row gt_right">NA</td>
<td headers="df" class="gt_row gt_right">NA</td>
<td headers="padj" class="gt_row gt_right">NA</td>
<td headers="genotype_pair" class="gt_row gt_center">HET-MUT</td>
<td headers="padj_format" class="gt_row gt_left">NA</td></tr>
  </tbody>
  
  
</table>
</div>
