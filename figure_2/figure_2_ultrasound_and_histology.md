figure 2 - ultrasound and histology data
================
Bradley Demarest
2023-03-15

``` r
library(data.table)
library(ggplot2)
library(tidyverse)
library(gt)
library(here)
library(stringr)
library(patchwork)
```

``` r
# Load ultrasound data for 4- and 6-month from two tab-delimited text files.

utab4 = fread(here("figure_2",
                   "jag2b_summarized_ultrasound_4month_batch_1_2_n42_20230315.txt"))

# Remove 'line' column.
set(utab4, j="line", value=NULL)

# March 17 2023. fish f0092 is missing As and Ad values. 
# Leo found the values in the original csv files. 
# We will insert the correct mean As and mean Ad values into the 
# current working tab-delimited text file. 
# Warning! We have not corrected the fish f0092 values in the intermediate
# data files (where ultrasound replicate values are in separate columns
# such as As_1, As_2, etc.) Also, we do not have value ventricle Volume data for
# fish f0092. Eventually, we should go back and re-run the csv processing
# script.

utab6 = fread(here("figure_2",
                   "jag2b_summarized_ultrasound_6_8_month_n46_f0092_update_20230317.txt"))
# Add batch column (containing NA values).
utab6[, batch:=NA_character_]

utab = rbind(utab4, utab6)

# Remove 8-month old fish data. We are not including this timepoint
# in plots because of very small sample sizes.
utab = utab[age %in% c("4month", "6month")]

# Compute derived ultrasound variables.
utab[, SA:=Ad_avg - As_avg]
utab[, EFA:=SA / Ad_avg]
utab[, CO:=SA * HR_avg]

# Create new group variable.
utab[age %in% "4month" & batch %in% "batch_1", group:="4-month old, batch 1"]
utab[age %in% "4month" & batch %in% "batch_2", group:="4-month old, batch 2"]
utab[age %in% "6month", group:="6-month old"]

# Use simple genotype labels (+/+, +/-, etc.)
convert_genotype_labels = c(WT="+/+", HET="+/-", MUT="-/-")
utab[, genotype2:=convert_genotype_labels[genotype]]
# Set factor and level order of 'genotype2' column.
utab[, genotype2:=factor(genotype2, levels=convert_genotype_labels)]
```

``` r
# Create panels for 3 groups (4-month batch 1, 4-month batch 2, 6-month)
# and 6 variables (weight, HR, Ad, As, CO, SA).

group_vec = c("4-month old, batch 1", "4-month old, batch 2", "6-month old")


variable_vec = c("weight_gr", "HR_avg", "Ad_avg", "As_avg", "CO", "SA")
variable_labels = c("'Weight (grams)'", "'Heart rate '('minute'^-1)",
                    "'Ventricle area, diastole '('mm'^2)",
                    "'Ventricle area, systole '('mm'^2)",
                    "'Cardiac output '('mm'^2 * 'minute'^-1)",
                    "'Stroke area '('mm'^2)")

# Collect y-axis ranges for all variables.
yrange_res = list()

for (i in seq_along(variable_vec)) {
  tmp_var = variable_vec[i]
  yrange_res[[tmp_var]] = range(utab[[tmp_var]], na.rm=TRUE)
}


# Ultrasound data panel loop.
# as.name() is equivalent to rlang::sym()

genotype_colors  = c( "WT"="#80b1d3",
                     "HET"="#b3de69",
                     "MUT"="#fb8072")

genotype2_colors  = c("+/+"="#80b1d3",
                      "+/-"="#b3de69",
                      "-/-"="#fb8072")

point_size = 2.6
errorbar_width = 0.6
errorbar_linewidth = 0.625
meanbar_half_width = 0.45
meanbar_linewidth = 1.2
# Expand y-axis 5% on bottom end and 25% on upper end, to make room for p-values.
yaxis_expand = expansion(mult=c(0.05, 0.25))

group_res = list()

# To do: 
# (1) [X] Use custom genotype colors.
# (2) [X] Apply per-variable y-axis range limits, so that all 3 panels have same range.
# (3) [X] Change order of genotypes on x-axis. Create factor, set levels.
# (4) [X] Remove x-axis titles ("genotype").
# (5) [X] Change color legend title from "genotype" to "Genotype".
# (6) [X] Parse/format y-axis variable labels.
# (7) [ ] 
# (8) [X] Add dark gray outline to plot points.
# (9) [X] Add mean (line) and std deviation (whiskers) to plot.
#(10) [X] Add extra space at top of each panel for adding p-values.

for (g in seq_along(group_vec)) {
  tmp_group = group_vec[g]
  tmp_data = utab[group %in% tmp_group]
  
  panel_res = list()
  
  for (v in seq_along(variable_vec)) {
    tmp_var = variable_vec[v]
    tmp_label = variable_labels[v]
    tmp_data[, label:=tmp_label]
    
    tmp_by_genotype = tmp_data[, list(mean_value=mean(get(tmp_var), na.rm=TRUE),
                                      sd_value=sd(get(tmp_var), na.rm=TRUE)),
                               by=list(genotype2)]
    
    tmp_panel = ggplot() +
                theme_bw() +
      
                geom_errorbar(data=tmp_by_genotype,
                              aes(x=genotype2,
                                  ymin=mean_value - sd_value,
                                  ymax=mean_value + sd_value,
                                  color=genotype2),
                              linewidth=errorbar_linewidth,
                              width=errorbar_width) +
                geom_segment(data=tmp_by_genotype,
                             aes(y=mean_value,
                                 yend=mean_value,
                                 x=as.integer(genotype2) - meanbar_half_width,
                                 xend=as.integer(genotype2) + meanbar_half_width,
                                 color=genotype2),
                             linewidth=meanbar_linewidth) +

                geom_point(data=tmp_data,
                           aes(y=!!as.name(tmp_var),
                               x=genotype2,
                               color=genotype2,
                               fill=genotype2),
                           size=point_size, shape=21, color="grey30") +
                scale_color_manual(values=genotype2_colors) +
                scale_fill_manual(values=genotype2_colors) +
                scale_y_continuous(limits=yrange_res[[tmp_var]],
                                   expand=yaxis_expand) +
                guides(color="none") +
                guides(fill="none") + 
                labs(fill="Genotype") + 
                labs(y=parse(text=tmp_label)) + 
                theme(axis.title.x=element_blank()) +
                theme(axis.title.y=element_text(size=rel(0.9)))
    
    panel_res[[tmp_var]] = tmp_panel
  }
  
  group_res[[tmp_group]] = wrap_plots(panel_res, nrow=1) #+
                           #plot_layout(guides="collect") +
                           #plot_annotation(title=tmp_group)

}


ultrasound_fig = group_res[[1]] / group_res[[2]] / group_res[[3]]

ggsave(here("figure_2", "figure2_ultrasound_panels_20230329.pdf"),
       plot=ultrasound_fig,
       width=10, height=8, 
       useDingbats=FALSE)
```

    ## Warning: Removed 1 rows containing missing values (`geom_point()`).
    ## Removed 1 rows containing missing values (`geom_point()`).

``` r
print(ultrasound_fig)
```

    ## Warning: Removed 1 rows containing missing values (`geom_point()`).
    ## Removed 1 rows containing missing values (`geom_point()`).

![](figure_2_ultrasound_and_histology_files/figure-gfm/print-ultrasound-figure-in-markdown-1.png)<!-- -->

#### Pairwise t-test results for 6 ultrasound variables.

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


group_vec = c("4-month old, batch 1", "4-month old, batch 2", "6-month old")


variable_vec = c("weight_gr", "HR_avg", "Ad_avg", "As_avg", "CO", "SA")


comp_tab = data.table(group1=rep(c("WT", "WT", "HET"), times=length(variable_vec)),
                      group2=rep(c("HET", "MUT", "MUT"), times=length(variable_vec)),
                      variable=rep(variable_vec, each=3), # modify
                      age_batch=rep(group_vec, each=3 * length(variable_vec)))

table(comp_tab$variable, comp_tab$age_batch)
```

    ##            
    ##             4-month old, batch 1 4-month old, batch 2 6-month old
    ##   Ad_avg                       3                    3           3
    ##   As_avg                       3                    3           3
    ##   CO                           3                    3           3
    ##   HR_avg                       3                    3           3
    ##   SA                           3                    3           3
    ##   weight_gr                    3                    3           3

``` r
tres_list = list()

for (i in seq(nrow(comp_tab))) {
  row_i = comp_tab[i]
  tmp_group1 = row_i$group1
  tmp_group2 = row_i$group2
  tmp_age_batch = row_i$age_batch
  tmp_var = row_i$variable
  group1_data = utab[genotype == tmp_group1 & group == tmp_age_batch, ..tmp_var]
  
  group2_data = utab[genotype == tmp_group2 & group == tmp_age_batch, ..tmp_var]
  
  if (nrow(group1_data) > 0L & nrow(group2_data) > 0L) {
      
    tmp_res = t.test(x=group1_data,
                     y=group2_data,
                     alternative = "two.sided",
                     conf.level=0.95)
  
    new_row = data.table(group1=tmp_group1,
                         group2=tmp_group2,
                         age_batch=tmp_age_batch,
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
                         age_batch=tmp_age_batch,
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
       file=here("figure_2",
                 "pairwise_ttest_results_6ultrasound_variables_3batches_20230403.txt"), 
       sep="\t")


# Write out a filtered, sorted version for Leo to use adding pvalues in AI.

stats2 = stats_tab[padj < 0.05, 
                   list(age_batch, comparison_variable, genotype_pair, padj_format)]



setorder(stats2, age_batch, comparison_variable, -genotype_pair)

library(writexl)
write_xlsx(stats2, 
           path=here("figure_2", 
                     "pairwise_ttest_padj05_table_for_figure_2_6ultrasound_vars_20230403.xlsx"))
```

<div id="nlnjmmuujy" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#nlnjmmuujy table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#nlnjmmuujy thead, #nlnjmmuujy tbody, #nlnjmmuujy tfoot, #nlnjmmuujy tr, #nlnjmmuujy td, #nlnjmmuujy th {
  border-style: none;
}

#nlnjmmuujy p {
  margin: 0;
  padding: 0;
}

#nlnjmmuujy .gt_table {
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

#nlnjmmuujy .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#nlnjmmuujy .gt_title {
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

#nlnjmmuujy .gt_subtitle {
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

#nlnjmmuujy .gt_heading {
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

#nlnjmmuujy .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#nlnjmmuujy .gt_col_headings {
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

#nlnjmmuujy .gt_col_heading {
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

#nlnjmmuujy .gt_column_spanner_outer {
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

#nlnjmmuujy .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#nlnjmmuujy .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#nlnjmmuujy .gt_column_spanner {
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

#nlnjmmuujy .gt_spanner_row {
  border-bottom-style: hidden;
}

#nlnjmmuujy .gt_group_heading {
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

#nlnjmmuujy .gt_empty_group_heading {
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

#nlnjmmuujy .gt_from_md > :first-child {
  margin-top: 0;
}

#nlnjmmuujy .gt_from_md > :last-child {
  margin-bottom: 0;
}

#nlnjmmuujy .gt_row {
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

#nlnjmmuujy .gt_stub {
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

#nlnjmmuujy .gt_stub_row_group {
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

#nlnjmmuujy .gt_row_group_first td {
  border-top-width: 2px;
}

#nlnjmmuujy .gt_row_group_first th {
  border-top-width: 2px;
}

#nlnjmmuujy .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#nlnjmmuujy .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#nlnjmmuujy .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#nlnjmmuujy .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#nlnjmmuujy .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#nlnjmmuujy .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#nlnjmmuujy .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#nlnjmmuujy .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#nlnjmmuujy .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#nlnjmmuujy .gt_footnotes {
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

#nlnjmmuujy .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#nlnjmmuujy .gt_sourcenotes {
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

#nlnjmmuujy .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#nlnjmmuujy .gt_left {
  text-align: left;
}

#nlnjmmuujy .gt_center {
  text-align: center;
}

#nlnjmmuujy .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#nlnjmmuujy .gt_font_normal {
  font-weight: normal;
}

#nlnjmmuujy .gt_font_bold {
  font-weight: bold;
}

#nlnjmmuujy .gt_font_italic {
  font-style: italic;
}

#nlnjmmuujy .gt_super {
  font-size: 65%;
}

#nlnjmmuujy .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#nlnjmmuujy .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#nlnjmmuujy .gt_indent_1 {
  text-indent: 5px;
}

#nlnjmmuujy .gt_indent_2 {
  text-indent: 10px;
}

#nlnjmmuujy .gt_indent_3 {
  text-indent: 15px;
}

#nlnjmmuujy .gt_indent_4 {
  text-indent: 20px;
}

#nlnjmmuujy .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_heading">
      <td colspan="4" class="gt_heading gt_title gt_font_normal gt_bottom_border" style>Ultrasound variables, pairwise t-test results within each batch-by-variable combo. FDR-adj p-value &lt; 0.05</td>
    </tr>
    
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="age_batch">age_batch</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="comparison_variable">comparison_variable</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="genotype_pair">genotype_pair</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="padj_format">padj_format</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="age_batch" class="gt_row gt_left">4-month old, batch 1</td>
<td headers="comparison_variable" class="gt_row gt_left">Ad_avg</td>
<td headers="genotype_pair" class="gt_row gt_center">WT-MUT</td>
<td headers="padj_format" class="gt_row gt_right">0.0098</td></tr>
    <tr><td headers="age_batch" class="gt_row gt_left">4-month old, batch 1</td>
<td headers="comparison_variable" class="gt_row gt_left">Ad_avg</td>
<td headers="genotype_pair" class="gt_row gt_center">HET-MUT</td>
<td headers="padj_format" class="gt_row gt_right">0.039</td></tr>
    <tr><td headers="age_batch" class="gt_row gt_left">4-month old, batch 1</td>
<td headers="comparison_variable" class="gt_row gt_left">As_avg</td>
<td headers="genotype_pair" class="gt_row gt_center">WT-MUT</td>
<td headers="padj_format" class="gt_row gt_right">0.01</td></tr>
    <tr><td headers="age_batch" class="gt_row gt_left">4-month old, batch 1</td>
<td headers="comparison_variable" class="gt_row gt_left">As_avg</td>
<td headers="genotype_pair" class="gt_row gt_center">HET-MUT</td>
<td headers="padj_format" class="gt_row gt_right">0.0068</td></tr>
    <tr><td headers="age_batch" class="gt_row gt_left">4-month old, batch 2</td>
<td headers="comparison_variable" class="gt_row gt_left">Ad_avg</td>
<td headers="genotype_pair" class="gt_row gt_center">WT-MUT</td>
<td headers="padj_format" class="gt_row gt_right">0.0068</td></tr>
    <tr><td headers="age_batch" class="gt_row gt_left">4-month old, batch 2</td>
<td headers="comparison_variable" class="gt_row gt_left">As_avg</td>
<td headers="genotype_pair" class="gt_row gt_center">WT-MUT</td>
<td headers="padj_format" class="gt_row gt_right">0.0068</td></tr>
    <tr><td headers="age_batch" class="gt_row gt_left">4-month old, batch 2</td>
<td headers="comparison_variable" class="gt_row gt_left">CO</td>
<td headers="genotype_pair" class="gt_row gt_center">HET-MUT</td>
<td headers="padj_format" class="gt_row gt_right">0.014</td></tr>
    <tr><td headers="age_batch" class="gt_row gt_left">6-month old</td>
<td headers="comparison_variable" class="gt_row gt_left">Ad_avg</td>
<td headers="genotype_pair" class="gt_row gt_center">WT-MUT</td>
<td headers="padj_format" class="gt_row gt_right">0.01</td></tr>
    <tr><td headers="age_batch" class="gt_row gt_left">6-month old</td>
<td headers="comparison_variable" class="gt_row gt_left">Ad_avg</td>
<td headers="genotype_pair" class="gt_row gt_center">HET-MUT</td>
<td headers="padj_format" class="gt_row gt_right">0.0049</td></tr>
    <tr><td headers="age_batch" class="gt_row gt_left">6-month old</td>
<td headers="comparison_variable" class="gt_row gt_left">As_avg</td>
<td headers="genotype_pair" class="gt_row gt_center">WT-MUT</td>
<td headers="padj_format" class="gt_row gt_right">0.0097</td></tr>
    <tr><td headers="age_batch" class="gt_row gt_left">6-month old</td>
<td headers="comparison_variable" class="gt_row gt_left">As_avg</td>
<td headers="genotype_pair" class="gt_row gt_center">HET-MUT</td>
<td headers="padj_format" class="gt_row gt_right">0.015</td></tr>
    <tr><td headers="age_batch" class="gt_row gt_left">6-month old</td>
<td headers="comparison_variable" class="gt_row gt_left">SA</td>
<td headers="genotype_pair" class="gt_row gt_center">HET-MUT</td>
<td headers="padj_format" class="gt_row gt_right">0.04</td></tr>
  </tbody>
  
  
</table>
</div>

#### Load mef2 histology data.

``` r
htab = fread(here("figure_2",
                  "jag2b_mef2_summarized_histology_4_6_8_month_n38_20230417.txt"))
setnames(htab, old="genotype2", new="genotype")

batch_info = fread(here("figure_2", 
                        "jag2b_mef2_histology_4month_batch_info.txt"))

batch_info[, fish_id2_fct:=paste(genotype, fish_id, sep="_")]

htab = merge(htab, 
             batch_info[, list(fish_id2_fct, age, genotype, batch_id)], 
             by=c("fish_id2_fct", "age", "genotype"),
             all.x=TRUE, all.y=TRUE)

# Remove 4-month batch 1 fish (n=2).
htab = htab[!(age == "4month" & batch_id == "batch_1")]

# Remove 8-month fish (n=5).
htab = htab[!(age == "8month")]

# Now, compute the area in um^2 of Leo's 650x650 sampling area square.
# 650 px * 0.325 um/px = 211.25 um
# 211.25 um * 211.25 um = 44626.56 um^2

mef2_area_denominator = 44625.56
htab[, mef2_fraction:=mean_of_means_area / mef2_area_denominator]

convert_genotype_labels = c(WT="+/+", HET="+/-", MUT="-/-")
htab[, genotype2:=convert_genotype_labels[toupper(genotype)]]
# Set factor and level order of 'genotype2' column.
htab[, genotype2:=factor(genotype2, levels=convert_genotype_labels)]

# Create new group column with more legible age labels.
htab[age == "4month", group:="4 months, batch 2"]
htab[age == "6month", group:="6 months"]
```

``` r
dcast(data=htab, genotype ~ group, fun.aggregate=length) %>% 
  as_tibble() %>%
  gt() %>%
  tab_header(title="Histology sample summary")
```

<div id="ieckulqcls" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#ieckulqcls table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#ieckulqcls thead, #ieckulqcls tbody, #ieckulqcls tfoot, #ieckulqcls tr, #ieckulqcls td, #ieckulqcls th {
  border-style: none;
}

#ieckulqcls p {
  margin: 0;
  padding: 0;
}

#ieckulqcls .gt_table {
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

#ieckulqcls .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#ieckulqcls .gt_title {
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

#ieckulqcls .gt_subtitle {
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

#ieckulqcls .gt_heading {
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

#ieckulqcls .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ieckulqcls .gt_col_headings {
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

#ieckulqcls .gt_col_heading {
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

#ieckulqcls .gt_column_spanner_outer {
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

#ieckulqcls .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#ieckulqcls .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#ieckulqcls .gt_column_spanner {
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

#ieckulqcls .gt_spanner_row {
  border-bottom-style: hidden;
}

#ieckulqcls .gt_group_heading {
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

#ieckulqcls .gt_empty_group_heading {
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

#ieckulqcls .gt_from_md > :first-child {
  margin-top: 0;
}

#ieckulqcls .gt_from_md > :last-child {
  margin-bottom: 0;
}

#ieckulqcls .gt_row {
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

#ieckulqcls .gt_stub {
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

#ieckulqcls .gt_stub_row_group {
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

#ieckulqcls .gt_row_group_first td {
  border-top-width: 2px;
}

#ieckulqcls .gt_row_group_first th {
  border-top-width: 2px;
}

#ieckulqcls .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#ieckulqcls .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#ieckulqcls .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#ieckulqcls .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ieckulqcls .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#ieckulqcls .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#ieckulqcls .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#ieckulqcls .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#ieckulqcls .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ieckulqcls .gt_footnotes {
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

#ieckulqcls .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#ieckulqcls .gt_sourcenotes {
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

#ieckulqcls .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#ieckulqcls .gt_left {
  text-align: left;
}

#ieckulqcls .gt_center {
  text-align: center;
}

#ieckulqcls .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#ieckulqcls .gt_font_normal {
  font-weight: normal;
}

#ieckulqcls .gt_font_bold {
  font-weight: bold;
}

#ieckulqcls .gt_font_italic {
  font-style: italic;
}

#ieckulqcls .gt_super {
  font-size: 65%;
}

#ieckulqcls .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#ieckulqcls .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#ieckulqcls .gt_indent_1 {
  text-indent: 5px;
}

#ieckulqcls .gt_indent_2 {
  text-indent: 10px;
}

#ieckulqcls .gt_indent_3 {
  text-indent: 15px;
}

#ieckulqcls .gt_indent_4 {
  text-indent: 20px;
}

#ieckulqcls .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_heading">
      <td colspan="3" class="gt_heading gt_title gt_font_normal gt_bottom_border" style>Histology sample summary</td>
    </tr>
    
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="genotype">genotype</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="4 months, batch 2">4 months, batch 2</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="6 months">6 months</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="genotype" class="gt_row gt_left">het</td>
<td headers="4 months, batch 2" class="gt_row gt_right">3</td>
<td headers="6 months" class="gt_row gt_right">5</td></tr>
    <tr><td headers="genotype" class="gt_row gt_left">mut</td>
<td headers="4 months, batch 2" class="gt_row gt_right">4</td>
<td headers="6 months" class="gt_row gt_right">7</td></tr>
    <tr><td headers="genotype" class="gt_row gt_left">wt</td>
<td headers="4 months, batch 2" class="gt_row gt_right">5</td>
<td headers="6 months" class="gt_row gt_right">7</td></tr>
  </tbody>
  
  
</table>
</div>

``` r
by_genotype = htab[, list(mef2_mean=mean(mef2_fraction),
                             mef2_sd=sd(mef2_fraction)),
                      by=list(group, genotype2)]

genotype2_colors  = c("+/+"="#80b1d3",
                      "+/-"="#b3de69",
                      "-/-"="#fb8072")

point_size = 2.6
errorbar_width = 0.6
errorbar_linewidth = 0.625
meanbar_half_width = 0.45
meanbar_linewidth = 1.2
# Expand y-axis 5% on bottom end and 25% on upper end, to make room for p-values.
yaxis_expand = expansion(mult=c(0.05, 0.25))


d1 = ggplot() +
     theme_bw() +
     geom_errorbar(data=by_genotype,
                   aes(x=genotype2,
                       ymin=mef2_mean - mef2_sd,
                       ymax=mef2_mean + mef2_sd,
                       color=genotype2),
                   linewidth=errorbar_linewidth,
                   width=errorbar_width) +
     geom_segment(data=by_genotype,
                  aes(y=mef2_mean,
                      yend=mef2_mean,
                      x=as.integer(genotype2) - meanbar_half_width,
                      xend=as.integer(genotype2) + meanbar_half_width,
                      color=genotype2),
                  linewidth=meanbar_linewidth) +
     geom_point(data=htab,
                aes(x=genotype2, 
                    y=mef2_fraction,
                    fill=genotype2),
                size=point_size,
                shape=21,
                color="grey30") +
     scale_color_manual(values=genotype2_colors) +
     scale_fill_manual(values=genotype2_colors) +
     scale_y_continuous(expand=yaxis_expand) +
     guides(color="none") +
     guides(fill="none") + 
     labs(x=NULL) +
     labs(y="Myocardium density (proportion)") +
     facet_grid(cols=vars(group))

ggsave(filename=here("figure_2",
                     "figure2_histology_panels_4_6_month_20230417.pdf"), 
       plot=d1, 
       width=3.0, height=2.8, 
       useDingbats=FALSE) 
```

#### Mef2 staining coverage (fraction) for 4-month and 6-month fish.

``` r
print(d1)
```

![](figure_2_ultrasound_and_histology_files/figure-gfm/print-histology-panels-1.png)<!-- -->

#### Pairwise t-test results for mef2 fraction data.

``` r
group_vec = c("4 months, batch 2", "6 months")

comp_tab = data.table(group1=c("WT", "WT", "HET"),
                      group2=c("HET", "MUT", "MUT"),
                      variable="mef2_fraction",
                      group=rep(group_vec, each=3))


tres_list = list()

for (i in seq(nrow(comp_tab))) {
  row_i = comp_tab[i]
  tmp_group1 = row_i$group1
  tmp_group2 = row_i$group2
  tmp_group = row_i$group
  group1_data = htab[toupper(genotype) == tmp_group1 & group == tmp_group, mef2_fraction]
  
  group2_data = htab[toupper(genotype) == tmp_group2 & group == tmp_group, mef2_fraction]
  
    tmp_res = t.test(x=group1_data,
                     y=group2_data,
                     alternative = "two.sided",
                     conf.level=0.95)
  
    new_row = data.table(group1=tmp_group1,
                         group2=tmp_group2,
                         age_batch=tmp_group,
                         comparison_variable="mef2_fraction",
                         effect_size=tmp_res$estimate[1] - tmp_res$estimate[2], # (mutant or het mean) - wt mean
                         pvalue=tmp_res$p.value,
                         conf_lower=tmp_res$conf.int[1],
                         conf_upper=tmp_res$conf.int[2],
                         sample_size_group1=nrow(group1_data),
                         sample_size_group2=nrow(group2_data),
                         t_statistic=tmp_res$statistic,
                         df=tmp_res$parameter)
 
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
       file=here("figure_2",
                 "pairwise_ttest_results_mef2_fraction_4_6_months_20230417.txt"), 
       sep="\t")
```

``` r
  stats_tab[, list(genotype_pair, age_batch, comparison_variable, padj_format, effect_size)] %>% as_tibble() %>%
  gt() %>%
  tab_header(title="Pairwise t-test results for mef2 fraction data.")
```

<div id="vxkkcqscve" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#vxkkcqscve table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#vxkkcqscve thead, #vxkkcqscve tbody, #vxkkcqscve tfoot, #vxkkcqscve tr, #vxkkcqscve td, #vxkkcqscve th {
  border-style: none;
}

#vxkkcqscve p {
  margin: 0;
  padding: 0;
}

#vxkkcqscve .gt_table {
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

#vxkkcqscve .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#vxkkcqscve .gt_title {
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

#vxkkcqscve .gt_subtitle {
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

#vxkkcqscve .gt_heading {
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

#vxkkcqscve .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#vxkkcqscve .gt_col_headings {
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

#vxkkcqscve .gt_col_heading {
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

#vxkkcqscve .gt_column_spanner_outer {
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

#vxkkcqscve .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#vxkkcqscve .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#vxkkcqscve .gt_column_spanner {
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

#vxkkcqscve .gt_spanner_row {
  border-bottom-style: hidden;
}

#vxkkcqscve .gt_group_heading {
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

#vxkkcqscve .gt_empty_group_heading {
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

#vxkkcqscve .gt_from_md > :first-child {
  margin-top: 0;
}

#vxkkcqscve .gt_from_md > :last-child {
  margin-bottom: 0;
}

#vxkkcqscve .gt_row {
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

#vxkkcqscve .gt_stub {
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

#vxkkcqscve .gt_stub_row_group {
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

#vxkkcqscve .gt_row_group_first td {
  border-top-width: 2px;
}

#vxkkcqscve .gt_row_group_first th {
  border-top-width: 2px;
}

#vxkkcqscve .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#vxkkcqscve .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#vxkkcqscve .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#vxkkcqscve .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#vxkkcqscve .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#vxkkcqscve .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#vxkkcqscve .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#vxkkcqscve .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#vxkkcqscve .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#vxkkcqscve .gt_footnotes {
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

#vxkkcqscve .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#vxkkcqscve .gt_sourcenotes {
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

#vxkkcqscve .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#vxkkcqscve .gt_left {
  text-align: left;
}

#vxkkcqscve .gt_center {
  text-align: center;
}

#vxkkcqscve .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#vxkkcqscve .gt_font_normal {
  font-weight: normal;
}

#vxkkcqscve .gt_font_bold {
  font-weight: bold;
}

#vxkkcqscve .gt_font_italic {
  font-style: italic;
}

#vxkkcqscve .gt_super {
  font-size: 65%;
}

#vxkkcqscve .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#vxkkcqscve .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#vxkkcqscve .gt_indent_1 {
  text-indent: 5px;
}

#vxkkcqscve .gt_indent_2 {
  text-indent: 10px;
}

#vxkkcqscve .gt_indent_3 {
  text-indent: 15px;
}

#vxkkcqscve .gt_indent_4 {
  text-indent: 20px;
}

#vxkkcqscve .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_heading">
      <td colspan="5" class="gt_heading gt_title gt_font_normal gt_bottom_border" style>Pairwise t-test results for mef2 fraction data.</td>
    </tr>
    
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="genotype_pair">genotype_pair</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="age_batch">age_batch</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="comparison_variable">comparison_variable</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="padj_format">padj_format</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="effect_size">effect_size</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="genotype_pair" class="gt_row gt_center">WT-HET</td>
<td headers="age_batch" class="gt_row gt_left">4 months, batch 2</td>
<td headers="comparison_variable" class="gt_row gt_left">mef2_fraction</td>
<td headers="padj_format" class="gt_row gt_right">0.66</td>
<td headers="effect_size" class="gt_row gt_right">-0.02168399</td></tr>
    <tr><td headers="genotype_pair" class="gt_row gt_center">WT-MUT</td>
<td headers="age_batch" class="gt_row gt_left">4 months, batch 2</td>
<td headers="comparison_variable" class="gt_row gt_left">mef2_fraction</td>
<td headers="padj_format" class="gt_row gt_right">0.35</td>
<td headers="effect_size" class="gt_row gt_right">-0.03987492</td></tr>
    <tr><td headers="genotype_pair" class="gt_row gt_center">HET-MUT</td>
<td headers="age_batch" class="gt_row gt_left">4 months, batch 2</td>
<td headers="comparison_variable" class="gt_row gt_left">mef2_fraction</td>
<td headers="padj_format" class="gt_row gt_right">0.66</td>
<td headers="effect_size" class="gt_row gt_right">-0.01819093</td></tr>
    <tr><td headers="genotype_pair" class="gt_row gt_center">WT-HET</td>
<td headers="age_batch" class="gt_row gt_left">6 months</td>
<td headers="comparison_variable" class="gt_row gt_left">mef2_fraction</td>
<td headers="padj_format" class="gt_row gt_right">0.66</td>
<td headers="effect_size" class="gt_row gt_right">0.01368578</td></tr>
    <tr><td headers="genotype_pair" class="gt_row gt_center">WT-MUT</td>
<td headers="age_batch" class="gt_row gt_left">6 months</td>
<td headers="comparison_variable" class="gt_row gt_left">mef2_fraction</td>
<td headers="padj_format" class="gt_row gt_right">0.0077</td>
<td headers="effect_size" class="gt_row gt_right">-0.12172493</td></tr>
    <tr><td headers="genotype_pair" class="gt_row gt_center">HET-MUT</td>
<td headers="age_batch" class="gt_row gt_left">6 months</td>
<td headers="comparison_variable" class="gt_row gt_left">mef2_fraction</td>
<td headers="padj_format" class="gt_row gt_right">0.00058</td>
<td headers="effect_size" class="gt_row gt_right">-0.13541071</td></tr>
  </tbody>
  
  
</table>
</div>
