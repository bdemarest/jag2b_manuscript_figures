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
meanbar_linewith = 1.2
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
                             linewidth=meanbar_linewith) +

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
# Problem: patchwork is not printing group titles for each set of panels
# when I try to combine the 3 panels.
```

``` r
print(ultrasound_fig)
```

    ## Warning: Removed 1 rows containing missing values (`geom_point()`).
    ## Removed 1 rows containing missing values (`geom_point()`).

![](figure_2_ultrasound_and_histology_files/figure-gfm/print-ultrasound-figure-in-markdown-1.png)<!-- -->

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

``` r
  stats2 %>% as_tibble() %>%
  gt() %>%
  tab_header(title="Pairwise t-test results within each batch-by-variable combo. FDR-adj p-value < 0.05")
```

<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#zkiwospaia .gt_table {
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

#zkiwospaia .gt_heading {
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

#zkiwospaia .gt_title {
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

#zkiwospaia .gt_subtitle {
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

#zkiwospaia .gt_bottom_border {
  border-bottom-style: solid;
  /* heading.border.bottom.style */
  border-bottom-width: 2px;
  /* heading.border.bottom.width */
  border-bottom-color: #D3D3D3;
  /* heading.border.bottom.color */
}

#zkiwospaia .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  padding-top: 4px;
  padding-bottom: 4px;
}

#zkiwospaia .gt_col_headings {
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

#zkiwospaia .gt_col_heading {
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

#zkiwospaia .gt_sep_right {
  border-right: 5px solid #FFFFFF;
}

#zkiwospaia .gt_group_heading {
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

#zkiwospaia .gt_empty_group_heading {
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

#zkiwospaia .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
  /* row.striping.background_color */
}

#zkiwospaia .gt_from_md > :first-child {
  margin-top: 0;
}

#zkiwospaia .gt_from_md > :last-child {
  margin-bottom: 0;
}

#zkiwospaia .gt_row {
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

#zkiwospaia .gt_stub {
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

#zkiwospaia .gt_summary_row {
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

#zkiwospaia .gt_first_summary_row {
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

#zkiwospaia .gt_grand_summary_row {
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

#zkiwospaia .gt_first_grand_summary_row {
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

#zkiwospaia .gt_table_body {
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

#zkiwospaia .gt_footnotes {
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

#zkiwospaia .gt_footnote {
  margin: 0px;
  font-size: 90%;
  /* footnotes.font.size */
  padding: 4px;
  /* footnotes.padding */
}

#zkiwospaia .gt_sourcenotes {
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

#zkiwospaia .gt_sourcenote {
  font-size: 90%;
  /* source_notes.font.size */
  padding: 4px;
  /* source_notes.padding */
}

#zkiwospaia .gt_left {
  text-align: left;
}

#zkiwospaia .gt_center {
  text-align: center;
}

#zkiwospaia .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#zkiwospaia .gt_font_normal {
  font-weight: normal;
}

#zkiwospaia .gt_font_bold {
  font-weight: bold;
}

#zkiwospaia .gt_font_italic {
  font-style: italic;
}

#zkiwospaia .gt_super {
  font-size: 65%;
}

#zkiwospaia .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>
<div id="zkiwospaia" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;"><table class="gt_table">
  <thead class="gt_header">
    <tr>
      <th colspan="4" class="gt_heading gt_title gt_font_normal gt_center" style>Pairwise t-test results within each batch-by-variable combo. FDR-adj p-value &lt; 0.05</th>
    </tr>
    <tr>
      <th colspan="4" class="gt_heading gt_subtitle gt_font_normal gt_center gt_bottom_border" style></th>
    </tr>
  </thead>
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">age_batch</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">comparison_variable</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">genotype_pair</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">padj_format</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr>
      <td class="gt_row gt_left">4-month old, batch 1</td>
      <td class="gt_row gt_left">Ad_avg</td>
      <td class="gt_row gt_center">WT-MUT</td>
      <td class="gt_row gt_left">0.0098</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_striped">4-month old, batch 1</td>
      <td class="gt_row gt_left gt_striped">Ad_avg</td>
      <td class="gt_row gt_center gt_striped">HET-MUT</td>
      <td class="gt_row gt_left gt_striped">0.039</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">4-month old, batch 1</td>
      <td class="gt_row gt_left">As_avg</td>
      <td class="gt_row gt_center">WT-MUT</td>
      <td class="gt_row gt_left">0.01</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_striped">4-month old, batch 1</td>
      <td class="gt_row gt_left gt_striped">As_avg</td>
      <td class="gt_row gt_center gt_striped">HET-MUT</td>
      <td class="gt_row gt_left gt_striped">0.0068</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">4-month old, batch 2</td>
      <td class="gt_row gt_left">Ad_avg</td>
      <td class="gt_row gt_center">WT-MUT</td>
      <td class="gt_row gt_left">0.0068</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_striped">4-month old, batch 2</td>
      <td class="gt_row gt_left gt_striped">As_avg</td>
      <td class="gt_row gt_center gt_striped">WT-MUT</td>
      <td class="gt_row gt_left gt_striped">0.0068</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">4-month old, batch 2</td>
      <td class="gt_row gt_left">CO</td>
      <td class="gt_row gt_center">HET-MUT</td>
      <td class="gt_row gt_left">0.014</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_striped">6-month old</td>
      <td class="gt_row gt_left gt_striped">Ad_avg</td>
      <td class="gt_row gt_center gt_striped">WT-MUT</td>
      <td class="gt_row gt_left gt_striped">0.01</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">6-month old</td>
      <td class="gt_row gt_left">Ad_avg</td>
      <td class="gt_row gt_center">HET-MUT</td>
      <td class="gt_row gt_left">0.0049</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_striped">6-month old</td>
      <td class="gt_row gt_left gt_striped">As_avg</td>
      <td class="gt_row gt_center gt_striped">WT-MUT</td>
      <td class="gt_row gt_left gt_striped">0.0097</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">6-month old</td>
      <td class="gt_row gt_left">As_avg</td>
      <td class="gt_row gt_center">HET-MUT</td>
      <td class="gt_row gt_left">0.015</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_striped">6-month old</td>
      <td class="gt_row gt_left gt_striped">SA</td>
      <td class="gt_row gt_center gt_striped">HET-MUT</td>
      <td class="gt_row gt_left gt_striped">0.04</td>
    </tr>
  </tbody>
  
  
</table></div>

``` r
  stats_tab %>% as_tibble() %>%
  gt() %>%
  tab_header(title="Pairwise t-test results within each batch-by-variable combo. All results.")
```

<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#ztsxcxvrks .gt_table {
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

#ztsxcxvrks .gt_heading {
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

#ztsxcxvrks .gt_title {
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

#ztsxcxvrks .gt_subtitle {
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

#ztsxcxvrks .gt_bottom_border {
  border-bottom-style: solid;
  /* heading.border.bottom.style */
  border-bottom-width: 2px;
  /* heading.border.bottom.width */
  border-bottom-color: #D3D3D3;
  /* heading.border.bottom.color */
}

#ztsxcxvrks .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  padding-top: 4px;
  padding-bottom: 4px;
}

#ztsxcxvrks .gt_col_headings {
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

#ztsxcxvrks .gt_col_heading {
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

#ztsxcxvrks .gt_sep_right {
  border-right: 5px solid #FFFFFF;
}

#ztsxcxvrks .gt_group_heading {
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

#ztsxcxvrks .gt_empty_group_heading {
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

#ztsxcxvrks .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
  /* row.striping.background_color */
}

#ztsxcxvrks .gt_from_md > :first-child {
  margin-top: 0;
}

#ztsxcxvrks .gt_from_md > :last-child {
  margin-bottom: 0;
}

#ztsxcxvrks .gt_row {
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

#ztsxcxvrks .gt_stub {
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

#ztsxcxvrks .gt_summary_row {
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

#ztsxcxvrks .gt_first_summary_row {
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

#ztsxcxvrks .gt_grand_summary_row {
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

#ztsxcxvrks .gt_first_grand_summary_row {
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

#ztsxcxvrks .gt_table_body {
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

#ztsxcxvrks .gt_footnotes {
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

#ztsxcxvrks .gt_footnote {
  margin: 0px;
  font-size: 90%;
  /* footnotes.font.size */
  padding: 4px;
  /* footnotes.padding */
}

#ztsxcxvrks .gt_sourcenotes {
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

#ztsxcxvrks .gt_sourcenote {
  font-size: 90%;
  /* source_notes.font.size */
  padding: 4px;
  /* source_notes.padding */
}

#ztsxcxvrks .gt_left {
  text-align: left;
}

#ztsxcxvrks .gt_center {
  text-align: center;
}

#ztsxcxvrks .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#ztsxcxvrks .gt_font_normal {
  font-weight: normal;
}

#ztsxcxvrks .gt_font_bold {
  font-weight: bold;
}

#ztsxcxvrks .gt_font_italic {
  font-style: italic;
}

#ztsxcxvrks .gt_super {
  font-size: 65%;
}

#ztsxcxvrks .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>
<div id="ztsxcxvrks" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;"><table class="gt_table">
  <thead class="gt_header">
    <tr>
      <th colspan="15" class="gt_heading gt_title gt_font_normal gt_center" style>Pairwise t-test results within each batch-by-variable combo. All results.</th>
    </tr>
    <tr>
      <th colspan="15" class="gt_heading gt_subtitle gt_font_normal gt_center gt_bottom_border" style></th>
    </tr>
  </thead>
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">group1</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">group2</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">age_batch</th>
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
      <td class="gt_row gt_left">4-month old, batch 1</td>
      <td class="gt_row gt_left">weight_gr</td>
      <td class="gt_row gt_right">1.149206e-02</td>
      <td class="gt_row gt_right">7.798918e-01</td>
      <td class="gt_row gt_right">-0.081047906</td>
      <td class="gt_row gt_right">0.10403203</td>
      <td class="gt_row gt_center">7</td>
      <td class="gt_row gt_center">9</td>
      <td class="gt_row gt_right">0.289693963</td>
      <td class="gt_row gt_right">7.504736</td>
      <td class="gt_row gt_right">0.897458308</td>
      <td class="gt_row gt_center">WT-HET</td>
      <td class="gt_row gt_left">0.9</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_striped">WT</td>
      <td class="gt_row gt_left gt_striped">MUT</td>
      <td class="gt_row gt_left gt_striped">4-month old, batch 1</td>
      <td class="gt_row gt_left gt_striped">weight_gr</td>
      <td class="gt_row gt_right gt_striped">8.296429e-02</td>
      <td class="gt_row gt_right gt_striped">1.003885e-01</td>
      <td class="gt_row gt_right gt_striped">-0.018743254</td>
      <td class="gt_row gt_right gt_striped">0.18467183</td>
      <td class="gt_row gt_center gt_striped">7</td>
      <td class="gt_row gt_center gt_striped">8</td>
      <td class="gt_row gt_right gt_striped">1.788154577</td>
      <td class="gt_row gt_right gt_striped">11.376370</td>
      <td class="gt_row gt_right gt_striped">0.235694665</td>
      <td class="gt_row gt_center gt_striped">WT-MUT</td>
      <td class="gt_row gt_left gt_striped">0.24</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">HET</td>
      <td class="gt_row gt_left">MUT</td>
      <td class="gt_row gt_left">4-month old, batch 1</td>
      <td class="gt_row gt_left">weight_gr</td>
      <td class="gt_row gt_right">7.147222e-02</td>
      <td class="gt_row gt_right">4.060407e-02</td>
      <td class="gt_row gt_right">0.003711067</td>
      <td class="gt_row gt_right">0.13923338</td>
      <td class="gt_row gt_center">9</td>
      <td class="gt_row gt_center">8</td>
      <td class="gt_row gt_right">2.345623024</td>
      <td class="gt_row gt_right">10.144963</td>
      <td class="gt_row gt_right">0.108419957</td>
      <td class="gt_row gt_center">HET-MUT</td>
      <td class="gt_row gt_left">0.11</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_striped">WT</td>
      <td class="gt_row gt_left gt_striped">HET</td>
      <td class="gt_row gt_left gt_striped">4-month old, batch 1</td>
      <td class="gt_row gt_left gt_striped">HR_avg</td>
      <td class="gt_row gt_right gt_striped">-5.797316e+00</td>
      <td class="gt_row gt_right gt_striped">6.692581e-01</td>
      <td class="gt_row gt_right gt_striped">-34.312072721</td>
      <td class="gt_row gt_right gt_striped">22.71744150</td>
      <td class="gt_row gt_center gt_striped">7</td>
      <td class="gt_row gt_center gt_striped">9</td>
      <td class="gt_row gt_right gt_striped">-0.436397414</td>
      <td class="gt_row gt_right gt_striped">13.883909</td>
      <td class="gt_row gt_right gt_striped">0.850727488</td>
      <td class="gt_row gt_center gt_striped">WT-HET</td>
      <td class="gt_row gt_left gt_striped">0.85</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">WT</td>
      <td class="gt_row gt_left">MUT</td>
      <td class="gt_row gt_left">4-month old, batch 1</td>
      <td class="gt_row gt_left">HR_avg</td>
      <td class="gt_row gt_right">-2.895495e+00</td>
      <td class="gt_row gt_right">7.742953e-01</td>
      <td class="gt_row gt_right">-24.656944303</td>
      <td class="gt_row gt_right">18.86595449</td>
      <td class="gt_row gt_center">7</td>
      <td class="gt_row gt_center">8</td>
      <td class="gt_row gt_right">-0.294182063</td>
      <td class="gt_row gt_right">10.607529</td>
      <td class="gt_row gt_right">0.897458308</td>
      <td class="gt_row gt_center">WT-MUT</td>
      <td class="gt_row gt_left">0.9</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_striped">HET</td>
      <td class="gt_row gt_left gt_striped">MUT</td>
      <td class="gt_row gt_left gt_striped">4-month old, batch 1</td>
      <td class="gt_row gt_left gt_striped">HR_avg</td>
      <td class="gt_row gt_right gt_striped">2.901821e+00</td>
      <td class="gt_row gt_right gt_striped">8.091449e-01</td>
      <td class="gt_row gt_right gt_striped">-22.724249034</td>
      <td class="gt_row gt_right gt_striped">28.52789044</td>
      <td class="gt_row gt_center gt_striped">9</td>
      <td class="gt_row gt_center gt_striped">8</td>
      <td class="gt_row gt_right gt_striped">0.246967247</td>
      <td class="gt_row gt_right gt_striped">11.893653</td>
      <td class="gt_row gt_right gt_striped">0.897458308</td>
      <td class="gt_row gt_center gt_striped">HET-MUT</td>
      <td class="gt_row gt_left gt_striped">0.9</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">WT</td>
      <td class="gt_row gt_left">HET</td>
      <td class="gt_row gt_left">4-month old, batch 1</td>
      <td class="gt_row gt_left">Ad_avg</td>
      <td class="gt_row gt_right">1.011032e-01</td>
      <td class="gt_row gt_right">4.849465e-01</td>
      <td class="gt_row gt_right">-0.201179878</td>
      <td class="gt_row gt_right">0.40338635</td>
      <td class="gt_row gt_center">7</td>
      <td class="gt_row gt_center">9</td>
      <td class="gt_row gt_right">0.717369487</td>
      <td class="gt_row gt_right">13.997382</td>
      <td class="gt_row gt_right">0.767690643</td>
      <td class="gt_row gt_center">WT-HET</td>
      <td class="gt_row gt_left">0.77</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_striped">WT</td>
      <td class="gt_row gt_left gt_striped">MUT</td>
      <td class="gt_row gt_left gt_striped">4-month old, batch 1</td>
      <td class="gt_row gt_left gt_striped">Ad_avg</td>
      <td class="gt_row gt_right gt_striped">5.100506e-01</td>
      <td class="gt_row gt_right gt_striped">1.092981e-03</td>
      <td class="gt_row gt_right gt_striped">0.248352744</td>
      <td class="gt_row gt_right gt_striped">0.77174840</td>
      <td class="gt_row gt_center gt_striped">7</td>
      <td class="gt_row gt_center gt_striped">8</td>
      <td class="gt_row gt_right gt_striped">4.233755970</td>
      <td class="gt_row gt_right gt_striped">12.335187</td>
      <td class="gt_row gt_right gt_striped">0.009836831</td>
      <td class="gt_row gt_center gt_striped">WT-MUT</td>
      <td class="gt_row gt_left gt_striped">0.0098</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">HET</td>
      <td class="gt_row gt_left">MUT</td>
      <td class="gt_row gt_left">4-month old, batch 1</td>
      <td class="gt_row gt_left">Ad_avg</td>
      <td class="gt_row gt_right">4.089473e-01</td>
      <td class="gt_row gt_right">8.024382e-03</td>
      <td class="gt_row gt_right">0.124622144</td>
      <td class="gt_row gt_right">0.69327252</td>
      <td class="gt_row gt_center">9</td>
      <td class="gt_row gt_center">8</td>
      <td class="gt_row gt_right">3.080806822</td>
      <td class="gt_row gt_right">14.199344</td>
      <td class="gt_row gt_right">0.039392419</td>
      <td class="gt_row gt_center">HET-MUT</td>
      <td class="gt_row gt_left">0.039</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_striped">WT</td>
      <td class="gt_row gt_left gt_striped">HET</td>
      <td class="gt_row gt_left gt_striped">4-month old, batch 1</td>
      <td class="gt_row gt_left gt_striped">As_avg</td>
      <td class="gt_row gt_right gt_striped">5.251318e-02</td>
      <td class="gt_row gt_right gt_striped">6.774311e-01</td>
      <td class="gt_row gt_right gt_striped">-0.215152964</td>
      <td class="gt_row gt_right gt_striped">0.32017932</td>
      <td class="gt_row gt_center gt_striped">7</td>
      <td class="gt_row gt_center gt_striped">9</td>
      <td class="gt_row gt_right gt_striped">0.425978545</td>
      <td class="gt_row gt_right gt_striped">12.388165</td>
      <td class="gt_row gt_right gt_striped">0.850727488</td>
      <td class="gt_row gt_center gt_striped">WT-HET</td>
      <td class="gt_row gt_left gt_striped">0.85</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">WT</td>
      <td class="gt_row gt_left">MUT</td>
      <td class="gt_row gt_left">4-month old, batch 1</td>
      <td class="gt_row gt_left">As_avg</td>
      <td class="gt_row gt_right">4.836127e-01</td>
      <td class="gt_row gt_right">1.524679e-03</td>
      <td class="gt_row gt_right">0.233858096</td>
      <td class="gt_row gt_right">0.73336736</td>
      <td class="gt_row gt_center">7</td>
      <td class="gt_row gt_center">8</td>
      <td class="gt_row gt_right">4.314001051</td>
      <td class="gt_row gt_right">10.007851</td>
      <td class="gt_row gt_right">0.010291582</td>
      <td class="gt_row gt_center">WT-MUT</td>
      <td class="gt_row gt_left">0.01</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_striped">HET</td>
      <td class="gt_row gt_left gt_striped">MUT</td>
      <td class="gt_row gt_left gt_striped">4-month old, batch 1</td>
      <td class="gt_row gt_left gt_striped">As_avg</td>
      <td class="gt_row gt_right gt_striped">4.310995e-01</td>
      <td class="gt_row gt_right gt_striped">5.030222e-04</td>
      <td class="gt_row gt_right gt_striped">0.224569973</td>
      <td class="gt_row gt_right gt_striped">0.63762912</td>
      <td class="gt_row gt_center gt_striped">9</td>
      <td class="gt_row gt_center gt_striped">8</td>
      <td class="gt_row gt_right gt_striped">4.467268067</td>
      <td class="gt_row gt_right gt_striped">14.329998</td>
      <td class="gt_row gt_right gt_striped">0.006790799</td>
      <td class="gt_row gt_center gt_striped">HET-MUT</td>
      <td class="gt_row gt_left gt_striped">0.0068</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">WT</td>
      <td class="gt_row gt_left">HET</td>
      <td class="gt_row gt_left">4-month old, batch 1</td>
      <td class="gt_row gt_left">CO</td>
      <td class="gt_row gt_right">4.247394e+00</td>
      <td class="gt_row gt_right">4.975453e-01</td>
      <td class="gt_row gt_right">-8.835009398</td>
      <td class="gt_row gt_right">17.32979768</td>
      <td class="gt_row gt_center">7</td>
      <td class="gt_row gt_center">9</td>
      <td class="gt_row gt_right">0.696501254</td>
      <td class="gt_row gt_right">13.964778</td>
      <td class="gt_row gt_right">0.767690643</td>
      <td class="gt_row gt_center">WT-HET</td>
      <td class="gt_row gt_left">0.77</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_striped">WT</td>
      <td class="gt_row gt_left gt_striped">MUT</td>
      <td class="gt_row gt_left gt_striped">4-month old, batch 1</td>
      <td class="gt_row gt_left gt_striped">CO</td>
      <td class="gt_row gt_right gt_striped">2.017389e+00</td>
      <td class="gt_row gt_right gt_striped">6.559486e-01</td>
      <td class="gt_row gt_right gt_striped">-7.862841632</td>
      <td class="gt_row gt_right gt_striped">11.89761904</td>
      <td class="gt_row gt_center gt_striped">7</td>
      <td class="gt_row gt_center gt_striped">8</td>
      <td class="gt_row gt_right gt_striped">0.460351264</td>
      <td class="gt_row gt_right gt_striped">9.202658</td>
      <td class="gt_row gt_right gt_striped">0.850727488</td>
      <td class="gt_row gt_center gt_striped">WT-MUT</td>
      <td class="gt_row gt_left gt_striped">0.85</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">HET</td>
      <td class="gt_row gt_left">MUT</td>
      <td class="gt_row gt_left">4-month old, batch 1</td>
      <td class="gt_row gt_left">CO</td>
      <td class="gt_row gt_right">-2.230005e+00</td>
      <td class="gt_row gt_right">6.725694e-01</td>
      <td class="gt_row gt_right">-13.548468612</td>
      <td class="gt_row gt_right">9.08845774</td>
      <td class="gt_row gt_center">9</td>
      <td class="gt_row gt_center">8</td>
      <td class="gt_row gt_right">-0.434298763</td>
      <td class="gt_row gt_right">10.866165</td>
      <td class="gt_row gt_right">0.850727488</td>
      <td class="gt_row gt_center">HET-MUT</td>
      <td class="gt_row gt_left">0.85</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_striped">WT</td>
      <td class="gt_row gt_left gt_striped">HET</td>
      <td class="gt_row gt_left gt_striped">4-month old, batch 1</td>
      <td class="gt_row gt_left gt_striped">SA</td>
      <td class="gt_row gt_right gt_striped">4.859006e-02</td>
      <td class="gt_row gt_right gt_striped">4.325318e-01</td>
      <td class="gt_row gt_right gt_striped">-0.080563285</td>
      <td class="gt_row gt_right gt_striped">0.17774340</td>
      <td class="gt_row gt_center gt_striped">7</td>
      <td class="gt_row gt_center gt_striped">9</td>
      <td class="gt_row gt_right gt_striped">0.808695791</td>
      <td class="gt_row gt_right gt_striped">13.678249</td>
      <td class="gt_row gt_right gt_striped">0.729897355</td>
      <td class="gt_row gt_center gt_striped">WT-HET</td>
      <td class="gt_row gt_left gt_striped">0.73</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">WT</td>
      <td class="gt_row gt_left">MUT</td>
      <td class="gt_row gt_left">4-month old, batch 1</td>
      <td class="gt_row gt_left">SA</td>
      <td class="gt_row gt_right">2.643785e-02</td>
      <td class="gt_row gt_right">6.214227e-01</td>
      <td class="gt_row gt_right">-0.088063581</td>
      <td class="gt_row gt_right">0.14093927</td>
      <td class="gt_row gt_center">7</td>
      <td class="gt_row gt_center">8</td>
      <td class="gt_row gt_right">0.508059812</td>
      <td class="gt_row gt_right">11.024435</td>
      <td class="gt_row gt_right">0.850727488</td>
      <td class="gt_row gt_center">WT-MUT</td>
      <td class="gt_row gt_left">0.85</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_striped">HET</td>
      <td class="gt_row gt_left gt_striped">MUT</td>
      <td class="gt_row gt_left gt_striped">4-month old, batch 1</td>
      <td class="gt_row gt_left gt_striped">SA</td>
      <td class="gt_row gt_right gt_striped">-2.215221e-02</td>
      <td class="gt_row gt_right gt_striped">6.749881e-01</td>
      <td class="gt_row gt_right gt_striped">-0.133113484</td>
      <td class="gt_row gt_right gt_striped">0.08880906</td>
      <td class="gt_row gt_center gt_striped">9</td>
      <td class="gt_row gt_center gt_striped">8</td>
      <td class="gt_row gt_right gt_striped">-0.428262448</td>
      <td class="gt_row gt_right gt_striped">13.972501</td>
      <td class="gt_row gt_right gt_striped">0.850727488</td>
      <td class="gt_row gt_center gt_striped">HET-MUT</td>
      <td class="gt_row gt_left gt_striped">0.85</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">WT</td>
      <td class="gt_row gt_left">HET</td>
      <td class="gt_row gt_left">4-month old, batch 2</td>
      <td class="gt_row gt_left">weight_gr</td>
      <td class="gt_row gt_right">-3.428571e-04</td>
      <td class="gt_row gt_right">9.955952e-01</td>
      <td class="gt_row gt_right">-0.152487237</td>
      <td class="gt_row gt_right">0.15180152</td>
      <td class="gt_row gt_center">7</td>
      <td class="gt_row gt_center">5</td>
      <td class="gt_row gt_right">-0.005803539</td>
      <td class="gt_row gt_right">4.969426</td>
      <td class="gt_row gt_right">0.995595235</td>
      <td class="gt_row gt_center">WT-HET</td>
      <td class="gt_row gt_left">1</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_striped">WT</td>
      <td class="gt_row gt_left gt_striped">MUT</td>
      <td class="gt_row gt_left gt_striped">4-month old, batch 2</td>
      <td class="gt_row gt_left gt_striped">weight_gr</td>
      <td class="gt_row gt_right gt_striped">7.035714e-02</td>
      <td class="gt_row gt_right gt_striped">3.233557e-02</td>
      <td class="gt_row gt_right gt_striped">0.007151442</td>
      <td class="gt_row gt_right gt_striped">0.13356284</td>
      <td class="gt_row gt_center gt_striped">7</td>
      <td class="gt_row gt_center gt_striped">6</td>
      <td class="gt_row gt_right gt_striped">2.459530373</td>
      <td class="gt_row gt_right gt_striped">10.661858</td>
      <td class="gt_row gt_right gt_striped">0.094848521</td>
      <td class="gt_row gt_center gt_striped">WT-MUT</td>
      <td class="gt_row gt_left gt_striped">0.095</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">HET</td>
      <td class="gt_row gt_left">MUT</td>
      <td class="gt_row gt_left">4-month old, batch 2</td>
      <td class="gt_row gt_left">weight_gr</td>
      <td class="gt_row gt_right">7.070000e-02</td>
      <td class="gt_row gt_right">2.879835e-01</td>
      <td class="gt_row gt_right">-0.081455707</td>
      <td class="gt_row gt_right">0.22285571</td>
      <td class="gt_row gt_center">5</td>
      <td class="gt_row gt_center">6</td>
      <td class="gt_row gt_right">1.184792149</td>
      <td class="gt_row gt_right">5.138865</td>
      <td class="gt_row gt_right">0.536245124</td>
      <td class="gt_row gt_center">HET-MUT</td>
      <td class="gt_row gt_left">0.54</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_striped">WT</td>
      <td class="gt_row gt_left gt_striped">HET</td>
      <td class="gt_row gt_left gt_striped">4-month old, batch 2</td>
      <td class="gt_row gt_left gt_striped">HR_avg</td>
      <td class="gt_row gt_right gt_striped">-2.922971e+00</td>
      <td class="gt_row gt_right gt_striped">8.411525e-01</td>
      <td class="gt_row gt_right gt_striped">-34.627363102</td>
      <td class="gt_row gt_right gt_striped">28.78142042</td>
      <td class="gt_row gt_center gt_striped">7</td>
      <td class="gt_row gt_center gt_striped">5</td>
      <td class="gt_row gt_right gt_striped">-0.205765769</td>
      <td class="gt_row gt_right gt_striped">9.878299</td>
      <td class="gt_row gt_right gt_striped">0.897822681</td>
      <td class="gt_row gt_center gt_striped">WT-HET</td>
      <td class="gt_row gt_left gt_striped">0.9</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">WT</td>
      <td class="gt_row gt_left">MUT</td>
      <td class="gt_row gt_left">4-month old, batch 2</td>
      <td class="gt_row gt_left">HR_avg</td>
      <td class="gt_row gt_right">4.702764e+00</td>
      <td class="gt_row gt_right">7.073076e-01</td>
      <td class="gt_row gt_right">-22.579931994</td>
      <td class="gt_row gt_right">31.98545993</td>
      <td class="gt_row gt_center">7</td>
      <td class="gt_row gt_center">6</td>
      <td class="gt_row gt_right">0.387003761</td>
      <td class="gt_row gt_right">9.469642</td>
      <td class="gt_row gt_right">0.868059290</td>
      <td class="gt_row gt_center">WT-MUT</td>
      <td class="gt_row gt_left">0.87</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_striped">HET</td>
      <td class="gt_row gt_left gt_striped">MUT</td>
      <td class="gt_row gt_left gt_striped">4-month old, batch 2</td>
      <td class="gt_row gt_left gt_striped">HR_avg</td>
      <td class="gt_row gt_right gt_striped">7.625735e+00</td>
      <td class="gt_row gt_right gt_striped">5.238605e-01</td>
      <td class="gt_row gt_right gt_striped">-19.251363831</td>
      <td class="gt_row gt_right gt_striped">34.50283445</td>
      <td class="gt_row gt_center gt_striped">5</td>
      <td class="gt_row gt_center gt_striped">6</td>
      <td class="gt_row gt_right gt_striped">0.670754669</td>
      <td class="gt_row gt_right gt_striped">7.007783</td>
      <td class="gt_row gt_right gt_striped">0.785790686</td>
      <td class="gt_row gt_center gt_striped">HET-MUT</td>
      <td class="gt_row gt_left gt_striped">0.79</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">WT</td>
      <td class="gt_row gt_left">HET</td>
      <td class="gt_row gt_left">4-month old, batch 2</td>
      <td class="gt_row gt_left">Ad_avg</td>
      <td class="gt_row gt_right">-2.441892e-01</td>
      <td class="gt_row gt_right">2.048853e-01</td>
      <td class="gt_row gt_right">-0.674478154</td>
      <td class="gt_row gt_right">0.18609981</td>
      <td class="gt_row gt_center">7</td>
      <td class="gt_row gt_center">5</td>
      <td class="gt_row gt_right">-1.455378436</td>
      <td class="gt_row gt_right">5.039446</td>
      <td class="gt_row gt_right">0.425530992</td>
      <td class="gt_row gt_center">WT-HET</td>
      <td class="gt_row gt_left">0.43</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_striped">WT</td>
      <td class="gt_row gt_left gt_striped">MUT</td>
      <td class="gt_row gt_left gt_striped">4-month old, batch 2</td>
      <td class="gt_row gt_left gt_striped">Ad_avg</td>
      <td class="gt_row gt_right gt_striped">3.510269e-01</td>
      <td class="gt_row gt_right gt_striped">4.667659e-04</td>
      <td class="gt_row gt_right gt_striped">0.195202363</td>
      <td class="gt_row gt_right gt_striped">0.50685149</td>
      <td class="gt_row gt_center gt_striped">7</td>
      <td class="gt_row gt_center gt_striped">6</td>
      <td class="gt_row gt_right gt_striped">4.983188870</td>
      <td class="gt_row gt_right gt_striped">10.565080</td>
      <td class="gt_row gt_right gt_striped">0.006790799</td>
      <td class="gt_row gt_center gt_striped">WT-MUT</td>
      <td class="gt_row gt_left gt_striped">0.0068</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">HET</td>
      <td class="gt_row gt_left">MUT</td>
      <td class="gt_row gt_left">4-month old, batch 2</td>
      <td class="gt_row gt_left">Ad_avg</td>
      <td class="gt_row gt_right">5.952161e-01</td>
      <td class="gt_row gt_right">1.742522e-02</td>
      <td class="gt_row gt_right">0.162847686</td>
      <td class="gt_row gt_right">1.02758451</td>
      <td class="gt_row gt_center">5</td>
      <td class="gt_row gt_center">6</td>
      <td class="gt_row gt_right">3.642816977</td>
      <td class="gt_row gt_right">4.564337</td>
      <td class="gt_row gt_right">0.062730781</td>
      <td class="gt_row gt_center">HET-MUT</td>
      <td class="gt_row gt_left">0.063</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_striped">WT</td>
      <td class="gt_row gt_left gt_striped">HET</td>
      <td class="gt_row gt_left gt_striped">4-month old, batch 2</td>
      <td class="gt_row gt_left gt_striped">As_avg</td>
      <td class="gt_row gt_right gt_striped">-1.646771e-01</td>
      <td class="gt_row gt_right gt_striped">2.691372e-01</td>
      <td class="gt_row gt_right gt_striped">-0.502899540</td>
      <td class="gt_row gt_right gt_striped">0.17354535</td>
      <td class="gt_row gt_center gt_striped">7</td>
      <td class="gt_row gt_center gt_striped">5</td>
      <td class="gt_row gt_right gt_striped">-1.236242104</td>
      <td class="gt_row gt_right gt_striped">5.214799</td>
      <td class="gt_row gt_right gt_striped">0.519050344</td>
      <td class="gt_row gt_center gt_striped">WT-HET</td>
      <td class="gt_row gt_left gt_striped">0.52</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">WT</td>
      <td class="gt_row gt_left">MUT</td>
      <td class="gt_row gt_left">4-month old, batch 2</td>
      <td class="gt_row gt_left">As_avg</td>
      <td class="gt_row gt_right">3.137057e-01</td>
      <td class="gt_row gt_right">2.809377e-04</td>
      <td class="gt_row gt_right">0.191069930</td>
      <td class="gt_row gt_right">0.43634143</td>
      <td class="gt_row gt_center">7</td>
      <td class="gt_row gt_center">6</td>
      <td class="gt_row gt_right">5.807476721</td>
      <td class="gt_row gt_right">8.793165</td>
      <td class="gt_row gt_right">0.006790799</td>
      <td class="gt_row gt_center">WT-MUT</td>
      <td class="gt_row gt_left">0.0068</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_striped">HET</td>
      <td class="gt_row gt_left gt_striped">MUT</td>
      <td class="gt_row gt_left gt_striped">4-month old, batch 2</td>
      <td class="gt_row gt_left gt_striped">As_avg</td>
      <td class="gt_row gt_right gt_striped">4.783828e-01</td>
      <td class="gt_row gt_right gt_striped">1.696068e-02</td>
      <td class="gt_row gt_right gt_striped">0.136714135</td>
      <td class="gt_row gt_right gt_striped">0.82005142</td>
      <td class="gt_row gt_center gt_striped">5</td>
      <td class="gt_row gt_center gt_striped">6</td>
      <td class="gt_row gt_right gt_striped">3.779193895</td>
      <td class="gt_row gt_right gt_striped">4.311213</td>
      <td class="gt_row gt_right gt_striped">0.062730781</td>
      <td class="gt_row gt_center gt_striped">HET-MUT</td>
      <td class="gt_row gt_left gt_striped">0.063</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">WT</td>
      <td class="gt_row gt_left">HET</td>
      <td class="gt_row gt_left">4-month old, batch 2</td>
      <td class="gt_row gt_left">CO</td>
      <td class="gt_row gt_right">-1.246161e+01</td>
      <td class="gt_row gt_right">2.760550e-02</td>
      <td class="gt_row gt_right">-23.239896309</td>
      <td class="gt_row gt_right">-1.68331909</td>
      <td class="gt_row gt_center">7</td>
      <td class="gt_row gt_center">5</td>
      <td class="gt_row gt_right">-2.576692590</td>
      <td class="gt_row gt_right">9.983705</td>
      <td class="gt_row gt_right">0.087688075</td>
      <td class="gt_row gt_center">WT-HET</td>
      <td class="gt_row gt_left">0.088</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_striped">WT</td>
      <td class="gt_row gt_left gt_striped">MUT</td>
      <td class="gt_row gt_left gt_striped">4-month old, batch 2</td>
      <td class="gt_row gt_left gt_striped">CO</td>
      <td class="gt_row gt_right gt_striped">5.825798e+00</td>
      <td class="gt_row gt_right gt_striped">2.643722e-01</td>
      <td class="gt_row gt_right gt_striped">-5.082889588</td>
      <td class="gt_row gt_right gt_striped">16.73448642</td>
      <td class="gt_row gt_center gt_striped">7</td>
      <td class="gt_row gt_center gt_striped">6</td>
      <td class="gt_row gt_right gt_striped">1.176659482</td>
      <td class="gt_row gt_right gt_striped">10.907252</td>
      <td class="gt_row gt_right gt_striped">0.519050344</td>
      <td class="gt_row gt_center gt_striped">WT-MUT</td>
      <td class="gt_row gt_left gt_striped">0.52</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">HET</td>
      <td class="gt_row gt_left">MUT</td>
      <td class="gt_row gt_left">4-month old, batch 2</td>
      <td class="gt_row gt_left">CO</td>
      <td class="gt_row gt_right">1.828741e+01</td>
      <td class="gt_row gt_right">2.302091e-03</td>
      <td class="gt_row gt_right">8.446230621</td>
      <td class="gt_row gt_right">28.12858160</td>
      <td class="gt_row gt_center">5</td>
      <td class="gt_row gt_center">6</td>
      <td class="gt_row gt_right">4.205395621</td>
      <td class="gt_row gt_right">8.975742</td>
      <td class="gt_row gt_right">0.013812544</td>
      <td class="gt_row gt_center">HET-MUT</td>
      <td class="gt_row gt_left">0.014</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_striped">WT</td>
      <td class="gt_row gt_left gt_striped">HET</td>
      <td class="gt_row gt_left gt_striped">4-month old, batch 2</td>
      <td class="gt_row gt_left gt_striped">SA</td>
      <td class="gt_row gt_right gt_striped">-7.951208e-02</td>
      <td class="gt_row gt_right gt_striped">1.446219e-01</td>
      <td class="gt_row gt_right gt_striped">-0.191981208</td>
      <td class="gt_row gt_right gt_striped">0.03295706</td>
      <td class="gt_row gt_center gt_striped">7</td>
      <td class="gt_row gt_center gt_striped">5</td>
      <td class="gt_row gt_right gt_striped">-1.594550776</td>
      <td class="gt_row gt_right gt_striped">9.178245</td>
      <td class="gt_row gt_right gt_striped">0.325399219</td>
      <td class="gt_row gt_center gt_striped">WT-HET</td>
      <td class="gt_row gt_left gt_striped">0.33</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">WT</td>
      <td class="gt_row gt_left">MUT</td>
      <td class="gt_row gt_left">4-month old, batch 2</td>
      <td class="gt_row gt_left">SA</td>
      <td class="gt_row gt_right">3.732125e-02</td>
      <td class="gt_row gt_right">3.948010e-01</td>
      <td class="gt_row gt_right">-0.055690957</td>
      <td class="gt_row gt_right">0.13033345</td>
      <td class="gt_row gt_center">7</td>
      <td class="gt_row gt_center">6</td>
      <td class="gt_row gt_right">0.886761882</td>
      <td class="gt_row gt_right">10.644282</td>
      <td class="gt_row gt_right">0.687717883</td>
      <td class="gt_row gt_center">WT-MUT</td>
      <td class="gt_row gt_left">0.69</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_striped">HET</td>
      <td class="gt_row gt_left gt_striped">MUT</td>
      <td class="gt_row gt_left gt_striped">4-month old, batch 2</td>
      <td class="gt_row gt_left gt_striped">SA</td>
      <td class="gt_row gt_right gt_striped">1.168333e-01</td>
      <td class="gt_row gt_right gt_striped">3.337263e-02</td>
      <td class="gt_row gt_right gt_striped">0.012059946</td>
      <td class="gt_row gt_right gt_striped">0.22160670</td>
      <td class="gt_row gt_center gt_striped">5</td>
      <td class="gt_row gt_center gt_striped">6</td>
      <td class="gt_row gt_right gt_striped">2.610512063</td>
      <td class="gt_row gt_right gt_striped">7.365468</td>
      <td class="gt_row gt_right gt_striped">0.094848521</td>
      <td class="gt_row gt_center gt_striped">HET-MUT</td>
      <td class="gt_row gt_left gt_striped">0.095</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">WT</td>
      <td class="gt_row gt_left">HET</td>
      <td class="gt_row gt_left">6-month old</td>
      <td class="gt_row gt_left">weight_gr</td>
      <td class="gt_row gt_right">3.496429e-02</td>
      <td class="gt_row gt_right">1.917333e-01</td>
      <td class="gt_row gt_right">-0.019421609</td>
      <td class="gt_row gt_right">0.08935018</td>
      <td class="gt_row gt_center">14</td>
      <td class="gt_row gt_center">8</td>
      <td class="gt_row gt_right">1.363327389</td>
      <td class="gt_row gt_right">15.934406</td>
      <td class="gt_row gt_right">0.414143957</td>
      <td class="gt_row gt_center">WT-HET</td>
      <td class="gt_row gt_left">0.41</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_striped">WT</td>
      <td class="gt_row gt_left gt_striped">MUT</td>
      <td class="gt_row gt_left gt_striped">6-month old</td>
      <td class="gt_row gt_left gt_striped">weight_gr</td>
      <td class="gt_row gt_right gt_striped">5.165873e-02</td>
      <td class="gt_row gt_right gt_striped">7.951237e-02</td>
      <td class="gt_row gt_right gt_striped">-0.006680162</td>
      <td class="gt_row gt_right gt_striped">0.10999762</td>
      <td class="gt_row gt_center gt_striped">14</td>
      <td class="gt_row gt_center gt_striped">9</td>
      <td class="gt_row gt_right gt_striped">1.849850999</td>
      <td class="gt_row gt_right gt_striped">19.547782</td>
      <td class="gt_row gt_right gt_striped">0.195166719</td>
      <td class="gt_row gt_center gt_striped">WT-MUT</td>
      <td class="gt_row gt_left gt_striped">0.2</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">HET</td>
      <td class="gt_row gt_left">MUT</td>
      <td class="gt_row gt_left">6-month old</td>
      <td class="gt_row gt_left">weight_gr</td>
      <td class="gt_row gt_right">1.669444e-02</td>
      <td class="gt_row gt_right">3.243707e-01</td>
      <td class="gt_row gt_right">-0.018518265</td>
      <td class="gt_row gt_right">0.05190715</td>
      <td class="gt_row gt_center">8</td>
      <td class="gt_row gt_center">9</td>
      <td class="gt_row gt_right">1.024362398</td>
      <td class="gt_row gt_right">12.984345</td>
      <td class="gt_row gt_right">0.583867280</td>
      <td class="gt_row gt_center">HET-MUT</td>
      <td class="gt_row gt_left">0.58</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_striped">WT</td>
      <td class="gt_row gt_left gt_striped">HET</td>
      <td class="gt_row gt_left gt_striped">6-month old</td>
      <td class="gt_row gt_left gt_striped">HR_avg</td>
      <td class="gt_row gt_right gt_striped">-1.269331e+00</td>
      <td class="gt_row gt_right gt_striped">9.272909e-01</td>
      <td class="gt_row gt_right gt_striped">-30.903283164</td>
      <td class="gt_row gt_right gt_striped">28.36462142</td>
      <td class="gt_row gt_center gt_striped">14</td>
      <td class="gt_row gt_center gt_striped">8</td>
      <td class="gt_row gt_right gt_striped">-0.093157431</td>
      <td class="gt_row gt_right gt_striped">12.199654</td>
      <td class="gt_row gt_right gt_striped">0.944786976</td>
      <td class="gt_row gt_center gt_striped">WT-HET</td>
      <td class="gt_row gt_left gt_striped">0.94</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">WT</td>
      <td class="gt_row gt_left">MUT</td>
      <td class="gt_row gt_left">6-month old</td>
      <td class="gt_row gt_left">HR_avg</td>
      <td class="gt_row gt_right">2.617000e+00</td>
      <td class="gt_row gt_right">8.479436e-01</td>
      <td class="gt_row gt_right">-26.113262759</td>
      <td class="gt_row gt_right">31.34726289</td>
      <td class="gt_row gt_center">14</td>
      <td class="gt_row gt_center">9</td>
      <td class="gt_row gt_right">0.195317116</td>
      <td class="gt_row gt_right">14.037191</td>
      <td class="gt_row gt_right">0.897822681</td>
      <td class="gt_row gt_center">WT-MUT</td>
      <td class="gt_row gt_left">0.9</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_striped">HET</td>
      <td class="gt_row gt_left gt_striped">MUT</td>
      <td class="gt_row gt_left gt_striped">6-month old</td>
      <td class="gt_row gt_left gt_striped">HR_avg</td>
      <td class="gt_row gt_right gt_striped">3.886331e+00</td>
      <td class="gt_row gt_right gt_striped">8.143603e-01</td>
      <td class="gt_row gt_right gt_striped">-30.796112865</td>
      <td class="gt_row gt_right gt_striped">38.56877474</td>
      <td class="gt_row gt_center gt_striped">8</td>
      <td class="gt_row gt_center gt_striped">9</td>
      <td class="gt_row gt_right gt_striped">0.239007786</td>
      <td class="gt_row gt_right gt_striped">14.879312</td>
      <td class="gt_row gt_right gt_striped">0.897458308</td>
      <td class="gt_row gt_center gt_striped">HET-MUT</td>
      <td class="gt_row gt_left gt_striped">0.9</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">WT</td>
      <td class="gt_row gt_left">HET</td>
      <td class="gt_row gt_left">6-month old</td>
      <td class="gt_row gt_left">Ad_avg</td>
      <td class="gt_row gt_right">4.383973e-02</td>
      <td class="gt_row gt_right">5.985582e-01</td>
      <td class="gt_row gt_right">-0.128049792</td>
      <td class="gt_row gt_right">0.21572924</td>
      <td class="gt_row gt_center">14</td>
      <td class="gt_row gt_center">8</td>
      <td class="gt_row gt_right">0.535981552</td>
      <td class="gt_row gt_right">17.930047</td>
      <td class="gt_row gt_right">0.850727488</td>
      <td class="gt_row gt_center">WT-HET</td>
      <td class="gt_row gt_left">0.85</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_striped">WT</td>
      <td class="gt_row gt_left gt_striped">MUT</td>
      <td class="gt_row gt_left gt_striped">6-month old</td>
      <td class="gt_row gt_left gt_striped">Ad_avg</td>
      <td class="gt_row gt_right gt_striped">3.085778e-01</td>
      <td class="gt_row gt_right gt_striped">1.368706e-03</td>
      <td class="gt_row gt_right gt_striped">0.137031171</td>
      <td class="gt_row gt_right gt_striped">0.48012445</td>
      <td class="gt_row gt_center gt_striped">14</td>
      <td class="gt_row gt_center gt_striped">9</td>
      <td class="gt_row gt_right gt_striped">3.777997356</td>
      <td class="gt_row gt_right gt_striped">18.075941</td>
      <td class="gt_row gt_right gt_striped">0.010291582</td>
      <td class="gt_row gt_center gt_striped">WT-MUT</td>
      <td class="gt_row gt_left gt_striped">0.01</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">HET</td>
      <td class="gt_row gt_left">MUT</td>
      <td class="gt_row gt_left">6-month old</td>
      <td class="gt_row gt_left">Ad_avg</td>
      <td class="gt_row gt_right">2.647381e-01</td>
      <td class="gt_row gt_right">9.083225e-05</td>
      <td class="gt_row gt_right">0.158204789</td>
      <td class="gt_row gt_right">0.37127138</td>
      <td class="gt_row gt_center">8</td>
      <td class="gt_row gt_center">9</td>
      <td class="gt_row gt_right">5.299264695</td>
      <td class="gt_row gt_right">14.917398</td>
      <td class="gt_row gt_right">0.004904942</td>
      <td class="gt_row gt_center">HET-MUT</td>
      <td class="gt_row gt_left">0.0049</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_striped">WT</td>
      <td class="gt_row gt_left gt_striped">HET</td>
      <td class="gt_row gt_left gt_striped">6-month old</td>
      <td class="gt_row gt_left gt_striped">As_avg</td>
      <td class="gt_row gt_right gt_striped">3.885680e-02</td>
      <td class="gt_row gt_right gt_striped">4.975773e-01</td>
      <td class="gt_row gt_right gt_striped">-0.078504862</td>
      <td class="gt_row gt_right gt_striped">0.15621846</td>
      <td class="gt_row gt_center gt_striped">14</td>
      <td class="gt_row gt_center gt_striped">8</td>
      <td class="gt_row gt_right gt_striped">0.690990869</td>
      <td class="gt_row gt_right gt_striped">19.839806</td>
      <td class="gt_row gt_right gt_striped">0.767690643</td>
      <td class="gt_row gt_center gt_striped">WT-HET</td>
      <td class="gt_row gt_left gt_striped">0.77</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">WT</td>
      <td class="gt_row gt_left">MUT</td>
      <td class="gt_row gt_left">6-month old</td>
      <td class="gt_row gt_left">As_avg</td>
      <td class="gt_row gt_right">2.078523e-01</td>
      <td class="gt_row gt_right">8.991178e-04</td>
      <td class="gt_row gt_right">0.096083915</td>
      <td class="gt_row gt_right">0.31962074</td>
      <td class="gt_row gt_center">14</td>
      <td class="gt_row gt_center">9</td>
      <td class="gt_row gt_right">3.869542146</td>
      <td class="gt_row gt_right">20.810337</td>
      <td class="gt_row gt_right">0.009710472</td>
      <td class="gt_row gt_center">WT-MUT</td>
      <td class="gt_row gt_left">0.0097</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_striped">HET</td>
      <td class="gt_row gt_left gt_striped">MUT</td>
      <td class="gt_row gt_left gt_striped">6-month old</td>
      <td class="gt_row gt_left gt_striped">As_avg</td>
      <td class="gt_row gt_right gt_striped">1.689955e-01</td>
      <td class="gt_row gt_right gt_striped">2.783756e-03</td>
      <td class="gt_row gt_right gt_striped">0.068522686</td>
      <td class="gt_row gt_right gt_striped">0.26946837</td>
      <td class="gt_row gt_center gt_striped">8</td>
      <td class="gt_row gt_center gt_striped">9</td>
      <td class="gt_row gt_right gt_striped">3.596710725</td>
      <td class="gt_row gt_right gt_striped">14.463904</td>
      <td class="gt_row gt_right gt_striped">0.015032284</td>
      <td class="gt_row gt_center gt_striped">HET-MUT</td>
      <td class="gt_row gt_left gt_striped">0.015</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">WT</td>
      <td class="gt_row gt_left">HET</td>
      <td class="gt_row gt_left">6-month old</td>
      <td class="gt_row gt_left">CO</td>
      <td class="gt_row gt_right">1.672668e+00</td>
      <td class="gt_row gt_right">8.080238e-01</td>
      <td class="gt_row gt_right">-12.543684807</td>
      <td class="gt_row gt_right">15.88902047</td>
      <td class="gt_row gt_center">14</td>
      <td class="gt_row gt_center">8</td>
      <td class="gt_row gt_right">0.246421639</td>
      <td class="gt_row gt_right">18.818802</td>
      <td class="gt_row gt_right">0.897458308</td>
      <td class="gt_row gt_center">WT-HET</td>
      <td class="gt_row gt_left">0.9</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_striped">WT</td>
      <td class="gt_row gt_left gt_striped">MUT</td>
      <td class="gt_row gt_left gt_striped">6-month old</td>
      <td class="gt_row gt_left gt_striped">CO</td>
      <td class="gt_row gt_right gt_striped">1.719828e+01</td>
      <td class="gt_row gt_right gt_striped">2.109048e-02</td>
      <td class="gt_row gt_right gt_striped">2.871598834</td>
      <td class="gt_row gt_right gt_striped">31.52497042</td>
      <td class="gt_row gt_center gt_striped">14</td>
      <td class="gt_row gt_center gt_striped">9</td>
      <td class="gt_row gt_right gt_striped">2.506862855</td>
      <td class="gt_row gt_right gt_striped">19.658086</td>
      <td class="gt_row gt_right gt_striped">0.071180375</td>
      <td class="gt_row gt_center gt_striped">WT-MUT</td>
      <td class="gt_row gt_left gt_striped">0.071</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">HET</td>
      <td class="gt_row gt_left">MUT</td>
      <td class="gt_row gt_left">6-month old</td>
      <td class="gt_row gt_left">CO</td>
      <td class="gt_row gt_right">1.552562e+01</td>
      <td class="gt_row gt_right">1.326690e-02</td>
      <td class="gt_row gt_right">3.737747071</td>
      <td class="gt_row gt_right">27.31348651</td>
      <td class="gt_row gt_center">8</td>
      <td class="gt_row gt_center">9</td>
      <td class="gt_row gt_right">2.807586458</td>
      <td class="gt_row gt_right">14.982356</td>
      <td class="gt_row gt_right">0.055108660</td>
      <td class="gt_row gt_center">HET-MUT</td>
      <td class="gt_row gt_left">0.055</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_striped">WT</td>
      <td class="gt_row gt_left gt_striped">HET</td>
      <td class="gt_row gt_left gt_striped">6-month old</td>
      <td class="gt_row gt_left gt_striped">SA</td>
      <td class="gt_row gt_right gt_striped">4.982929e-03</td>
      <td class="gt_row gt_right gt_striped">9.111531e-01</td>
      <td class="gt_row gt_right gt_striped">-0.087438402</td>
      <td class="gt_row gt_right gt_striped">0.09740426</td>
      <td class="gt_row gt_center gt_striped">14</td>
      <td class="gt_row gt_center gt_striped">8</td>
      <td class="gt_row gt_right gt_striped">0.113137709</td>
      <td class="gt_row gt_right gt_striped">18.302953</td>
      <td class="gt_row gt_right gt_striped">0.944786976</td>
      <td class="gt_row gt_center gt_striped">WT-HET</td>
      <td class="gt_row gt_left gt_striped">0.94</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">WT</td>
      <td class="gt_row gt_left">MUT</td>
      <td class="gt_row gt_left">6-month old</td>
      <td class="gt_row gt_left">SA</td>
      <td class="gt_row gt_right">1.007255e-01</td>
      <td class="gt_row gt_right">4.216332e-02</td>
      <td class="gt_row gt_right">0.003929575</td>
      <td class="gt_row gt_right">0.19752139</td>
      <td class="gt_row gt_center">14</td>
      <td class="gt_row gt_center">9</td>
      <td class="gt_row gt_right">2.169245181</td>
      <td class="gt_row gt_right">20.203543</td>
      <td class="gt_row gt_right">0.108419957</td>
      <td class="gt_row gt_center">WT-MUT</td>
      <td class="gt_row gt_left">0.11</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_striped">HET</td>
      <td class="gt_row gt_left gt_striped">MUT</td>
      <td class="gt_row gt_left gt_striped">6-month old</td>
      <td class="gt_row gt_left gt_striped">SA</td>
      <td class="gt_row gt_right gt_striped">9.574256e-02</td>
      <td class="gt_row gt_right gt_striped">8.935434e-03</td>
      <td class="gt_row gt_right gt_striped">0.027852736</td>
      <td class="gt_row gt_right gt_striped">0.16363238</td>
      <td class="gt_row gt_center gt_striped">8</td>
      <td class="gt_row gt_center gt_striped">9</td>
      <td class="gt_row gt_right gt_striped">3.011623937</td>
      <td class="gt_row gt_right gt_striped">14.679958</td>
      <td class="gt_row gt_right gt_striped">0.040209455</td>
      <td class="gt_row gt_center gt_striped">HET-MUT</td>
      <td class="gt_row gt_left gt_striped">0.04</td>
    </tr>
  </tbody>
  
  
</table></div>
