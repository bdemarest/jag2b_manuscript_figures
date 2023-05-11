figure 3 - histology vs. ultrasound
================
Bradley Demarest
2023-05-11

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
# Possible fix for github markdown / gt problem:
# https://github.com/rstudio/gt/issues/104
# May not work until I update R and related packages.

dcast(data=htab, genotype ~ group, fun.aggregate=length) %>% 
  as_tibble() %>%
  gt() %>%
  tab_header(title="Histology sample summary") %>%
  as_raw_html()
```

<div id="smgcuggfih" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
  
  <table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false" style="-webkit-font-smoothing: antialiased; -moz-osx-font-smoothing: grayscale; font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji'; display: table; border-collapse: collapse; line-height: normal; margin-left: auto; margin-right: auto; color: #333333; font-size: 16px; font-weight: normal; font-style: normal; background-color: #FFFFFF; width: auto; border-top-style: solid; border-top-width: 2px; border-top-color: #A8A8A8; border-right-style: none; border-right-width: 2px; border-right-color: #D3D3D3; border-bottom-style: solid; border-bottom-width: 2px; border-bottom-color: #A8A8A8; border-left-style: none; border-left-width: 2px; border-left-color: #D3D3D3;" bgcolor="#FFFFFF">
  <thead style="border-style: none;">
    <tr class="gt_heading" style="border-style: none; background-color: #FFFFFF; text-align: center; border-bottom-color: #FFFFFF; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3;" bgcolor="#FFFFFF" align="center">
      <td colspan="3" class="gt_heading gt_title gt_font_normal gt_bottom_border" style="border-style: none; color: #333333; font-size: 125%; padding-top: 4px; padding-bottom: 4px; padding-left: 5px; padding-right: 5px; background-color: #FFFFFF; text-align: center; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; border-bottom-style: solid; border-bottom-width: 2px; border-bottom-color: #D3D3D3; font-weight: normal;" bgcolor="#FFFFFF" align="center">Histology sample summary</td>
    </tr>
    
    <tr class="gt_col_headings" style="border-style: none; border-top-style: solid; border-top-width: 2px; border-top-color: #D3D3D3; border-bottom-style: solid; border-bottom-width: 2px; border-bottom-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3;">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="genotype" style="border-style: none; color: #333333; background-color: #FFFFFF; font-size: 100%; font-weight: normal; text-transform: inherit; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: bottom; padding-top: 5px; padding-bottom: 6px; padding-left: 5px; padding-right: 5px; overflow-x: hidden; text-align: left;" bgcolor="#FFFFFF" valign="bottom" align="left">genotype</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="4 months, batch 2" style="border-style: none; color: #333333; background-color: #FFFFFF; font-size: 100%; font-weight: normal; text-transform: inherit; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: bottom; padding-top: 5px; padding-bottom: 6px; padding-left: 5px; padding-right: 5px; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums;" bgcolor="#FFFFFF" valign="bottom" align="right">4 months, batch 2</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="6 months" style="border-style: none; color: #333333; background-color: #FFFFFF; font-size: 100%; font-weight: normal; text-transform: inherit; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: bottom; padding-top: 5px; padding-bottom: 6px; padding-left: 5px; padding-right: 5px; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums;" bgcolor="#FFFFFF" valign="bottom" align="right">6 months</th>
    </tr>
  </thead>
  <tbody class="gt_table_body" style="border-style: none; border-top-style: solid; border-top-width: 2px; border-top-color: #D3D3D3; border-bottom-style: solid; border-bottom-width: 2px; border-bottom-color: #D3D3D3;">
    <tr style="border-style: none;"><td headers="genotype" class="gt_row gt_left" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: left;" valign="middle" align="left">het</td>
<td headers="4 months, batch 2" class="gt_row gt_right" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums;" valign="middle" align="right">3</td>
<td headers="6 months" class="gt_row gt_right" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums;" valign="middle" align="right">5</td></tr>
    <tr style="border-style: none;"><td headers="genotype" class="gt_row gt_left" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: left;" valign="middle" align="left">mut</td>
<td headers="4 months, batch 2" class="gt_row gt_right" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums;" valign="middle" align="right">4</td>
<td headers="6 months" class="gt_row gt_right" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums;" valign="middle" align="right">7</td></tr>
    <tr style="border-style: none;"><td headers="genotype" class="gt_row gt_left" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: left;" valign="middle" align="left">wt</td>
<td headers="4 months, batch 2" class="gt_row gt_right" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums;" valign="middle" align="right">5</td>
<td headers="6 months" class="gt_row gt_right" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums;" valign="middle" align="right">7</td></tr>
  </tbody>
  
  
</table>
</div>

#### Histology vs. ultrasound data for n = 17 6-month old fish.

``` r
# Fix the fish_id column in 'htab'.
htab[, fish_id:=str_remove(fish_id2_fct, "^.+_")]
htab[, fish_id:=str_replace(fish_id, "F", "f")]

ctab = merge(utab, 
             htab[, list(fish_id, age, genotype=toupper(genotype), 
                         genotype2, mef2_fraction)], 
             by=c("fish_id", "age", "genotype2", "genotype"))
# 
# ctab should have 17 fish (fish_ids that have both ultrasound and
# histology data). Currently only 16 fish are included.

# 17 Apr 2023 Leo audited fish f0118 because it was
# recorded as WT in the ultrasound data file, and as HET in the 
# histology data. 
# Lab notebook shows that correct f0118 genotype is WT.


# f0118 fish genotype needs to be corrected:
# [ ] (1) Need to correct excel sheet 
#         mef2_histology_analysis_jag2b_6_month_2023-03-09_correct_age.xlsx
# [ ] (2) Need to re-run .Rmd and commit and push to
#         https://github.com/bdemarest/jag2b_mef2_histology
# [XX] (3) Need to correct
#         ~/work/lalmero/jag2b_manuscript_figures/figure_2/jag2b_mef2_summarized_histology_4_6_8_month_n38_20230310.txt
# [ ] (4) re-run, commit and push to 
#         https://github.com/bdemarest/jag2b_manuscript_figures



# EDA.
# Plot weight, len, all ultrasound variables, against mef2_fraction.

# Measurement units notes:
# Weight: grams
# Length: centimeters
# Ventricle areas: mm-squared
# Heartrate: beats per minute
# Inflow, outflow: mm per second
# Stroke area: mm-squared
# Ejection fraction: dimensionless
# Cardiac Output: mm-squared per minute

     
xvar_vec = c("weight_gr", "length_cm", "Ad_avg", "As_avg",
             "HR_avg", "inflow_avg", "outflow_avg", "SA",
             "EFA", "CO")

xvar_labels = c("'Weight (grams)'",
                "'Length (cm)'",
                "'Ventricle area, diastole '('mm'^2)",
                "'Ventricle area, systole '('mm'^2)",
                "'Heart rate '('minute'^-1)",
                "'Inflow '('mm' %.% 'sec'^-1)",
                "'Outflow '('mm' %.% 'sec'^-1)",
                "'Stroke area '('mm'^2)",
                "'Ejection fraction '('area')", 
                "'Cardiac output '('mm'^2 %.% 'minute'^-1)")


genotype2_colors  = c("+/+"="#80b1d3",
                      "+/-"="#b3de69",
                      "-/-"="#fb8072")

point_size = 2.6
point_outline_color = "grey30"
fit_line_width = 1.2
fit_line_color = "grey60"
fit_line_width_small = 0.8
xtitle_scale = 0.9



# Expand y-axis 5% on bottom end and 25% on upper end, to make room for p-values.
yaxis_expand = expansion(mult=c(0.05, 0.25))

#mef2_axis_label = "Myocardium density"
mef2_axis_label = "Fraction of ventricle covered\nin trabecular myocardium"

plot_page_title = "jag2b histology and ultrasound data, 6-month, n = 17."
  
plt_list = list()


for (i in seq_along(xvar_vec)) {
  tmp_xvar = xvar_vec[i]
  tmp_label = xvar_labels[i] 
  
  tmp_plot = ggplot(data=ctab,
                    aes(x=!!as.name(tmp_xvar),
                        y=mef2_fraction,
                        color=genotype2,
                        fill=genotype2)) +
             theme_bw() +
             geom_point(size=point_size,
                        shape=21,
                        color=point_outline_color) +
             scale_fill_manual(values=genotype2_colors) +
             scale_color_manual(values=genotype2_colors) +
             guides(color="none") +
             labs(x=parse(text=tmp_label)) + 
             labs(y=mef2_axis_label) +
             theme(axis.title.x=element_text(size=rel(xtitle_scale)))

   
  
  plt_list[[i]] = tmp_plot
  
}


p01 =  wrap_plots(plt_list, nrow=3) + 
       plot_layout(guides="collect") +
       plot_annotation(title=plot_page_title)


#---------------
# p02, linear fits for overall + individual genotypes.

plt_list2 = list()

for (i in seq_along(xvar_vec)) {
  tmp_xvar = xvar_vec[i]
  tmp_label = xvar_labels[i] 
  
  tmp_plot = ggplot(data=ctab,
                    aes(x=!!as.name(tmp_xvar),
                        y=mef2_fraction,
                        color=genotype2,
                        fill=genotype2)) +
             theme_bw() +
             geom_smooth(aes(group=1),
                         method="lm",
                         se=FALSE,
                         linewidth=fit_line_width,
                         color=fit_line_color,
                         show.legend=FALSE) +
             geom_smooth(method="lm",
                         se=FALSE,
                         linewidth=fit_line_width_small,
                         show.legend=FALSE) +
             geom_point(size=point_size,
                        shape=21,
                        color=fit_line_color) +
             scale_fill_manual(values=genotype2_colors) +
             scale_color_manual(values=genotype2_colors) +
             guides(color="none") +
             labs(x=parse(text=tmp_label)) + 
             labs(y=mef2_axis_label) +
             theme(axis.title.x=element_text(size=rel(xtitle_scale)))
   
  
  plt_list2[[i]] = tmp_plot
  
}

p02 =  wrap_plots(plt_list2, nrow=3) + 
       plot_layout(guides="collect") +
       plot_annotation(title=plot_page_title)


#---------------
# p03, linear fits for overall + individual genotypes, with std error ribbons for both.

plt_list3 = list()

for (i in seq_along(xvar_vec)) {
  tmp_xvar = xvar_vec[i]
  tmp_label = xvar_labels[i] 
  
  tmp_plot = ggplot(data=ctab,
                    aes(x=!!as.name(tmp_xvar),
                        y=mef2_fraction,
                        color=genotype2,
                        fill=genotype2)) +
             theme_bw() +
             geom_smooth(aes(group=1),
                         method="lm",
                         se=TRUE,
                         linewidth=fit_line_width,
                         color=fit_line_color,
                         show.legend=FALSE) +
             geom_smooth(method="lm",
                         se=TRUE,
                         linewidth=fit_line_width_small,
                         show.legend=FALSE) +
             geom_point(size=point_size,
                        shape=21,
                        color=point_outline_color) +
             scale_fill_manual(values=genotype2_colors) +
             scale_color_manual(values=genotype2_colors) +
             guides(color="none") +
             labs(x=parse(text=tmp_label)) + 
             labs(y=mef2_axis_label) +
             theme(axis.title.x=element_text(size=rel(xtitle_scale)))
   
  
  plt_list3[[i]] = tmp_plot
  
}

p03 =  wrap_plots(plt_list3, nrow=3) + 
       plot_layout(guides="collect") +
       plot_annotation(title=plot_page_title)

#---------------
# p04, linear fits for overall only, with std error ribbon.

plt_list4 = list()

for (i in seq_along(xvar_vec)) {
  tmp_xvar = xvar_vec[i]
  tmp_label = xvar_labels[i] 
  
  tmp_plot = ggplot(data=ctab,
                    aes(x=!!as.name(tmp_xvar),
                        y=mef2_fraction,
                        color=genotype2,
                        fill=genotype2)) +
             theme_bw() +
             geom_smooth(aes(group=1),
                         method="lm",
                         se=TRUE,
                         linewidth=fit_line_width,
                         color=fit_line_color,
                         show.legend=FALSE) +
             geom_point(size=point_size,
                        shape=21,
                        color=point_outline_color) +
             scale_fill_manual(values=genotype2_colors) +
             scale_color_manual(values=genotype2_colors) +
             guides(color="none") +
             labs(x=parse(text=tmp_label)) + 
             labs(y=mef2_axis_label) +
             theme(axis.title.x=element_text(size=rel(xtitle_scale)))
   
  
  plt_list4[[i]] = tmp_plot
  
}

p04 =  wrap_plots(plt_list4, nrow=3) + 
       plot_layout(guides="collect") +
       plot_annotation(title=plot_page_title)




#---------------
# p05, linear fits for overall only, _without_ std error ribbon.

plt_list5 = list()

for (i in seq_along(xvar_vec)) {
  tmp_xvar = xvar_vec[i]
  tmp_label = xvar_labels[i] 
  
  tmp_plot = ggplot(data=ctab,
                    aes(x=!!as.name(tmp_xvar),
                        y=mef2_fraction,
                        color=genotype2,
                        fill=genotype2)) +
             theme_bw() +
             geom_smooth(aes(group=1),
                         method="lm",
                         se=FALSE,
                         linewidth=fit_line_width,
                         color=fit_line_color,
                         show.legend=FALSE) +
             geom_point(size=point_size,
                        shape=21,
                        color=point_outline_color) +
             scale_fill_manual(values=genotype2_colors) +
             scale_color_manual(values=genotype2_colors) +
             guides(color="none") +
             labs(x=parse(text=tmp_label)) + 
             labs(y=mef2_axis_label) +
             theme(axis.title.x=element_text(size=rel(xtitle_scale)))
   
  
  plt_list5[[i]] = tmp_plot
  
}

p05 =  wrap_plots(plt_list5, nrow=3) + 
       plot_layout(guides="collect") +
       plot_annotation(title=plot_page_title)



#---------------
# p06, linear fits only for individual genotypes.

plt_list6 = list()

for (i in seq_along(xvar_vec)) {
  tmp_xvar = xvar_vec[i]
  tmp_label = xvar_labels[i] 
  
  tmp_plot = ggplot(data=ctab,
                    aes(x=!!as.name(tmp_xvar),
                        y=mef2_fraction,
                        color=genotype2,
                        fill=genotype2)) +
             theme_bw() +
             geom_smooth(method="lm",
                         se=FALSE,
                         linewidth=fit_line_width_small,
                         show.legend=FALSE) +
             geom_point(size=point_size,
                        shape=21,
                        color=fit_line_color) +
             scale_fill_manual(values=genotype2_colors) +
             scale_color_manual(values=genotype2_colors) +
             guides(color="none") +
             labs(x=parse(text=tmp_label)) + 
             labs(y=mef2_axis_label) +
             theme(axis.title.x=element_text(size=rel(xtitle_scale)))
   
  
  plt_list6[[i]] = tmp_plot
  
}

p06 =  wrap_plots(plt_list6, nrow=3) + 
       plot_layout(guides="collect") +
       plot_annotation(title=plot_page_title)

#---------------
# p07, linear fits for individual genotypes, with std error ribbons.

plt_list7 = list()

for (i in seq_along(xvar_vec)) {
  tmp_xvar = xvar_vec[i]
  tmp_label = xvar_labels[i] 
  
  tmp_plot = ggplot(data=ctab,
                    aes(x=!!as.name(tmp_xvar),
                        y=mef2_fraction,
                        color=genotype2,
                        fill=genotype2)) +
             theme_bw() +
             geom_smooth(method="lm",
                         se=TRUE,
                         linewidth=fit_line_width_small,
                         show.legend=FALSE) +
             geom_point(size=point_size,
                        shape=21,
                        color=point_outline_color) +
             scale_fill_manual(values=genotype2_colors) +
             scale_color_manual(values=genotype2_colors) +
             guides(color="none") +
             labs(x=parse(text=tmp_label)) + 
             labs(y=mef2_axis_label) +
             theme(axis.title.x=element_text(size=rel(xtitle_scale)))
   
  
  plt_list7[[i]] = tmp_plot
  
}

p07 =  wrap_plots(plt_list7, nrow=3) + 
       plot_layout(guides="collect") +
       plot_annotation(title=plot_page_title)


#---------------
# p08, Remove hets. linear fits for individual genotypes.

plt_list8 = list()

for (i in seq_along(xvar_vec)) {
  tmp_xvar = xvar_vec[i]
  tmp_label = xvar_labels[i] 
  
  tmp_plot = ggplot(data=ctab[!genotype %in% "HET"],
                    aes(x=!!as.name(tmp_xvar),
                        y=mef2_fraction,
                        color=genotype2,
                        fill=genotype2)) +
             theme_bw() +
             geom_smooth(method="lm",
                         se=FALSE,
                         linewidth=fit_line_width_small,
                         show.legend=FALSE) +
             geom_point(size=point_size,
                        shape=21,
                        color=fit_line_color) +
             scale_fill_manual(values=genotype2_colors) +
             scale_color_manual(values=genotype2_colors) +
             guides(color="none") +
             labs(x=parse(text=tmp_label)) + 
             labs(y=mef2_axis_label) +
             theme(axis.title.x=element_text(size=rel(xtitle_scale)))
   
  
  plt_list8[[i]] = tmp_plot
  
}

p08 =  wrap_plots(plt_list8, nrow=3) + 
       plot_layout(guides="collect") +
       plot_annotation(title=plot_page_title)

#---------------
# p09, Remove hets. 
# linear fits for individual genotypes, with std error ribbons.

plt_list9 = list()

for (i in seq_along(xvar_vec)) {
  tmp_xvar = xvar_vec[i]
  tmp_label = xvar_labels[i] 
  
  tmp_plot = ggplot(data=ctab[!genotype %in% "HET"],
                    aes(x=!!as.name(tmp_xvar),
                        y=mef2_fraction,
                        color=genotype2,
                        fill=genotype2)) +
             theme_bw() +
             geom_smooth(method="lm",
                         se=TRUE,
                         linewidth=fit_line_width_small,
                         show.legend=FALSE) +
             geom_point(size=point_size,
                        shape=21,
                        color=point_outline_color) +
             scale_fill_manual(values=genotype2_colors) +
             scale_color_manual(values=genotype2_colors) +
             guides(color="none") +
             labs(x=parse(text=tmp_label)) + 
             labs(y=mef2_axis_label) +
             theme(axis.title.x=element_text(size=rel(xtitle_scale)))
   
  
  plt_list9[[i]] = tmp_plot
  
}

p09 =  wrap_plots(plt_list9, nrow=3) + 
       plot_layout(guides="collect") +
       plot_annotation(title=plot_page_title)
```

``` r
# 2 panels wide and 5 panels tall.
# Weight and length, As and Ad, inflow and outflow, SA and HR, EFA and CO

pdf(here("figure_3",
         "mef2_fraction_vs_10ultrasound_vars_6month_n17_all_versions_20230511.pdf"),
         width=12, height=8, useDingbats=FALSE)

print(p06)
print(p07)
print(p08)
print(p09)


print(p05)
print(p04)
print(p01)
print(p02)
print(p03)


dev.off()
```

    ## quartz_off_screen 
    ##                 2

#### Second set of plots, formatted vertically.

2 panels wide, 5 panels tall.

#### Histology vs. ultrasound data for n = 17 6-month old fish.

``` r
# Fix the fish_id column in 'htab'.
htab[, fish_id:=str_remove(fish_id2_fct, "^.+_")]
htab[, fish_id:=str_replace(fish_id, "F", "f")]

ctab = merge(utab, 
             htab[, list(fish_id, age, genotype=toupper(genotype), 
                         genotype2, mef2_fraction)], 
             by=c("fish_id", "age", "genotype2", "genotype"))


# EDA.
# Plot weight, len, all ultrasound variables, against mef2_fraction.

# Measurement units notes:
# Weight: grams
# Length: centimeters
# Ventricle areas: mm-squared
# Heartrate: beats per minute
# Inflow, outflow: mm per second
# Stroke area: mm-squared
# Ejection fraction: dimensionless
# Cardiac Output: mm-squared per minute


# Rearrange order of panels, to match:
# Weight and length, As and Ad, inflow and outflow, SA and HR, EFA and CO

xvar_vec = c("weight_gr", "length_cm", 
             "Ad_avg", "As_avg",
             "inflow_avg", "outflow_avg", 
             "SA", "HR_avg",
             "EFA", "CO")

xvar_labels = c("'Weight (grams)'",
                "'Length (cm)'",
                "'Ventricle area, diastole '('mm'^2)",
                "'Ventricle area, systole '('mm'^2)",
                "'Inflow '('mm' %.% 'sec'^-1)",
                "'Outflow '('mm' %.% 'sec'^-1)",
                "'Stroke area '('mm'^2)",
                "'Heart rate '('minute'^-1)",
                "'Ejection fraction '('area')", 
                "'Cardiac output '('mm'^2 %.% 'minute'^-1)")


genotype2_colors  = c("+/+"="#80b1d3",
                      "+/-"="#b3de69",
                      "-/-"="#fb8072")

point_size = 2.6
point_outline_color = "grey30"
fit_line_width = 1.2
fit_line_color = "grey60"
fit_line_width_small = 0.8
xtitle_scale = 0.9



# Expand y-axis 5% on bottom end and 25% on upper end, to make room for p-values.
yaxis_expand = expansion(mult=c(0.05, 0.25))

#mef2_axis_label = "Myocardium density"
mef2_axis_label = "Fraction of ventricle covered\nin trabecular myocardium"

plot_page_title = "jag2b histology and ultrasound data, 6-month, n = 17."
  


#---------------
# p06, linear fits only for individual genotypes.

plt_list6 = list()

for (i in seq_along(xvar_vec)) {
  tmp_xvar = xvar_vec[i]
  tmp_label = xvar_labels[i] 
  
  tmp_plot = ggplot(data=ctab,
                    aes(x=!!as.name(tmp_xvar),
                        y=mef2_fraction,
                        color=genotype2,
                        fill=genotype2)) +
             theme_bw() +
             geom_smooth(method="lm",
                         se=FALSE,
                         linewidth=fit_line_width_small,
                         show.legend=FALSE) +
             geom_point(size=point_size,
                        shape=21,
                        color=fit_line_color) +
             scale_fill_manual(values=genotype2_colors) +
             scale_color_manual(values=genotype2_colors) +
             guides(color="none") +
             labs(x=parse(text=tmp_label)) + 
             labs(y=mef2_axis_label) +
             theme(axis.title.x=element_text(size=rel(xtitle_scale)))
   
  
  plt_list6[[i]] = tmp_plot
  
}

p06 =  wrap_plots(plt_list6, nrow=5) + 
       plot_layout(guides="collect") +
       plot_annotation(title=plot_page_title)

#---------------
# p07, linear fits for individual genotypes, with std error ribbons.

plt_list7 = list()

for (i in seq_along(xvar_vec)) {
  tmp_xvar = xvar_vec[i]
  tmp_label = xvar_labels[i] 
  
  tmp_plot = ggplot(data=ctab,
                    aes(x=!!as.name(tmp_xvar),
                        y=mef2_fraction,
                        color=genotype2,
                        fill=genotype2)) +
             theme_bw() +
             geom_smooth(method="lm",
                         se=TRUE,
                         linewidth=fit_line_width_small,
                         show.legend=FALSE) +
             geom_point(size=point_size,
                        shape=21,
                        color=point_outline_color) +
             scale_fill_manual(values=genotype2_colors) +
             scale_color_manual(values=genotype2_colors) +
             guides(color="none") +
             labs(x=parse(text=tmp_label)) + 
             labs(y=mef2_axis_label) +
             theme(axis.title.x=element_text(size=rel(xtitle_scale)))
   
  
  plt_list7[[i]] = tmp_plot
  
}

p07 =  wrap_plots(plt_list7, nrow=5) + 
       plot_layout(guides="collect") +
       plot_annotation(title=plot_page_title)


#---------------
# p08, Remove hets. linear fits for individual genotypes.

plt_list8 = list()

for (i in seq_along(xvar_vec)) {
  tmp_xvar = xvar_vec[i]
  tmp_label = xvar_labels[i] 
  
  tmp_plot = ggplot(data=ctab[!genotype %in% "HET"],
                    aes(x=!!as.name(tmp_xvar),
                        y=mef2_fraction,
                        color=genotype2,
                        fill=genotype2)) +
             theme_bw() +
             geom_smooth(method="lm",
                         se=FALSE,
                         linewidth=fit_line_width_small,
                         show.legend=FALSE) +
             geom_point(size=point_size,
                        shape=21,
                        color=fit_line_color) +
             scale_fill_manual(values=genotype2_colors) +
             scale_color_manual(values=genotype2_colors) +
             guides(color="none") +
             labs(x=parse(text=tmp_label)) + 
             labs(y=mef2_axis_label) +
             theme(axis.title.x=element_text(size=rel(xtitle_scale)))
   
  
  plt_list8[[i]] = tmp_plot
  
}

p08 =  wrap_plots(plt_list8, nrow=5) + 
       plot_layout(guides="collect") +
       plot_annotation(title=plot_page_title)

#---------------
# p09, Remove hets. 
# linear fits for individual genotypes, with std error ribbons.

plt_list9 = list()

for (i in seq_along(xvar_vec)) {
  tmp_xvar = xvar_vec[i]
  tmp_label = xvar_labels[i] 
  
  tmp_plot = ggplot(data=ctab[!genotype %in% "HET"],
                    aes(x=!!as.name(tmp_xvar),
                        y=mef2_fraction,
                        color=genotype2,
                        fill=genotype2)) +
             theme_bw() +
             geom_smooth(method="lm",
                         se=TRUE,
                         linewidth=fit_line_width_small,
                         show.legend=FALSE) +
             geom_point(size=point_size,
                        shape=21,
                        color=point_outline_color) +
             scale_fill_manual(values=genotype2_colors) +
             scale_color_manual(values=genotype2_colors) +
             guides(color="none") +
             labs(x=parse(text=tmp_label)) + 
             labs(y=mef2_axis_label) +
             theme(axis.title.x=element_text(size=rel(xtitle_scale)))
   
  
  plt_list9[[i]] = tmp_plot
  
}

p09 =  wrap_plots(plt_list9, nrow=5) + 
       plot_layout(guides="collect") +
       plot_annotation(title=plot_page_title)
```

``` r
# 2 panels wide and 5 panels tall.
# Weight and length, As and Ad, inflow and outflow, SA and HR, EFA and CO

pdf(here("figure_3",
         "mef2_fraction_vs_10ultrasound_vars_6month_n17_vertical_20230511.pdf"),
         width=6.6, height=13, useDingbats=FALSE)

print(p06)
print(p07)
print(p08)
print(p09)

dev.off()
```

    ## quartz_off_screen 
    ##                 2
