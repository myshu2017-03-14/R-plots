# R-plot

Some scripts for R plots

# Example1 gene_cluster_plot
You should run in Rstudio. And you must install **shiny** and **ggplot2**. 

The running code just as below:

```{r}
library(shiny)
shiny::runGitHub('R-plots', 'myshu2017-03-14',subdir = "R-shiny-apps/gene_cluster_plot")
```

The input file is GFF format.

The results just as below:

![genecluster](/Images/gene_cluster_shot.png)

# Example2 taxa_bar_plot
You should run in Rstudio. And you must install **tidyverse**, **reshape**, **shiny** and **ggplot2**.

The running code just as below:

```{r}
library(shiny)
shiny::runGitHub('R-plots', 'myshu2017-03-14',subdir = "R-shiny-apps/taxa_bar_plot")
```
The input format just a tab file as below:
![genecluster](/Images/taxa_tab_file.png)

The plot just as below:
![genecluster](/Images/bar_plot_shot.png)

You can also see the taxa table:
![genecluster](/Images/bar_plot_shot2.png)

And you can download the plot by Click the **`Download plot`** button. Save the plot as "name.pdf".

# Example3 flower_venn_plot
You should run in Rstudio. And you must install **plotrix**, **shiny** and **ggplot2**.
The code just as below:

```{r}
library(shiny)
shiny::runGitHub('R-plots', 'myshu2017-03-14',subdir = "R-shiny-apps/flower_venn_plot")
```
The input format just a tab file as below:
![genecluster](/Images/taxa_tab_file.png)

The plot just as below:
![genecluster](/Images/flower_venn_plot_shot.png)



