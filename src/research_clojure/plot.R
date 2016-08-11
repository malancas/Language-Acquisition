library(magrittr)
library(ggplot2)
library(reshape2)
library(dplyr)

# load the data and have a look at the first few entries
full_path_to_file <- "~/Downloads/out.csv"
dd <- read.csv(full_path_to_file, stringsAsFactors=FALSE)
dd %>% head

## plot 1 with error bars (may be a bug as the error bars are not symetric. This is not the plot you want anyways.

## "melt" data from wide to long, group by variable then take the mean and the standard error of each variable. 
dd_clean <- dd %>%
            melt(., id="observation") %>%
            group_by(variable) %>%
            summarise(averageSentences=mean(value),
            twoSD=2*sd(value))

ggplot(dd_clean, aes(x=variable, y=averageSentences, col=variable)) =
                 geom_point() +
                 geom_errorbar(aes(ymin=-twoSD,
                                   ymax=twoSD))

## plot 2. boxplots make a bit more sense here

## melt the data, but there is not no need to summarise further, as the boxplot will do so.
dd_long <- dd %>%  melt(., id="observation")
ggplot(dd_long, aes(x=factor(variable), y=value, col=variable)) +
                geom_boxplot() +
                xlab("parameter") +
                ylab("iterations at convergence") +
                theme(legend.position="none")
