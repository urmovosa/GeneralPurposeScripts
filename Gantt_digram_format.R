# Load neccessary packages
library(data.table)
library(ggplot2)
library(stringr)

# Read in input data
# format: 
# content,group,member,start,end,status
# task1,John,NA,30/01/17,30/02/17,finished
# task2,Team1,NA,30/01/17,30/02/17,not started
# Deadline: task3,John,NA,30/01/17,30/01/17,in progress
dat <- fread('example_timeline.txt')

# Prepare data
dat$start <- as.Date(dat$start, format = '%d/%m/%y')
dat$end <- as.Date(dat$end, format = '%d/%m/%y')
dat <- dat[order(dat$end, decreasing = TRUE), ]

# Here order the individuals/teams
dat$group <- factor(dat$group, levels = c('John', 'Jack', 'Mary', 'Team1'))
dat$content <- factor(dat$content, levels = as.character(dat$content))

# Separately parse deadlines
deadlines <- dat[str_detect(dat$content, 'Deadline'), ]

# Visualize
gantt <- ggplot(data = dat) +
  geom_segment(aes(x = start, xend = end, y = content, yend = content, colour = status), size = 2.5) +
  theme_bw() + theme_bw() + facet_grid(group ~ ., drop = TRUE, scales = "free_y", space = "free") + 
  theme(axis.text = element_text(size = 6), panel.grid.minor.y = element_blank(), 
        panel.grid.major.y = element_blank(), 
        strip.text.y = element_text(angle = 0),
        strip.background = element_rect(colour = "black", fill = "white",
                                        size = 0)) + 
  geom_vline(aes(xintercept = as.numeric(Sys.Date())), colour="red", linetype = 1, size = 0.25) +
  geom_vline(data = deadlines, aes(xintercept = as.numeric(deadlines$start)), colour = 'darkred', linetype = 2) + 
  scale_colour_manual(values = c("finished" = "grey", "in progress" = "lightblue", "not started" = "salmon")) + ylab('') + xlab('')

gantt

# Adjust size and save as .png  
ggsave('gantt_test.png', dpi = 600, width = 8 * 1.3, height = 4 * 1.3, units = 'in')
