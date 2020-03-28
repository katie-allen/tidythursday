# trying to recreate the poster  hanging on my wall,
# but with Jane Eyre instead of P&P

# load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, gutenbergr, tidytext, plotrix)

# pull in the data
# oh wait...what is the jane eyre id?
?gutenberg_download
gutenberg_works() %>% 
  filter(str_detect(title, "Jane")) %>% 
  select(gutenberg_id, title, author)
jane_eyre_raw <- gutenberg_download(1260)

# what does the data look like?
head(jane_eyre_raw,20)
# huh. for now, let's not worry about chapter titles, etc

# count the words 
jane_eyre_counts <- jane_eyre_raw %>% 
  unnest_tokens(output = word,
                input = text,
                token = "words") %>% 
  count(word, sort=TRUE)
jane_eyre_counts

# remove stop words
jane_eyre_counts <- jane_eyre_counts %>% 
  anti_join(stop_words)
jane_eyre_counts

# keep top 10
jane_eyre_top10 <- jane_eyre_counts[1:10,]
jane_eyre_top10

# need an ordered factor for easier plotting
jane_eyre_top10$word <- factor(jane_eyre_top10$word, levels = jane_eyre_top10$word)

# ok, start plotting!

# BASIC BAR PLOT ------------------

jane_eyre_top10 %>% 
  ggplot(aes(x=word, y=n)) +
  geom_bar(stat="identity")


# RADIAL PLOT ---------------------

# following this:
# https://stackoverflow.com/questions/15751442/making-a-circular-barplot-with-a-hollow-center-aka-race-track-plot
jane_eyre_top10 %>% 
  ggplot(aes(x=word, y=n, fill=word)) +
  geom_bar(stat="identity") +
  coord_polar(theta = "y")

# oops, need to flip the order
jane_eyre_top10 %>% 
  ggplot(aes(x=forcats::fct_rev(word), y=n, fill=word)) +
  geom_bar(stat="identity") +
  coord_polar(theta = "y") +
  xlab("") + ylab("") +
  ylim(c(0,500)) +
  ggtitle("This is a radial plot") +
  theme_minimal() +
  theme(legend.position = "none")


# RADIAL PLOT w/ BLANK SPACE ---------------

jane_eyre_top10
len <- 4
temp <- tibble(word = letters[1:len], 
              n = rep(0, len), 
              name = rep("", len))
jane_eyre_top10$name <- str_to_title(jane_eyre_top10$word)
jane_eyre_wBlank <- rbind(jane_eyre_top10,temp)  
jane_eyre_wBlank

# set factor so it will plot in descending order 
jane_eyre_wBlank$word <-
  factor(jane_eyre_wBlank$word, 
         levels=rev(jane_eyre_wBlank$word))

# ok now plot again
jane_eyre_wBlank
jane_eyre_wBlank %>% 
  ggplot(aes(x = word, y = n, fill = name)) + 
  geom_bar(width = 0.9, stat="identity") + 
  coord_polar(theta = "y") +
  xlab("") + ylab("") +
  ylim(c(0,600)) +
  ggtitle("Most Common Words in Jane Eyre") +
  geom_text(data = jane_eyre_top10, 
            hjust = 1, size = 3,
            aes(x = word, y = 0, label = paste(name," "))) +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_blank())


# plot again, but not with ggplot -------------------

# settings
x <- jane_eyre_top10$n/(max(jane_eyre_top10$n)*2)
labels <- jane_eyre_top10$name
colors <- rainbow(length(x))
cex.lab <- 1

par(mar = c(1,1,1,1))
plot(0, xlim=c(-1.1,1.1),ylim=c(-4,4),type="n",axes=F, xlab=NA, ylab=NA)
radii <- seq(1, 0.3, length.out=length(x))
draw.circle(0,0,radii,border="lightgrey")
angles <- (1/4 - x)*2*pi
draw.arc(0, 0, radii, angles, pi/2, col=colors, lwd=5, lend=2, n=1000)
ymult <- (par("usr")[4]-par("usr")[3])/(par("usr")[2]-par("usr")[1])*par("pin")[1]/par("pin")[2]
text(x=-0.02, y=radii*ymult, labels=labels, pos=2, cex=cex.lab)

