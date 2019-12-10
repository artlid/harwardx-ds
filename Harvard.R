#https://rafalab.github.io/dsbook/

#Git
#https://git-scm.com/docs/gitignore - files that are not being tracked in GitHub repo



library(ggplot2)
library(dplyr)
library(tidyverse)
library(dslabs) ## number of different datasets
library(ggthemes) ## different themes for ggplot
library(ggrepel) ## alter labels features
library(gridExtra) ## combine plots on one grid
library(RColorBrewer) ## provides color palettes 

display.brewer.all(type = "seq") # div or seq

# MURDERS CASE

data("murders")
head(murders)
levels(murders)
names(murders)
class(murders)

mean(murders$total)
murders <- murders %>% mutate(murder_rate = total/population*100000)
mean(murders$murder_rate)
summarize(murders, mean(murder_rate))

us_murder_rate <- murders %>% 
  summarise(rate = sum(total)/sum(population)*100000) %>% 
  .$rate
us_murder_rate

class(us_murder_rate)

murders %>%
  group_by(region) %>%
  summarise(median_rate = median(murder_rate))

murders %>% arrange(desc(murder_rate)) %>% head()
murders %>% top_n(12, desc(murder_rate))
murders %>% arrange((murder_rate)) %>% top_n(12)
### na.rm = TRUE - ignore NA values

# Defines the slope of the line
r <- murders %>% summarise(rate = sum(total) / sum(population) * 10^6) %>% .$rate
r

murders %>% ggplot(aes(population/10^6, total, label = abb)) +
  #geom_text(nudge_x = 0.075) + # changing the labels features
  geom_abline(intercept = log10(r), lty = 2, color = "darkgrey") + # adding average line
  geom_point(aes(col=region), size = 3) + #changing the points features
  geom_text_repel() +
  scale_x_log10() +
  scale_y_log10() + 
  xlab("Population in millions (log scale)") +
  ylab("Total number of murders (log scale)") +
  ggtitle("US Gun Murders in US 2010") +
  scale_color_discrete(name = "Region") + #changing legend features
  theme_economist() 

# HIGHTS CASE

heights %>% 
  group_by(sex) %>%
  summarise(average = mean(height), standard_deviation = sd(height))


data("heights")
head(heights)

heights %>% filter(sex=="Male") %>%
  ggplot(aes(x = height)) + 
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  #geom_text_repel(height, aes(label = "height")) +
  xlab("Male heights in inches") +
  ggtitle("Heights Histogram")

heights %>% filter(sex=="Male") %>%
  ggplot(aes(x = height)) + 
  geom_density(fill = "blue")

# QQ Plot

param <- heights %>%
  filter(sex=="Male") %>%
  summarise(mean = mean(height), sd = sd(height))
  
heights %>% filter(sex=="Male") %>%
  ggplot(aes(sample = height)) +
  geom_qq(dparams = param) + 
  geom_abline()

##use standard units

heights %>% filter(sex=="Male") %>%
  ggplot(aes(sample = scale(height))) +
  geom_qq() + 
  geom_abline()

## grids of plots
p <- heights %>% filter(sex=="Male") %>% ggplot(aes(x = height))
p1 <- p + geom_histogram(binwidth = 1, fill = "blue", color = "black")
p2 <- p + geom_histogram(binwidth = 2, fill = "blue", color = "black")
p3 <- p + geom_histogram(binwidth = 3, fill = "blue", color = "black")
grid.arrange(p1, p2, p3, ncol = 3)


###########################

data("murders")
class(murders)

str(murders)
head(murders)

length(murders$population)

murders$region

?MESS

mean(c(0,0, 1,3,5,7))
median(c(0,0, 1,3,5,7))

beads <- rep(c("red", "blue"), times = c(2, 3))
beads
sample(beads, 2)

# CARDS
suits <- c("Diamonds", "Clubs", "Hearts", "Spades")
numbers <- c("Ace", "Deuce", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine", "Ten", "Jack", "Queen", "King")
deck <-expand.grid(number = numbers, suit = suits)
deck <- paste(deck$number, deck$suit)

aces <- paste("Ace", suits)

facecard <- c("King", "Queen", "Jack", "Ten")
facecard <- expand.grid( number = facecard, suit = suits)
facecard <- paste( facecard$number, facecard$suit)

hands <- combinations(52, 2, v = deck)
hands

mean((hands[,1] %in% aces & hands[,2] %in% facecard) | (hands[,2] %in% aces & hands[,1] %in% facecard))

# Duplicated BDays
n <- 50
B <- 10000
results <- replicate(B, {
  bdays <- sample(1:365, n, replace = TRUE)
  any(duplicated(bdays))
})
mean(results)


# CASINO
library(magrittr)

color <- rep(c("Red", "Black", "Green"), c(18, 18, 2))
n <- 1000
X <- sample(ifelse(color == "Red", -1, 1), n, replace = TRUE)
X[1:10]

n <- 1000
B <- 10000
S <- replicate(B, {
  X1 <- sample(c(-1, 1), n, replace = TRUE, prob=c(9/19, 10/19))
  sum(X1)
})
mean(S < 0)
hist(S)
qqnorm(S)
qqline(S, col = "red", lwd = 2)

# qqPlot(S)
mean(S)
sd(S)

s <- seq(min(S), max(S), length = 100)c
normal_density <- data.frame(s = s, f = dnorm(s, mean(S), sd(S)))
data.frame(S=S) %>% ggplot(aes(S, ..density..)) +
  geom_histogram(color = "black", binwidth = 10) +
  ylab("Probability") +
  geom_line(data = normal_density, mapping = aes(s,f), color = "blue")


# E[X] = a*p + b*(1-p) - expected value
# sdev = abs(b-a) * sqrt(p*(1-p)) - standard deviation
# SE[X] = sqrt(n) * sdev - standard error

#E[X~] = p - expected value of the average X~
#SE[X~] = sqr(p*(1-p)/N) - standard error of the average

# Binomial distribution - derive the distribution of a random variable (Independent draws from the urn)
# Poison distribution - when the probability of success is very low

                        # GAPMINDER
data("gapminder")
head(gapminder)

gapminder <- gapminder %>%
  mutate(dollars_per_day = gdp/population/365)

filter(gapminder, year == 1962) %>% 
  ggplot(aes(fertility, life_expectancy, color = continent)) +
  geom_point()

filter(gapminder, year%in%c(1962, 2012)) %>% 
  ggplot(aes(fertility, life_expectancy, color = continent)) +
  geom_point() +
  facet_grid(continent~year)

years <- c(1962, 1980, 1990, 2000, 2012)
continents <- c("Asia", "Europe")

filter(gapminder, year %in% years & continent %in% continents) %>% 
  ggplot(aes(fertility, life_expectancy, color = continent)) +
  geom_point() +
  facet_wrap(.~year)

#PLOTS
##Time series plots
countries <- c("South Korea", "Germany")
#gapminder %>% filter(country == "United States") %>%
gapminder %>% filter(country %in% countries) %>%
  ggplot(aes(year, fertility, color  = country)) + 
  geom_line()

length(levels(gapminder$region)) # shows a number of regions

labels <- data.frame(country = countries, x = c(1975, 1965), y = c(60, 72))
gapminder %>% filter(country %in% countries) %>%
  ggplot(aes(year, life_expectancy, color  = country)) + 
  geom_line() +
  geom_text(data = labels, aes(x, y, label = country), size = 5) +
  theme(legend.position = "none")

##Histogram with log transformation
past_year <- 1970
gapminder %>%
  filter(year == past_year & !is.na(gdp)) %>%
  ggplot(aes(dollars_per_day)) + 
  scale_x_continuous(trans = "log2") +
  geom_histogram(binwidth = 1, color = "black")

##BoxPlots
gapminder %>%
  filter(year == past_year & !is.na(gdp)) %>%
  ggplot(aes(region, dollars_per_day)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) #rotates the label, hjust places text next to the axis


gapminder %>%
  filter(year == past_year & !is.na(gdp)) %>%
  mutate(region = reorder(region, -dollars_per_day, FUN = median)) %>% #adding "-" sign changing derection for reorder
  ggplot(aes(region, dollars_per_day, fill = continent)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("") + 
  scale_y_continuous(trans = "log2") +
  geom_point(show.legend = FALSE)


##Comparing Distributions
west <- c("Western Europe", "Northern Europe", "Southern Europe", 
          "Northern America", "Australia and New Zealand")

past_year <- 1970
present_year <- 2010

gapminder %>%
  filter(year %in% c(past_year, present_year) & !is.na(gdp)) %>%
  mutate(group = ifelse(region %in% west, "West", "Developing")) %>%
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth = 1, color = "black") +
  scale_x_continuous(trans = "log2") +
  facet_grid(year ~ group)

###comparing only relevant countries
country_list_1 <- gapminder %>%
  filter(year == past_year & !is.na(dollars_per_day)) %>% .$country # . - get a character vector
country_list_2 <- gapminder %>%
  filter(year == present_year & !is.na(dollars_per_day)) %>% .$country

#head(country_list_1)
#country_list_2

country_list <- intersect(country_list_1, country_list_2)
length(country_list)

gapminder %>%
  filter(year %in% c(past_year, present_year) & country %in% country_list) %>%
  mutate(group = ifelse(region %in% west, "West", "Developing")) %>%
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth = 1, color = "black") +
  scale_x_continuous(trans = "log2") +
  facet_grid(year ~ group)

###Which one increased more?
gapminder %>%
  filter(year %in% c(past_year, present_year) & country %in% country_list) %>%
  mutate(region = reorder(region, dollars_per_day, FUN = median)) %>%
  ggplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("") + 
  scale_y_continuous(trans = "log2") +
  geom_boxplot(aes(region, dollars_per_day, fill = factor(year))) #change to factor to assign a color


##Smooth Density Plots
gapminder %>%
  filter(year %in% c(past_year, present_year) & country %in% country_list) %>%
  mutate(group = ifelse(region %in% west, "West", "Developing")) %>%
  ggplot(aes(dollars_per_day, y = ..count.., fill = group)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_continuous(trans = "log2") +
  geom_density(alpha = 0.2, bw = 0.75) + #bw - change the smoothness
  facet_grid(year ~ .)

gapminder <- gapminder %>%
  mutate(group = case_when(
    .$region %in% west ~ "West",
    .$region %in% c("Eastern Asia", "South-Eastern Asia") ~ "East Asia",
    .$region %in% c("Caribbean", "Central America", "South America") ~ "Latin America",
    .$continent == "Africa" & .$region != "Northern Africa" ~ "Sub-Saharan Africa",
    TRUE ~ "Others" ))


gapminder <- gapminder %>%
  mutate(group = factor(group, levels = c("Others", "Latin America", 
                                          "East Asia", "Sub-Saharan Africa",
                                          "West"))) # create a particular order  


###Stacking approach
gapminder %>%
  filter(year %in% c(past_year, present_year) & country %in% country_list) %>% 
  ggplot(aes(dollars_per_day, y = ..count.., fill = group)) +
  theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
  scale_x_continuous(trans = "log2") +
  geom_density(alpha = 0.2, bw = 0.75, position = "stack") + #bw - change the smoothness
  facet_grid(year ~ .)

####weight the smooth densities
gapminder %>%
  filter(year %in% c(past_year, present_year) & country %in% country_list) %>% 
  group_by(year) %>%
  mutate(weight = population/sum(population)*2) %>%
  ungroup() %>%
  ggplot(aes(dollars_per_day, fill = group, weight = weight)) +
  scale_x_continuous(trans = "log2") +
  geom_density(alpha = 0.2, bw = 0.75, position = "stack") + #bw - change the smoothness
  facet_grid(year ~ .)

##EXTRA
gapminder <- gapminder %>%
  mutate(group = case_when(
    .$region %in% west ~ "The West",
    .$region %in% "Northern Africa" ~ "Northern Africa",
    .$region %in% c("Eastern Asia", "South-Eastern Asia") ~ "East Asia",
    .$region == "Southern Asia" ~ "Southern Asia",
    .$region %in% c("Caribbean", "Central America", "South America") ~ "Latin America",
    .$continent == "Africa" & .$region != "Northern Africa" ~ "Sub-Saharan Africa",
    .$region %in% c("Melanesia", "Micronesia", "Polynesia") ~ "Pacific Islands"))
# TRUE ~ "Others" ))

#count(gapminder, group)
#gapminder %>% count(group)
head(gapminder)

esurv_income <- gapminder %>%
  filter(year %in% present_year & !is.na(gdp) & !is.na(infant_mortality) & !is.na(group)) %>%
  group_by(group) %>%
  summarize(income = sum(gdp)/sum(population)/365,
            infant_survival_rate = 1-sum(infant_mortality/1000*population)/sum(population))

surv_income %>% arrange(income)

surv_income %>% ggplot(aes(income, infant_survival_rate, label = group, color = group)) +
  scale_x_continuous(trans = "log2", limit = c(0.25, 150)) +
  scale_y_continuous(trans = "log2", limit = c(0.875, .9981), 
                     breaks = c(.85, .90, .95, .99, .995, .998)) + #set the location of the axis labels
  geom_label(size = 3, show.legend = FALSE)

                      # US DISEASES
data("us_contagious_diseases")
head(us_contagious_diseases)

count(us_contagious_diseases, disease)
summary(us_contagious_diseases)


the_disease <- "Measles"
df <- us_contagious_diseases %>%
  filter(!state %in% c("Hawaii", "Alaska") & disease == the_disease) %>%
  mutate(rate = count / population *10000) %>%
  mutate(state = reorder(state, rate))

df %>% filter(state == "California") %>%
  ggplot(aes(year, rate)) +
  geom_line() + ylab("Case per 10,000") +
  geom_vline(xintercept = 1963, col = "blue")


### Awesome Plots!
df %>% ggplot(aes(year, state, fill = rate)) +
  geom_tile(color = "grey50") +
  scale_x_continuous(expand = c(0,0)) +
  scale_fill_gradientn(colors = brewer.pal(9, "Reds"), trans = "sqrt") +
  geom_vline(xintercept =  1963, col = "blue") +
  theme_minimal() + theme(panel.grid = element_blank()) + 
  ggtitle(the_disease) + 
  ylab("") +
  xlab("")


avg <- us_contagious_diseases %>%
  filter(disease == the_disease) %>% group_by(year) %>%
  summarize(us_rate = sum(count, na.rm = TRUE) / sum(population, na.rm = TRUE)*10000)
  
df %>%
  ggplot() +
  geom_line(aes(year, rate, group = state),  color = "grey50", 
            show.legend = FALSE, alpha = 0.2, size = 1) + 
  geom_line(mapping = aes(year, us_rate),  
            data = avg, size = 1, color = "black") +
  scale_y_continuous(trans = "sqrt", breaks = c(5,25,125,300)) + 
  ggtitle("Cases per 10,000 by state") + 
  xlab("") + 
  ylab("") +
  geom_text(data = data.frame(x=1955, y=50), 
            mapping = aes(x, y, label="US average"), color="black") + 
  geom_vline(xintercept=1963, col = "blue")


                  # INFERENCE



