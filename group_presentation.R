## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = F, message = F, warning = F, fig.height = 6, fig.width = 8)

## ------------------------------------------------------------------------
library(readit)
library(dplyr)
library(tidyr)
library(ggplot2)
cpgp <- readit("cpgp.csv")

## ------------------------------------------------------------------------
cpgp <- cpgp %>%
  mutate(pct = round((CPGP - Control) / Control * 100,1))
cpgp_td <- cpgp %>%
  gather(group, yield, CPGP:Control) 

## ------------------------------------------------------------------------
dry <- cpgp_td %>% filter(group == "Control", season == "dry")

## ------------------------------------------------------------------------

plot1a <- cpgp %>%
  filter(season == "dry") %>%
  mutate(province = reorder(province, -Control)) %>%
  gather(group, yield, CPGP:Control) %>%
  ggplot(aes(province, yield, fill = group)) +
  geom_col(position = position_dodge(), alpha = 0.7, color = "black") +
  scale_fill_grey(start = 0.2, end = .7) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  xlab("Province") +
  ylab("Yield (no. of sacks/ha)") +
  guides(fill=guide_legend(title="Treatment")) +
  ggtitle("Rice Yield by Province", subtitle = "Dry Season, 2018") +
  labs(caption = "Source: Field verification testing results of carageenan plant food supplement (CPGP), DOST-Region 2") 

## ------------------------------------------------------------------------

plot1b <- cpgp %>%
  filter(season == "dry") %>%
  mutate(province = reorder(province, -CPGP)) %>%
  gather(group, yield, CPGP:Control) %>%
  ggplot(aes(province, yield, fill = group)) +
  geom_col(position = position_dodge(), alpha = 0.7, color = "black") +
  scale_fill_grey(start = 0.2, end = .7) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  xlab("Province") +
  ylab("Yield (no. of sacks/ha)") +
  guides(fill=guide_legend(title="Treatment")) +
  ggtitle("Rice Yield by Province", subtitle = "Dry Season, 2018") +
  labs(caption = "Source: Field verification testing results of carageenan plant food supplement (CPGP), DOST-Region 2") 

## ------------------------------------------------------------------------


plot2a <- cpgp %>%
  filter(season == "wet") %>%
  mutate(province = reorder(province, -Control)) %>%
  gather(group, yield, CPGP:Control) %>%
  ggplot(aes(province, yield, fill = group)) +
  geom_col(position = position_dodge(), alpha = 0.7, color = "black") +
  scale_fill_grey(start = 0.2, end = .7) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  xlab("Province") +
  ylab("Yield (no. of sacks/ha)") +
  guides(fill=guide_legend(title="Treatment")) +
  ggtitle("Rice Yield by Province", subtitle = "Wet Season, 2018") +
  labs(caption = "Source: Field verification testing results of carageenan plant food supplement (CPGP), DOST-Region 2") 


## ------------------------------------------------------------------------


plot2b <- cpgp %>%
  filter(season == "wet") %>%
  mutate(province = reorder(province, -CPGP)) %>%
  gather(group, yield, CPGP:Control) %>%
  ggplot(aes(province, yield, fill = group)) +
  geom_col(position = position_dodge(), alpha = 0.7, color = "black") +
  scale_fill_grey(start = 0.2, end = .7) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  xlab("Province") +
  ylab("Yield (no. of sacks/ha)") +
  guides(fill=guide_legend(title="Treatment")) +
  ggtitle("Rice Yield by Province", subtitle = "Wet Season, 2018") +
  labs(caption = "Source: Field verification testing results of carageenan plant food supplement (CPGP), DOST-Region 2") 


## ------------------------------------------------------------------------


cpgp2 <- cpgp %>%
  mutate(province.season = paste(province, season, sep = ".")) %>%
  mutate(province.season = factor(province.season, levels = province.season[order(pct)])) %>%
  mutate(season = case_when(season == "dry"~"Dry Season",
                            TRUE~"Wet Season"))
plot3 <- cpgp2 %>%
  ggplot(aes(province.season, pct)) +
  geom_col(alpha = 0.7, color = "black", width = 0.7) +
  scale_x_discrete(name = "Province", breaks = cpgp2$province.season, labels = cpgp2$province) +
  facet_wrap(~season, scales = "free") +
  coord_flip() +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  xlab("Province") +
  ylab("% Yield (in no. of sacks/ha)") +
  guides(fill=guide_legend(title="Treatment")) +
  ggtitle("Rice Yield Percentage Difference by Province across Seasons (2018)", subtitle = "Carageenan over Non-Carageenan Group") +
  labs(caption = "Source: Field verification testing results of carageenan plant food supplement (CPGP), DOST-Region 2") 

## ------------------------------------------------------------------------
