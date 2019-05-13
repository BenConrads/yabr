library(ggpol)
library(ggplot2)

### Podział partyjny Senatu
sen_frame <- data.frame(party=c("Democrats", "Republicans"), seats=c(47, 53))
sen_frame %>%
  ggplot(aes(seats=seats, fill=party)) +
  geom_parliament() +
  scale_fill_manual(values=c('#0015BC', "#DE0100"),
                     labels=c("Partia Demokratyczna", "Partia Republikańska"),
                     name="Parta") +
  xlab("") +
  ylab("") +
  ggtitle("Podział miejsc w Senacie") +
  scale_x_continuous(breaks=NULL) +
  scale_y_continuous(breaks=NULL) +
  theme_minimal()
  
### Podział partyjny Izby Reprezentantów
hor_frame <- data.frame(party=c("Democrats", "Republicans", "Vacants"), seats=c(235, 197, 3))
hor_frame %>%
  ggplot(aes(seats=seats, fill=party)) +
  geom_parliament() +
  scale_fill_manual(values=c('#0015BC', "#DE0100", "#AAAAAA"),
                    labels=c("Partia Demokratyczna", "Partia Republikańska", "Wakat"),
                    name="Parta") +
  xlab("") +
  ylab("") +
  ggtitle("Podział miejsc w Izbie Reprezentantów") +
  scale_x_continuous(breaks=NULL) +
  scale_y_continuous(breaks=NULL) +
  theme_minimal()

### Skład wypowiadających się senatorów
speeches_df %>%
  filter(house=='Senate') %>%
  distinct(speaker,club) %>%
  group_by(club) %>%
  summarise(seats=n()) %>%
  ggplot(aes(seats=seats, fill=club)) +
  geom_parliament() +
  scale_fill_manual(values=c('#0015BC', "#DE0100"),
                    labels=c("Partia Demokratyczna", "Partia Republikańska"),
                    name="Parta") +
  xlab("") +
  ylab("") +
  ggtitle("Podział wypowiadających się senatorów") +
  scale_x_continuous(breaks=NULL) +
  scale_y_continuous(breaks=NULL) +
  theme_minimal()

### Skład wypowiadających się kongresmenów
speeches_df %>%
  filter(house!='Senate') %>%
  distinct(speaker,club) %>%
  group_by(club) %>%
  summarise(seats=n()) %>%
  ggplot(aes(seats=seats, fill=club)) +
  geom_parliament() +
  scale_fill_manual(values=c('#0015BC', "#DE0100"),
                    labels=c("Partia Demokratyczna", "Partia Republikańska"),
                    name="Parta") +
  xlab("") +
  ylab("") +
  ggtitle("Podział wypowiadających się kongresmenów") +
  scale_x_continuous(breaks=NULL) +
  scale_y_continuous(breaks=NULL) +
  theme_minimal()


### Skład wszystkich przemów
speeches_df %>%
  ggplot(aes(x=factor(1), y=..count../sum(..count..), fill=club)) +
  geom_bar(width=1) +
  coord_polar("y") +
  scale_fill_manual(values=c('#0015BC', "#DE0100"),
                    labels=c("Partia Demokratyczna", "Partia Republikańska"),
                    name="Parta") +
  scale_y_continuous(labels = scales::percent) + 
  xlab("") +
  ylab("Udział partii wśród wszystkich przemów") +
  theme_minimal()

### Skład przemów senackich
speeches_df %>%
  filter(house=='Senate') %>%
  ggplot(aes(x=factor(1), y=..count../sum(..count..), fill=club)) +
  geom_bar(width=1) +
  coord_polar("y") +
  scale_fill_manual(values=c('#0015BC', "#DE0100"),
                    labels=c("Partia Demokratyczna", "Partia Republikańska"),
                    name="Parta") +
  scale_y_continuous(labels = scales::percent) + 
  xlab("") +
  ylab("Udział partii wśród przemów senackich") +
  theme_minimal()

###Skład przemów w Izbie
speeches_df %>%
  filter(house %in% c('HoR', 'EoR')) %>%
  ggplot(aes(x=factor(1), y=..count../sum(..count..), fill=club)) +
  geom_bar(width=1) +
  coord_polar("y") +
  scale_fill_manual(values=c('#0015BC', "#DE0100"),
                    labels=c("Partia Demokratyczna", "Partia Republikańska"),
                    name="Parta") +
  scale_y_continuous(labels = scales::percent) + 
  xlab("") +
  ylab("Udział partii wśród przemów kongresowych") +
  theme_minimal()
