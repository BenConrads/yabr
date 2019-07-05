library(ggpol)
library(ggplot2)

### Podzia³ partyjny Senatu
sen_frame <- data.frame(party=c("Democrats", "Republicans"), seats=c(47, 53))
sen_frame %>%
  ggplot(aes(seats=seats, fill=party)) +
  geom_parliament() +
  scale_fill_manual(values=c('#0015BC', "#DE0100"),
                     labels=c("Partia Demokratyczna", "Partia Republikañska"),
                     name="Partia") +
  xlab("") +
  ylab("") +
  ggtitle("Podzia³ miejsc w Senacie") +
  scale_x_continuous(breaks=NULL) +
  scale_y_continuous(breaks=NULL) +
  theme_minimal()
  
### Podzia³ partyjny Izby Reprezentantów
hor_frame <- data.frame(party=c("Democrats", "Republicans", "Vacants"), seats=c(235, 197, 3))
hor_frame %>%
  ggplot(aes(seats=seats, fill=party)) +
  geom_parliament() +
  scale_fill_manual(values=c('#0015BC', "#DE0100", "#AAAAAA"),
                    labels=c("Partia Demokratyczna", "Partia Republikañska", "Wakat"),
                    name="Parta") +
  xlab("") +
  ylab("") +
  ggtitle("Podzia³ miejsc w Izbie Reprezentantów") +
  scale_x_continuous(breaks=NULL) +
  scale_y_continuous(breaks=NULL) +
  theme_minimal()

### Sk³ad wypowiadaj¹cych siê senatorów
speeches_df %>%
  filter(house=='Senate') %>%
  distinct(speaker,club) %>%
  group_by(club) %>%
  summarise(seats=n()) %>%
  ggplot(aes(seats=seats, fill=club)) +
  geom_parliament() +
  scale_fill_manual(values=c('#0015BC', "#DE0100"),
                    labels=c("Partia Demokratyczna", "Partia Republikañska"),
                    name="Parta") +
  xlab("") +
  ylab("") +
  ggtitle("Podzia³ wypowiadaj¹cych siê senatorów") +
  scale_x_continuous(breaks=NULL) +
  scale_y_continuous(breaks=NULL) +
  theme_minimal()

### Sk³ad wypowiadaj¹cych siê kongresmenów
speeches_df %>%
  filter(house!='Senate') %>%
  distinct(speaker,club) %>%
  group_by(club) %>%
  summarise(seats=n()) %>%
  ggplot(aes(seats=seats, fill=club)) +
  geom_parliament() +
  scale_fill_manual(values=c('#0015BC', "#DE0100"),
                    labels=c("Partia Demokratyczna", "Partia Republikañska"),
                    name="Parta") +
  xlab("") +
  ylab("") +
  ggtitle("Podzia³ wypowiadaj¹cych siê kongresmenów") +
  scale_x_continuous(breaks=NULL) +
  scale_y_continuous(breaks=NULL) +
  theme_minimal()


### Sk³ad wszystkich przemów
speeches_df %>%
  ggplot(aes(x=factor(1), y=..count../sum(..count..), fill=club)) +
  geom_bar(width=1) +
  coord_polar("y") +
  scale_fill_manual(values=c('#0015BC', "#DE0100"),
                    labels=c("Partia Demokratyczna", "Partia Republikañska"),
                    name="Parta") +
  scale_y_continuous(labels = scales::percent) + 
  xlab("") +
  ylab("Udzia³ partii wœród wszystkich przemów") +
  theme_minimal()

### Sk³ad przemów senackich
speeches_df %>%
  filter(house=='Senate') %>%
  ggplot(aes(x=factor(1), y=..count../sum(..count..), fill=club)) +
  geom_bar(width=1) +
  coord_polar("y") +
  scale_fill_manual(values=c('#0015BC', "#DE0100"),
                    labels=c("Partia Demokratyczna", "Partia Republikañska"),
                    name="Parta") +
  scale_y_continuous(labels = scales::percent) + 
  xlab("") +
  ylab("Udzia³ partii wœród przemów senackich") +
  theme_minimal()

###Sk³ad przemów w Izbie
speeches_df %>%
  filter(house %in% c('HoR', 'EoR')) %>%
  ggplot(aes(x=factor(1), y=..count../sum(..count..), fill=club)) +
  geom_bar(width=1) +
  coord_polar("y") +
  scale_fill_manual(values=c('#0015BC', "#DE0100"),
                    labels=c("Partia Demokratyczna", "Partia Republikañska"),
                    name="Parta") +
  scale_y_continuous(labels = scales::percent) + 
  xlab("") +
  ylab("Udzia³ partii wœród przemów kongresowych") +
  theme_minimal()
