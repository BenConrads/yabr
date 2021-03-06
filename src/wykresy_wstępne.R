library(ggpol)
library(ggplot2)

### Podzia� partyjny Senatu
sen_frame <- data.frame(party=c("Democrats", "Republicans"), seats=c(47, 53))
sen_frame %>%
  ggplot(aes(seats=seats, fill=party)) +
  geom_parliament() +
  scale_fill_manual(values=c('#0015BC', "#DE0100"),
                     labels=c("Partia Demokratyczna", "Partia Republika�ska"),
                     name="Partia") +
  xlab("") +
  ylab("") +
  ggtitle("Podzia� miejsc w Senacie") +
  scale_x_continuous(breaks=NULL) +
  scale_y_continuous(breaks=NULL) +
  theme_minimal()
  
### Podzia� partyjny Izby Reprezentant�w
hor_frame <- data.frame(party=c("Democrats", "Republicans", "Vacants"), seats=c(235, 197, 3))
hor_frame %>%
  ggplot(aes(seats=seats, fill=party)) +
  geom_parliament() +
  scale_fill_manual(values=c('#0015BC', "#DE0100", "#AAAAAA"),
                    labels=c("Partia Demokratyczna", "Partia Republika�ska", "Wakat"),
                    name="Parta") +
  xlab("") +
  ylab("") +
  ggtitle("Podzia� miejsc w Izbie Reprezentant�w") +
  scale_x_continuous(breaks=NULL) +
  scale_y_continuous(breaks=NULL) +
  theme_minimal()

### Sk�ad wypowiadaj�cych si� senator�w
speeches_df %>%
  filter(house=='Senate') %>%
  distinct(speaker,club) %>%
  group_by(club) %>%
  summarise(seats=n()) %>%
  ggplot(aes(seats=seats, fill=club)) +
  geom_parliament() +
  scale_fill_manual(values=c('#0015BC', "#DE0100"),
                    labels=c("Partia Demokratyczna", "Partia Republika�ska"),
                    name="Parta") +
  xlab("") +
  ylab("") +
  ggtitle("Podzia� wypowiadaj�cych si� senator�w") +
  scale_x_continuous(breaks=NULL) +
  scale_y_continuous(breaks=NULL) +
  theme_minimal()

### Sk�ad wypowiadaj�cych si� kongresmen�w
speeches_df %>%
  filter(house!='Senate') %>%
  distinct(speaker,club) %>%
  group_by(club) %>%
  summarise(seats=n()) %>%
  ggplot(aes(seats=seats, fill=club)) +
  geom_parliament() +
  scale_fill_manual(values=c('#0015BC', "#DE0100"),
                    labels=c("Partia Demokratyczna", "Partia Republika�ska"),
                    name="Parta") +
  xlab("") +
  ylab("") +
  ggtitle("Podzia� wypowiadaj�cych si� kongresmen�w") +
  scale_x_continuous(breaks=NULL) +
  scale_y_continuous(breaks=NULL) +
  theme_minimal()


### Sk�ad wszystkich przem�w
speeches_df %>%
  ggplot(aes(x=factor(1), y=..count../sum(..count..), fill=club)) +
  geom_bar(width=1) +
  coord_polar("y") +
  scale_fill_manual(values=c('#0015BC', "#DE0100"),
                    labels=c("Partia Demokratyczna", "Partia Republika�ska"),
                    name="Parta") +
  scale_y_continuous(labels = scales::percent) + 
  xlab("") +
  ylab("Udzia� partii w�r�d wszystkich przem�w") +
  theme_minimal()

### Sk�ad przem�w senackich
speeches_df %>%
  filter(house=='Senate') %>%
  ggplot(aes(x=factor(1), y=..count../sum(..count..), fill=club)) +
  geom_bar(width=1) +
  coord_polar("y") +
  scale_fill_manual(values=c('#0015BC', "#DE0100"),
                    labels=c("Partia Demokratyczna", "Partia Republika�ska"),
                    name="Parta") +
  scale_y_continuous(labels = scales::percent) + 
  xlab("") +
  ylab("Udzia� partii w�r�d przem�w senackich") +
  theme_minimal()

###Sk�ad przem�w w Izbie
speeches_df %>%
  filter(house %in% c('HoR', 'EoR')) %>%
  ggplot(aes(x=factor(1), y=..count../sum(..count..), fill=club)) +
  geom_bar(width=1) +
  coord_polar("y") +
  scale_fill_manual(values=c('#0015BC', "#DE0100"),
                    labels=c("Partia Demokratyczna", "Partia Republika�ska"),
                    name="Parta") +
  scale_y_continuous(labels = scales::percent) + 
  xlab("") +
  ylab("Udzia� partii w�r�d przem�w kongresowych") +
  theme_minimal()
