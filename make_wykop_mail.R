library(tidyverse)
library(httr)
library(glue)
library(jsonlite)
library(lubridate)

# tutaj mamy zapisane klucze do API Wykopu
load("wykop_api.rda") # zmienne wykop_api_key oraz wykop_secret_key

# pytamy API Wykopu o linki ze strony głównej (dostaniemy 25 sztuk)
response <- GET(paste0("https://a2.wykop.pl/Links/Promoted/appkey/", wykop_api_key))

# tłumaczymy JSON na data.frame
json <- rawToChar(response$content)
wykopki <- as_tibble(fromJSON(json)$data)

# zostawiamy sobie tylko te z ostatnich 12 godin
wykopki <- wykopki %>%
  filter(date >= now() - hours(12))

# budujemy HTMLa będącego treścią maila
wykstr <- wykopki %>%
  # formatujemy datę wykopaliska
  mutate(date = ymd_hms(date) %>% format("%e/%m @ %H:%M")) %>%
  # z kolejnych wierszy data.frame składamy tabelkę, żeby ładnie wyglądało w mailu
  mutate(prev = glue('<tr>
                      \t<td colspan="2"><a href="{source_url}"><h3>{title}</h3></a></td>
                     </tr>
                     <tr>
                    \t<td rowspan="2"><a href="{source_url}"><img src="{preview}" /></a></td>
                    \t<td><strong>{description}</strong></td>
                     </tr>
                     <tr>
                      \t<td>{date} | <a href="https://www.wykop.pl/link/{id}">Wykopy: {vote_count} | Komentarze: {comments_count}</a></td>
                     </tr>')) %>%
  # przygotowane wiersze sklejamy w jeden długi tekst
  pull(prev) %>%
  paste(., collapse = "\n") %>%
  # dodajemy nagółek i stopkę do wierszy tabeli
  paste0('<center><h3>Strona główna Wykop.pl</h3>\n',
         '<p>znaleziska dodane w ostatnich 12 godzinach<br/>',
         '<em>(aktualizacja: ', format(now(), "%e/%m %H:%M"), ')</em></p>\n',
         '\n<table width="80%" cellspacing="5px">\n', ., '\n</table>\n</center>\n')

# gotowy HTML zapisujemy do pliku na dysku
write_lines(wykstr, "wykop.html")

