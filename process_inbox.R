setwd("/home/lemur/RProjects/automatic_reports")

suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(gmailr))

# autoryzacja w GMail
# PRZECZYTAJ KONIECZNIE https://gmailr.r-lib.org/#setup
gm_auth_configure()
gm_auth(email = TRUE, cache = ".secret")

# czego szukamy? Dzisiejsze maile w "inbox"
gmail_search_query = "in:inbox is:unread"

# pobieramy maile (max 9999 sztuk)
ids <- gm_messages(search = gmail_search_query, num_results = 9999, include_spam_trash = FALSE)
ids <- gm_id(ids)

get_mail_content <- function(id)
{
  # pobierz maila o podanym id ze skrzynki
  mail_message <- gm_message(id, format = 'full')

  # czysty nadawca czyli sam adres email
  from_raw = str_match(gm_from(mail_message), "<(.*@.*)>")[[2]]
  if(is.na(from_raw)) from_raw = gm_from(mail_message)

  tibble(from = gm_from(mail_message),       # nadawca
         from_raw = from_raw,                # oczyszczona wersja nadawcy
         date = gm_date(mail_message),       # data
         subject = gm_subject(mail_message)) # tytuł maila
}

grabbed_mails <- ids %>%
  map_dfr(get_mail_content) %>%
  mutate(message_id = ids)

send_wykop <- function(reciver) {
  # funkcja przugotowuje maila z informacjami z Wykopu i go wysyła do "reciver"

  # czytujemy odpowiedź ze statycznego HTMLa
  wykop_str <- read_lines("wykop.html") %>%
    paste0(., collapse = "")

  # budujemy maila
  email <- gm_mime() %>%
    gm_to(reciver) %>%
    gm_subject("Hity dnia z Wykopu") %>%
    gm_html_body(wykop_str)

  # wysyłamy
  ret_val <- gm_send_message(email)
}


send_bitcoin <- function(reciver) {
  # funkcja przygotowuje dedykowanego PDFa i wysyła go jako załącznik do "reciver"

  # potrzebujemy pliku tymczasowego na raport w PDFie
  temp_raport_file = tempfile(fileext = ".pdf")

  # renderujemy PDFa do tego pliku tymczasowego
  rmarkdown::render("bitcoin.Rmd",
                    output_file = temp_raport_file,
                    # parametry dla raportu
                    params = list(odbiorca_raportu = reciver),
                    quiet = TRUE)

  # gmailr ma problem z załącznikiem i treścią jednocześnie, więc:
  # hack via https://github.com/r-lib/gmailr/issues/60
  body_txt <- "Raport znajdziesz w załączniku"
  email <- gm_mime() %>%
    gm_to(reciver) %>%
    gm_subject("Notowania BTC") %>%
    gm_html_body(body_txt)%>%
    gm_attach_file(temp_raport_file, type = "application/pdf", name = "raport.pdf")

  # wysyłamy maila
  ret_val <- gm_send_message(email)

  # kasujemy plik tymczasowy
  unlink(temp_raport_file)
}



for(i in seq_len(nrow(grabbed_mails))) {

  # od kogo mail?
  reciver <- as.character(grabbed_mails[i, 'from_raw'])

  # czy "daj wykop"?
  if(str_to_lower(grabbed_mails[i, "subject"]) == "daj wykop") {
    # przygotuj i wyślij odpowiedź (wykop)
    send_wykop(reciver)
    # przenosimy maila z pytaniem do kosza - żeby nie mieć go w przyszłości w inboxie
    gm_trash_message(as.character(grabbed_mails[i, "message_id"]))
  }

  # czy "daj bitcoin"?
  if(str_to_lower(grabbed_mails[i, "subject"]) == "daj bitcoin") {
    # przygotuj i wyślij odpowiedź (raport)
    send_bitcoin(reciver)
    # przenosimy maila z pytaniem do kosza
    gm_trash_message(as.character(grabbed_mails[i, "message_id"]))
  }
}
