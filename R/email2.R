#library(mailR)

sendEmail <- function(body="",r=c(),sub="Auto"){
        sender <- "christian_stock_preds@yahoo.com"
        send.mail(from = sender,
                  to = r,
                  subject = sub,
                  body = body,
                  smtp = list(host.name = "smtp.mail.yahoo.com", port = 465,
                              user.name = "christian_stock_preds@yahoo.com",
                              passwd = "Ilik32eat3141", ssl = TRUE),
                  authenticate = TRUE,
                  send = TRUE)
}

