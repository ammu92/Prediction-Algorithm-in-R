library(RDCOMClient)
#Outlook should be installed and open
OutApp <- COMCreate("Outlook.Application")

outMail = OutApp$CreateItem(0)
outMail[["To"]] = "amrutakulkarni@outlook.com"
outMail[["subject"]] = "Test email from R"
outMail[["body"]] = "Test email from R"
outMail$Send()