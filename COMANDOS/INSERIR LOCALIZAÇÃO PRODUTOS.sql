SELECT * FROM AD_LOCALEMP WHERE CODPROD = 34966

SELECT * FROM TGFPRO
WHERE CODPROD NOT IN (SELECT CODPROD FROM AD_LOCALEMP WHERE CODEMP =1)
AND USOPROD = 'R'

INSERT�INTO AD_LOCALEMP VALUES ('34966','01',LTrim (Rtrim('PRODUTO SEM LOCALIZA��O!')) , LTrim (Rtrim('PRODUTO SEM LOCALIZA��O!')) ,NULL)
INSERT�INTO AD_LOCALEMP VALUES ('34966','02',LTrim (Rtrim('PRODUTO SEM LOCALIZA��O!')) , LTrim (Rtrim('PRODUTO SEM LOCALIZA��O!')) ,NULL)
INSERT�INTO AD_LOCALEMP VALUES ('34966','03',LTrim (Rtrim('PRODUTO SEM LOCALIZA��O!')) , LTrim (Rtrim('PRODUTO SEM LOCALIZA��O!')) ,NULL)
INSERT�INTO AD_LOCALEMP VALUES ('34966','04',LTrim (Rtrim('PRODUTO SEM LOCALIZA��O!')) , LTrim (Rtrim('PRODUTO SEM LOCALIZA��O!')) ,NULL)
INSERT�INTO AD_LOCALEMP VALUES ('34966','06',LTrim (Rtrim('PRODUTO SEM LOCALIZA��O!')) , LTrim (Rtrim('PRODUTO SEM LOCALIZA��O!')) ,NULL)
INSERT�INTO AD_LOCALEMP VALUES ('34966','07',LTrim (Rtrim('PRODUTO SEM LOCALIZA��O!')) , LTrim (Rtrim('PRODUTO SEM LOCALIZA��O!')) ,NULL)
INSERT�INTO AD_LOCALEMP VALUES ('34966','08',LTrim (Rtrim('PRODUTO SEM LOCALIZA��O!')) , LTrim (Rtrim('PRODUTO SEM LOCALIZA��O!')) ,NULL)
INSERT�INTO AD_LOCALEMP VALUES ('34966','09',LTrim (Rtrim('PRODUTO SEM LOCALIZA��O!')) , LTrim (Rtrim('PRODUTO SEM LOCALIZA��O!')) ,NULL)
INSERT�INTO AD_LOCALEMP VALUES ('34966','11',LTrim (Rtrim('PRODUTO SEM LOCALIZA��O!')) , LTrim (Rtrim('PRODUTO SEM LOCALIZA��O!')) ,NULL)
INSERT�INTO AD_LOCALEMP VALUES ('34966','12',LTrim (Rtrim('PRODUTO SEM LOCALIZA��O!')) , LTrim (Rtrim('PRODUTO SEM LOCALIZA��O!')) ,NULL)