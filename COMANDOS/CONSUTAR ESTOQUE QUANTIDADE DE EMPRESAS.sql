SELECT CODPROD,  COUNT(CODPROD)
FROM TGFEST 
WHERE ATIVO = 'S'
GROUP BY CODPROD
HAVING COUNT(CODPROD) <=9
ORDER BY CODPROD


