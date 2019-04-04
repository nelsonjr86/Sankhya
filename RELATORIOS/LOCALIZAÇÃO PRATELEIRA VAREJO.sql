SELECT * 
  FROM (
SELECT *
    FROM (

SELECT PRO.REFERENCIA, PRO.DESCRPROD, LOC.*, EST.ESTOQUE, BAR.CODBARRA
FROM TGFPRO PRO 
	INNER JOIN AD_LOCALEMP LOC ON (LOC.CODPROD=PRO.CODPROD)
	INNER JOIN TGFEST EST ON (EST.CODPROD=PRO.CODPROD AND EST.CODEMP=LOC.CODEMP)
	INNER JOIN TGFBAR BAR ON (BAR.CODPROD=PRO.CODPROD)
	INNER JOIN TSIUSU USU ON (USU.CODUSU=SANKHYA.STP_GET_CODUSULOGADO())
WHERE (LOC.CODLOCATA IS NULL OR LOC.CODLOCATA LIKE :LOCAL+'%')
AND (
		(USU.CODEMP IS NULL AND LOC.CODEMP = :CODEMP) OR
		(USU.CODEMP IS NOT NULL AND LOC.CODEMP = USU.CODEMP)
	)
AND PRO.ATIVO = :ATIVO
--AND EST.ESTOQUE > 0
AND (
	:GRUPO IS NULL OR :GRUPO IS NOT NULL AND (
	(LEN(:GRUPO) > 4 AND PRO.REFERENCIA LIKE :GRUPO) OR
	(LEN(:GRUPO) <= 4 AND PRO.REFERENCIA LIKE :GRUPO+'%')
  ))
AND :TIPO = 'A'
--ORDER BY LOC.CODLOCATA

UNION ALL

SELECT PRO.REFERENCIA, PRO.DESCRPROD, LOC.*, EST.ESTOQUE, BAR.CODBARRA
FROM TGFPRO PRO 
	INNER JOIN AD_LOCALEMP LOC ON (LOC.CODPROD=PRO.CODPROD)
	INNER JOIN TGFEST EST ON (EST.CODPROD=PRO.CODPROD AND EST.CODEMP=LOC.CODEMP)
	INNER JOIN TGFBAR BAR ON (BAR.CODPROD=PRO.CODPROD)
	INNER JOIN TSIUSU USU ON (USU.CODUSU=SANKHYA.STP_GET_CODUSULOGADO())
WHERE (LOC.CODLOCAVA IS NULL OR LOC.CODLOCAVA LIKE :LOCAL+'%')
AND (
		(USU.CODEMP IS NULL AND LOC.CODEMP = :CODEMP) OR
		(USU.CODEMP IS NOT NULL AND LOC.CODEMP = USU.CODEMP)
	)
AND PRO.ATIVO = :ATIVO
--AND EST.ESTOQUE > 0
AND (
	:GRUPO IS NULL OR :GRUPO IS NOT NULL AND (
	(LEN(:GRUPO) > 4 AND PRO.REFERENCIA LIKE :GRUPO) OR
	(LEN(:GRUPO) <= 4 AND PRO.REFERENCIA LIKE :GRUPO+'%')
  ))
AND :TIPO = 'V'
--ORDER BY LOC.CODLOCAVA

) SUB) X
