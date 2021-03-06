SELECT /* RELARTORIO JUSTIFICATIVA DE PERDA*/
CAB.CODEMP AS 'CODEMP', PRO.REFERENCIA AS 'REFERENCIA' ,PRO.DESCRPROD AS 'DESCRPROD' , CAB.CODPARC AS 'CODPARC', CAB.DTNEG AS 'DATA', GIR.CURVAQTD AS 'CURVA',
CASE ITE.AD_JUSTPERDA
	WHEN 1 THEN 'Pre�o'
	WHEN 2 THEN 'Estoque Insuficiente (Existe estoque, mas n�o atende o cliente)'
	WHEN 3 THEN 'Erro de Estoque (Estoque informado no sistema � diferente do real)'
	WHEN 4 THEN 'Estoque zerado (n�o existe o produto no estoque)'
	WHEN 5 THEN 'Outros (Justifique)'
	ELSE 'N�O CADASTRADO NO RELATORIO'
END AS JUSTIFICATIVA,

ITE.OBSERVACAO,

VEN.APELIDO AS 'VENDEDOR'

FROM TGFITE ITE 
	INNER JOIN TGFCAB CAB ON (CAB.NUNOTA=ITE.NUNOTA)
	INNER JOIN TGFPRO PRO ON (PRO.CODPROD=ITE.CODPROD)
	LEFT JOIN TGFVEN VEN ON (CAB.CODVEND=VEN.CODVEND)
	LEFT JOIN TGFGIR1 GIR ON (GIR.CODPROD=ITE.CODPROD AND GIR.CODEMP=ITE.CODEMP)
	INNER JOIN TSIUSU USU ON (USU.CODUSU=SANKHYA.STP_GET_CODUSULOGADO())
WHERE ITE.AD_JUSTPERDA IS NOT NULL
AND (
		(USU.CODEMP IS NULL AND ITE.CODEMP IN :CODEMP) OR
		(USU.CODEMP IS NOT NULL AND ITE.CODEMP = USU.CODEMP)
	)
AND (
	:GRUPO IS NULL OR :GRUPO IS NOT NULL AND (
	(LEN(:GRUPO) > 4 AND PRO.REFERENCIA LIKE :GRUPO) OR
	(LEN(:GRUPO) <= 4 AND PRO.REFERENCIA LIKE :GRUPO+'%')
))
AND CAB.DTNEG BETWEEN :PERIODO.INI AND :PERIODO.FIN
--ORDER BY CAB.CODEMP, PRO.REFERENCIA

UNION ALL

SELECT DISTINCT
ITE.CODEMP AS 'CODEMP', PRO.REFERENCIA AS 'REFERENCIA' ,PRO.DESCRPROD AS 'DESCRPROD' , ITE.CODPARC AS 'CODPARC', ITE.DATA AS 'DATA', GIR.CURVAQTD AS 'CURVA',
CASE ITE.JUSTPERDA
	WHEN 1 THEN 'Pre�o'
	WHEN 2 THEN 'Estoque Insuficiente (Existe estoque, mas n�o atende o cliente)'
	WHEN 3 THEN 'Erro de Estoque (Estoque informado no sistema � diferente do real)'
	WHEN 4 THEN 'Estoque zerado (n�o existe o produto no estoque)'
	WHEN 5 THEN 'Outros (Justifique)'
	ELSE 'N�O CADASTRADO NO RELATORIO'
END AS JUSTIFICATIVA,

ITE.OBSERVACAO,

VEN.APELIDO AS 'VENDEDOR'

FROM AD_JUSPERVEN ITE	
		INNER JOIN TGFPRO PRO ON (PRO.CODPROD=ITE.CODPROD)
		LEFT JOIN TGFVEN VEN ON (VEN.CODVEND = ITE.CODVEND)
		LEFT JOIN TGFGIR1 GIR ON (GIR.CODPROD=PRO.CODPROD AND GIR.CODEMP=ITE.CODEMP)
		INNER JOIN TSIUSU USU ON (USU.CODUSU=SANKHYA.STP_GET_CODUSULOGADO())
WHERE ITE.JUSTPERDA IS NOT NULL
AND (
		(USU.CODEMP IS NULL AND ITE.CODEMP IN :CODEMP) OR
		(USU.CODEMP IS NOT NULL AND ITE.CODEMP = USU.CODEMP)
	)
AND (
	:GRUPO IS NULL OR :GRUPO IS NOT NULL AND (
	(LEN(:GRUPO) > 4 AND PRO.REFERENCIA LIKE :GRUPO) OR
	(LEN(:GRUPO) <= 4 AND PRO.REFERENCIA LIKE :GRUPO+'%')
))
AND ITE.DATA BETWEEN :PERIODO.INI AND :PERIODO.FIN
ORDER BY CAB.CODEMP, PRO.REFERENCIA