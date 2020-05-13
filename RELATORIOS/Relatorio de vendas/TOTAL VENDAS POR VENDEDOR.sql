SELECT VENDEDOR,
	SUM(TOTAL_VENDAS) TOTAL_VENDA,
	SUM(DEVOLUCAO) DEVOLUCAO,
	SUM(TOTAL_VENDAS)+SUM(DEVOLUCAO) RESULTADO,
	SUM(PERC) PERC,
	SUM(TOTAL_VENDAS1)+SUM(DEVOLUCAO1) REALONIBUS,
	CODVEND
FROM (
SELECT /*TOTAL VENDAS POR VENDEDOR*/
    CAB.CODVEND,
	VEN.APELIDO VENDEDOR,
    SUM((ITE.VLRTOT-ITE.VLRDESC)*CASE WHEN :GERENCIAL = 'N' THEN VGF.INDITENS ELSE 1 END) TOTAL_VENDAS,
	0 TOTAL_VENDAS1,
	0 DEVOLUCAO,
	0 DEVOLUCAO1,
	(SUM((ITE.VLRTOT-ITE.VLRDESC)*CASE WHEN :GERENCIAL = 'N' THEN VGF.INDITENS ELSE 1 END)*100/(SELECT /*TOTAL VENDAS POR VENDEDOR*/
      SUM((ITE.VLRTOT-ITE.VLRDESC)*CASE WHEN :GERENCIAL = 'N' THEN VGF.INDITENS ELSE 1 END) TOTAL_VENDAS
FROM TGFCAB CAB
INNER JOIN TGFITE ITE ON (ITE.NUNOTA=CAB.NUNOTA)
INNER JOIN TGFPRO PRD ON (PRD.CODPROD=ITE.CODPROD)
INNER JOIN TGFPAR PAR ON (PAR.CODPARC=CAB.CODPARC)
INNER JOIN TGFVEN VEN ON (VEN.CODVEND=CAB.CODVEND)
INNER JOIN TSIUSU USU ON (USU.CODUSU=SANKHYA.STP_GET_CODUSULOGADO())
 LEFT JOIN VGFCAB VGF ON (CAB.NUNOTA=VGF.NUNOTA)
WHERE /*AND (
		(USU.CODEMP IS NULL AND CAB.CODEMP IN :CODEMP) OR
		(USU.CODEMP IS NOT NULL AND CAB.CODEMP = USU.CODEMP)
	)*/
CAB.CODEMP IN :CODEMP
AND CAB.DTNEG BETWEEN :PERIODO.INI AND :PERIODO.FIN
AND CAB.CODTIPOPER IN (900,901,931,934,935)
AND CAB.TIPMOV='V'
AND CAB.STATUSNOTA = 'L'
AND (PRD.REFERENCIA LIKE :REFERENCIA+'%' OR :REFERENCIA IS NULL)
AND (
	(:GERENCIAL = 'S' AND ((CAB.CODVEND <> 59 AND CAB.CODPARC NOT IN (7441) AND CAB.NUNOTA IN (SELECT NUNOTA FROM TGFFIN WHERE ORIGEM = 'E' AND CODTIPTIT <> 69)) OR (CAB.CODTIPOPER NOT IN (934,931,912) AND CAB.CODPARC NOT IN (7441)))
		OR :GERENCIAL = 'N'
	)))) AS 'PERC'
FROM TGFCAB CAB
INNER JOIN TGFITE ITE ON (ITE.NUNOTA=CAB.NUNOTA)
INNER JOIN TGFPRO PRD ON (PRD.CODPROD=ITE.CODPROD)
INNER JOIN TGFPAR PAR ON (PAR.CODPARC=CAB.CODPARC)
INNER JOIN TGFVEN VEN ON (VEN.CODVEND=CAB.CODVEND)
INNER JOIN TSIUSU USU ON (USU.CODUSU=SANKHYA.STP_GET_CODUSULOGADO())
 LEFT JOIN VGFCAB VGF ON (CAB.NUNOTA=VGF.NUNOTA)
WHERE /*AND (
		(USU.CODEMP IS NULL AND CAB.CODEMP IN :CODEMP) OR
		(USU.CODEMP IS NOT NULL AND CAB.CODEMP = USU.CODEMP)
	)*/
CAB.CODEMP IN :CODEMP
AND CAB.DTNEG BETWEEN :PERIODO.INI AND :PERIODO.FIN
AND CAB.CODTIPOPER IN (900,901,931,934,935)
AND CAB.TIPMOV='V'
AND CAB.STATUSNOTA = 'L'
AND (PRD.REFERENCIA LIKE :REFERENCIA+'%' OR :REFERENCIA IS NULL)
AND (
	(:GERENCIAL = 'S' AND ((CAB.CODVEND <> 59 AND CAB.CODPARC NOT IN (7441) AND CAB.NUNOTA IN (SELECT NUNOTA FROM TGFFIN WHERE ORIGEM = 'E' AND CODTIPTIT <> 69)) OR (CAB.CODTIPOPER NOT IN (934,931,912) AND CAB.CODPARC NOT IN (7441)))
		OR :GERENCIAL = 'N'
	))
GROUP BY VEN.APELIDO, CAB.CODVEND

UNION ALL

SELECT /*TOTAL VENDAS POR VENDEDOR*/
    CAB.CODVEND,
	VEN.APELIDO VENDEDOR,
    0 TOTAL_VENDAS,
	0 TOTAL_VENDAS1,
	SUM((ITE.VLRTOT-ITE.VLRDESC)*CASE WHEN 'S' = 'N' THEN VGF.INDITENS ELSE 1 END)*-1 DEVOLUCAO,
	0 DEVOLUCAO1,
	0 AS 'PERC'
FROM TGFITE ITE
    INNER JOIN TGFCAB CAB ON (CAB.NUNOTA=ITE.NUNOTA)
    INNER JOIN TSIEMP EMP ON (EMP.CODEMP=CAB.CODEMP)
    INNER JOIN TGFPRO PRD ON (PRD.CODPROD=ITE.CODPROD)
	INNER JOIN TGFVEN VEN ON (VEN.CODVEND=CAB.CODVEND)
	INNER JOIN TSIUSU USU ON (USU.CODUSU=SANKHYA.STP_GET_CODUSULOGADO())
     LEFT JOIN VGFCAB VGF ON CAB.NUNOTA = VGF.NUNOTA
WHERE CAB.CODTIPOPER IN (1000,1001,1007,1009,792,1011,1012)
	AND CAB.STATUSNOTA = 'L'
    AND CAB.DTMOV BETWEEN :PERIODO.INI AND :PERIODO.FIN
    /*AND (
		(USU.CODEMP IS NULL AND CAB.CODEMP IN :CODEMP) OR
		(USU.CODEMP IS NOT NULL AND CAB.CODEMP = USU.CODEMP)
	)*/
	AND CAB.CODEMP IN :CODEMP
    AND (PRD.REFERENCIA LIKE :REFERENCIA+'%' OR :REFERENCIA IS NULL)
	AND (
		(:GERENCIAL = 'S' AND ((CAB.CODVEND <> 59 AND CAB.CODPARC NOT IN (7441) AND CAB.NUNOTA IN (SELECT NUNOTA FROM TGFFIN WHERE ORIGEM = 'E' AND CODTIPTIT <> 69)) OR (CAB.CODTIPOPER NOT IN (934,931,912) AND CAB.CODPARC NOT IN (7441)))
		OR :GERENCIAL = 'N'
	))
GROUP BY VEN.APELIDO, CAB.CODVEND

UNION ALL

SELECT /*TOTAL REAL ONIBUS*/
    CAB.CODVEND,
	VEN.APELIDO VENDEDOR,
	0 TOTAL_VENDAS,
    SUM((ITE.VLRTOT-ITE.VLRDESC)*CASE WHEN :GERENCIAL = 'N' THEN VGF.INDITENS ELSE 1 END) TOTAL_VENDAS1,
	0 DEVOLUCAO,
	0 DEVOLUCAO1,
	(SUM((ITE.VLRTOT-ITE.VLRDESC)*CASE WHEN :GERENCIAL = 'N' THEN VGF.INDITENS ELSE 1 END)*100/(SELECT /*TOTAL VENDAS POR VENDEDOR*/
      SUM((ITE.VLRTOT-ITE.VLRDESC)*CASE WHEN :GERENCIAL = 'N' THEN VGF.INDITENS ELSE 1 END) TOTAL_VENDAS1
FROM TGFCAB CAB
INNER JOIN TGFITE ITE ON (ITE.NUNOTA=CAB.NUNOTA)
INNER JOIN TGFPRO PRD ON (PRD.CODPROD=ITE.CODPROD)
INNER JOIN TGFPAR PAR ON (PAR.CODPARC=CAB.CODPARC)
INNER JOIN TGFVEN VEN ON (VEN.CODVEND=CAB.CODVEND)
INNER JOIN TSIUSU USU ON (USU.CODUSU=SANKHYA.STP_GET_CODUSULOGADO())
 LEFT JOIN VGFCAB VGF ON (CAB.NUNOTA=VGF.NUNOTA)
WHERE /*AND (
		(USU.CODEMP IS NULL AND CAB.CODEMP IN :CODEMP) OR
		(USU.CODEMP IS NOT NULL AND CAB.CODEMP = USU.CODEMP)
	)*/
CAB.CODEMP IN :CODEMP
AND CAB.DTNEG BETWEEN :PERIODO.INI AND :PERIODO.FIN
AND CAB.CODTIPOPER IN (900,901,931,934,935)
AND CAB.CODPARC IN (6055,6056,6057,6058,6059,6060,6061,6062,6063,6064,6065)
AND CAB.TIPMOV='V'
AND CAB.STATUSNOTA = 'L'
AND (PRD.REFERENCIA LIKE :REFERENCIA+'%' OR :REFERENCIA IS NULL)
AND (
	(:GERENCIAL = 'S' AND ((CAB.CODVEND <> 59 AND CAB.CODPARC NOT IN (7441) AND CAB.NUNOTA IN (SELECT NUNOTA FROM TGFFIN WHERE ORIGEM = 'E' AND CODTIPTIT <> 69)) OR (CAB.CODTIPOPER NOT IN (934,931,912) AND CAB.CODPARC NOT IN (7441)))
		OR :GERENCIAL = 'N'
	)))) AS 'PERC'
FROM TGFCAB CAB
INNER JOIN TGFITE ITE ON (ITE.NUNOTA=CAB.NUNOTA)
INNER JOIN TGFPRO PRD ON (PRD.CODPROD=ITE.CODPROD)
INNER JOIN TGFPAR PAR ON (PAR.CODPARC=CAB.CODPARC)
INNER JOIN TGFVEN VEN ON (VEN.CODVEND=CAB.CODVEND)
INNER JOIN TSIUSU USU ON (USU.CODUSU=SANKHYA.STP_GET_CODUSULOGADO())
 LEFT JOIN VGFCAB VGF ON (CAB.NUNOTA=VGF.NUNOTA)
WHERE /*AND (
		(USU.CODEMP IS NULL AND CAB.CODEMP IN :CODEMP) OR
		(USU.CODEMP IS NOT NULL AND CAB.CODEMP = USU.CODEMP)
	)*/
CAB.CODEMP IN :CODEMP
AND CAB.DTNEG BETWEEN :PERIODO.INI AND :PERIODO.FIN
AND CAB.CODTIPOPER IN (900,901,931,934,935)
AND CAB.CODPARC IN (6055,6056,6057,6058,6059,6060,6061,6062,6063,6064,6065)
AND CAB.TIPMOV='V'
AND CAB.STATUSNOTA = 'L'
AND (PRD.REFERENCIA LIKE :REFERENCIA+'%' OR :REFERENCIA IS NULL)
AND (
	(:GERENCIAL = 'S' AND ((CAB.CODVEND <> 59 AND CAB.CODPARC NOT IN (7441) AND CAB.NUNOTA IN (SELECT NUNOTA FROM TGFFIN WHERE ORIGEM = 'E' AND CODTIPTIT <> 69)) OR (CAB.CODTIPOPER NOT IN (934,931,912) AND CAB.CODPARC NOT IN (7441)))
		OR :GERENCIAL = 'N'
	))
GROUP BY VEN.APELIDO, CAB.CODVEND

UNION ALL

SELECT /*TOTAL REAL ONIBUS*/
    CAB.CODVEND,
	VEN.APELIDO VENDEDOR,
    0 TOTAL_VENDAS,
	0 TOTAL_VENDAS1,
	0 DEVOLUCAO,
	SUM((ITE.VLRTOT-ITE.VLRDESC)*CASE WHEN 'S' = 'N' THEN VGF.INDITENS ELSE 1 END)*-1 DEVOLUCAO1,
	0 AS 'PERC'
FROM TGFITE ITE
    INNER JOIN TGFCAB CAB ON (CAB.NUNOTA=ITE.NUNOTA)
    INNER JOIN TSIEMP EMP ON (EMP.CODEMP=CAB.CODEMP)
    INNER JOIN TGFPRO PRD ON (PRD.CODPROD=ITE.CODPROD)
	INNER JOIN TGFVEN VEN ON (VEN.CODVEND=CAB.CODVEND)
	INNER JOIN TSIUSU USU ON (USU.CODUSU=SANKHYA.STP_GET_CODUSULOGADO())
     LEFT JOIN VGFCAB VGF ON CAB.NUNOTA = VGF.NUNOTA
WHERE CAB.CODTIPOPER IN (1000,1001,1007,1009,792,1011,1012)
	AND CAB.CODPARC IN (6055,6056,6057,6058,6059,6060,6061,6062,6063,6064,6065)
	AND CAB.STATUSNOTA = 'L'
    AND CAB.DTMOV BETWEEN :PERIODO.INI AND :PERIODO.FIN
    /*AND (
		(USU.CODEMP IS NULL AND CAB.CODEMP IN :CODEMP) OR
		(USU.CODEMP IS NOT NULL AND CAB.CODEMP = USU.CODEMP)
	)*/
	AND CAB.CODEMP IN :CODEMP
    AND (PRD.REFERENCIA LIKE :REFERENCIA+'%' OR :REFERENCIA IS NULL)
	AND (
		(:GERENCIAL = 'S' AND ((CAB.CODVEND <> 59 AND CAB.CODPARC NOT IN (7441) AND CAB.NUNOTA IN (SELECT NUNOTA FROM TGFFIN WHERE ORIGEM = 'E' AND CODTIPTIT <> 69)) OR (CAB.CODTIPOPER NOT IN (934,931,912) AND CAB.CODPARC NOT IN (7441)))
		OR :GERENCIAL = 'N'
	))
GROUP BY VEN.APELIDO, CAB.CODVEND

) SUB
GROUP BY VENDEDOR, CODVEND
ORDER BY VENDEDOR