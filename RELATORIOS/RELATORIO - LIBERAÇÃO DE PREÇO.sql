SELECT /*LIBERA��O DE PRE�O*/
	PRO.CODPROD, DE.CODEMP, PRO.REFERENCIA, PRO.DESCRPROD, LTRIM(RTRIM(PRO.UNIDADE)) AS 'UNIDADE',
	CASE WHEN DE.VLRDESC IN (0,0.01) THEN DE.VLRDESC WHEN DE.VLRDESC > 0.01 AND CODEMP<> 1 THEN DE.VLRDESC+2 WHEN DE.VLRDESC > 0.01 THEN DE.VLRDESC END  AS 'PERC_DESC'
	,SANKHYA.SNK_PRECO(0,PRO.CODPROD) AS 'PRINCIPAL'
	, SANKHYA.SNK_PRECO(0,PRO.CODPROD)-(SANKHYA.SNK_PRECO(0,PRO.CODPROD)*CASE WHEN DE.CODEMP IN (1) THEN (ISNULL(DE.VLRDESC,1)/100) WHEN DE.CODEMP IN (6,8,9,11) THEN (ISNULL(DE.VLRDESC+2,1)/100) END) AS 'DES_PRINCIPAL'
	, SANKHYA.SNK_PRECO(1,PRO.CODPROD) AS 'FILIAL_BALCAO'
	, SANKHYA.SNK_PRECO(1,PRO.CODPROD)-(SANKHYA.SNK_PRECO(1,PRO.CODPROD)*CASE WHEN DE.CODEMP IN (2,3,4,7,12) THEN (ISNULL(DE.VLRDESC,1)/100) END) AS 'DES_FILIAL_BALCAO'
	, SANKHYA.SNK_PRECO(8,PRO.CODPROD) AS 'ESPECIAL'
	--, SANKHYA.SNK_PRECO(8,PRO.CODPROD)-(SANKHYA.SNK_PRECO(8,PRO.CODPROD)*(ISNULL(DE.VLRDESC+2,1)/100)) AS 'DES_ESPECIAL'
	, SANKHYA.SNK_PRECO(2,PRO.CODPROD) AS 'AZUL'
	, SANKHYA.SNK_PRECO(5,PRO.CODPROD) AS 'VERDE'
	, SANKHYA.SNK_PRECO(6,PRO.CODPROD) AS 'VERMELHA'
	, ISNULL([sankhya].[SNK_GET_CUSTO]('ENTRADACOMICMS',PRO.CODPROD,DE.CODEMP,0,' ',GETDATE(),'S','N','N'),0) AS 'CUSTO_ATUAL' 
       
FROM TGFPRO PRO LEFT JOIN AD_DESCEMP DE ON PRO.CODPROD = DE.CODPROD
WHERE PRO.CODPROD = ISNULL(:CODPROD,PRO.CODPROD)
ORDER BY PRO.REFERENCIA, DE.CODEMP
