SELECT FIN.CODEMP
	 , FIN.DTNEG DTMOV
	 , MONTH (FIN.DTNEG) 'MES'
	 , YEAR (FIN.DTNEG) 'ANO'
	 , NAT.CODCTACTB
	 , PLA.DESCRCTA
	 , NAT.CODNAT
	 , NAT.DESCRNAT
	 , PLA.CODCTACTBSUBST
	 , PLA.CTACTB
	 , FIN.HISTORICO COMPLHIST
	 , EMP.NOMEFANTASIA
	 , FIN.NUMNOTA NUMDOC
	 , FIN.NUFIN
	 , FIN.CODCENCUS
	 , CUS.DESCRCENCUS
	 , FIN.CODPROJ
	 , FIN.VLRLIQUIDO*FIN.RECDESP AS VALOR
	 , FIN.CODNAT
	 , NAT.DESCRNAT
	 , FIN.CODPARC
	 , PAR.RAZAOSOCIAL
FROM VGFFINRAT FIN INNER JOIN TGFNAT NAT ON (NAT.CODNAT= FIN.CODNAT)
                   INNER JOIN TSIEMP EMP ON (EMP.CODEMP = FIN.CODEMP)
                   INNER JOIN TSICUS CUS ON (FIN.CODCENCUS = CUS.CODCENCUS)
                   INNER JOIN TGFPAR PAR ON FIN.CODPARC = PAR.CODPARC
				    LEFT JOIN TCBPLA PLA ON NAT.CODCTACTB = PLA.CODCTACTB
WHERE FIN.DTENTSAI BETWEEN :PERIODO.INI AND :PERIODO.FIN
  AND FIN.CODEMP IN :EMPRESA
  AND CONVERT(INT,FIN.CODNAT/1000000) IN :CODNAT