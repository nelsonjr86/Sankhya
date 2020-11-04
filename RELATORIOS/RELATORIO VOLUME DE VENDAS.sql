WITH X AS ( 
SELECT /* PLANILHA SIGNA */

		ANO,MES,DIA,CODVEND,
		ROUND(SUM(VOLUME_DE_VENDAS_MENSAL),2) VOL_VENDAS_MENSAL,
		SUM(CLIENTES_ATENDIDOS) CLIENTES_ATENDIDOS,
		SUM(NOTAS_EMITIDAS) NUM_PEDIDOS_VENDAS_EMITIDOS,
		SUM(QUATIDADE_ITENS) NUM_ITENS_VENDIDOS,
		SUM(VENDA_PERDIDA_PRECO) VENDAS_PERDIDAS_PRECO,
		SUM(RUPTURAS) NUM_RUPTURAS,
		SUM(DEVOLUCAO_VENDA) NUM_DEVOLUCAO_VENDA_QTE,
		SUM(ORCAMENTO_SOLICITADO) NUM_ORCAMENTO_SOLICITADO_QTE,
		SUM(ORCAMENTO_SOLICITADO_APROVADO) NUM_ORCAMENTO_SOLICITADO_APROVADO_QTE

		FROM (

			-- VOLUME DE VENDAS MENSAL (R$)

			SELECT YEAR(DTNEG) ANO,MONTH(DTNEG) MES,DAY(DTNEG) DIA,CODVEND, SUM(VLRNOTA) VOLUME_DE_VENDAS_MENSAL,0 CLIENTES_ATENDIDOS,0 NIVEL_ESTOQUE,0 NOTAS_EMITIDAS,0 TRANSFERENCIAS,0 DEVOLUCAO_COMPRA,0 DEVOLUCAO_VENDA,0 VENDA_POR_PRECO_CUSTO,0 RUPTURAS,0 VENDA_PERDIDA_PRECO, 0 QUATIDADE_ITENS, 0 ORCAMENTO_SOLICITADO, 0 ORCAMENTO_SOLICITADO_APROVADO
			FROM TGFCAB WITH (NOLOCK)
			WHERE TIPMOV='V' AND CODVEND <> 59 AND NUNOTA IN (SELECT NUNOTA FROM TGFFIN WITH (NOLOCK) WHERE ORIGEM = 'E' AND CODTIPTIT <> 69)
			AND CODTIPOPER IN (900,934)

			AND DTNEG >= CAST( CONCAT('01/', DATEPART(MONTH,DATEADD(MONTH, -13, GETDATE())),'/',DATEPART(YEAR,DATEADD(MONTH, -13, GETDATE())) ) AS DATE)
			--AND DTNEG < CAST( CONCAT('01/', DATEPART(MONTH,GETDATE()),'/',DATEPART(YEAR,GETDATE()) ) AS DATE)
			AND DTNEG > '2019-07-01' 
			AND DTNEG <= GETDATE()
			AND TGFCAB.CODEMP NOT IN (5,10,13,26,27)

			GROUP BY YEAR(DTNEG),MONTH(DTNEG),DAY(DTNEG),CODVEND
			
			UNION ALL
			-- N� DE OR�AMENTOS SOLICITADOS (QTE)

			SELECT YEAR(CAB.DTNEG) ANO,MONTH(CAB.DTNEG) MES,DAY(CAB.DTNEG) DIA,CAB.CODVEND,0 VOLUME_DE_VENDAS_MENSAL, COUNT(CAB.CODPARC) CLIENTES_ATENDIDOS, 0 NIVEL_ESTOQUE,0 NOTAS_EMITIDAS,0 TRANSFERENCIAS,0 DEVOLUCAO_COMPRA,0 DEVOLUCAO_VENDA,0 VENDA_POR_PRECO_CUSTO,0 RUPTURAS,0 VENDA_PERDIDA_PRECO, 0 QUATIDADE_ITENS, 0 ORCAMENTO_SOLICITADO, 0 ORCAMENTO_SOLICITADO_APROVADO
			FROM TGFCAB CAB WITH (NOLOCK)
			WHERE CAB.TIPMOV='P'
			--AND CAB.CODTIPOPER  IN (230)

			AND CAB.DTNEG >= CAST( CONCAT('01/', DATEPART(MONTH,DATEADD(MONTH, -13, GETDATE())),'/',DATEPART(YEAR,DATEADD(MONTH, -13, GETDATE())) ) AS DATE)
			--AND DTNEG < CAST( CONCAT('01/', DATEPART(MONTH,GETDATE()),'/',DATEPART(YEAR,GETDATE()) ) AS DATE)
			AND CAB.DTNEG > '2019-07-01'
			AND CAB.DTNEG <= GETDATE()
			AND CAB.CODEMP NOT IN (5,10,13,26,27)

			GROUP BY YEAR(CAB.DTNEG),MONTH(CAB.DTNEG),DAY(DTNEG),CAB.CODVEND, CAB.CODPARC

			UNION ALL


			-- N� DE TICKETS EMITIDOS (QTE)

			SELECT YEAR(DTNEG) ANO,MONTH(DTNEG) MES,DAY(DTNEG) DIA,CODVEND,0 VOLUME_DE_VENDAS_MENSAL,0 CLIENTES_ATENDIDOS,0 NIVEL_ESTOQUE,COUNT(NUNOTA) NOTAS_EMITIDAS,0 TRANSFERENCIAS,0 DEVOLUCAO_COMPRA,0 DEVOLUCAO_VENDA,0 VENDA_POR_PRECO_CUSTO,0 RUPTURAS,0 VENDA_PERDIDA_PRECO, 0 QUATIDADE_ITENS, 0 ORCAMENTO_SOLICITADO, 0 ORCAMENTO_SOLICITADO_APROVADO
			FROM TGFCAB WITH (NOLOCK)
			WHERE TIPMOV='V' AND CODVEND <> 59 AND NUNOTA IN (SELECT NUNOTA FROM TGFFIN WITH (NOLOCK) WHERE ORIGEM = 'E' AND CODTIPTIT <> 69)
			AND CODTIPOPER IN (900,934)

			AND DTNEG >= CAST( CONCAT('01/', DATEPART(MONTH,DATEADD(MONTH, -13, GETDATE())),'/',DATEPART(YEAR,DATEADD(MONTH, -13, GETDATE())) ) AS DATE)
			--AND DTNEG < CAST( CONCAT('01/', DATEPART(MONTH,GETDATE()),'/',DATEPART(YEAR,GETDATE()) ) AS DATE)
			AND DTNEG > '2019-07-01'
			AND DTNEG <= GETDATE()
			AND TGFCAB.CODEMP NOT IN (5,10,13,26,27)

			GROUP BY YEAR(DTNEG),MONTH(DTNEG),DAY(DTNEG),CODVEND


			UNION ALL

			-- N� DE DEVOLU��ES DE VENDAS (QTE)

			SELECT YEAR(DTNEG) ANO,MONTH(DTNEG) MES,DAY(DTNEG) DIA,CODVEND,0 VOLUME_DE_VENDAS_MENSAL,0 CLIENTES_ATENDIDOS,0 NIVEL_ESTOQUE,0 NOTAS_EMITIDAS,0 TRANSFERENCIAS,0 DEVOLUCAO_COMPRA,COUNT(NUNOTA) DEVOLUCAO_VENDA,0 VENDA_POR_PRECO_CUSTO,0 RUPTURAS,0 VENDA_PERDIDA_PRECO, 0 QUATIDADE_ITENS, 0 ORCAMENTO_SOLICITADO, 0 ORCAMENTO_SOLICITADO_APROVADO
			FROM TGFCAB WITH (NOLOCK)
			WHERE TIPMOV='D'

			AND DTNEG >= CAST( CONCAT('01/', DATEPART(MONTH,DATEADD(MONTH, -13, GETDATE())),'/',DATEPART(YEAR,DATEADD(MONTH, -13, GETDATE())) ) AS DATE)
			--AND DTNEG < CAST( CONCAT('01/', DATEPART(MONTH,GETDATE()),'/',DATEPART(YEAR,GETDATE()) ) AS DATE)
			AND DTNEG > '2019-07-01'
			AND DTNEG <= GETDATE()
			AND TGFCAB.CODEMP NOT IN (5,10,13,26,27)

			GROUP BY YEAR(DTNEG),MONTH(DTNEG),DAY(DTNEG),CODVEND
					   

			UNION ALL
					   
			-- N� DE RUPTURAS DE ESTOQUES NO M�S (QTE)

			SELECT YEAR(DATA) ANO,MONTH(DATA) MES,DAY(DATA) DIA,CODVEND,0 VOLUME_DE_VENDAS_MENSAL,0 CLIENTES_ATENDIDOS,0 NIVEL_ESTOQUE,0 NOTAS_EMITIDAS,0 TRANSFERENCIAS,0 DEVOLUCAO_COMPRA,0 DEVOLUCAO_VENDA,0 VENDA_POR_PRECO_CUSTO,SUM(QTE) RUPTURAS,0 VENDA_PERDIDA_PRECO, 0 QUATIDADE_ITENS, 0 ORCAMENTO_SOLICITADO, 0 ORCAMENTO_SOLICITADO_APROVADO
			FROM (

				--SELECT C.DTNEG DATA,1 QTE
				SELECT C.DTNEG DATA,I.QTDNEG QTE,C.CODVEND
				FROM TGFITE I WITH (NOLOCK)
				INNER JOIN TGFCAB C WITH (NOLOCK) ON (C.NUNOTA=I.NUNOTA)
				WHERE I.AD_JUSTPERDA = 4
				AND C.DTNEG >= CAST( CONCAT('01/', DATEPART(MONTH,DATEADD(MONTH, -13, GETDATE())),'/',DATEPART(YEAR,DATEADD(MONTH, -13, GETDATE())) ) AS DATE)
				--AND C.DTNEG < CAST( CONCAT('01/', DATEPART(MONTH,GETDATE()),'/',DATEPART(YEAR,GETDATE()) ) AS DATE)
			 	AND C.DTNEG > '2019-07-01'
			    AND DTNEG <= GETDATE()
			    AND I.CODEMP NOT IN (5,10,13,26,27)

				UNION ALL

				SELECT I.DATA DATA,1 QTE,I.CODVEND
				FROM AD_JUSPERVEN I WITH (NOLOCK)
				WHERE I.JUSTPERDA = 4
				AND I.DATA >= CAST( CONCAT('01/', DATEPART(MONTH,DATEADD(MONTH, -13, GETDATE())),'/',DATEPART(YEAR,DATEADD(MONTH, -13, GETDATE())) ) AS DATE)
				--AND I.DATA < CAST( CONCAT('01/', DATEPART(MONTH,GETDATE()),'/',DATEPART(YEAR,GETDATE()) ) AS DATE)
				AND I.DATA > '2019-07-01'
			    AND I.DATA <= GETDATE()
			    AND I.CODEMP NOT IN (5,10,13,26,27)

			)SUB2

			GROUP BY YEAR(DATA),MONTH(DATA),DAY(DATA),CODVEND


			UNION ALL


			-- N� DE VENDAS PERDIDAS POR PRE�O (QTE)

			SELECT YEAR(DATA) ANO,MONTH(DATA) MES,DAY(DATA) DIA,CODVEND,0 VOLUME_DE_VENDAS_MENSAL,0 CLIENTES_ATENDIDOS,0 NIVEL_ESTOQUE,0 NOTAS_EMITIDAS,0 TRANSFERENCIAS,0 DEVOLUCAO_COMPRA,0 DEVOLUCAO_VENDA,0 VENDA_POR_PRECO_CUSTO,0 RUPTURAS,SUM(QTE) VENDA_PERDIDA_PRECO, 0 QUATIDADE_ITENS, 0 ORCAMENTO_SOLICITADO, 0 ORCAMENTO_SOLICITADO_APROVADO
			FROM (

				--SELECT C.DTNEG DATA,1 QTE
				SELECT C.DTNEG DATA,I.QTDNEG QTE, C.CODVEND
				FROM TGFITE I WITH (NOLOCK)
				INNER JOIN TGFCAB C WITH (NOLOCK) ON (C.NUNOTA=I.NUNOTA)
				WHERE I.AD_JUSTPERDA = 1
				AND C.DTNEG >= CAST( CONCAT('01/', DATEPART(MONTH,DATEADD(MONTH, -13, GETDATE())),'/',DATEPART(YEAR,DATEADD(MONTH, -13, GETDATE())) ) AS DATE)
				--AND C.DTNEG < CAST( CONCAT('01/', DATEPART(MONTH,GETDATE()),'/',DATEPART(YEAR,GETDATE()) ) AS DATE)
				AND C.DTNEG > '2019-07-01'
			    AND C.DTNEG <= GETDATE()
			    AND I.CODEMP NOT IN (5,10,13,26,27)

				UNION ALL

				SELECT I.DATA DATA,1 QTE, I.CODVEND
				FROM AD_JUSPERVEN I WITH (NOLOCK)
				WHERE I.JUSTPERDA = 1
				AND I.DATA >= CAST( CONCAT('01/', DATEPART(MONTH,DATEADD(MONTH, -13, GETDATE())),'/',DATEPART(YEAR,DATEADD(MONTH, -13, GETDATE())) ) AS DATE)
				--AND I.DATA < CAST( CONCAT('01/', DATEPART(MONTH,GETDATE()),'/',DATEPART(YEAR,GETDATE()) ) AS DATE)
				AND I.DATA > '2019-07-01'
			    AND I.DATA <= GETDATE()
			    AND I.CODEMP NOT IN (5,10,13,26,27)

			)SUB3

			GROUP BY YEAR(DATA),MONTH(DATA),DAY(DATA),CODVEND

			UNION ALL

			-- QUANTIDADE DE ITENS VENDIDOS (QTE)

			SELECT YEAR(CAB.DTNEG) ANO,MONTH(CAB.DTNEG) MES,DAY(CAB.DTNEG) DIA,CAB.CODVEND,0 VOLUME_DE_VENDAS_MENSAL,0 CLIENTES_ATENDIDOS,0 NIVEL_ESTOQUE,0 NOTAS_EMITIDAS,0 TRANSFERENCIAS,0 DEVOLUCAO_COMPRA,0 DEVOLUCAO_VENDA,0 VENDA_POR_PRECO_CUSTO,0 RUPTURAS,0 VENDA_PERDIDA_PRECO, SUM(ITE.QTDNEG) QUATIDADE_ITENS, 0 ORCAMENTO_SOLICITADO, 0 ORCAMENTO_SOLICITADO_APROVADO
			FROM TGFCAB CAB WITH (NOLOCK) INNER JOIN TGFITE ITE ON (ITE.NUNOTA=CAB.NUNOTA)
			WHERE CAB.TIPMOV='V'
			AND CAB.STATUSNOTA = 'L'
			AND CAB.CODTIPOPER NOT IN (908,912)

			AND CAB.DTNEG >= CAST( CONCAT('01/', DATEPART(MONTH,DATEADD(MONTH, -13, GETDATE())),'/',DATEPART(YEAR,DATEADD(MONTH, -13, GETDATE())) ) AS DATE)
			--AND DTNEG < CAST( CONCAT('01/', DATEPART(MONTH,GETDATE()),'/',DATEPART(YEAR,GETDATE()) ) AS DATE)
			AND CAB.DTNEG > '2019-07-01'
			AND CAB.DTNEG <= GETDATE()
			AND CAB.CODEMP NOT IN (5,10,13,26,27)

			GROUP BY YEAR(CAB.DTNEG),MONTH(CAB.DTNEG),DAY(CAB.DTNEG),CAB.CODVEND

			UNION ALL

			-- N� DE OR�AMENTOS SOLICITADOS (QTE)

			SELECT YEAR(CAB.DTNEG) ANO,MONTH(CAB.DTNEG) MES,DAY(CAB.DTNEG) DIA,CAB.CODVEND,0 VOLUME_DE_VENDAS_MENSAL,0 CLIENTES_ATENDIDOS,0 NIVEL_ESTOQUE,0 NOTAS_EMITIDAS,0 TRANSFERENCIAS,0 DEVOLUCAO_COMPRA,0 DEVOLUCAO_VENDA,0 VENDA_POR_PRECO_CUSTO,0 RUPTURAS,0 VENDA_PERDIDA_PRECO, 0 QUATIDADE_ITENS, COUNT(CAB.NUNOTA) ORCAMENTO_SOLICITADO, 0 ORCAMENTO_SOLICITADO_APROVADO
			FROM TGFCAB CAB WITH (NOLOCK)
			WHERE CAB.TIPMOV='P'
			--AND CAB.STATUSNOTA = 'L'
			AND CAB.CODTIPOPER  IN (200)

			AND CAB.DTNEG >= CAST( CONCAT('01/', DATEPART(MONTH,DATEADD(MONTH, -13, GETDATE())),'/',DATEPART(YEAR,DATEADD(MONTH, -13, GETDATE())) ) AS DATE)
			--AND DTNEG < CAST( CONCAT('01/', DATEPART(MONTH,GETDATE()),'/',DATEPART(YEAR,GETDATE()) ) AS DATE)
			AND CAB.DTNEG > '2019-07-01'
			AND CAB.DTNEG <= GETDATE()
			AND CAB.CODEMP NOT IN (5,10,13,26,27)

			GROUP BY YEAR(CAB.DTNEG),MONTH(CAB.DTNEG),DAY(CAB.DTNEG),CAB.CODVEND

			UNION ALL

			-- N� DE OR�AMENTOS SOLICITADOS (QTE)

			SELECT YEAR(CAB.DTNEG) ANO,MONTH(CAB.DTNEG) MES,DAY(CAB.DTNEG) DIA,CAB.CODVEND,0 VOLUME_DE_VENDAS_MENSAL,0 CLIENTES_ATENDIDOS,0 NIVEL_ESTOQUE,0 NOTAS_EMITIDAS,0 TRANSFERENCIAS,0 DEVOLUCAO_COMPRA,0 DEVOLUCAO_VENDA,0 VENDA_POR_PRECO_CUSTO,0 RUPTURAS,0 VENDA_PERDIDA_PRECO, 0 QUATIDADE_ITENS, 0 ORCAMENTO_SOLICITADO, COUNT(CAB.NUNOTA) ORCAMENTO_SOLICITADO_APROVADO
			FROM TGFCAB CAB WITH (NOLOCK)
			WHERE CAB.TIPMOV='P'
			AND CAB.STATUSNOTA = 'L'
			--AND CAB.PENDENTE = 'N'
			AND CAB.CODTIPOPER  IN (200)

			AND CAB.DTNEG >= CAST( CONCAT('01/', DATEPART(MONTH,DATEADD(MONTH, -13, GETDATE())),'/',DATEPART(YEAR,DATEADD(MONTH, -13, GETDATE())) ) AS DATE)
			--AND DTNEG < CAST( CONCAT('01/', DATEPART(MONTH,GETDATE()),'/',DATEPART(YEAR,GETDATE()) ) AS DATE)
			AND CAB.DTNEG > '2019-07-01'
			AND CAB.DTNEG <= GETDATE()
			AND CAB.CODEMP NOT IN (5,10,13,26,27)

			GROUP BY YEAR(CAB.DTNEG),MONTH(CAB.DTNEG),DAY(CAB.DTNEG),CAB.CODVEND

		) S
		GROUP BY ANO,MES,DIA, CODVEND
		--ORDER BY ANO DESC ,MES DESC , CODVEND ASC
)	SELECT X.*, TGFVEN.APELIDO AS 'VENDEDOR' 
		FROM X INNER JOIN TGFVEN ON X.CODVEND=TGFVEN.CODVEND
		WHERE X.DIA BETWEEN DAY(:PERIODO.INI) AND DAY(:PERIODO.FIN)
			  AND X.MES BETWEEN MONTH(:PERIODO.INI) AND MONTH(:PERIODO.FIN)
			  AND X.ANO BETWEEN YEAR(:PERIODO.INI) AND YEAR(:PERIODO.FIN)
			  AND (X.CODVEND = :CODVEND OR :CODVEND IS NULL)
	ORDER BY X.ANO DESC ,X.MES DESC, X.DIA DESC , X.CODVEND ASC