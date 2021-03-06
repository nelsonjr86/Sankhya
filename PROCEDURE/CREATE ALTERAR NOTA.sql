Create PROCEDURE AD_STP_ALTERASTATUS_NFE (
       @P_CODUSU INT,                -- Código do usuário logado
       @P_IDSESSAO VARCHAR(4000),    -- Identificador da execução. Serve para buscar informações dos parâmetros/campos da execução.
       @P_QTDLINHAS INT,             -- Informa a quantidade de registros selecionados no momento da execução.
       @P_MENSAGEM VARCHAR(4000) OUT -- Caso seja passada uma mensagem aqui, ela será exibida como uma informação ao usuário.
) AS
DECLARE
       @FIELD_NUNOTA INT,
       @I INT,
	   @STATUS	CHAR(1)
BEGIN

       -- Os valores informados pelo formulário de parâmetros, podem ser obtidos com as funções:
       --     ACT_INT_PARAM
       --     ACT_DEC_PARAM
       --     ACT_TXT_PARAM
       --     ACT_DTA_PARAM
       -- Estas funções recebem 2 argumentos:
       --     ID DA SESSÃO - Identificador da execução (Obtido através de P_IDSESSAO))
       --     NOME DO PARAMETRO - Determina qual parametro deve se deseja obter.


       SET @I = 1 -- A variável "I" representa o registro corrente.
       WHILE @I <= @P_QTDLINHAS -- Este loop permite obter o valor de campos dos registros envolvidos na execução.
       BEGIN
           -- Para obter o valor dos campos utilize uma das seguintes funções:
           --     ACT_INT_FIELD (Retorna o valor de um campo tipo NUMÉRICO INTEIRO))
           --     ACT_DEC_FIELD (Retorna o valor de um campo tipo NUMÉRICO DECIMAL))
           --     ACT_TXT_FIELD (Retorna o valor de um campo tipo TEXTO),
           --     ACT_DTA_FIELD (Retorna o valor de um campo tipo DATA)
           -- Estas funções recebem 3 argumentos:
           --     ID DA SESSÃO - Identificador da execução (Obtido através do parâmetro P_IDSESSAO))
           --     NÚMERO DA LINHA - Relativo a qual linha selecionada.
           --     NOME DO CAMPO - Determina qual campo deve ser obtido.
           SET @FIELD_NUNOTA = SANKHYA.ACT_INT_FIELD(@P_IDSESSAO, @I, 'NUNOTA')

		   IF @I > 1
		   BEGIN
				SET @P_MENSAGEM = 'Somente uma nota por vez pode ser selecionada'
				RETURN
		   END


		   SELECT @STATUS = STATUSNFE FROM TGFCAB WHERE NUNOTA = @FIELD_NUNOTA

		   IF @STATUS <> 'E'
		   BEGIN
				SET @P_MENSAGEM = 'Somente nota com status "Aguardando Autorização, pode ser alterada!"'
				RETURN
		   END

		   IF @STATUS = 'E'
		   BEGIN
				UPDATE TGFCAB SET STATUSNFE = 'R'
				 WHERE NUNOTA = @FIELD_NUNOTA
				
				SET @P_MENSAGEM = 'Status NF-e alterado com sucesso!'

		   END


-- <ESCREVA SEU CÓDIGO AQUI (SERÁ EXECUTADO PARA CADA REGISTRO SELECIONADO)> --



           SET @I = @I + 1
       END




-- <ESCREVA SEU CÓDIGO DE FINALIZAÇÃO AQUI> --



END
