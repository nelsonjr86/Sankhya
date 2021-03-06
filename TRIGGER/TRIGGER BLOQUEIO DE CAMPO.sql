
-- ===============================================================
-- Author: xxx
--
-- Create date: 27/03/2018
-- Description: Proibir alteração a Referencia de produtos
--
-- ===============================================================
CREATE TRIGGER [sankhya].[AD_TRG_UPD_TGFPRO]
   ON  [sankhya].[TGFPRO]
   FOR UPDATE
AS
BEGIN
    DECLARE
	   @V_GRUPOUSU			INT,
	   @V_AUX				INT,

	   @N_USOPROD			VARCHAR(1),

	   @N_REFERENCIA		VARCHAR(255),
	   @A_REFERENCIA		VARCHAR(255),

	   @ERRMSG				VARCHAR(255),
	   @SOLICITANTE			CHAR(30)

	SET NOCOUNT ON


	SELECT
		@N_REFERENCIA = I.REFERENCIA,
		@V_GRUPOUSU = (SELECT CODGRUPO FROM TSIUSU WHERE CODUSU = I.CODUSU)
	FROM INSERTED I

	SELECT
		@A_REFERENCIA = REFERENCIA,
		@N_USOPROD = USOPROD
	FROM DELETED


	IF @V_GRUPOUSU IN (0,17,18,19,1,39,40,41,42,43,44,49,45) 
		RETURN

	IF @A_REFERENCIA <> @N_REFERENCIA  AND @N_USOPROD = 'R'
	BEGIN
		SET @ERRMSG = '------------------------------------------------


A REFERENCIA não pode ser alterado, favor procurar CPD.


------------------------------------------------'
		GOTO ERROR
	END

	

	RETURN
	ERROR:
	EXEC SANKHYA.SNK_ERROR @ERRMSG

    SELECT @SOLICITANTE = program_name
    FROM MASTER.DBO.SYSPROCESSES (NOLOCK)
    WHERE SPID = @@SPID

    IF (UPPER(@SOLICITANTE) LIKE 'MICROSOFT%') OR
             (@SOLICITANTE = 'MS SQLEM') OR 
             (@SOLICITANTE = 'MS SQL QUERY ANALYZER') OR
        (UPPER(@SOLICITANTE) LIKE 'TOAD%')     
    ROLLBACK TRANSACTION
END