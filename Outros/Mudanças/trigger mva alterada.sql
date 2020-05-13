USE [SANKHYA_PROD]
GO
/****** Object:  Trigger [sankhya].[TRG_INC_UPD_TGFCAB_MVA]    Script Date: 04/02/2020 15:57:02 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
ALTER TRIGGER [sankhya].[TRG_INC_UPD_TGFCAB_MVA] ON [sankhya].[TGFCAB]
    WITH EXEC AS CALLER
    AFTER INSERT, UPDATE
    AS
    BEGIN
        DECLARE
            @P_COUNT   INT,
            @P_VALIDAR BIT,
            @P_VLRSUBST FLOAT,
            @P_QTD      FLOAT,
            @P_VLRICMS  FLOAT,
            @P_VLRIPI  FLOAT,
            @P_VLRUNIT  FLOAT,
            @P_BASEST   FLOAT,
            @P_BASEICMS FLOAT,
            @P_MVA      FLOAT,
            @P_ST       FLOAT,
            @P_DIFMVA   FLOAT,
            @P_SEQUENCIA INT,
            @P_NUNOTA INT,
            @P_CODPROD INT,
            @P_CODEMP INT,
            @P_CIDORIG INT,
            @P_CIDDEST INT,
            @P_CODPARC INT,
            @P_UFORIG INT,
            @P_UFDEST INT,
            @P_CODGRUPOICMS INT,
            @P_CODGRUPPAR INT,
            @P_ALIQCALC FLOAT,
            @P_MVACALC FLOAT,
            @P_ALIQSTCALC FLOAT,
            @P_CALC FLOAT,
            @P_STCALC FLOAT,
            @P_ICMSCALC FLOAT,
            @MVAAJUSTADO CHAR(1),
            @P_TIPMOV CHAR(1),
            @P_VLRFRETE FLOAT,
            @P_VLRFRETEITE FLOAT,
            @P_VLRNOTA FLOAT,
            @P_CALCFRETE FLOAT
        BEGIN

            --EXEC SANKHYA.SNK_ERROR 'CHEGOU11';

            IF (SANKHYA.STP_GET_ATUALIZANDO() = 1)
                RETURN

            EXEC FPODEVALIDAR 'TGFCAB', @P_VALIDAR OUTPUT
            IF @P_VALIDAR = 0
                RETURN

            SELECT @P_VLRSUBST = I.VLRSUBST FROM INSERTED I;
            --EXEC SANKHYA.SNK_ERROR @P_VLRSUBST
            --EXEC SANKHYA.SNK_ERROR @P_NUNOTA
            /*IF (@P_VLRSUBST) = 0
              RETURN;
            */

            SET @P_VLRUNIT = 0;
            SET @P_QTD     = 0;
            SET @P_VLRICMS = 0;
            SET @P_BASEST  = 0;
            SET @P_BASEICMS= 0;
            SET @P_MVA     = 0;
            SET @P_SEQUENCIA = 1;

            SELECT
                    @P_NUNOTA =I.NUNOTA,
                    @P_CODEMP=I.CODEMP,
                    @P_CODPARC =I.CODPARC,
                    @P_TIPMOV = I.TIPMOV,
                    @P_VLRFRETE = CASE WHEN I.TIPFRETE = 'N' THEN I.VLRFRETE ELSE 0 END,
                    @P_VLRNOTA = SUM(ITE.VLRTOT)
            FROM INSERTED I, TGFCAB CAB, TGFITE ITE
            WHERE I.CODPARC = CAB.CODPARC
              AND CAB.NUNOTA = I.NUNOTA
              AND CAB.NUNOTA = ITE.NUNOTA
            GROUP BY I.NUNOTA, I.CODEMP, I.CODPARC, I.TIPMOV, CASE WHEN I.TIPFRETE = 'N' THEN I.VLRFRETE ELSE 0 END

            --EXEC SANKHYA.SNK_ERROR @P_VLRNOTA

            IF @P_TIPMOV NOT IN ('O','C')
                RETURN

            SELECT @P_COUNT = MAX(SEQUENCIA) FROM TGFITE WHERE NUNOTA = @P_NUNOTA;

            --RAISE_APPLICATION_ERROR(-20101, 'NUNOTA: '||@P_NUNOTA);

            WHILE (@P_SEQUENCIA <= @P_COUNT)
                BEGIN
                    /**
                       se for a primeira execução o calculo e feito com base nos dados gravados na TGFITE
                       caso contrário, os dados são usadas apartir de um espelho da entrada da nota de compra
                     */
                    IF EXISTS(SELECT 1 FROM AD_TBHBDIST WHERE NUNOTA = @P_NUNOTA AND SEQUENCIA = @P_SEQUENCIA) BEGIN
                        SELECT
                                @P_QTD=QTDNEG,
                                @P_VLRUNIT=VLRUNIT,
                                @P_VLRICMS=VLRICMS,
                                @P_BASEICMS=BASEICMS,
                                @P_BASEST=BASESUBSTIT,
                                @P_ST=VLRSUBST,
                                @P_VLRIPI = VLRIPI,
                                @P_VLRFRETEITE = ((VLRUNIT*QTDNEG)/(CASE WHEN @P_VLRNOTA = 0 THEN 1 ELSE @P_VLRNOTA END)) * @P_VLRFRETE
                        FROM AD_TBHBDIST
                        WHERE NUNOTA = @P_NUNOTA
                          AND SEQUENCIA = @P_SEQUENCIA;
                    END
                    ELSE
                        BEGIN
                            SELECT
                                    @P_QTD=QTDNEG,
                                    @P_VLRUNIT=VLRUNIT,
                                    @P_VLRICMS=VLRICMS,
                                    @P_BASEICMS=BASEICMS,
                                    @P_BASEST=BASESUBSTIT,
                                    @P_ST=VLRSUBST,
                                    @P_VLRIPI = VLRIPI,
                                    @P_VLRFRETEITE = ((VLRUNIT*QTDNEG)/(CASE WHEN @P_VLRNOTA = 0 THEN 1 ELSE @P_VLRNOTA END)) * @P_VLRFRETE
                            FROM TGFITE
                            WHERE NUNOTA = @P_NUNOTA
                              AND SEQUENCIA = @P_SEQUENCIA;

                            -- incluir dados para criar espelho da nota de compra
                            IF @P_QTD > 0 BEGIN
                                INSERT INTO AD_TBHBDIST (NUNOTA, SEQUENCIA,QTDNEG,VLRUNIT,VLRICMS,BASEICMS,BASESUBSTIT,VLRSUBST,VLRIPI)
                                VALUES (@P_NUNOTA, @P_SEQUENCIA,@P_QTD,@P_VLRUNIT,@P_VLRICMS,@P_BASEICMS,@P_BASEST,@P_ST,@P_VLRIPI);
                            END
                            
                        END

                    SELECT @P_CODPROD=CODPROD
                    FROM TGFITE
                    WHERE SEQUENCIA = @P_SEQUENCIA
                      AND NUNOTA = @P_NUNOTA;

                    SELECT @P_CIDDEST=CODCID
                    FROM TSIEMP
                    WHERE CODEMP = @P_CODEMP;

                    SELECT @P_UFDEST=UF
                    FROM TSICID
                    WHERE CODCID = @P_CIDDEST;

                    SELECT @P_CIDORIG=CODCID
                    FROM TGFPAR
                    WHERE CODPARC = @P_CODPARC;

                    SELECT @P_CODGRUPPAR = GRUPOICMS
                    FROM TGFPAEM
                    WHERE CODEMP = @P_CODEMP
                      AND CODPARC = @P_CODPARC

                    SELECT @P_UFORIG=UF
                    FROM TSICID
                    WHERE CODCID = @P_CIDORIG;

                    SELECT @P_CODGRUPOICMS=GRUPOICMS2
                    FROM TGFPRO
                    WHERE CODPROD = @P_CODPROD;

                    SELECT @P_MVA = IVA
                    FROM TGFDIN
                    WHERE NUNOTA = @P_NUNOTA
                      AND SEQUENCIA = @P_SEQUENCIA
                      AND CODIMP = 2

                    IF (@P_BASEICMS) <=0
                        BEGIN
                            SET @P_MVA = 1
                            SET @P_BASEICMS = @P_BASEST
                        END;
                    --ELSE
                    --SET @P_MVA = ((@P_BASEST - @P_BASEICMS)* 100) / @P_BASEICMS;

                    SELECT @P_ALIQCALC=ALIQUOTA,@P_MVACALC=MARGLUCRO,@P_ALIQSTCALC=ALIQSUBTRIB, @MVAAJUSTADO = CALCMVAAJUSTADO
                    FROM TGFICM
                    WHERE UFORIG = @P_UFORIG
                      AND UFDEST = @P_UFDEST
                      AND (CODRESTRICAO2 = @P_CODGRUPOICMS)
                      AND CODRESTRICAO = @P_CODGRUPPAR
                      AND MARGLUCRO <> 0;

                    IF @MVAAJUSTADO = 'A'
                        BEGIN
                            SET @P_MVACALC = (((1+(@P_MVACALC/100))*(1-(@P_ALIQCALC/100))/(1-(@P_ALIQSTCALC/100)))-1)*100
                        END


                    SET @P_CALC =      ((@P_VLRUNIT*@P_QTD) + @P_VLRIPI +  ((@P_MVACALC/100)*(@P_VLRUNIT*@P_QTD + @P_VLRIPI))) * (@P_ALIQSTCALC/100);
                    SET @P_CALCFRETE = (@P_VLRFRETEITE + ((@P_MVACALC/100)*(@P_VLRFRETEITE))) * (@P_ALIQSTCALC/100);
                    SET @P_ICMSCALC = (@P_VLRUNIT*@P_QTD) * (@P_ALIQCALC/100);
                    SET @P_STCALC = @P_CALC - @P_ICMSCALC;
                    SET @P_DIFMVA = @P_STCALC + @P_CALCFRETE - @P_ST;
                    SET @P_MVA = (@P_MVA-1)*100;
                    --EXEC SANKHYA.SNK_ERROR @P_VLRFRETE
                    --RAISE_APPLICATION_ERROR(-20101, @P_QTD||' '||@P_MVACALC||' '||@P_CALC||' '||@P_ICMSCALC||' '||@P_STCALC);

					IF EXISTS(SELECT 1 FROM TGFDIN WHERE NUNOTA = @P_NUNOTA AND SEQUENCIA = @P_SEQUENCIA AND CODIMP = 2)
					BEGIN
						--UPDATE TGFCAB SET OBSERVACAO = 'ENTROU UPDATE ' WHERE NUNOTA = @P_NUNOTA;
						UPDATE TGFDIN SET DIGITADO = 'S',  AD_MVA = @P_MVA, AD_ICMSCALC = @P_ICMSCALC, AD_STCALC = @P_STCALC, AD_MVACALC = @P_MVACALC, AD_DIFST = CASE WHEN @P_DIFMVA BETWEEN -0.01 AND 0.01 THEN 0 ELSE @P_DIFMVA END , AD_VALORST = @P_ST
						WHERE NUNOTA = @P_NUNOTA AND SEQUENCIA = @P_SEQUENCIA AND CODIMP = 2;
					END
					ELSE
					BEGIN
						--UPDATE TGFCAB SET OBSERVACAO = 'ENTROU INSERT' WHERE NUNOTA = @P_NUNOTA;
						INSERT INTO TGFDIN (NUNOTA,SEQUENCIA,CODIMP,DIGITADO,AD_MVA,AD_ICMSCALC,AD_STCALC,AD_MVACALC,AD_DIFST,AD_VALORST,CODINC)
						VALUES ( @P_NUNOTA,@P_SEQUENCIA,2,'S', @P_MVA,@P_ICMSCALC,@P_STCALC,@P_MVACALC,CASE WHEN @P_DIFMVA BETWEEN -0.01 AND 0.01 THEN 0 ELSE @P_DIFMVA END,@P_ST,1);
					END
					
				
                    --EXEC SANKHYA.SNK_ERROR @P_CODPARC
                    SET @P_SEQUENCIA = @P_SEQUENCIA + 1;

                END  ;
        END;
    END;
