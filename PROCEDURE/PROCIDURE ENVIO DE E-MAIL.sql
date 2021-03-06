

-- ===============================================================
-- Author: NELSON NUNES
-- Create date: 08/06/2020
-- Description: Envia e-mail para todos os clientes
-- ===============================================================
-- 
-- SQL para acompanhamento
/*
SELECT 
	COUNT(*) AS REGISTROS,
	CASE AD_CONTROLEEMAIL WHEN 'S' THEN 'ENVIOU' ELSE 'PENDENTE' END AS DISPARO
FROM [SANKHYA_PROD].[sankhya].[TGFPAR]
WHERE ATIVO = 'S'
	AND CLIENTE = 'S'
	AND EMAIL IS NOT NULL
	AND RTRIM(EMAIL) NOT LIKE '%@realonibus.com.br'
	GROUP BY AD_CONTROLEEMAIL
*/
CREATE PROCEDURE [sankhya].[AD_ENVIAMAILLING1] AS
DECLARE
       @V_EMAIL VARCHAR(80),
       @V_CODPARC INT,
	   @V_CONTADOR SMALLINT,
	   @V_LIMITE SMALLINT,
	   @V_HORA SMALLINT
BEGIN
	SET NOCOUNT ON
	SET @V_CONTADOR = 0

	-- Regra da TASK
	SELECT @V_HORA = CAST(DATEPART(HOUR, GETDATE()) AS SMALLINT)
	IF @V_HORA > 19 OR @V_HORA < 7
		SET @V_LIMITE = 6
	ELSE
		SET @V_LIMITE = 3

	WHILE @V_CONTADOR < @V_LIMITE
	BEGIN

		SELECT TOP 1
			@V_CODPARC = CODPARC,
			@V_EMAIL = EMAIL
		FROM TGFPAR
		WHERE ATIVO = 'S'
			AND CLIENTE = 'S'
			AND (AD_CONTROLEEMAIL IS NULL OR AD_CONTROLEEMAIL <> 'S')
			AND EMAIL IS NOT NULL
			AND RTRIM(EMAIL) NOT LIKE '%@realonibus.com.br'
		ORDER BY NEWID()

		IF @V_CODPARC > 0
		BEGIN
			INSERT INTO TMDFMG (
				CODFILA,
				CODCON,
				DTENTRADA,
				STATUS,
				MENSAGEM,
				TIPOENVIO,
				ASSUNTO,
				EMAIL,
				MAXTENTENVIO,
				MIMETYPE,
				TIPODOC,
				CODSMTP
			) VALUES (
				(SELECT MAX(CODFILA)+1 FROM TMDFMG),
				0,
				GETDATE(),
				'Pendente',
				'<!doctype html>
<html xmlns="http://www.w3.org/1999/xhtml" xmlns:v="urn:schemas-microsoft-com:vml" xmlns:o="urn:schemas-microsoft-com:office:office">
<head>
</head><head>
<!-- NAME: BOLD -->
<!--[if gte mso 15]>
<xml>
<o:OfficeDocumentSettings>
<o:AllowPNG/>
<o:PixelsPerInch>96</o:PixelsPerInch>
</o:OfficeDocumentSettings>
</xml>
<![endif]-->
<meta charset="UTF-8">
<meta http-equiv="X-UA-Compatible" content="IE=edge">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Mudança no processo de devolução de venda</title>
<style type="text/css">
p{
margin:10px 0;
padding:0;
}
table{
border-collapse:collapse;
}
h1,h2,h3,h4,h5,h6{
display:block;
margin:0;
padding:0;
}
img,a img{
border:0;
height:auto;
outline:none;
text-decoration:none;
}
body,#bodyTable,#bodyCell{
height:100%;
margin:0;
padding:0;
width:100%;
}
.mcnPreviewText{
display:none !important;
}
#outlook a{
padding:0;
}
img{
-ms-interpolation-mode:bicubic;
}
table{
mso-table-lspace:0pt;
mso-table-rspace:0pt;
}
.ReadMsgBody{
width:100%;
}
.ExternalClass{
width:100%;
}
p,a,li,td,blockquote{
mso-line-height-rule:exactly;
}
a[href^=tel],a[href^=sms]{
color:inherit;
cursor:default;
text-decoration:none;
}
p,a,li,td,body,table,blockquote{
-ms-text-size-adjust:100%;
-webkit-text-size-adjust:100%;
}
.ExternalClass,.ExternalClass p,.ExternalClass td,.ExternalClass div,.ExternalClass span,.ExternalClass font{
line-height:100%;
}
a[x-apple-data-detectors]{
color:inherit !important;
text-decoration:none !important;
font-size:inherit !important;
font-family:inherit !important;
font-weight:inherit !important;
line-height:inherit !important;
}
a.mcnButton{
display:block;
}
.mcnImage,.mcnRetinaImage{
vertical-align:bottom;
}
.mcnTextContent{
word-break:break-word;
}
.mcnTextContent img{
height:auto !important;
}
.mcnDividerBlock{
table-layout:fixed !important;
}
body,#bodyTable{
background-color:#FFFFFF;
}
#bodyCell{
border-top:0;
}
#templateContainer{
border:0;
}
h1{
color:#0e0e0e !important;
font-family:Helvetica;
font-size:30px;
font-style:normal;
font-weight:bold;
line-height:125%;
letter-spacing:1px;
text-align:center;
}
h2{
color:#202020 !important;
font-family:Helvetica;
font-size:26px;
font-style:normal;
font-weight:bold;
line-height:125%;
letter-spacing:normal;
text-align:left;
}
h3{
color:#202020 !important;
font-family:Helvetica;
font-size:20px;
font-style:normal;
font-weight:bold;
line-height:125%;
letter-spacing:normal;
text-align:left;
}
h4{
color:#808080 !important;
font-family:Helvetica;
font-size:16px;
font-style:normal;
font-weight:bold;
line-height:125%;
letter-spacing:normal;
text-align:left;
}
#templatePreheader{
background-color:#FFFFFF;
border-top:0;
border-bottom:0;
}
.preheaderContainer .mcnTextContent,.preheaderContainer .mcnTextContent p{
color:#606060;
font-family:Helvetica;
font-size:11px;
line-height:125%;
text-align:left;
}
.preheaderContainer .mcnTextContent a{
color:#606060;
font-weight:normal;
text-decoration:underline;
}
#templateHeader{
background-color:#e4e5e7;
border-top:0;
border-bottom:0;
}
.headerContainer .mcnTextContent,.headerContainer .mcnTextContent p{
color:#202020;
font-family:Helvetica;
font-size:20px;
line-height:150%;
text-align:center;
}
.headerContainer .mcnTextContent a{
color:#222222;
font-weight:normal;
text-decoration:underline;
}
#templateBody{
background-color:#FFFFFF;
border-top:0;
border-bottom:0;
}
.bodyContainer .mcnTextContent,.bodyContainer .mcnTextContent p{
color:#202020;
font-family:Helvetica;
font-size:16px;
line-height:150%;
text-align:left;
}
.bodyContainer .mcnTextContent a{
color:#e4e5e7;
font-weight:normal;
text-decoration:underline;
}
#templateFooter{
background-color:#FFFFFF;
border-top:0;
border-bottom:0;
}
.footerContainer .mcnTextContent,.footerContainer .mcnTextContent p{
color:#808080;
font-family:Helvetica;
font-size:10px;
line-height:125%;
text-align:center;
}
.footerContainer .mcnTextContent a{
color:#808080;
font-weight:normal;
text-decoration:underline;
}
@media only screen and (max-width: 480px){
body,table,td,p,a,li,blockquote{
-webkit-text-size-adjust:none !important;
}

}	@media only screen and (max-width: 480px){
body{
width:100% !important;
min-width:100% !important;
}

}	@media only screen and (max-width: 480px){
#templateContainer,#templatePreheader,#templateHeader,#templateBody,#templateFooter{
max-width:600px !important;
width:100% !important;
}

}	@media only screen and (max-width: 480px){
.mcnRetinaImage{
max-width:100% !important;
}

}	@media only screen and (max-width: 480px){
.mcnImage{
height:auto !important;
width:100% !important;
}

}	@media only screen and (max-width: 480px){
.mcnCartContainer,.mcnCaptionTopContent,.mcnRecContentContainer,.mcnCaptionBottomContent,.mcnTextContentContainer,.mcnBoxedTextContentContainer,.mcnImageGroupContentContainer,.mcnCaptionLeftTextContentContainer,.mcnCaptionRightTextContentContainer,.mcnCaptionLeftImageContentContainer,.mcnCaptionRightImageContentContainer,.mcnImageCardLeftTextContentContainer,.mcnImageCardRightTextContentContainer,.mcnImageCardLeftImageContentContainer,.mcnImageCardRightImageContentContainer{
max-width:100% !important;
width:100% !important;
}

}	@media only screen and (max-width: 480px){
.mcnBoxedTextContentContainer{
min-width:100% !important;
}

}	@media only screen and (max-width: 480px){
.mcnImageGroupContent{
padding:9px !important;
}

}	@media only screen and (max-width: 480px){
.mcnCaptionLeftContentOuter .mcnTextContent,.mcnCaptionRightContentOuter .mcnTextContent{
padding-top:9px !important;
}

}	@media only screen and (max-width: 480px){
.mcnImageCardTopImageContent,.mcnCaptionBottomContent:last-child .mcnCaptionBottomImageContent,.mcnCaptionBlockInner .mcnCaptionTopContent:last-child .mcnTextContent{
padding-top:18px !important;
}

}	@media only screen and (max-width: 480px){
.mcnImageCardBottomImageContent{
padding-bottom:9px !important;
}

}	@media only screen and (max-width: 480px){
.mcnImageGroupBlockInner{
padding-top:0 !important;
padding-bottom:0 !important;
}

}	@media only screen and (max-width: 480px){
.mcnImageGroupBlockOuter{
padding-top:9px !important;
padding-bottom:9px !important;
}

}	@media only screen and (max-width: 480px){
.mcnTextContent,.mcnBoxedTextContentColumn{
padding-right:18px !important;
padding-left:18px !important;
}

}	@media only screen and (max-width: 480px){
.mcnImageCardLeftImageContent,.mcnImageCardRightImageContent{
padding-right:18px !important;
padding-bottom:0 !important;
padding-left:18px !important;
}

}	@media only screen and (max-width: 480px){
.mcpreview-image-uploader{
display:none !important;
width:100% !important;
}

}	@media only screen and (max-width: 480px){
h1{
font-size:24px !important;
line-height:125% !important;
}

}	@media only screen and (max-width: 480px){
h2{
font-size:20px !important;
line-height:125% !important;
}

}	@media only screen and (max-width: 480px){
h3{
font-size:18px !important;
line-height:125% !important;
}

}	@media only screen and (max-width: 480px){
h4{
font-size:16px !important;
line-height:125% !important;
}

}	@media only screen and (max-width: 480px){
.mcnBoxedTextContentContainer .mcnTextContent,.mcnBoxedTextContentContainer .mcnTextContent p{
font-size:18px !important;
line-height:125% !important;
}

}	@media only screen and (max-width: 480px){
#templatePreheader{
display:block !important;
}

}	@media only screen and (max-width: 480px){
.preheaderContainer .mcnTextContent,.preheaderContainer .mcnTextContent p{
font-size:14px !important;
line-height:115% !important;
}

}	@media only screen and (max-width: 480px){
.headerContainer .mcnTextContent,.headerContainer .mcnTextContent p{
font-size:18px !important;
line-height:125% !important;
}

}	@media only screen and (max-width: 480px){
.bodyContainer .mcnTextContent,.bodyContainer .mcnTextContent p{
font-size:18px !important;
line-height:125% !important;
}

}	@media only screen and (max-width: 480px){
.footerContainer .mcnTextContent,.footerContainer .mcnTextContent p{
font-size:14px !important;
line-height:115% !important;
}

}</style></head>
<body leftmargin="0" marginwidth="0" topmargin="0" marginheight="0" offset="0" style="height: 100%;margin: 0;padding: 0;width: 100%;-ms-text-size-adjust: 100%;-webkit-text-size-adjust: 100%;background-color: #FFFFFF;">
<!--[if !gte mso 9]><!----><span class="mcnPreviewText" style="display:none; font-size:0px; line-height:0px; max-height:0px; max-width:0px; opacity:0; overflow:hidden; visibility:hidden; mso-hide:all;">A Real Ônibus está mudando o processo de devolução de vendas.</span><!--<![endif]-->
<center>
<table align="center" border="0" cellpadding="0" cellspacing="0" height="100%" width="100%" id="bodyTable" style="border-collapse: collapse;mso-table-lspace: 0pt;mso-table-rspace: 0pt;-ms-text-size-adjust: 100%;-webkit-text-size-adjust: 100%;height: 100%;margin: 0;padding: 0;width: 100%;background-color: #FFFFFF;">
<tr>
<td align="center" valign="top" id="bodyCell" style="mso-line-height-rule: exactly;-ms-text-size-adjust: 100%;-webkit-text-size-adjust: 100%;height: 100%;margin: 0;padding: 0;width: 100%;border-top: 0;">
<!-- BEGIN TEMPLATE // -->
<table border="0" cellpadding="0" cellspacing="0" width="600" id="templateContainer" style="border-collapse: collapse;mso-table-lspace: 0pt;mso-table-rspace: 0pt;-ms-text-size-adjust: 100%;-webkit-text-size-adjust: 100%;border: 0;">
<tr>
<td align="center" valign="top" style="mso-line-height-rule: exactly;-ms-text-size-adjust: 100%;-webkit-text-size-adjust: 100%;">
<!-- BEGIN PREHEADER // -->
<table border="0" cellpadding="0" cellspacing="0" width="600" id="templatePreheader" style="border-collapse: collapse;mso-table-lspace: 0pt;mso-table-rspace: 0pt;-ms-text-size-adjust: 100%;-webkit-text-size-adjust: 100%;background-color: #FFFFFF;border-top: 0;border-bottom: 0;">
<tr>
<td valign="top" class="preheaderContainer" style="padding-top: 9px;mso-line-height-rule: exactly;-ms-text-size-adjust: 100%;-webkit-text-size-adjust: 100%;"><table border="0" cellpadding="0" cellspacing="0" width="100%" class="mcnImageBlock" style="min-width: 100%;border-collapse: collapse;mso-table-lspace: 0pt;mso-table-rspace: 0pt;-ms-text-size-adjust: 100%;-webkit-text-size-adjust: 100%;">
<tbody class="mcnImageBlockOuter">
<tr>
<td valign="top" style="padding: 0px;mso-line-height-rule: exactly;-ms-text-size-adjust: 100%;-webkit-text-size-adjust: 100%;" class="mcnImageBlockInner">
<table align="left" width="100%" border="0" cellpadding="0" cellspacing="0" class="mcnImageContentContainer" style="min-width: 100%;border-collapse: collapse;mso-table-lspace: 0pt;mso-table-rspace: 0pt;-ms-text-size-adjust: 100%;-webkit-text-size-adjust: 100%;">
<tbody><tr>
<td class="mcnImageContent" valign="top" style="padding-right: 0px;padding-left: 0px;padding-top: 0;padding-bottom: 0;text-align: center;mso-line-height-rule: exactly;-ms-text-size-adjust: 100%;-webkit-text-size-adjust: 100%;">
<a href="https://www.realonibus.com.br/" title="" class="" target="_blank" style="mso-line-height-rule: exactly;-ms-text-size-adjust: 100%;-webkit-text-size-adjust: 100%;">
<img alt="Real Onibus" align="center" alt="" src="https://www.realonibus.com.br/images/real_onibus_logo.jpg" width="104" style="max-width: 1024px;padding-bottom: 0;display: inline !important;vertical-align: bottom;border: 0;height: auto;outline: none;text-decoration: none;-ms-interpolation-mode: bicubic;" class="mcnImage">
</a>
</td>
</tr>
</tbody></table>
</td>
</tr>
</tbody>
</table>
</td>
</tr>
</table>

<!--*|PREHEADER|*-->

<!-- // END PREHEADER -->
</td>
</tr>
<tr>
<td align="center" valign="top" style="mso-line-height-rule: exactly;-ms-text-size-adjust: 100%;-webkit-text-size-adjust: 100%;">
<!-- BEGIN HEADER // -->
<table border="0" cellpadding="0" cellspacing="0" width="600" id="templateHeader" style="border-collapse: collapse;mso-table-lspace: 0pt;mso-table-rspace: 0pt;-ms-text-size-adjust: 100%;-webkit-text-size-adjust: 100%;background-color: #FFFFFF;border-top: 0;border-bottom: 0;">
<tr>
<td valign="top" class="headerContainer" style="mso-line-height-rule: exactly;-ms-text-size-adjust: 100%;-webkit-text-size-adjust: 100%;"><table border="0" cellpadding="0" cellspacing="0" width="100%" class="mcnTextBlock" style="min-width: 100%;border-collapse: collapse;mso-table-lspace: 0pt;mso-table-rspace: 0pt;-ms-text-size-adjust: 100%;-webkit-text-size-adjust: 100%;">
<tbody class="mcnTextBlockOuter">
<tr>
<td valign="top" class="mcnTextBlockInner" style="padding-top: 9px;mso-line-height-rule: exactly;-ms-text-size-adjust: 100%;-webkit-text-size-adjust: 100%;">
<!--[if mso]>
<table align="left" border="0" cellspacing="0" cellpadding="0" width="100%" style="width:100%;">
<tr>
<![endif]-->

<!--[if mso]>
<td valign="top" width="600" style="width:600px;">
<![endif]-->
<table align="left" border="0" cellpadding="0" cellspacing="0" style="max-width: 100%;min-width: 100%;border-collapse: collapse;mso-table-lspace: 0pt;mso-table-rspace: 0pt;-ms-text-size-adjust: 100%;-webkit-text-size-adjust: 100%;" width="100%" class="mcnTextContentContainer">
<tbody><tr>

<td valign="top" class="mcnTextContent" style="padding: 0px 18px 9px;color: #242424;text-align: left;mso-line-height-rule: exactly;-ms-text-size-adjust: 100%;-webkit-text-size-adjust: 100%;word-break: break-word;font-family: Helvetica;font-size: 20px;line-height: 150%;">

<p style="color: #242424;text-align: center;margin: 10px 0;padding: 0;mso-line-height-rule: exactly;-ms-text-size-adjust: 100%;-webkit-text-size-adjust: 100%;font-family: Helvetica;font-size: 20px;line-height: 150%;"><span style="font-size:13px">

<strong>MUDANÇA NO PROCESSO DE DEVOLUÇÃO DE VENDAS</strong>

</span></p>

<p style="color: #242424;text-align: justify;margin: 10px 0;padding: 0;mso-line-height-rule: exactly;-ms-text-size-adjust: 100%;-webkit-text-size-adjust: 100%;font-family: Helvetica;font-size: 20px;line-height: 100%;"><span style="font-size:13px">

Prezados clientes. <BR />
<BR />
Visando a melhoria contínua do nosso atendimento ao cliente, mudaremos o nosso processo de devolução de venda. <BR />
A partir do dia 08/06/2020, todas as devoluções deverão ser formalizadas através de um formulário próprio, conforme modelo abaixo, no qual serão indicadas as informações da devolução e as devidas autorizações para oficialização da operação.<BR />
Os nossos motoristas estão orientados a não receberem mercadorias sem o formulário de autorização devidamente preenchido e assinado.<BR />
Contamos com a colaboração de todos e, em caso de dúvida, favor procurar os nossos vendedores.<BR />
Lembramos que a adoção dessa medida é para, cada vez mais, prestar um serviço de qualidade a você.<BR />
<BR />
Desde já, gratos pela atenção.<BR />
<BR />
Diretoria<BR />

</span></p>

<p style="color: #242424;text-align: center;margin: 10px 0;padding: 0;mso-line-height-rule: exactly;-ms-text-size-adjust: 100%;-webkit-text-size-adjust: 100%;font-family: Helvetica;font-size: 20px;line-height: 150%;"><span style="font-size:13px">

Site: <a href="https://www.realonibus.com.br">www.realonibus.com.br</a>

</span></p>

<p style="color: #242424;text-align: left;margin: 10px 0;padding: 0;mso-line-height-rule: exactly;-ms-text-size-adjust: 100%;-webkit-text-size-adjust: 100%;font-family: Helvetica;font-size: 20px;line-height: 100%;"><span style="font-size:13px">

Atenciosamente,<BR />
Grupo Real Ônibus

</span></p>

</td>
</tr>
</tbody></table>
<!--[if mso]>
</td>
<![endif]-->

<!--[if mso]>
</tr>
</table>
<![endif]-->
</td>
</tr>
</tbody>
</table></td>
</tr>
</table>

<p style="color: #242424;text-align: center;margin: 10px 0;padding: 0;mso-line-height-rule: exactly;-ms-text-size-adjust: 100%;-webkit-text-size-adjust: 100%;font-family: Helvetica;font-size: 15px;line-height: 100%;"><span style="font-size:11px">

<!--*|BQ_RODAPE|*-->

</span></p>

<!-- // END HEADER -->
</td>
</tr>
</table>
<!-- // END TEMPLATE -->
</td>
</tr>
</table>
</center>
<br /></body>',
				'E',
				'MUDANÇA NO PROCESSO DE DEVOLUÇÃO DE VENDAS',
				@V_EMAIL,
				3,
				'text/html',
				'N',
				9
			)
			UPDATE TGFPAR SET AD_CONTROLEEMAIL = 'S' WHERE CODPARC = @V_CODPARC -- Registra o envio para este cliente
		END
		ELSE
			UPDATE TSIAAG SET ATIVO = 'N' WHERE TSIAAG.NUAAG = 23 -- Terminou de enviar. Desativa a acao agendada

		SET @V_CONTADOR = @V_CONTADOR + 1
	END

	-- Atualiza o ultimo codigo da fila de email
	UPDATE TGFNUM SET ULTCOD = (SELECT MAX(CODFILA) FROM TMDFMG) WHERE ARQUIVO='TMDFMG'

END