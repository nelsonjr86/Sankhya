--Exemplo da nota 761 ou 135086
-- Para alterar a quantida do tem deve fazer.

--- deletar linha, acesar o gatilho TRG_DLT_TGFITS e desabilitar
--delete
--from tgfits
--where NUNOTA=135086
--and SEQUENCIA =7

--- selecionar linha
select * 
from tgfits
where NUNOTA=135086
and SEQUENCIA =7


--update do valor
--UPDATE TGFITS SET QTDSAI=14 where NUNOTA=135086 and SEQUENCIA =7

-- foi criado pelo ronal da sankhya,para replicar a linha
select * FROM TGFITSBKP