SELECT *
FROM TGFPAP
WHERE CODPARC = 1859
AND CODPROPARC = ' 1604120'

UPDATE TGFPAP
SET SEQUENCIA = 0
WHERE CODPARC = 1859
AND CODPROPARC = ' 1604120'
AND SEQUENCIA = 3