SELECT *
FROM authorization.ClientSession
WHERE (YEAR(modTime) = 2014
OR YEAR(modTime) = 2015 OR YEAR(modTime) = 2016);


##joining ClientSession and Contract to get contractTypeId
SELECT *
FROM authorization.ClientSession cs
INNER JOIN authorization.Contract c
ON cs.contractId = c.contractId
WHERE (c.contractTypeId = 1 AND rand() <= 0.2);

SELECT *
FROM authorization.ClientSession cs
INNER JOIN authorization.Contract c
ON cs.contractId = c.contractId
WHERE (c.contractTypeId = 2 AND rand() <= 0.2);

SELECT *
FROM authorization.ClientSession cs
INNER JOIN authorization.Contract c
ON cs.contractId = c.contractId
WHERE (c.contractTypeId = 3);

SELECT *
FROM authorization.ClientSession cs
INNER JOIN authorization.Contract c
ON cs.contractId = c.contractId
WHERE (c.contractTypeId = 4);

SELECT *
FROM authorization.ClientSession cs
INNER JOIN authorization.Contract c
ON cs.contractId = c.contractId
WHERE (c.contractTypeId = 5);

SELECT *
FROM authorization.ClientSession cs
INNER JOIN authorization.Contract c
ON cs.contractId = c.contractId
WHERE (c.contractTypeId = 6);

SELECT *
FROM authorization.ClientSession cs
INNER JOIN authorization.Contract c
ON cs.contractId = c.contractId
WHERE (c.contractTypeId = 7);

SELECT *
FROM authorization.ClientSession cs
INNER JOIN authorization.Contract c
ON cs.contractId = c.contractId
WHERE (c.contractTypeId = 8);



