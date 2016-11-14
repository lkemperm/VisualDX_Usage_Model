SELECT *
FROM authorization.ClientSession
WHERE (YEAR(modTime) = 2014
OR YEAR(modTime) = 2015 OR YEAR(modTime) = 2016);


##joining with WHERE
SELECT *
FROM authorization.ClientSession cs, authorization.Contract c
WHERE (cs.contractId = c.contractId AND c.contractTypeId = 1);

##inner join
SELECT *
FROM authorization.ClientSession cs
INNER JOIN authorization.Contract c
ON cs.contractId = c.contractId
WHERE (c.contractTypeId = 1);



