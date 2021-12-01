CREATE TABLE scans(depth INTEGER, idx INTEGER PRIMARY KEY AUTOINCREMENT);
.import input.txt scans

SELECT "The answer to the first part is: " || COUNT(1)
FROM (SELECT depth > LAG(depth, 1, null) OVER (ORDER BY idx) AS isDeeper FROM scans)
WHERE isDeeper;

SELECT "The answer to the second part is: " || COUNT(1)
FROM (
	SELECT depth > LAG(depth, 1, null) OVER (ORDER BY idx) AS isDeeper FROM (
		SELECT idx, SUM(depth) OVER (ORDER BY idx ROWS BETWEEN 1 PRECEDING AND 1 FOLLOWING) AS depth FROM scans
	)
	WHERE idx > 1 AND idx < (SELECT COUNT(1) FROM scans)
)
WHERE isDeeper;
