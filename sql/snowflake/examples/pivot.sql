SELECT *
FROM monthly_sales
UNPIVOT (
    sales FOR month IN (
      jan AS january,
      feb AS february,
      mar AS march,
      apr AS april
    )
)
ORDER BY empid;

SELECT *
FROM monthly_sales
UNPIVOT INCLUDE NULLS (sales FOR month IN (jan, feb, mar, apr))
ORDER BY empid;