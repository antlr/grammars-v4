CREATE SEMANTIC VIEW tpch_rev_analysis

  TABLES (
    orders AS SNOWFLAKE_SAMPLE_DATA.TPCH_SF1.ORDERS
      PRIMARY KEY (o_orderkey)
      WITH SYNONYMS ('sales orders')
      COMMENT = 'All orders table for the sales domain',
    customers AS SNOWFLAKE_SAMPLE_DATA.TPCH_SF1.CUSTOMER
      PRIMARY KEY (c_custkey)
      COMMENT = 'Main table for customer data',
    line_items AS SNOWFLAKE_SAMPLE_DATA.TPCH_SF1.LINEITEM
      PRIMARY KEY (l_orderkey, l_linenumber)
      COMMENT = 'Line items in orders'
  )

  RELATIONSHIPS (
    orders_to_customers AS
      orders (o_custkey) REFERENCES customers,
    line_item_to_orders AS
      line_items (l_orderkey) REFERENCES orders
  )

  FACTS (
    line_items.line_item_id AS CONCAT(l_orderkey, '-', l_linenumber),
    orders.count_line_items AS COUNT(line_items.line_item_id),
    line_items.discounted_price AS l_extendedprice * (1 - l_discount)
      COMMENT = 'Extended price after discount'
  )

  DIMENSIONS (
    customers.customer_name AS customers.c_name
      WITH SYNONYMS = ('customer name')
      COMMENT = 'Name of the customer',
    orders.order_date AS o_orderdate
      COMMENT = 'Date when the order was placed',
    orders.order_year AS YEAR(o_orderdate)
      COMMENT = 'Year when the order was placed'
  )

  METRICS (
    customers.customer_count AS COUNT(c_custkey)
      COMMENT = 'Count of number of customers',
    orders.order_average_value AS AVG(orders.o_totalprice)
      COMMENT = 'Average order value across all orders',
    orders.average_line_items_per_order AS AVG(orders.count_line_items)
      COMMENT = 'Average number of line items per order'
  )

  COMMENT = 'Semantic view for revenue analysis';