ALTER TABLE t_order_item 
    ADD PRIMARY KEY (order_id), FOREIGN KEY (order_id) REFERENCES t_order (order_id) 
    ON UPDATE CASCADE ON DELETE CASCADE ;
