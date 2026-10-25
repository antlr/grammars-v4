update my_table set my_column = 1 return my_column into l_column;
update my_table set my_column = 1 returning my_column into l_column;

update my_table set my_column = 1 returning new my_column into l_column;
update my_table set my_column = 1 returning old my_column into l_column;

update my_table set my_column = 1 returning - new my_column into l_column;
update my_table set my_column = 1 returning + new my_column into l_column;

update my_table set my_column = 1 return old my_column, new my_column into l_column_old, l_column_new;
update my_table set my_column = 1 returning old my_column, new my_column into l_column_old, l_column_new;
