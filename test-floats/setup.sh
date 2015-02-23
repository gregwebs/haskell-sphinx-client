db=sphinxtest
user=root

mysqladmin -u $user drop $db
mysqladmin -u $user create $db
mysql -u $user $db < create.sql
