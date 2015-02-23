create table docs (
  id int(11) NOT NULL PRIMARY KEY,
  title varchar(255) not null,
  price numeric(5,2) not null,
  rating float not null 
);

insert into docs (id, title, price, rating) values (1, "cake", 23.99, 3.4);
insert into docs (id, title, price, rating) values (2, "soda", 1.99, 4.4);
insert into docs (id, title, price, rating) values (3, "cake", 123.99, 2.4);
