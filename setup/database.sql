/*
Ideally, we'd want to have the "shoes" table to represent the shoe styles.
Colors (shoe_id, color) and sizes (shoe_id, size) would be in their own table to
make it easier for customers to find what colors and sizes the shoes are available in.
There would also be a table tracking the history of the shoes (did it sell?  was it
damanged?).
*/

CREATE TABLE shoes (
	id serial primary key,
	description varchar(150),
	color varchar(50),
	size int,
	quantity int default 0
);
/*
CREATE TABLE shoe_sizes (
	shoe_id int,
	size int,

	primary key (shoe_id, size),
	CONSTRAINT shoes_id_fkey FOREIGN KEY (shoe_id) REFERENCES shoes (id)
);

CREATE TABLE shoe_colors (
	shoe_id int,
	color varchar(50),

	primary key (shoe_id, color),
	CONSTRAINT shoes_id_fkey FOREIGN KEY (shoe_id) REFERENCES shoes (id)
);

CREATE TABLE transaction_types (
	name varchar(50) primary key,
	gain bool
);

INSERT INTO transaction_types (name, gain)
VALUES
	('Purchase', true),
	('Return', true),
	('Sold', false),
	('Damaged', false),
	('Lost', false);

CREATE TABLE shoe_history (
	id serial primary key,
	shoe_id int,
	size int,
	color varchar(50),
	transaction varchar(50),
	date_added timestamptz default now(),

	CONSTRAINT shoe_history_style_size_fkey FOREIGN KEY (shoe_id, size) REFERENCES shoe_sizes (shoe_id, size),
	CONSTRAINT shoe_history_style_color_fkey FOREIGN KEY (shoe_id, color) REFERENCES shoe_colors (shoe_id, color),
	CONSTRAINT shoe_history_transaction_fkey FOREIGN KEY (transaction) REFERENCES transaction_types (name)
);
*/
