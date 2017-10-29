/* 
	LII 29.10.17
	Uppsetning á töflum f. Groceries verkefnið á azure db
	Töflurnar koma ein af annari í þetta script eftir því sem ég geri þær
*/
-- Create Person table

CREATE TABLE TRAIN
(
Id   INT IDENTITY PRIMARY KEY,
store_nbr   INT NOT NULL,
item_nbr INT NOT NULL,
onpromotion INT NULL,
date DATE NOT NULL,
unit_sales DECIMAL(18,4) NULL
)
