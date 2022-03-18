create table employees (name text, age int);
select age, count(*)
from employees
group by age;
