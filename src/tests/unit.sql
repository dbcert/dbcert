create table table1 (a int, b int, c int);
create table table0 (a int, b int, c int);
create table table2 (a int, b bool, c text);

select * from table1;
select a from table1;
select a as g from table1;
select t.d as g from table1 as t(d,e,f);
select * from table1 where a = b;
select a, b from table1;
select a, b from table1 where a = b;
select a, a + b from table1;
select * from table1 as t1 where t1.a < 4;
select * from table1 as t1 where t1.a > 4;
select * from table1 as t1 where t1.a = 4;
select * from table1 as t1 where t1.a <> 4;
select * from table1 as t1 where t1.a < 4 or t1.a >= 7;
select * from table1 where 2 * a < 4;
select t1.d as g from table1 as t1(d,e,f), table0 where t1.d = a;
select t1.d as g from table1, table0 as t1(d,e,f) where t1.d = a;
select * from table1 where (a+b) <= all (select (a0 + c1) as a0_plus_c1 from table0 as t0(a0,b0,c0), table1 as t1(a1,b1,c1));
select a, count(b) as count_b from table1 group by a having avg(c) <= all (select a from table1);
select a, count(b) as count_b from table1 group by a having sum(c) <= all (select a from table1);
select a, count(b) as count_b from table1 group by a having sum(c) <= any (select a from table1);
select a + b * sum(c) from table1 group by a, b;
select a, c, sum(b) as sumb from table0 group by a, c having a in (select b from table1);
select * from table1, (select a as a1 from table1) as t1;
select * from table1, (select a as a1 from table1) as t1(d);
select * from table1, (select * from table1) as t1;
select * from table1, (select * from table1) as t1(d,e,f);
select * from table1, (select * from table0 as t0(d,e,f), table1) as t2 where table1.a = t2.b;
select * from table1 as t1, (select * from table0 as t0(d,e,f), table1) as t2 where t1.a = t2.b;
select * from table1 where table1.a = 3;
select * from table1 where a = 3;
select * from table1 where table1.a in (select table1.a from table1 where table1.b = 4);
select * from table1 as t2 where (t2.a, t2.b) in (select t1.a, t1.b from table1 as t1);
select * from table1 as t1 where a+3 <= all (select b from table0);
select * from table1 as t1 where exists (select * from table0 where a = t1.b);
select a1 from table1 as t1(a1,b1,b2) group by a1 having exists (select a4 from table0 as t4(a4,b4,c4) group by a4 having sum(1+0*a1) = 10);
select sum(a) from table1;
select * from table1 as t1 where not exists (select * from table0 where a = t1.b);
select a1 from table1 as t1(a1,b1,b2) group by a1 having not exists (select a4 from table0 as t4(a4,b4,c4) group by a4 having sum(1+0*a1) = 10);
select * from table1 as t1 where not a+3 <= all (select b from table0);
select a, count(b) as count_b from table1 group by a having not sum(c) <= any (select a from table1);
select * from table1 as t2 where (t2.a, t2.b) not in (select t1.a, t1.b from table1 as t1);
table table1;
select a from (table table1) as t2;
select t2.a from (table table1) as t2;
select b from table1 where (a,b,c) in (table table0);
(select * from table1) union (table table0);
(select * from table1) intersect (table table0);
(select * from table1) except (table table0);
select * from table1 where true;
select * from table2;
select - a from table1;


-- We do not support queries with grouping on an expression with is not an attribute
select 2*(a+c) as twotimesaplusc, b as b2 from table1 group by (a+c), b having b > 1;
select a+b as a_b, count(b) as count_b from table1 group by a+b having sum(c) <= all (select a from table1);
select a+b as a_b, count(b) as count_b from table1 group by a_b having sum(c) <= all (select a from table1);

-- However, the following is supported
select a_b, count(b) as count_b from (select a+b as a_b, b, c from table1) t1(a_b, b, c) group by a_b having sum(c) <= all (select a from table1);


-- We do not support "in" constructs where the names are the same on both sides

select * from table1 as t1 where (t1.a, t1.b) in (select t1.a, t1.b from table1 as t1);

-- However, the following is supported

select * from table1 as t1 where (t1.a, t1.b) in (select t2.a, t2.b from table1 as t2);
select * from table1 as t1 where (t1.a, t1.b) in (select a, b from table1);
