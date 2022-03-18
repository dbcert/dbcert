create table t1 (a1 int, b1 int);
create table t2 (a2 int, b2 int);


-- q5 (queries_nested.js)
select a1, max(b1) from t1 group by a1;
-- Expected: (a1=1,m=10); (a1=2,m=10); (a1=3,m=5); (a1=4,m=10)
-- Got: [{"m":10,"a1":1},{"m":10,"a1":2},{"m":5,"a1":3},{"m":10,"a1":4}]
-- ok

-- q6 (queries_nested_1.js)
select a1 from t1 group by a1 having exists (select a2 from t2 group by a2 having sum(1+0*a1) = 10);
-- Expected: (a1=1); (a1=2)
-- Got: [{"a1":1},{"a1":2}]
-- ok

-- q7 (queries_nested_2.js)
select a1 from t1 group by a1 having exists (select a2 from t2 group by a2 having sum(1+0*a2) = 10);
-- Expected: empty
-- Got: []
-- ok

-- q8 (queries_nested_3.js)
select a1 from t1 group by a1 having exists (select a2 from t2 group by a2 having sum(1+0*a2) = 2);
-- Expected: (a1=1); (a1=2); (a1=3); (a1=4)
-- Got: [{"a1":1},{"a1":2},{"a1":3},{"a1":4}]
-- ok

-- q9(2) (queries_nested_4.js)
select a1 from t1 group by a1 having exists (select a2 from t2 group by a2 having sum(1) = 2);
-- Expected: (a1=1); (a1=2); (a1=3); (a1=4)
-- Got: [{"a1":1},{"a1":2},{"a1":3},{"a1":4}]
-- ok

-- q9(10) (queries_nested_5.js)
select a1 from t1 group by a1 having exists (select a2 from t2 group by a2 having sum(1) = 10);
-- Expected: empty
-- Got: []
-- ok

-- q10 (queries_nested_6.js)
select a1 from t1 group by a1 having exists (select a2 from t2 group by a2 having sum(1+0*a1)+sum(1+0*a2) = 12);
-- Expected: (a1=1); (a1=2)
-- Got: [{"a1":1},{"a1":2}]
-- ok

-- q11(2) (queries_nested_7.js)
select a1 from t1 group by a1 having exists (select a2 from t2 group by a2 having sum(1+0*a1+0*a2) = 2);
-- Expected: (a1=1); (a1=2); (a1=3); (a1=4)
-- Got: [{"a1":1},{"a1":2},{"a1":3},{"a1":4}]
-- ok

-- q11(3) (queries_nested_8.js)
select a1 from t1 group by a1 having exists (select a2 from t2 group by a2 having sum(1+0*a1+0*a2) = 3);
-- Expected: empty
-- Got: []
-- ok

-- q12
-- select a1 from t1 group by a1 having exists (select a2 from t2 group by a2 having sum(1+0*b1+0*b2) = 10);
-- ill-formed

-- q13(2) (queries_nested_9.js)
select a1 from t1 group by a1 having exists (select a2 from t2 group by a2 having sum(1+0*a1+0*b2) = 2);
-- Expected: (a1=1); (a1=2); (a1=3); (a1=4)
-- Got: [{"a1":1},{"a1":2},{"a1":3},{"a1":4}]
-- ok

-- q13(3) (queries_nested_10.js)
select a1 from t1 group by a1 having exists (select a2 from t2 group by a2 having sum(1+0*a1+0*b2) = 3);
-- Expected: empty
-- Got: []
-- ok

-- q14
-- select a1 from t1 group by a1 having exists (select a2 from t2 group by a2 having sum(1+0*b1+0*a2) = 12);
-- ill-formed
