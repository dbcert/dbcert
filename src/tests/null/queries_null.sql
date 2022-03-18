create table R (A int);
create table S (A int);
create table T (A int);

-- q1 (queries_null.js)
select R.A from R where R.A not in (select S.A from S);
-- Expected: []
-- Got: []
-- ok

-- q2 (queries_null_1.js)
select R.A from R where not exists (select * from S where S.A = R.A);
-- Expected: [{"A":1},{"A":null}]
-- Got: [{"A":null},{"A":1}]
-- ok

-- q3 (queries_null_2.js)
select R.A from R except select S.A from S;
-- Expected: [{"A":1}]
-- Got: [{"A":1}]
-- ok

-- q4 (queries_null_3.js)
select T.A,count(*) as c from T group by T.A;
-- Expected: [{"A":null, "c":2},{"A":1,"c":1}]
-- Got: [{"A":null,"c":{"$nat":2}},{"A":1,"c":{"$nat":1}}]
-- ok
