create table thunks (
  id integer primary key not null,
  tid blob,
  sync blob
)

create table keys (
  id integer primary key not null,
  kid blob
)

create table thunk_read (
  thunk_id integer not null,
  key_id integer not null,
  foreign key (thunk_id) references thunks(id) on delete cascade,
  foreign key (key_id) references keys(id) on delete cascade,
  key_value blob,
  unique(thunk_id, key_id)
)

create table thunk_write (
  thunk_id integer not null,
  key_id integer not null,
  foreign key (thunk_id) references thunks(id) on delete cascade,
  foreign key (key_id) references keys(id) on delete cascade,
  key_value blob,
  unique(thunk_id, key_id)
)

create index if not exists KeyIndex on keys(kid);


-- PRAGMA schema.user_version;
-- PRAGMA foreign_keys = on;
