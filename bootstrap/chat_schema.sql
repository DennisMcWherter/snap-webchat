-- chat schema
-- Author: Dennis J. McWherter, Jr.
CREATE TABLE chat (
  id SERIAL NOT NULL PRIMARY KEY,
  user_id INTEGER NOT NULL REFERENCES snap_auth_user(uid),
  message VARCHAR(1024) NOT NULL,
  date TIMESTAMP WITHOUT TIME ZONE DEFAULT (NOW() AT TIME ZONE 'UTC')
);
CREATE INDEX ON chat(user_id);
