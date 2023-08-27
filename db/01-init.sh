#!/bin/bash
set -e
export PGPASSWORD=$POSTGRES_PASSWORD;
psql -v ON_ERROR_STOP=1 --username "$POSTGRES_USER" --dbname "$POSTGRES_DB" <<-EOSQL
  CREATE USER $APP_DB_USER WITH PASSWORD '$APP_DB_PASS';
  CREATE DATABASE $APP_DB_NAME;
  GRANT ALL PRIVILEGES ON DATABASE $APP_DB_NAME TO $APP_DB_USER;
  \connect $APP_DB_NAME $APP_DB_USER
  BEGIN;
	CREATE TABLE IF NOT EXISTS users (
      user_id SERIAL PRIMARY KEY,
      cpf VARCHAR,
      username VARCHAR,
      hash VARCHAR
    );
    CREATE TABLE IF NOT EXISTS tasks (
      task_id SERIAL PRIMARY KEY,
      name VARCHAR,
      description TEXT,
      priority INT,
      status VARCHAR,
      user_id INT,
      FOREIGN KEY (user_id) REFERENCES users(user_id)
    );
  COMMIT;
EOSQL