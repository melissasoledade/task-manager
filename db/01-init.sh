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
      userId SERIAL PRIMARY KEY,
      cpf VARCHAR,
      username VARCHAR,
      hash VARCHAR
    );
    CREATE TABLE IF NOT EXISTS tasks (
      taskId SERIAL PRIMARY KEY,
      name VARCHAR,
      description TEXT,
      priority INT,
      taskStatus VARCHAR,
      userId INT,
      FOREIGN KEY (userId) REFERENCES users(userId)
    );
    INSERT INTO users (cpf, username, hash)
    VALUES ('12345678900', 'usuario1', 'hash1'),
           ('98765432100', 'usuario2', 'hash2');
    INSERT INTO tasks (name, description, priority, taskStatus, userId)
    VALUES ('Tarefa 1', 'Descrição da tarefa 1', 1, 'Em andamento', 1),
           ('Tarefa 2', 'Descrição da tarefa 2', 2, 'Concluída', 2);
  COMMIT;
EOSQL