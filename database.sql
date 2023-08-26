CREATE TABLE users (
    user_id SERIAL PRIMARY KEY,
    cpf VARCHAR,
    username VARCHAR,
    hash VARCHAR
);

CREATE TABLE tasks (
    task_id SERIAL PRIMARY KEY,
    name VARCHAR,
    description TEXT,
    priority INT,
    status VARCHAR,
    user_id INT,
    FOREIGN KEY (user_id) REFERENCES users(user_id)
);

-- Exemplo em users
INSERT INTO users (cpf, username, hash) VALUES
    ('12345678900', 'usuario1', 'hash1'),
    ('98765432100', 'usuario2', 'hash2');

-- Inserções em tasks
INSERT INTO tasks (name, description, priority, status, user_id) VALUES
    ('Tarefa 1', 'Descrição da tarefa 1', 1, 'Em andamento', 1),
    ('Tarefa 2', 'Descrição da tarefa 2', 2, 'Concluída', 2);
