# task-manager

## Como fazer o build e executar o projeto

1. Docker: será necessário instalar o docker e subir o container. Com o docker instalado, digite os comandos:
   `docker-compose build`
   `docker-compose up`
2. Faca o build do projeto executando `stack build`
3. Rode o projeto com `stack run`

OBS: caso haja alguma instância postgresql na porta 5432, o banco não irá conectar. Verificar com:
 `lsof -n -i:5432 | grep LISTEN`
 "Matar" todos os processos/pid's que possuam a instância de postgresql com `sudo kill numero_pid`