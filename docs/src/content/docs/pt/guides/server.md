---
title: Servidor
description: Executando o PetraDB como um servidor HTTP.
---

## Instalacao

```bash
npm install -g @petradb/server
```

## Uso

```bash
petradb-server [OPTIONS] [path]
```

Se um caminho for fornecido, o servidor abre (ou cria) um banco de dados persistente naquele local. Sem um caminho, um banco de dados em memoria e utilizado.

### Opcoes

| Opcao | Descricao |
|--------|-------------|
| `-m`, `--memory` | Usar um banco de dados em memoria |
| `-p`, `--port` | Numero da porta (padrao: `5480`) |
| `-h`, `--host` | Endereco do host (padrao: `127.0.0.1`) |
| `-c`, `--config` | Caminho para um arquivo de configuracao TOML |

### Exemplos

```bash
# Banco de dados em memoria na porta padrao
petradb-server

# Banco de dados persistente em uma porta personalizada
petradb-server -p 8080 mydata.db

# Com um arquivo de configuracao
petradb-server -c petradb.toml mydata.db
```

## Configuracao

Um arquivo TOML controla autenticacao, CORS e limites de sessao.

```toml
auth = "basic"

[[users]]
username = "admin"
password = "$HASHED_PASSWORD"

[[users]]
username = "reader"
password = "$HASHED_PASSWORD"

[cors]
origin = "*"          # "*" (padrao), "none", ou uma origem especifica

[sessions]
max_sessions = 100    # 0 = ilimitado (padrao)
```

### Modos de Autenticacao

| Modo | Descricao |
|------|-------------|
| `"none"` | Sem autenticacao (padrao quando nenhum arquivo de configuracao e fornecido) |
| `"basic"` | Autenticacao HTTP Basic contra a lista `[[users]]` |

Senhas no arquivo de configuracao sao armazenadas como hashes PBKDF2.

Quando `auth = "basic"`, toda requisicao (exceto `GET /health`) deve incluir um cabecalho `Authorization: Basic <credentials>`.

### CORS

| Valor de `origin` | Comportamento |
|----------------|-----------|
| `"*"` (padrao) | Permitir todas as origens |
| `"none"` | Sem cabecalhos CORS |
| Uma URL (ex: `"https://app.example.com"`) | Permitir apenas aquela origem |

## API HTTP

Todas as requisicoes e respostas SQL usam o codec binario do PetraDB (`application/octet-stream`). Use a biblioteca [`@petradb/client`](/guides/client/) em vez de chamar esses endpoints diretamente.

### Executar SQL

```
POST /sql
Content-Type: application/octet-stream
X-Session-Id: <session-id>   (opcional)

<sql text>
```

Retorna uma `Seq[Result]` codificada em binario. Sem um cabecalho `X-Session-Id`, cada requisicao executa em uma sessao transiente unica.

### Criar Sessao

```
POST /session
```

Retorna `{ "sessionId": "<id>" }`. Use o ID retornado nos cabecalhos `X-Session-Id` subsequentes para compartilhar estado transacional entre requisicoes.

### Fechar Sessao

```
DELETE /session/<id>
```

Retorna `{ "ok": "true" }` em caso de sucesso, ou `404` se a sessao nao existe.

### Verificacao de Saude

```
GET /health
```

Retorna `{ "status": "ok" }`. Nao esta sujeito a autenticacao.

## Respostas de Erro

| Status | Significado |
|--------|---------|
| 400 | Erro de parse SQL, erro de tipo ou referencia indefinida |
| 401 | Credenciais ausentes ou invalidas |
| 409 | Violacao de schema ou restricao |
| 503 | Limite de sessoes atingido |
