# erlang-chat-client-server
Pour lancer le chat serveur:
```erlang
c(chat_client).
c(chat_server).
chat_server:start(4000).
```

Pour connecter au serveur pour chatter via terminal:
```bash
telnet localhost 4000
```
