# erlchat

Getting started with sending messages around.

## setup

Install Erlang and OTP

## run

Make sure to `cd erlchat`.

Then run a server node. The node name needs to be explicitly `server` (it's hardcoded as `server@yourhostname`) and compile `simple` module.

```shell

$ erl -sname server
> c(simple).

```

Then run one or more client nodes and compile the module

```shell

$ erl -sname c1
> c(simple).

```

On the server node, start the server loop

```erlang

> simple:start_server().

```

On the client node, start the client loop

```erlang

> simple:start_client(server@yourhostname, "userA").

```

Start sending messages to other clients with

```erlang

> simple:client_send(server@yourhostname, "Hello everyone", "userA").

```

You can ping users, slack style

```erlang

> simple:client_send(server@yourhostname, "@userB @userC whatup?", "userA").

```




