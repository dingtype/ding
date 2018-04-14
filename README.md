# simplechat

Getting started with sending messages around.

## setup

Install Erlang and OTP

## run

Make sure to `cd erlchat`.

Then run a server node. The node name needs to be explicitly `server` (it's hardcoded as `server@yourhostname`) and compile the `server` module.

```bash

# This is the server node
$ erl -sname server
> c(server).

```

Then run one or more client nodes and compile the `client` module

```bash

# This is the client node
$ erl -sname c1
> c(client).

```

On the server node, start the server loop

```erlang

> server:start().

```

On the client node, start the client loop

```erlang

> client:logon(server@yourhostname, "userA").

```

Start sending messages to other clients with

```erlang

> client:send(server@yourhostname, "Hello everyone", "userA").

```

You can ping users slack style

```erlang

> client:send(server@yourhostname, "@userB hello @userC whatup?", "userA").

```

To logoff

```erlang

> client:logoff(server@yourhostname, "userA").

```


