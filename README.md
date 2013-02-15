About the package
=================
The purpose of this package is to allow 'guest' accounts that can use the server without registering. Guest accounts can only talk to accounts listed in the configuration file (i.e. no messages can go to them except from whitelisted accounts, and they can only send to whitelisted accounts).

In addition, for a subset of the whitelisted accounts (as specified in the configuration) the guest account will see the presence of the whitelisted account, and likewise the presence of the guest account will be sent to the specified account.

Using the module
================
Build the package as follows (adjusting the paths to your ejabberd source directory):

```bash
erlc -I /usr/src/ejabberd-2.1.11/src/ -pz /usr/src/ejabberd-2.1.11/src/ *.erl
```

Install the beam files into your ebin directory, for example with:

```bash
sudo cp *.beam /usr/local/lib/ejabberd/ebin/
```

Add guest to the list of auth methods in ejabberd.cfg; if you haven't changed it from the default, you might have to uncomment the line as well. It might read something like this (possibly with more alternative auth methods listed).

```erlang
{auth_method, [internal, guest]}.
```

Next, change the modules configuration to add mod_guest. You will want to find the list of modules in ejabberd.cfg, and add a new entry to the list (don't forget to add a comma after the last entry if you add it to the end):

```erlang
  {mod_guest, [{guest_access, [{"username1", true}, {"username2", false}]}]}
```
Note that you can have any number of entries in the inner list - the above says that username1@host (for all hosts supported by the server) and username2@host can talk to guests connected as guest-something@host (where something can be any string). The true after username1 means that all guests that log in are added to username1's roster with a two-way subscription, so they can see each other's presence status.

Things that still need doing
============================
Guests who log off currently stay permanently in the roster if they are added automatically (as for username1 above). It would be good to have code to automatically expire these roster entries so it doesn't get out of hand.
