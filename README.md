bean
=====

A rebar plugin

Build
-----

    $ rebar3 compile

Use
---

Add the plugin to your rebar config:

    {plugins, [
        {bean, {git, "https://host/user/bean.git", {tag, "0.1.0"}}}
    ]}.

Then just call your plugin directly in an existing application:


    $ rebar3 bean
    ===> Fetching bean
    ===> Compiling bean
    <Plugin Output>
