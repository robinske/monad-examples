# Scala Base App
**Bare-bones SBT-based project definition to get you started**

[Clone](https://help.github.com/articles/cloning-a-repository/) or [fork](https://help.github.com/articles/fork-a-repo/)
this repository to get started on your next Scala project.

#### Directory Structure
Every Scala project follows a similar directory structure. It's not required to format this way, but it's standard and one of
the subset of ways that SBT supports. For more information on the directory structure, see the [SBT docs](http://www.scala-sbt.org/0.13/docs/Directories.html).
```
.
├── LICENSE
├── README.md
├── build.sbt                        # define build configurations
├── project
│   └── build.properties             # define the SBT version
└── src
    ├── main                         # main directory of source code
    │   ├── resources                # place static files or other resources here
    │   └── scala                    # place scala code here
    │       └── tld
    │           └── domain
    │               └── Main.scala
    └── test                         # test directory, helps SBT find test code
        ├── resources                # place static test files or other resources here
        └── scala                    # place scala test code here
            └── tld
                └── domain
                    └── Spec.scala
```

##### What's with the `tld/domain` nested directories?

This is a convention taken from Maven/Java. The class path isn't as important for Scala code, but many people still
follow this convention.

Use the [tld](https://en.wikipedia.org/wiki/Top-level_domain) of your site (company, organization, project, or personal).

For example:
* My personal site: [http://krobinson.me](http://krobinson.me)
* My directory structure:
```
└── src
    ├── main
    │   ├── resources
    │   └── scala
    │       └── me
    │           └── krobinson
    │               └── Main.scala
```

The following directory structure also works if you don't want to include the organization:
```
└── src
    ├── main
    │   ├── resources
    │   └── scala
    │       └── Main.scala
```


#### Things you'll need to change:
* 'name' in [build.sbt](https://github.com/robinske/scala-base-app/blob/master/build.sbt) - make it your own project name
* The names of the folders in your directory structure (see above)


#### Configuring SBT

To set the sbt version, update scala-base-app/project/build.properties

Run SBT (`scala-base-app $ sbt`) from the top directory, or you're gonna have a bad time.

#### Useful SBT commands:
These are useful once you're already inside of sbt (after you've run '`$ sbt` from the top level of this directory)

| Command        | Notes                                                 |
| -------------- | ----------------------------------------------------- |
| **run**        | runs the main class (In this project, `NameTheMonth`  |
| **test**       | runs all the tests                                    |
| **test-quick** | runs a subset of tests based on what code has changed |
| **compile**    | compiles the code                                     |
| **reload**     | reloads changes to the `build.sbt` file               |

Note: prepend any of the above with `~` for re-evaluation. i.e. `~test-quick` will rerun the subset of affected tests every time code is changed.


#### Resources

This is an intentionally brief overview and a quick start to a Scala project. Here are some more resources to expand your Scala knowledge:
* https://twitter.github.io/scala_school/
* http://www.scala-lang.org/api/current/
* https://twitter.github.io/effectivescala/
* http://docs.scala-lang.org/
* http://www.scala-sbt.org/0.13/docs/


#### Challenges

If you're new to Scala and have no idea where to start, here are some ideas to expand this (forked or cloned) project for your own learning:
* Take user input for the date
* Print different formats
* Turn this into a web-application
* Consume an API ([http://www.timeapi.org/](http://www.timeapi.org/))
* As always, write more tests!

